use crate::{error, Format, Key, KeyName, Kind, RawValue, ValueName};
use once_cell::sync::Lazy;
use regex::Regex;

mod group {
    pub const DELETE: &str = "delete";
    pub const NAME: &str = "name";
    pub const DATA: &str = "data";
    pub const DEFAULT: &str = "default";
    pub const SZ: &str = "sz";
    pub const DWORD: &str = "dword";
    pub const HEX: &str = "hex";
    pub const KIND: &str = "kind";
}

fn unescape(raw: &str) -> String {
    raw.replace("\\\\", "\\").replace("\\\"", "\"")
}

/// Normalize reg file content before deserializing.
///
/// * Removes `;` comments
/// * Concatenates hex lines that end with `,\`
pub fn normalize(text: &str) -> String {
    static COMMENTS: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
                (?<escape> \\" )
                | (?P<quote> "(?: \\" | [^"] )*" )
                | (?P<comment> \s*;.* )
        "#,
        )
        .unwrap()
    });

    static CONTINUATIONS: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
            (,\\[\r\n]+\s*)
        "#,
        )
        .unwrap()
    });

    let text = COMMENTS.replace_all(text, "${escape}${quote}");
    let text = CONTINUATIONS.replace_all(&text, ",");

    text.to_string()
}

pub fn format(raw: &str) -> Result<Format, error::Deserialize> {
    match raw {
        Format::REGEDIT5 => Ok(Format::Regedit5),
        Format::REGEDIT4 => Ok(Format::Regedit4),
        x => Err(error::Deserialize::UnknownFormat(x.to_string())),
    }
}

pub fn key(raw: &str) -> Option<(KeyName, Key)> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r"(?x)
            ^\s*
            \[
            (?P<delete>-)?
            (?P<name>[^\s-].*)
            \]
        ",
        )
        .unwrap()
    });

    let caps = RE.captures(raw)?;
    let delete = caps.name(group::DELETE).is_some();
    let name = KeyName(caps.name(group::NAME)?.as_str().to_string());

    if delete {
        Some((name, Key::Delete))
    } else {
        Some((name, Key::new()))
    }
}

pub fn named_value(raw: &str) -> Option<(ValueName, RawValue)> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
            \s*
            (?P<name> @ | "([^"]|\\")+" )
            \s* = \s*
            (?P<data> .+ )
        "#,
        )
        .unwrap()
    });

    let caps = RE.captures(raw)?;
    let name = caps.name(group::NAME)?.as_str();
    let data = caps.name(group::DATA)?.as_str();

    Some((value_name(name.trim())?, value(data.trim())?))
}

pub fn value_name(raw: &str) -> Option<ValueName> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
            (?P<default> @ )
            | "(?P<name> ([^"\\]|\\.)+ )"
        "#,
        )
        .unwrap()
    });

    let caps = RE.captures(raw)?;

    if caps.name(group::DEFAULT).is_some() {
        Some(ValueName::Default)
    } else {
        let name = unescape(caps.name(group::NAME)?.as_str());
        Some(ValueName::Named(name))
    }
}

pub fn value(raw: &str) -> Option<RawValue> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
            ^
            (
                (?P<delete>-)
                | "(?P<sz> ([^"\\]|\\.)+ )"
                | dword: \s* (?P<dword>[0-9a-fA-Z]{8})
                | (?<hex>
                    hex
                    (\( (?P<kind>[0-9a-fA-F]) \))?
                    : \s*
                    (?P<data>
                        ([0-9a-fA-F]{1,2})(\s*,\s*[0-9a-fA-F]{1,2})*\s*,?\s*
                    )?
                )
            )
            $
        "#,
        )
        .unwrap()
    });

    let caps = RE.captures(raw)?;

    if caps.name(group::DELETE).is_some() {
        Some(RawValue::Delete)
    } else if let Some(sz) = caps.name(group::SZ) {
        Some(RawValue::Sz(unescape(sz.as_str())))
    } else if let Some(dword) = caps.name(group::DWORD) {
        let data = u32::from_str_radix(dword.as_str(), 16).ok()?;
        Some(RawValue::Dword(data))
    } else if caps.name(group::HEX).is_some() {
        let kind = match caps.name(group::KIND) {
            Some(kind) => Kind::from(u8::from_str_radix(kind.as_str(), 16).ok()?),
            None => Kind::Binary,
        };
        let bytes: Vec<_> = if let Some(data) = caps.name(group::DATA) {
            data.as_str()
                .split(',')
                .map(|x| x.trim())
                .filter_map(|x| u8::from_str_radix(x, 16).ok())
                .collect()
        } else {
            vec![]
        };
        Some(RawValue::Hex { kind, bytes })
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use test_case::test_case;

    #[test_case("Windows Registry Editor Version 5.00; blah", "Windows Registry Editor Version 5.00" ; "comment after format")]
    #[test_case("[HKEY_CURRENT_USER] ; blah", "[HKEY_CURRENT_USER]" ; "comment after key")]
    #[test_case("; blah", "" ; "whole line comment")]
    #[test_case("\"a;b\"=\"x;y\";foo", "\"a;b\"=\"x;y\"" ; "quoted semicolons")]
    #[test_case("\"a;b\\\"c\"=\"x\\\"y;z\";foo", "\"a;b\\\"c\"=\"x\\\"y;z\"" ; "quoted semicolons plus escaped quotes")]
    #[test_case("\"foo\"=hex:00,\\\n  01,\\\n  02", "\"foo\"=hex:00,01,02" ; "concatenated lines")]
    #[test_case("\"foo\"=hex:00,\\ ; blah\n  01; blah", "\"foo\"=hex:00,01" ; "concatenated line with comment")]
    #[test_case("\"x\"=dword:00000000\\", "\"x\"=dword:00000000\\" ; "stray backslash is preserved")]
    fn normalization(input: &str, output: &str) {
        assert_eq!(output.to_string(), normalize(input))
    }

    #[test_case("[foo]", "foo", Key::new() ; "simple")]
    #[test_case(" [foo] ", "foo", Key::new() ; "outer spaces")]
    #[test_case("[foo ]", "foo ", Key::new() ; "inner trailing space")]
    #[test_case("[[baz]]", "[baz]", Key::new() ; "extra brackets")]
    #[test_case("[foo] ; bar", "foo", Key::new() ; "ignored comment")]
    fn valid_keys(raw: &str, name: &str, parsed: Key) {
        assert_eq!(Some((KeyName(name.to_string()), parsed)), key(raw));
    }

    #[test_case("[]" ; "blank add key")]
    #[test_case("[-]" ; "blank delete key")]
    #[test_case("[ foo]" ; "inner leading space")]
    fn invalid_keys(raw: &str) {
        assert_eq!(None, key(raw));
    }

    #[test_case("@=\"a\"", ValueName::Default, RawValue::Sz("a".to_string()) ; "simple")]
    #[test_case("  @ = \"b\" ", ValueName::Default, RawValue::Sz("b".to_string()) ; "whitespace")]
    #[test_case("\"eq=s\"=\"EQ=S\"", ValueName::Named("eq=s".to_string()), RawValue::Sz("EQ=S".to_string()) ; "quoted equal signs")]
    fn valid_named_values(raw: &str, name: ValueName, parsed: RawValue) {
        assert_eq!(Some((name, parsed)), named_value(raw));
    }

    #[test_case("@", ValueName::Default ; "default")]
    #[test_case("  @ ", ValueName::Default ; "default whitespace")]
    #[test_case("\"foo\"", ValueName::Named("foo".to_string()) ; "sz simple")]
    #[test_case("\"eq=s\"", ValueName::Named("eq=s".to_string()) ; "sz inner equal signs")]
    #[test_case(r#""sp\\ec\"ial""#, ValueName::Named(r#"sp\ec"ial"#.to_string()) ; "sz escaped characters")]
    fn valid_value_names(raw: &str, parsed: ValueName) {
        assert_eq!(Some(parsed), value_name(raw));
    }

    #[test_case("\"foo\"", RawValue::Sz("foo".to_string()) ; "sz simple")]
    #[test_case(r#""sp\\ec\"ial""#, RawValue::Sz(r#"sp\ec"ial"#.to_string()) ; "sz escaped characters")]
    #[test_case("dword:00000000", RawValue::Dword(0) ; "dword 0")]
    #[test_case("dword:000000ff", RawValue::Dword(255) ; "dword 255")]
    #[test_case("-", RawValue::Delete ; "delete")]
    #[test_case("hex:", RawValue::Hex { kind: Kind::Binary, bytes: vec![] } ; "hex empty")]
    #[test_case("hex:61", RawValue::Hex { kind: Kind::Binary, bytes: vec![0x61] } ; "hex a")]
    #[test_case("hex(2):00", RawValue::Hex { kind: Kind::ExpandSz, bytes: vec![0] } ; "hex expand-sz empty")]
    #[test_case("hex(7):00", RawValue::Hex { kind: Kind::MultiSz, bytes: vec![0] } ; "hex multi-sz empty")]
    #[test_case("hex(b):00,00,00,00,00,00,00,00", RawValue::Hex { kind: Kind::Qword, bytes: vec![0, 0, 0, 0, 0, 0, 0, 0] } ; "hex qword 0")]
    #[test_case("hex(b):01,00,00,00,00,00,00,00", RawValue::Hex { kind: Kind::Qword, bytes: vec![1, 0, 0, 0, 0, 0, 0, 0] } ; "hex qword 1")]
    #[test_case("hex(b):ff,00,00,00,00,00,00,00", RawValue::Hex { kind: Kind::Qword, bytes: vec![255, 0, 0, 0, 0, 0, 0, 0] } ; "hex qword 255")]
    fn valid_values(raw: &str, parsed: RawValue) {
        assert_eq!(Some(parsed), value(raw));
    }
}
