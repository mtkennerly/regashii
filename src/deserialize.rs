use crate::{error, wine, Format, Key, KeyName, RawValue, ValueName};
use once_cell::sync::Lazy;
use regex::Regex;

mod parse;

mod group {
    pub const COMMENT: &str = "comment";
    pub const DATA: &str = "data";
    pub const ESCAPE: &str = "escape";
    pub const KEY: &str = "key";
    pub const OTHER: &str = "other";
    pub const QUOTE: &str = "quote";
}

macro_rules! regex {
    ($name:ident, $pattern:expr) => {
        static $name: Lazy<Regex> = Lazy::new(|| {
            #[allow(unused_imports)]
            use group::*;
            Regex::new(&format!($pattern)).unwrap()
        });
    };
}

pub fn unescape_wine_unicode(raw: &str) -> String {
    if !raw.contains(r"\x") {
        return raw.to_string();
    }

    regex!(
        UNICODE,
        r#"(?xs)
            (\\x (?<{DATA}> [0-9a-fA-F]{{4}}) )
            | (?<{OTHER}> . )
        "#
    );

    let mut points = vec![];
    for capture in UNICODE.captures_iter(raw) {
        if let Some(raw_code) = capture.name(group::DATA) {
            let Ok(code) = u16::from_str_radix(raw_code.as_str(), 16) else {
                continue;
            };
            points.push(code);
        } else if let Some(other) = capture.name(group::OTHER) {
            points.extend(other.as_str().encode_utf16());
        }
    }
    String::from_utf16_lossy(&points)
}

/// Normalize reg file content before deserializing.
///
/// * Removes `;` comments
/// * Concatenates hex lines that end with `,\`
pub fn normalize(text: &str) -> String {
    regex!(
        COMMENTS,
        r#"(?x)
            (?<{KEY}> \[.+\])
            | (?<{ESCAPE}> \\" )
            | (?<{QUOTE}> "(?: \\" | [^"] )*" )
            | (?<{COMMENT}> \s*;.* )
        "#
    );

    regex!(
        CONTINUATIONS,
        r#"(?x)
            (,\\[\r\n]+\s*)
        "#
    );

    let text = COMMENTS.replace_all(text, "${key}${escape}${quote}");
    let text = CONTINUATIONS.replace_all(&text, ",");

    text.to_string()
}

pub fn format(raw: &str) -> Result<Format, error::Deserialize> {
    match raw {
        Format::REGEDIT5 => Ok(Format::Regedit5),
        Format::REGEDIT4 => Ok(Format::Regedit4),
        Format::WINE2 => Ok(Format::Wine2),
        x => Err(error::Deserialize::UnknownFormat(x.to_string())),
    }
}

pub fn key(raw: &str, format: Format) -> Option<(KeyName, Key)> {
    parse::key(raw, format).ok()?.1
}

pub fn named_value(raw: &str, format: Format) -> Option<(ValueName, RawValue)> {
    let (input, (name, value)) = parse::named_value(raw, format).ok()?;
    input.is_empty().then_some((name, value))
}

#[cfg(test)]
pub fn value_name(raw: &str, format: Format) -> Option<ValueName> {
    Some(parse::value_name(raw, format).ok()?.1)
}

#[cfg(test)]
pub fn value(raw: &str, format: Format) -> Option<RawValue> {
    Some(parse::value_data(raw, format).ok()?.1)
}

pub fn wine_global_option(raw: &str) -> Option<wine::GlobalOption> {
    Some(parse::wine_global_option(raw).ok()?.1)
}

pub fn wine_key_option(raw: &str) -> Option<wine::KeyOption> {
    Some(parse::wine_key_option(raw).ok()?.1)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Kind;
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
    #[test_case("[foo;bar]", "[foo;bar]" ; "semicolon in key name")]
    #[test_case("[foo]bar]", "[foo]bar]" ; "bracket in key name")]
    #[test_case("[foo]bar;];", "[foo]bar;]" ; "semicolon and bracket in key name")]
    #[test_case("[foo\"bar]\n\"baz;\"=hex:00", "[foo\"bar]\n\"baz;\"=hex:00" ; "quote in key name")]
    fn normalization(input: &str, output: &str) {
        assert_eq!(output.to_string(), normalize(input))
    }

    #[test_case("[foo]", "foo", Key::new() ; "simple")]
    #[test_case(" [foo] ", "foo", Key::new() ; "outer spaces")]
    #[test_case("[foo ]", "foo ", Key::new() ; "inner trailing space")]
    #[test_case("[[baz]]", "[baz]", Key::new() ; "extra brackets")]
    #[test_case("[foo] bar", "foo", Key::new().with_addendum("bar".to_string()) ; "addendum")]
    #[test_case(r"[foo\bar]", r"foo\bar", Key::new() ; "one backslash")]
    #[test_case(r"[foo\\bar]", r"foo\bar", Key::new() ; "multiple backslashes")]
    fn valid_keys(raw: &str, name: &str, parsed: Key) {
        assert_eq!(Some((KeyName(name.to_string()), parsed)), key(raw, Format::Regedit5));
    }

    #[test_case("[]" ; "blank add key")]
    #[test_case("[-]" ; "blank delete key")]
    #[test_case("[ foo]" ; "inner leading space")]
    fn invalid_keys(raw: &str) {
        assert_eq!(None, key(raw, Format::Regedit5));
    }

    #[test_case(r#"foo\bar"#, r#"[foo\\bar]"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"[foo"bar]"# ; "quote")]
    #[test_case("foo\nbar", "[foo\nbar]" ; "new line")]
    #[test_case("foo\rbar", "[foo\rbar]" ; "carriage return")]
    #[test_case("foo\0bar", "[foo\0bar]" ; "null")]
    #[test_case("fooあbar", "[fooあbar]" ; "Unicode")]
    fn escaped_keys_regedit5(unescaped: &str, raw: &str) {
        assert_eq!(unescaped, &key(raw, Format::Regedit5).unwrap().0 .0);
    }

    #[test_case(r#"foo\bar"#, r#"[foo\\bar]"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"[foo"bar]"# ; "quote")]
    #[test_case(r#"fo[o]bar"#, r#"[fo\[o\]bar]"# ; "bracket")]
    #[test_case("fooあbar", r"[foo\x3042bar]" ; "Unicode")]
    #[test_case(r"Control Panel\International\🌎🌏🌍", r"[Control Panel\\International\\\xd83c\xdf0e\xd83c\xdf0f\xd83c\xdf0d]" ; "surrogate pair")]
    fn escaped_keys_wine(unescaped: &str, raw: &str) {
        assert_eq!(unescaped, &key(raw, Format::Wine2).unwrap().0 .0);
    }

    #[test_case("@=\"a\"", ValueName::Default, RawValue::Sz("a".to_string()) ; "simple")]
    #[test_case("  @ = \"b\" ", ValueName::Default, RawValue::Sz("b".to_string()) ; "whitespace")]
    #[test_case("\"eq=s\"=\"EQ=S\"", ValueName::Named("eq=s".to_string()), RawValue::Sz("EQ=S".to_string()) ; "quoted equal signs")]
    fn valid_named_values(raw: &str, name: ValueName, parsed: RawValue) {
        assert_eq!(Some((name, parsed)), named_value(raw, Format::Regedit5));
    }

    #[test_case("@", ValueName::Default ; "default")]
    #[test_case("  @ ", ValueName::Default ; "default whitespace")]
    #[test_case("\"foo\"", ValueName::Named("foo".to_string()) ; "sz simple")]
    #[test_case("\"eq=s\"", ValueName::Named("eq=s".to_string()) ; "sz inner equal signs")]
    #[test_case(r#""sp\\ec\"ial""#, ValueName::Named(r#"sp\ec"ial"#.to_string()) ; "sz escaped characters")]
    fn valid_value_names(raw: &str, parsed: ValueName) {
        assert_eq!(Some(parsed), value_name(raw, Format::Regedit5));
    }

    #[test_case("\"\"", RawValue::Sz("".to_string()) ; "sz empty")]
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
        assert_eq!(Some(parsed), value(raw, Format::Regedit5));
    }

    #[test_case("str:\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::Sz } ; "str sz")]
    #[test_case("str(2):\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::ExpandSz } ; "str expand")]
    #[test_case("str(7):\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::MultiSz } ; "str multi")]
    fn valid_values_wine(raw: &str, parsed: RawValue) {
        assert_eq!(Some(parsed), value(raw, Format::Wine2));
    }

    #[test_case("str(0):\"foo\"" ; "str invalid")]
    fn invalid_values(raw: &str) {
        assert_eq!(None, value(raw, Format::Regedit5));
    }

    #[test_case(r"", r#""""# ; "empty")]
    #[test_case(r#"foo\bar"#, r#""foo\\bar""# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#""foo\"bar""# ; "quote")]
    fn escaped_values_regedit5(unescaped: &str, raw: &str) {
        assert_eq!(
            RawValue::Sz(unescaped.to_string()),
            value(raw, Format::Regedit5).unwrap()
        );
    }

    #[test_case(r"", r#""""# ; "empty")]
    #[test_case(r#"foo\bar"#, r#""foo\\bar""# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#""foo\"bar""# ; "quote")]
    #[test_case("foo\nbar", r#""foo\nbar""# ; "new line")]
    #[test_case(r"foo\nx", r#""foo\\nx""# ; "backslash not new line")]
    #[test_case("foo\rbar", r#""foo\rbar""# ; "carriage return")]
    #[test_case(r"foo\rx", r#""foo\\rx""# ; "backslash not carriage return")]
    #[test_case("foo\0bar", r#""foo\0bar""# ; "null")]
    #[test_case("fooあbar", r#""foo\x3042bar""# ; "Unicode")]
    fn escaped_values_wine(unescaped: &str, raw: &str) {
        assert_eq!(RawValue::Sz(unescaped.to_string()), value(raw, Format::Wine2).unwrap());
    }

    #[test_case("#arch=win32", wine::GlobalOption::Arch("win32".to_string()) ; "arch")]
    #[test_case("#foo", wine::GlobalOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_global_options(raw: &str, parsed: wine::GlobalOption) {
        assert_eq!(Some(parsed), wine_global_option(raw));
    }

    #[test_case("#time=1f", wine::KeyOption::Time("1f".to_string()) ; "time")]
    #[test_case("#class=\"foo\"", wine::KeyOption::Class("foo".to_string()) ; "class")]
    #[test_case("#link", wine::KeyOption::Link ; "link")]
    #[test_case("#foo", wine::KeyOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_key_options(raw: &str, parsed: wine::KeyOption) {
        assert_eq!(Some(parsed), wine_key_option(raw));
    }
}
