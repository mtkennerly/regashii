use crate::{error, wine, Format, Key, KeyName, Kind, RawValue, ValueName};
use once_cell::sync::Lazy;
use regex::Regex;

mod group {
    pub const DELETE: &str = "delete";
    pub const NAME: &str = "name";
    pub const DATA: &str = "data";
    pub const DEFAULT: &str = "default";
    pub const SZ: &str = "sz";
    pub const STR: &str = "str";
    pub const STR_KIND: &str = "str_kind";
    pub const STR_DATA: &str = "str_data";
    pub const DWORD: &str = "dword";
    pub const HEX: &str = "hex";
    pub const KIND: &str = "kind";
    pub const ADDENDUM: &str = "addendum";
    pub const ARCH: &str = "arch";
    pub const CLASS: &str = "class";
    pub const TIME: &str = "time";
    pub const LINK: &str = "link";
    pub const OTHER: &str = "other";
    pub const ESCAPE_BACKSLASH: &str = "escape_bs";
    pub const ESCAPE_NEW_LINE: &str = "escape_nl";
    pub const ESCAPE_CARRIAGE_RETURN: &str = "escape_cr";
    pub const ESCAPE_NULL: &str = "escape_null";
    pub const ESCAPE_QUOTE: &str = "escape_quote";
    pub const ESCAPE_BRACKET_OPEN: &str = "escape_bracket1";
    pub const ESCAPE_BRACKET_CLOSE: &str = "escape_bracket2";
}

fn unescape_key(raw: &str, format: Format) -> String {
    if format.is_wine() {
        static ESCAPES: Lazy<Regex> = Lazy::new(|| {
            use group::*;
            Regex::new(&format!(
                r#"(?x)
                (?<{ESCAPE_BACKSLASH}> \\\\)
                | (?<{ESCAPE_BRACKET_OPEN}> \\\[ )
                | (?<{ESCAPE_BRACKET_CLOSE}> \\\] )
            "#
            ))
            .unwrap()
        });

        let normalized = ESCAPES.replace_all(raw, |captures: &regex::Captures| {
            if captures.name(group::ESCAPE_BACKSLASH).is_some() {
                "\\"
            } else if captures.name(group::ESCAPE_BRACKET_OPEN).is_some() {
                "["
            } else if captures.name(group::ESCAPE_BRACKET_CLOSE).is_some() {
                "]"
            } else {
                ""
            }
        });

        unescape_wine_unicode(&normalized)
    } else {
        raw.to_string()
    }
}

fn unescape_value(raw: &str, format: Format) -> String {
    if format.is_wine() {
        static ESCAPES: Lazy<Regex> = Lazy::new(|| {
            use group::*;
            Regex::new(&format!(
                r#"(?x)
                (?<{ESCAPE_BACKSLASH}> \\\\)
                | (?<{ESCAPE_QUOTE}> \\" )
                | (?<{ESCAPE_NEW_LINE}> \\n )
                | (?<{ESCAPE_CARRIAGE_RETURN}> \\r )
                | (?<{ESCAPE_NULL}> \\0 )
            "#
            ))
            .unwrap()
        });

        let normalized = ESCAPES.replace_all(raw, |captures: &regex::Captures| {
            if captures.name(group::ESCAPE_BACKSLASH).is_some() {
                "\\"
            } else if captures.name(group::ESCAPE_QUOTE).is_some() {
                "\""
            } else if captures.name(group::ESCAPE_NEW_LINE).is_some() {
                "\n"
            } else if captures.name(group::ESCAPE_CARRIAGE_RETURN).is_some() {
                "\r"
            } else if captures.name(group::ESCAPE_NULL).is_some() {
                "\0"
            } else {
                ""
            }
        });

        unescape_wine_unicode(&normalized)
    } else {
        static ESCAPES: Lazy<Regex> = Lazy::new(|| {
            use group::*;
            Regex::new(&format!(
                r#"(?x)
                (?<{ESCAPE_BACKSLASH}> \\\\)
                | (?<{ESCAPE_QUOTE}> \\" )
            "#
            ))
            .unwrap()
        });

        ESCAPES
            .replace_all(raw, |captures: &regex::Captures| {
                if captures.name(group::ESCAPE_BACKSLASH).is_some() {
                    "\\"
                } else if captures.name(group::ESCAPE_QUOTE).is_some() {
                    "\""
                } else {
                    ""
                }
            })
            .to_string()
    }
}

fn unescape_wine_unicode(raw: &str) -> String {
    static UNICODE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?xs)
            (\\x (?P<data> [0-9a-fA-F]{4}) )
            | (?<other> . )
        "#,
        )
        .unwrap()
    });

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
    static COMMENTS: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
                (?<key> \[.+\])
                | (?<escape> \\" )
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
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r"(?x)
            ^\s*
            \[
            (?P<delete>-)?
            (?P<name>[^\s-].*)
            \]
            \s*
            (?P<addendum> [^;]+)?
        ",
        )
        .unwrap()
    });

    let caps = RE.captures(raw)?;
    let delete = caps.name(group::DELETE).is_some();
    let name = KeyName::new(unescape_key(caps.name(group::NAME)?.as_str(), format));
    let addendum = caps.name(group::ADDENDUM).map(|x| x.as_str().trim().to_string());

    if delete {
        Some((name, Key::Delete))
    } else if let Some(addendum) = addendum {
        Some((name, Key::new().with_addendum(addendum)))
    } else {
        Some((name, Key::new()))
    }
}

pub fn named_value(raw: &str, format: Format) -> Option<(ValueName, RawValue)> {
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

    Some((value_name(name.trim(), format)?, value(data.trim(), format)?))
}

pub fn value_name(raw: &str, format: Format) -> Option<ValueName> {
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
        let name = unescape_value(caps.name(group::NAME)?.as_str(), format);
        Some(ValueName::Named(name))
    }
}

pub fn value(raw: &str, format: Format) -> Option<RawValue> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
            ^
            (
                (?P<delete>-)
                | "(?P<sz> ([^"\\]|\\.)* )"
                | (?P<str>
                    str
                    (\( (?P<str_kind>[0-9a-fA-F]) \))?
                    : \s*
                    "(?P<str_data> ([^"\\]|\\.)* )"
                  )
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
        Some(RawValue::Sz(unescape_value(sz.as_str(), format)))
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
    } else if caps.name(group::STR).is_some() {
        let kind = caps
            .name(group::STR_KIND)
            .and_then(|kind| u8::from_str_radix(kind.as_str(), 16).ok())
            .map(Kind::from)
            .unwrap_or(Kind::Sz);

        match kind {
            Kind::Sz | Kind::ExpandSz | Kind::MultiSz => {}
            _ => return None,
        }

        let data = caps
            .name(group::STR_DATA)
            .map(|data| unescape_value(data.as_str(), format))?;

        Some(RawValue::Str { kind, data })
    } else {
        None
    }
}

pub fn wine_global_option(raw: &str) -> Option<wine::GlobalOption> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
            ^
            \#(
                arch=(?P<arch> .+)
                | (?P<other> .+)
            )
            $
        "#,
        )
        .unwrap()
    });

    let caps = RE.captures(raw)?;

    if let Some(arch) = caps.name(group::ARCH) {
        Some(wine::GlobalOption::Arch(arch.as_str().to_string()))
    } else {
        caps.name(group::OTHER)
            .map(|other| wine::GlobalOption::Other(other.as_str().to_string()))
    }
}

pub fn wine_key_option(raw: &str) -> Option<wine::KeyOption> {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?x)
            ^
            \#(
                class="(?P<class> [^"]+)"
                | time=(?P<time> [0-9a-fA-F]+)
                | (?P<link> link)
                | (?P<other> .+)
            )
            $
        "#,
        )
        .unwrap()
    });

    let caps = RE.captures(raw)?;

    if let Some(class) = caps.name(group::CLASS) {
        Some(wine::KeyOption::Class(class.as_str().to_string()))
    } else if let Some(time) = caps.name(group::TIME) {
        Some(wine::KeyOption::Time(time.as_str().to_string()))
    } else if caps.name(group::LINK).is_some() {
        Some(wine::KeyOption::Link)
    } else {
        caps.name(group::OTHER)
            .map(|other| wine::KeyOption::Other(other.as_str().to_string()))
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
    #[test_case("[foo;bar]", "[foo;bar]" ; "semicolon in key name")]
    #[test_case("[foo]bar]", "[foo]bar]" ; "bracket in key name")]
    #[test_case("[foo]bar;];", "[foo]bar;]" ; "semicolon and bracket in key name")]
    #[test_case("[foo\"bar]\n\"baz;\"=hex:00", "[foo\"bar]\n\"baz;\"=hex:00" ; "quote in key name")]
    fn normalization(input: &str, output: &str) {
        assert_eq!(output.to_string(), normalize(input))
    }

    #[test_case("", "" ; "empty")]
    #[test_case(r#"foo\bar"#, r#"foo\\bar"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"foo"bar"# ; "quote")]
    #[test_case(r#"fo[o]bar"#, r#"fo\[o\]bar"# ; "bracket")]
    #[test_case("fooあbar", r"foo\x3042bar" ; "Unicode")]
    #[test_case(r"Control Panel\International\🌎🌏🌍", r"Control Panel\\International\\\xd83c\xdf0e\xd83c\xdf0f\xd83c\xdf0d" ; "surrogate pair")]
    fn unescape_key_wine2(unescaped: &str, raw: &str) {
        assert_eq!(unescaped.to_string(), unescape_key(raw, Format::Wine2));
    }

    #[test_case("", "" ; "empty")]
    #[test_case(r#"foo\bar"#, r#"foo\\bar"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"foo\"bar"# ; "quote")]
    #[test_case("foo\nbar", "foo\nbar" ; "new line")]
    #[test_case("foo\rbar", "foo\rbar" ; "carriage return")]
    #[test_case("foo\0bar", "foo\0bar" ; "null")]
    #[test_case("fooあbar", "fooあbar" ; "Unicode")]
    fn unescape_value_regedit5(unescaped: &str, raw: &str) {
        assert_eq!(unescaped.to_string(), unescape_value(raw, Format::Regedit5));
    }

    #[test_case("", "" ; "empty")]
    #[test_case(r#"foo\bar"#, r#"foo\\bar"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"foo\"bar"# ; "quote")]
    #[test_case("foo\nbar", r"foo\nbar" ; "new line")]
    #[test_case(r"foo\nx", r"foo\\nx" ; "backsash not new line")]
    #[test_case("foo\rbar", r"foo\rbar" ; "carriage return")]
    #[test_case(r"foo\rx", r"foo\\rx" ; "backslash not carriage return")]
    #[test_case("foo\0bar", r"foo\0bar" ; "null")]
    #[test_case("fooあbar", r"foo\x3042bar" ; "Unicode")]
    fn unescape_value_wine2(unescaped: &str, raw: &str) {
        assert_eq!(unescaped.to_string(), unescape_value(raw, Format::Wine2));
    }

    #[test_case("[foo]", "foo", Key::new() ; "simple")]
    #[test_case(" [foo] ", "foo", Key::new() ; "outer spaces")]
    #[test_case("[foo ]", "foo ", Key::new() ; "inner trailing space")]
    #[test_case("[[baz]]", "[baz]", Key::new() ; "extra brackets")]
    #[test_case("[foo] bar ; baz", "foo", Key::new().with_addendum("bar".to_string()) ; "ignored comment")]
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

    #[test_case("\"foo\"", RawValue::Sz("foo".to_string()) ; "sz simple")]
    #[test_case(r#""sp\\ec\"ial""#, RawValue::Sz(r#"sp\ec"ial"#.to_string()) ; "sz escaped characters")]
    #[test_case("str:\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::Sz } ; "str sz")]
    #[test_case("str(2):\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::ExpandSz } ; "str expand")]
    #[test_case("str(7):\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::MultiSz } ; "str multi")]
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

    #[test_case("str(0):\"foo\"" ; "str invalid")]
    fn invalid_values(raw: &str) {
        assert_eq!(None, value(raw, Format::Regedit5));
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
