use std::collections::{BTreeMap, BTreeSet};

use crate::{wine, Format, Key, KeyName, Kind, RawValue, ValueName};

fn quoted(raw: &str) -> String {
    format!("\"{raw}\"")
}

fn escape_key(raw: &str, format: Format) -> String {
    match format {
        Format::Regedit5 | Format::Regedit4 => raw.to_string(),
        Format::Wine2 => {
            let escaped = raw.replace('\\', r"\\").replace('[', r"\[").replace(']', r"\]");
            escape_wine_unicode(&escaped)
        }
    }
}

fn escape_value(raw: &str, format: Format) -> String {
    let mut escaped = raw.replace('\\', r"\\").replace('"', r#"\""#);

    if format.is_wine() {
        escaped = escaped.replace('\0', r"\0").replace('\n', r"\n").replace('\r', r"\r");
        escaped = escape_wine_unicode(&escaped);
    }

    escaped
}

fn escape_wine_unicode(raw: &str) -> String {
    if raw.is_ascii() {
        return raw.to_string();
    }

    let mut ascii = String::new();
    for point in raw.encode_utf16() {
        if point < 128 {
            ascii.push(point as u8 as char);
        } else {
            ascii.push_str(&format!("\\x{:x}", point));
        }
    }
    ascii
}

pub fn format(format: Format) -> &'static str {
    match format {
        Format::Regedit5 => Format::REGEDIT5,
        Format::Regedit4 => Format::REGEDIT4,
        Format::Wine2 => Format::WINE2,
    }
}

pub fn key_name(name: &KeyName, format: Format) -> String {
    match format {
        Format::Regedit5 | Format::Regedit4 => name.0.clone(),
        Format::Wine2 => escape_key(&name.0, format),
    }
}

pub fn key_added(name: &KeyName, addendum: Option<&String>, format: Format) -> String {
    let name = key_name(name, format);

    match addendum {
        Some(addendum) => format!("[{name}] {addendum}"),
        None => format!("[{name}]"),
    }
}

pub fn key_deleted(name: &KeyName, format: Format) -> String {
    let name = key_name(name, format);
    format!("[-{name}]")
}

pub fn key(name: &KeyName, key: &Key, format: Format) -> String {
    match key {
        Key::Delete => key_deleted(name, format),
        Key::Add {
            values,
            addendum,
            wine_options,
        } => {
            let mut lines = vec![key_added(name, addendum.as_ref(), format)];

            if format.is_wine() {
                lines.extend(wine_key_options(wine_options));
            }

            let values: BTreeMap<_, _> = values
                .clone()
                .into_iter()
                .map(|(n, v)| (n, v.into_raw(format)))
                .collect();
            lines.extend(self::values(&values, format));

            lines.join("\n")
        }
        Key::Replace {
            values,
            addendum,
            wine_options,
        } => {
            let mut lines = vec![
                key_deleted(name, format),
                "".to_string(),
                key_added(name, addendum.as_ref(), format),
            ];

            if format.is_wine() {
                lines.extend(wine_key_options(wine_options));
            }

            let values: BTreeMap<_, _> = values
                .clone()
                .into_iter()
                .map(|(n, v)| (n, v.into_raw(format)))
                .collect();
            lines.extend(self::values(&values, format));

            lines.join("\n")
        }
    }
}

pub fn values(values: &BTreeMap<ValueName, RawValue>, format: Format) -> Vec<String> {
    let mut lines = vec![];

    for (name, val) in values {
        let name = value_name(name, format);
        let value = value(val, name.len() + 1, format);
        lines.push(format!("{name}={value}"));
    }

    lines
}

pub fn value_name(name: &ValueName, format: Format) -> String {
    match name {
        ValueName::Default => "@".to_string(),
        ValueName::Named(name) => quoted(&escape_value(name, format)),
    }
}

pub fn value(value: &RawValue, offset: usize, format: Format) -> String {
    match value {
        RawValue::Delete => "-".to_string(),
        RawValue::Sz(data) => quoted(&escape_value(data, format)),
        RawValue::Dword(data) => format!("dword:{:0>8x}", data),
        RawValue::Str { kind, data } => match kind {
            Kind::Sz => format!("str:{}", quoted(&escape_value(data, format))),
            kind => format!("str({:x}):{}", u8::from(*kind), quoted(&escape_value(data, format))),
        },
        RawValue::Hex { kind, bytes } => {
            let mut out = String::new();

            match kind {
                Kind::Binary => out.push_str("hex:"),
                kind => out.push_str(&format!("hex({:x}):", u8::from(*kind))),
            }

            let mut running_len = offset + out.len();
            for (i, byte) in bytes.iter().map(|x| format!("{:0>2x}", x)).enumerate() {
                out.push_str(&byte);
                running_len += 2;

                if i + 1 < bytes.len() {
                    out.push(',');
                    running_len += 1;

                    if running_len > 76 {
                        out.push_str("\\\n  ");
                        running_len = 2;
                    }
                }
            }

            out
        }
    }
}

pub fn wine_global_options(options: &BTreeSet<wine::GlobalOption>) -> Vec<String> {
    let mut lines = vec![];

    for option in options {
        lines.push(wine_global_option(option));
    }

    lines
}

pub fn wine_global_option(option: &wine::GlobalOption) -> String {
    match option {
        wine::GlobalOption::Arch(arch) => format!("#arch={arch}"),
        wine::GlobalOption::Other(other) => format!("#{other}"),
    }
}

pub fn wine_key_options(options: &BTreeSet<wine::KeyOption>) -> Vec<String> {
    let mut lines = vec![];

    for option in options {
        lines.push(wine_key_option(option));
    }

    lines
}

pub fn wine_key_option(option: &wine::KeyOption) -> String {
    match option {
        wine::KeyOption::Class(class) => format!("#class=\"{class}\""),
        wine::KeyOption::Time(time) => format!("#time={time}"),
        wine::KeyOption::Link => "#link".to_string(),
        wine::KeyOption::Other(other) => format!("#{other}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use test_case::test_case;

    #[test_case("", "" ; "empty")]
    #[test_case(r#"foo\bar"#, r#"foo\\bar"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"foo"bar"# ; "quote")]
    #[test_case(r#"fo[o]bar"#, r#"fo\[o\]bar"# ; "bracket")]
    #[test_case("fooあbar", r"foo\x3042bar" ; "Unicode")]
    #[test_case(r"Control Panel\International\🌎🌏🌍", r"Control Panel\\International\\\xd83c\xdf0e\xd83c\xdf0f\xd83c\xdf0d" ; "surrogate pair")]
    fn escape_key_wine2(raw: &str, escaped: &str) {
        assert_eq!(escaped.to_string(), escape_key(raw, Format::Wine2));
    }

    #[test_case("", "" ; "empty")]
    #[test_case(r#"foo\bar"#, r#"foo\\bar"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"foo\"bar"# ; "quote")]
    #[test_case("foo\nbar", "foo\nbar" ; "new line")]
    #[test_case("foo\rbar", "foo\rbar" ; "carriage return")]
    #[test_case("foo\0bar", "foo\0bar" ; "null")]
    #[test_case("fooあbar", "fooあbar" ; "Unicode")]
    fn escape_value_regedit5(raw: &str, escaped: &str) {
        assert_eq!(escaped.to_string(), escape_value(raw, Format::Regedit5));
    }

    #[test_case("", "" ; "empty")]
    #[test_case(r#"foo\bar"#, r#"foo\\bar"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"foo\"bar"# ; "quote")]
    #[test_case("foo\nbar", r"foo\nbar" ; "new line")]
    #[test_case("foo\rbar", r"foo\rbar" ; "carriage return")]
    #[test_case("foo\0bar", r"foo\0bar" ; "null")]
    #[test_case("fooあbar", r"foo\x3042bar" ; "Unicode")]
    fn escape_value_wine2(raw: &str, escaped: &str) {
        assert_eq!(escaped.to_string(), escape_value(raw, Format::Wine2));
    }

    #[test_case("[foo]", "foo", Key::new() ; "add")]
    #[test_case("[-foo]", "foo", Key::Delete ; "delete")]
    #[test_case("[foo] bar", "foo", Key::new().with_addendum("bar".to_string()) ; "addendum")]
    #[test_case(r"[foo\bar]", r"foo\bar", Key::new() ; "one backslash")]
    #[test_case(r"[foo\bar]", r"foo\\bar", Key::new() ; "multiple backslashes")]
    fn valid_keys(raw: &str, name: &str, parsed: Key) {
        assert_eq!(raw.to_string(), key(&KeyName::new(name), &parsed, Format::Regedit5));
    }

    #[test_case("@", ValueName::Default ; "default")]
    #[test_case("\"foo\"", ValueName::Named("foo".to_string()) ; "sz simple")]
    #[test_case("\"eq=s\"", ValueName::Named("eq=s".to_string()) ; "sz inner equal signs")]
    #[test_case(r#""sp\\ec\"ial""#, ValueName::Named(r#"sp\ec"ial"#.to_string()) ; "sz escaped characters")]
    fn valid_value_names(raw: &str, parsed: ValueName) {
        assert_eq!(raw.to_string(), value_name(&parsed, Format::Regedit5));
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
        assert_eq!(raw, value(&parsed, 0, Format::Regedit5));
    }

    #[test_case("#arch=win32", wine::GlobalOption::Arch("win32".to_string()) ; "arch")]
    #[test_case("#foo", wine::GlobalOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_global_options(raw: &str, parsed: wine::GlobalOption) {
        assert_eq!(raw, wine_global_option(&parsed));
    }

    #[test_case("#time=1f", wine::KeyOption::Time("1f".to_string()) ; "time")]
    #[test_case("#class=\"foo\"", wine::KeyOption::Class("foo".to_string()) ; "class")]
    #[test_case("#link", wine::KeyOption::Link ; "link")]
    #[test_case("#foo", wine::KeyOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_key_options(raw: &str, parsed: wine::KeyOption) {
        assert_eq!(raw, wine_key_option(&parsed));
    }
}
