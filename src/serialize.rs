use std::collections::{BTreeMap, BTreeSet};

use crate::{Format, Key, KeyName, Kind, RawValue, ValueName, WineGlobalOption, WineKeyOption};

fn quoted(raw: &str) -> String {
    format!("\"{raw}\"")
}

fn escape(raw: &str) -> String {
    raw.replace('\\', "\\\\").replace('"', "\\\"")
}

pub fn format(format: Format) -> &'static str {
    match format {
        Format::Regedit5 => Format::REGEDIT5,
        Format::Regedit4 => Format::REGEDIT4,
        Format::Wine2 => Format::WINE2,
    }
}

pub fn key_name(name: &KeyName, addendum: Option<&String>) -> String {
    let name = &name.0;

    match addendum {
        Some(addendum) => format!("[{name}] {addendum}"),
        None => format!("[{name}]"),
    }
}

pub fn key_name_deleted(name: &KeyName) -> String {
    format!("[-{}]", name.0)
}

pub fn key(name: &KeyName, key: &Key, format: Format) -> String {
    match key {
        Key::Delete => key_name_deleted(name),
        Key::Add {
            values,
            addendum,
            wine_options,
        } => {
            let mut lines = vec![key_name(name, addendum.as_ref())];

            if format.is_wine() {
                lines.extend(wine_key_options(wine_options));
            }

            let values: BTreeMap<_, _> = values
                .clone()
                .into_iter()
                .map(|(n, v)| (n, v.into_raw(format)))
                .collect();
            lines.extend(self::values(&values));

            lines.join("\n")
        }
        Key::Replace {
            values,
            addendum,
            wine_options,
        } => {
            let mut lines = vec![
                key_name_deleted(name),
                "".to_string(),
                key_name(name, addendum.as_ref()),
            ];

            if format.is_wine() {
                lines.extend(wine_key_options(wine_options));
            }

            let values: BTreeMap<_, _> = values
                .clone()
                .into_iter()
                .map(|(n, v)| (n, v.into_raw(format)))
                .collect();
            lines.extend(self::values(&values));

            lines.join("\n")
        }
    }
}

pub fn values(values: &BTreeMap<ValueName, RawValue>) -> Vec<String> {
    let mut lines = vec![];

    for (name, val) in values {
        let name = value_name(name);
        let value = value(val, name.len() + 1);
        lines.push(format!("{name}={value}"));
    }

    lines
}

pub fn value_name(name: &ValueName) -> String {
    match name {
        ValueName::Default => "@".to_string(),
        ValueName::Named(name) => quoted(&escape(name)),
    }
}

pub fn value(value: &RawValue, offset: usize) -> String {
    match value {
        RawValue::Delete => "-".to_string(),
        RawValue::Sz(data) => quoted(&escape(data)),
        RawValue::Dword(data) => format!("dword:{:0>8x}", data),
        RawValue::Str { kind, data } => match kind {
            Kind::Sz => format!("str:{}", quoted(&escape(data))),
            kind => format!("str({:x}):{}", u8::from(*kind), quoted(&escape(data))),
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

pub fn wine_global_options(options: &BTreeSet<WineGlobalOption>) -> Vec<String> {
    let mut lines = vec![];

    for option in options {
        lines.push(wine_global_option(option));
    }

    lines
}

pub fn wine_global_option(option: &WineGlobalOption) -> String {
    match option {
        WineGlobalOption::Arch(arch) => format!("#arch={arch}"),
        WineGlobalOption::Other(other) => format!("#{other}"),
    }
}

pub fn wine_key_options(options: &BTreeSet<WineKeyOption>) -> Vec<String> {
    let mut lines = vec![];

    for option in options {
        lines.push(wine_key_option(option));
    }

    lines
}

pub fn wine_key_option(option: &WineKeyOption) -> String {
    match option {
        WineKeyOption::Class(class) => format!("#class=\"{class}\""),
        WineKeyOption::Time(time) => format!("#time={time}"),
        WineKeyOption::Link => "#link".to_string(),
        WineKeyOption::Other(other) => format!("#{other}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use test_case::test_case;

    #[test_case("[foo]", "foo", Key::new() ; "add")]
    #[test_case("[-foo]", "foo", Key::Delete ; "delete")]
    #[test_case("[foo] bar", "foo", Key::new().with_addendum("bar".to_string()) ; "addendum")]
    fn valid_keys(raw: &str, name: &str, parsed: Key) {
        assert_eq!(
            raw.to_string(),
            key(&KeyName(name.to_string()), &parsed, Format::Regedit5)
        );
    }

    #[test_case("@", ValueName::Default ; "default")]
    #[test_case("\"foo\"", ValueName::Named("foo".to_string()) ; "sz simple")]
    #[test_case("\"eq=s\"", ValueName::Named("eq=s".to_string()) ; "sz inner equal signs")]
    #[test_case(r#""sp\\ec\"ial""#, ValueName::Named(r#"sp\ec"ial"#.to_string()) ; "sz escaped characters")]
    fn valid_value_names(raw: &str, parsed: ValueName) {
        assert_eq!(raw.to_string(), value_name(&parsed));
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
        assert_eq!(raw, value(&parsed, 0));
    }

    #[test_case("#arch=win32", WineGlobalOption::Arch("win32".to_string()) ; "arch")]
    #[test_case("#foo", WineGlobalOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_global_options(raw: &str, parsed: WineGlobalOption) {
        assert_eq!(raw, wine_global_option(&parsed));
    }

    #[test_case("#time=100", WineKeyOption::Time(100) ; "time")]
    #[test_case("#class=\"foo\"", WineKeyOption::Class("foo".to_string()) ; "class")]
    #[test_case("#link", WineKeyOption::Link ; "link")]
    #[test_case("#foo", WineKeyOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_key_options(raw: &str, parsed: WineKeyOption) {
        assert_eq!(raw, wine_key_option(&parsed));
    }
}
