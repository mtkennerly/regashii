use std::collections::BTreeMap;

use itertools::Itertools;

use crate::{Format, Key, KeyName, Kind, RawValue, ValueName};

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
    }
}

pub fn key(name: &KeyName, key: &Key, format: Format) -> String {
    match key {
        Key::Delete => format!("[-{}]", name.0),
        Key::Add(xs) => {
            let mut lines = vec![format!("[{}]", name.0)];

            let xs: BTreeMap<_, _> = xs.clone().into_iter().map(|(n, v)| (n, v.into_raw(format))).collect();
            lines.extend(values(&xs));

            lines.join("\n")
        }
        Key::Replace(xs) => {
            let mut lines = vec![format!("[-{}]\n", name.0), format!("[{}]", name.0)];

            let xs: BTreeMap<_, _> = xs.clone().into_iter().map(|(n, v)| (n, v.into_raw(format))).collect();
            lines.extend(values(&xs));

            lines.join("\n")
        }
    }
}

pub fn values(values: &BTreeMap<ValueName, RawValue>) -> Vec<String> {
    let mut lines = vec![];

    for (name, val) in values {
        let name = value_name(name);
        let value = value(val);
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

pub fn value(value: &RawValue) -> String {
    match value {
        RawValue::Delete => "-".to_string(),
        RawValue::Sz(data) => quoted(&escape(data)),
        RawValue::Dword(data) => format!("dword:{:0>8x}", data),
        RawValue::Hex { kind, bytes } => {
            let bytes = bytes.iter().map(|x| format!("{:0>2x}", x)).join(",");
            match kind {
                Kind::Binary => format!("hex:{}", bytes),
                kind => format!("hex({:x}):{}", u8::from(*kind), bytes),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use test_case::test_case;

    #[test_case("[foo]", "foo", Key::new() ; "add")]
    #[test_case("[-foo]", "foo", Key::Delete ; "delete")]
    fn invalid_keys(raw: &str, name: &str, parsed: Key) {
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
        assert_eq!(raw, value(&parsed));
    }
}
