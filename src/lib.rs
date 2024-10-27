#![doc = include_str!("../README.md")]

mod deserialize;
pub mod error;
mod etc;
mod serialize;
pub mod wine;

use std::{
    collections::{BTreeMap, BTreeSet},
    path::Path,
};

/// Main struct for all *.reg file content.
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Registry {
    format: Format,
    keys: BTreeMap<KeyName, Key>,
    // Lowercase -> stored.
    key_names: BTreeMap<KeyName, KeyName>,
    wine_options: BTreeSet<wine::GlobalOption>,
}

impl Registry {
    /// Initialize with a specific format.
    pub fn new(format: Format) -> Self {
        Self {
            format,
            ..Default::default()
        }
    }

    /// Add or update a key (method chain style).
    /// Will try to reuse an equivalent existing key name, if any.
    pub fn with(mut self, requested_name: impl Into<KeyName>, key: Key) -> Self {
        self.update(requested_name.into(), key);
        self
    }

    /// Add a Wine option (method chain style).
    pub fn with_wine_option(mut self, option: wine::GlobalOption) -> Self {
        self.wine_options.insert(option);
        self
    }

    /// Access the raw representation of keys.
    /// You can use this if you want to bypass key name normalization.
    pub fn keys(&self) -> &BTreeMap<KeyName, Key> {
        &self.keys
    }

    /// Access the raw representation of keys mutably.
    /// You can use this if you want to bypass key name normalization.
    pub fn keys_mut(&mut self) -> &mut BTreeMap<KeyName, Key> {
        &mut self.keys
    }

    fn insert_key(&mut self, name: KeyName, key: Key) {
        self.key_names.insert(name.to_lowercase(), name.clone());
        self.keys.insert(name, key);
    }

    /// Add or update a key.
    /// Will try to reuse an equivalent existing key name, if any.
    pub fn update(&mut self, requested_name: KeyName, key: Key) {
        let lookup = self.key_name(&requested_name);

        if let Some(mut stored) = self.keys.remove(&lookup) {
            match (stored.kind, key.kind) {
                (_, KeyKind::Delete) => {
                    self.insert_key(requested_name, Key::deleted());
                }
                (KeyKind::Delete, KeyKind::Add | KeyKind::Replace) => {
                    self.insert_key(
                        requested_name,
                        Key {
                            kind: KeyKind::Replace,
                            ..key
                        },
                    );
                }
                (KeyKind::Add, KeyKind::Add) => {
                    stored.values.extend(key.values);
                    stored.wine_options.extend(key.wine_options);
                    self.insert_key(
                        lookup,
                        Key {
                            kind: KeyKind::Add,
                            value_names: Key::build_name_map(stored.values.keys()),
                            values: stored.values,
                            wine_options: stored.wine_options,
                            addendum: key.addendum,
                        },
                    );
                }
                (KeyKind::Add | KeyKind::Replace, KeyKind::Replace) => {
                    stored.values.extend(key.values);
                    stored.wine_options.extend(key.wine_options);
                    self.insert_key(
                        lookup,
                        Key {
                            kind: KeyKind::Replace,
                            value_names: Key::build_name_map(stored.values.keys()),
                            values: stored.values,
                            wine_options: stored.wine_options,
                            addendum: key.addendum,
                        },
                    );
                }
                (KeyKind::Replace, KeyKind::Add) => {
                    stored.values.extend(key.values);
                    stored.wine_options.extend(key.wine_options);
                    self.insert_key(
                        lookup,
                        Key {
                            kind: KeyKind::Replace,
                            value_names: Key::build_name_map(stored.values.keys()),
                            values: stored.values,
                            wine_options: stored.wine_options,
                            addendum: key.addendum,
                        },
                    );
                }
            }
        } else {
            self.insert_key(lookup, key);
        }
    }

    /// Access the Wine options.
    pub fn wine_options(&self) -> &BTreeSet<wine::GlobalOption> {
        &self.wine_options
    }

    /// Access the Wine options mutably.
    pub fn wine_options_mut(&mut self) -> &mut BTreeSet<wine::GlobalOption> {
        &mut self.wine_options
    }

    /// Reuse an equivalent existing key name or normalize the requested one.
    /// Capitalization and trailing backslashes don't matter,
    /// but whitespace is significant.
    pub fn key_name(&self, requested: &KeyName) -> KeyName {
        if self.keys.contains_key(requested) {
            return requested.clone();
        }

        if let Some(known) = self.key_names.get(&requested.to_lowercase()) {
            return known.clone();
        }

        requested.clone()
    }

    /// Read a *.reg file.
    pub fn deserialize_file<P: AsRef<Path>>(file: P) -> Result<Self, error::Read> {
        let bytes = std::fs::read(file)?;

        let Some(raw) = etc::read_bytes(bytes) else {
            return Err(error::Read::UnsupportedEncoding);
        };

        Ok(Self::deserialize(&raw)?)
    }

    /// Parse the content of a *.reg file.
    pub fn deserialize(raw: &str) -> Result<Self, error::Deserialize> {
        use deserialize::Element;

        let mut out = Self::default();

        let (format, elements) = deserialize::registry(raw)?;
        out.format = format;

        let mut current_key_name = None;
        let mut current_key = None;

        for element in elements {
            match element {
                Element::Key(key_name, key) => {
                    if let (Some(n), Some(k)) = (current_key_name, current_key) {
                        out.update(n, k);
                    }

                    current_key_name = Some(key_name);
                    current_key = Some(key);
                }
                Element::Value(value_name, value) => {
                    if let Some(key) = current_key.as_mut() {
                        key.update(value_name, Value::from_raw(value, out.format));
                    }
                }
                Element::WineGlobalOption(option) => {
                    out.wine_options.insert(option);
                }
                Element::WineKeyOption(option) => {
                    if let Some(key) = current_key.as_mut() {
                        key.add_wine_option(option);
                    }
                }
                Element::Comment | Element::Blank => continue,
            }
        }

        if let (Some(n), Some(k)) = (current_key_name, current_key) {
            out.update(n, k);
        }

        Ok(out)
    }

    /// Serialize content for a *.reg file.
    pub fn serialize(&self) -> String {
        let mut parts = vec![serialize::format(self.format).to_string()];

        let wine_options = serialize::wine_global_options(&self.wine_options);
        if !wine_options.is_empty() {
            parts.push("\n".to_string());
            parts.extend(wine_options);
        }

        for (name, key) in &self.keys {
            parts.push("\n\n".to_string());
            parts.push(serialize::key(name, key, self.format));
        }

        parts.push("\n".to_string());
        parts.join("")
    }

    /// Write a *.reg file.
    /// The output encoding will depend on the selected `Format`,
    /// to match Regedit's behavior:
    ///
    /// * `Format::Regedit5` => UTF-16
    /// * `Format::Regedit4` => UTF-8
    pub fn serialize_file<P: AsRef<Path>>(&self, file: P) -> Result<(), error::Write> {
        etc::write_file(file, self.serialize(), self.format)
    }
}

/// Serialization format.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Format {
    /// This corresponds to Regedit's "registration files (*.reg)" option.
    #[default]
    Regedit5,
    /// This corresponds to Regedit's "Win9x/NT4 registration files (*.reg)" option.
    Regedit4,
    /// This corresponds to the format used by Wine on Linux.
    ///
    /// Wine does not recommend directly editing its registry files,
    /// so use this at your own discretion.
    Wine2,
}

impl Format {
    pub const REGEDIT5: &'static str = "Windows Registry Editor Version 5.00";
    pub const REGEDIT4: &'static str = "REGEDIT4";
    pub const WINE2: &'static str = "WINE REGISTRY Version 2";

    fn is_wine(&self) -> bool {
        match self {
            Format::Regedit5 | Format::Regedit4 => false,
            Format::Wine2 => true,
        }
    }
}

/// A registry key name.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeyName(String);

impl KeyName {
    /// This will normalize multiple consecutive backslashes to a single backslash,
    /// and it will remove any trailing backslashes.
    pub fn new(raw: impl AsRef<str> + ToString) -> Self {
        let mut normalized = raw.as_ref().trim_end_matches('\\').to_string();

        while normalized.contains(r"\\") {
            normalized = normalized.replace(r"\\", r"\");
        }

        Self(normalized)
    }

    fn to_lowercase(&self) -> Self {
        Self(self.0.to_lowercase())
    }
}

impl From<String> for KeyName {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for KeyName {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum KeyKind {
    #[default]
    /// This key should be added to the registry.
    Add,
    /// This key should be first deleted and then added to the registry.
    Replace,
    /// This key should be deleted from the registry.
    Delete,
}

/// Representation of a `[registry\key]` section.
#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Key {
    kind: KeyKind,
    values: BTreeMap<ValueName, Value>,
    // Lowercase -> stored.
    value_names: BTreeMap<ValueName, ValueName>,
    /// Any extra content after the key name's closing bracket.
    /// Wine uses this to store the modified time.
    addendum: Option<String>,
    wine_options: BTreeSet<wine::KeyOption>,
}

impl Key {
    /// Initialize a default `KeyKind::Add` variant.
    pub fn new() -> Self {
        Self::default()
    }

    /// Initialize a default `KeyKind::Replace` variant.
    pub fn replaced() -> Self {
        Self {
            kind: KeyKind::Replace,
            ..Default::default()
        }
    }

    /// Initialize a default `KeyKind::Delete` variant.
    pub fn deleted() -> Self {
        Self {
            kind: KeyKind::Delete,
            ..Default::default()
        }
    }

    /// This key's kind.
    pub fn kind(&self) -> KeyKind {
        self.kind
    }

    /// This key's values.
    pub fn values(&self) -> &BTreeMap<ValueName, Value> {
        &self.values
    }

    /// This key's addendum (text after key name).
    pub fn addendum(&self) -> Option<&String> {
        self.addendum.as_ref()
    }

    /// This key's Wine options.
    pub fn wine_options(&self) -> &BTreeSet<wine::KeyOption> {
        &self.wine_options
    }

    /// Add or update a value (method chain style).
    /// Will try to reuse an equivalent existing value name, if any.
    pub fn with(mut self, name: impl Into<ValueName>, value: Value) -> Self {
        self.update(name.into(), value);

        self
    }

    /// Add an addendum after the key name (method chain style).
    /// Does nothing for `Key::Delete`.
    pub fn with_addendum(mut self, addendum: String) -> Self {
        self.addendum = Some(addendum);
        self
    }

    /// Add a Wine option (method chain style).
    pub fn with_wine_option(mut self, option: wine::KeyOption) -> Self {
        self.wine_options.insert(option);
        self
    }

    /// Add or update a value.
    /// Will try to reuse an equivalent existing value name, if any.
    pub fn update(&mut self, requested_name: ValueName, value: Value) {
        let lookup = self.value_name(&requested_name);

        if let Some((stored_name, stored)) = self.values.remove_entry(&lookup) {
            if stored == Value::Delete || value == Value::Delete {
                self.insert_value(requested_name, value);
            } else {
                self.insert_value(stored_name, value);
            }
        } else {
            self.insert_value(lookup, value);
        }
    }

    /// Add an addendum after the key name.
    pub fn add_addendum(&mut self, addendum: String) {
        self.addendum = Some(addendum);
    }

    /// Add a Wine option.
    pub fn add_wine_option(&mut self, option: wine::KeyOption) {
        self.wine_options.insert(option);
    }

    /// Reuse an equivalent existing value name or normalize the requested one.
    /// Capitalization doesn't matter.
    pub fn value_name<'a>(&'a self, name: &'a ValueName) -> ValueName {
        if self.values.contains_key(name) {
            return name.clone();
        }

        if let Some(known) = self.value_names.get(&name.to_lowercase()) {
            return known.clone();
        }

        name.clone()
    }

    fn insert_value(&mut self, name: ValueName, key: Value) {
        self.value_names.insert(name.to_lowercase(), name.clone());
        self.values.insert(name, key);
    }

    fn build_name_map<'a>(stored: impl Iterator<Item = &'a ValueName>) -> BTreeMap<ValueName, ValueName> {
        stored.map(|stored| (stored.to_lowercase(), stored.clone())).collect()
    }
}

/// A registry value name.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueName {
    /// The unnamed default value (`@`).
    Default,
    /// A named value.
    Named(String),
}

impl ValueName {
    /// Initializes a `ValueName::Named` variant.
    pub fn named(name: impl Into<String>) -> Self {
        Self::Named(name.into())
    }

    fn to_lowercase(&self) -> Self {
        match self {
            Self::Default => Self::Default,
            Self::Named(name) => Self::Named(name.to_lowercase()),
        }
    }
}

impl From<String> for ValueName {
    fn from(value: String) -> Self {
        Self::Named(value)
    }
}

impl From<&str> for ValueName {
    fn from(value: &str) -> Self {
        Self::Named(value.to_string())
    }
}

/// A low-level representation of a serialized registry value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum RawValue {
    /// This value should be deleted.
    Delete,
    /// A string value.
    Sz(String),
    /// A string value used by Wine.
    Str { kind: Kind, data: String },
    /// A dword value
    Dword(u32),
    /// A value represented as hexadecimal numbers.
    /// This is used for most types.
    Hex { kind: Kind, bytes: Vec<u8> },
}

/// A high-level representation of a registry value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Delete,
    Sz(String),
    ExpandSz(String),
    Binary(Vec<u8>),
    Dword(u32),
    DwordBigEndian(u32),
    // Link
    MultiSz(Vec<String>),
    // ResourceList
    // FullResourceList
    // ResourceRequirementsList
    Qword(u64),
    Hex { kind: Kind, bytes: Vec<u8> },
}

impl Value {
    fn from_raw(value: RawValue, format: Format) -> Self {
        match value {
            RawValue::Delete => Self::Delete,
            RawValue::Sz(x) => Self::Sz(x),
            RawValue::Str { kind, data } => {
                match kind {
                    Kind::Sz => Self::Sz(data),
                    Kind::ExpandSz => Self::ExpandSz(data),
                    Kind::MultiSz => Self::MultiSz(data.split('\0').map(|x| x.to_string()).collect()),
                    _ => Self::Delete, // Invalid
                }
            }
            RawValue::Dword(x) => Self::Dword(x),
            RawValue::Hex { kind, bytes } => {
                let fallback = Self::Hex {
                    kind,
                    bytes: bytes.clone(),
                };

                macro_rules! fallback {
                    ($maybe:expr) => {
                        match $maybe {
                            Some(x) => x,
                            None => {
                                return fallback;
                            }
                        }
                    };
                }

                match kind {
                    Kind::None
                    | Kind::Link
                    | Kind::ResourceList
                    | Kind::FullResourceList
                    | Kind::ResourceRequirementsList
                    | Kind::Unknown(_) => fallback,
                    Kind::Sz => match format {
                        Format::Regedit5 => Self::Sz(fallback!(etc::utf16_bytes_to_str(bytes))),
                        Format::Regedit4 | Format::Wine2 => Self::Sz(fallback!(etc::ascii_bytes_to_str(bytes))),
                    },
                    Kind::ExpandSz => match format {
                        Format::Regedit5 => Self::ExpandSz(fallback!(etc::utf16_bytes_to_str(bytes))),
                        Format::Regedit4 | Format::Wine2 => Self::ExpandSz(fallback!(etc::ascii_bytes_to_str(bytes))),
                    },
                    Kind::Binary => Self::Binary(bytes),
                    Kind::Dword => {
                        if bytes.len() != 4 {
                            return fallback;
                        }
                        let x = u32::from_le_bytes(fallback!(bytes[0..4].try_into().ok()));
                        Self::Dword(x)
                    }
                    Kind::DwordBigEndian => {
                        let x = u32::from_be_bytes(fallback!(bytes[0..4].try_into().ok()));
                        Self::Dword(x)
                    }
                    Kind::MultiSz => {
                        let flat = match format {
                            Format::Regedit5 => {
                                fallback!(etc::utf16_bytes_to_str(bytes))
                            }
                            Format::Regedit4 | Format::Wine2 => {
                                fallback!(etc::ascii_bytes_to_str(bytes))
                            }
                        };

                        let split: Vec<_> = flat
                            .split('\0')
                            .filter(|x| !x.is_empty())
                            .map(|x| x.to_string())
                            .collect();

                        Self::MultiSz(split)
                    }
                    Kind::Qword => {
                        if bytes.len() != 8 {
                            return fallback;
                        }
                        let x = u64::from_le_bytes(fallback!(bytes[0..8].try_into().ok()));
                        Self::Qword(x)
                    }
                }
            }
        }
    }

    fn into_raw(self, format: Format) -> RawValue {
        match self {
            Value::Delete => RawValue::Delete,
            Value::Sz(x) => match format {
                Format::Regedit5 => {
                    if x.contains(etc::SZ_INVALID_CHARS) {
                        RawValue::Hex {
                            kind: Kind::Sz,
                            bytes: etc::str_to_utf16_bytes(&x),
                        }
                    } else {
                        RawValue::Sz(x)
                    }
                }
                Format::Regedit4 => {
                    if x.contains(etc::SZ_INVALID_CHARS) {
                        RawValue::Hex {
                            kind: Kind::Sz,
                            bytes: etc::str_to_ascii_bytes(&x),
                        }
                    } else {
                        let ascii: String = x.chars().map(|c| if c.is_ascii() { c } else { '?' }).collect();
                        RawValue::Sz(ascii)
                    }
                }
                Format::Wine2 => RawValue::Sz(x),
            },
            Value::ExpandSz(x) => match format {
                Format::Regedit5 => RawValue::Hex {
                    kind: Kind::ExpandSz,
                    bytes: etc::str_to_utf16_bytes(&x),
                },
                Format::Regedit4 => RawValue::Hex {
                    kind: Kind::ExpandSz,
                    bytes: etc::str_to_ascii_bytes(&x),
                },
                Format::Wine2 => RawValue::Str {
                    kind: Kind::ExpandSz,
                    data: x,
                },
            },
            Value::Binary(bytes) => RawValue::Hex {
                kind: Kind::Binary,
                bytes,
            },
            Value::Dword(x) => RawValue::Dword(x),
            Value::DwordBigEndian(x) => RawValue::Hex {
                kind: Kind::DwordBigEndian,
                bytes: x.to_be_bytes().to_vec(),
            },
            Value::MultiSz(xs) => {
                if format.is_wine() {
                    return RawValue::Str {
                        kind: Kind::MultiSz,
                        data: xs.join("\0"),
                    };
                }

                let mut bytes = vec![];

                for x in xs {
                    if x.is_empty() {
                        continue;
                    }

                    match format {
                        Format::Regedit5 => {
                            bytes.extend(etc::str_to_utf16_bytes(&x));
                        }
                        Format::Regedit4 => {
                            bytes.extend(etc::str_to_ascii_bytes(&x));
                        }
                        Format::Wine2 => unreachable!(),
                    }
                }

                match format {
                    Format::Regedit5 => {
                        bytes.push(0);
                        bytes.push(0);
                    }
                    Format::Regedit4 => {
                        bytes.push(0);
                    }
                    Format::Wine2 => unreachable!(),
                }

                RawValue::Hex {
                    kind: Kind::MultiSz,
                    bytes,
                }
            }
            Value::Qword(x) => {
                let bytes = x.to_le_bytes().to_vec();
                RawValue::Hex {
                    kind: Kind::Qword,
                    bytes,
                }
            }
            Value::Hex { kind, bytes } => RawValue::Hex { kind, bytes },
        }
    }
}

/// Registry value types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Kind {
    None,
    Sz,
    ExpandSz,
    Binary,
    Dword,
    DwordBigEndian,
    Link,
    MultiSz,
    ResourceList,
    FullResourceList,
    ResourceRequirementsList,
    Qword,
    Unknown(u8),
}

impl From<u8> for Kind {
    fn from(value: u8) -> Self {
        match value {
            0 => Self::None,
            1 => Self::Sz,
            2 => Self::ExpandSz,
            3 => Self::Binary,
            4 => Self::Dword,
            5 => Self::DwordBigEndian,
            6 => Self::Link,
            7 => Self::MultiSz,
            8 => Self::ResourceList,
            9 => Self::FullResourceList,
            10 => Self::ResourceRequirementsList,
            11 => Self::Qword,
            x => Self::Unknown(x),
        }
    }
}

impl From<Kind> for u8 {
    fn from(value: Kind) -> Self {
        match value {
            Kind::None => 0,
            Kind::Sz => 1,
            Kind::ExpandSz => 2,
            Kind::Binary => 3,
            Kind::Dword => 4,
            Kind::DwordBigEndian => 5,
            Kind::Link => 6,
            Kind::MultiSz => 7,
            Kind::ResourceList => 8,
            Kind::FullResourceList => 9,
            Kind::ResourceRequirementsList => 10,
            Kind::Qword => 11,
            Kind::Unknown(x) => x,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn key_name() {
        assert_eq!(KeyName(r"foo".to_string()), KeyName::new(r"foo"));
        assert_eq!(KeyName(r"foo".to_string()), KeyName::new(r"foo\"));
        assert_eq!(KeyName(r"foo\bar".to_string()), KeyName::new(r"foo\bar"));
        assert_eq!(KeyName(r"foo\bar".to_string()), KeyName::new(r"foo\\bar"));
    }

    #[test]
    fn simple_regedit5() {
        let registry = Registry::deserialize_file("tests/simple-regedit5.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit5)
            .with(r"HKEY_CURRENT_USER\Software\regashii\remove", Key::deleted())
            .with(
                r"HKEY_CURRENT_USER\Software\regashii\simple",
                Key::new()
                    .with(ValueName::Default, Value::Sz("default".to_string()))
                    .with("sz-a", Value::Sz("a".to_string()))
                    .with("dword-1", Value::Dword(1))
                    .with("binary-a", Value::Binary(vec![0x61]))
                    .with("remove", Value::Delete),
            );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
Windows Registry Editor Version 5.00

[-HKEY_CURRENT_USER\Software\regashii\remove]

[HKEY_CURRENT_USER\Software\regashii\simple]
@="default"
"binary-a"=hex:61
"dword-1"=dword:00000001
"remove"=-
"sz-a"="a"
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn simple_regedit4() {
        let registry = Registry::deserialize_file("tests/simple-regedit4.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit4)
            .with(r"HKEY_CURRENT_USER\Software\regashii\remove", Key::deleted())
            .with(
                r"HKEY_CURRENT_USER\Software\regashii\simple",
                Key::new()
                    .with(ValueName::Default, Value::Sz("default".to_string()))
                    .with("sz-a", Value::Sz("a".to_string()))
                    .with("dword-1", Value::Dword(1))
                    .with("binary-a", Value::Binary(vec![0x61]))
                    .with("remove", Value::Delete),
            );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
REGEDIT4

[-HKEY_CURRENT_USER\Software\regashii\remove]

[HKEY_CURRENT_USER\Software\regashii\simple]
@="default"
"binary-a"=hex:61
"dword-1"=dword:00000001
"remove"=-
"sz-a"="a"
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn complex_regedit5() {
        let registry = Registry::deserialize_file("tests/complex-regedit5.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit5).with(
            r"HKEY_CURRENT_USER\Software\regashii\complex",
            Key::new()
                .with("binary-a", Value::Binary(vec![0x61]))
                .with("binary-a-b", Value::Binary(vec![0x61, 0x62]))
                .with("dword-0", Value::Dword(0))
                .with("dword-255", Value::Dword(255))
                .with("expand-sz", Value::ExpandSz("".to_string()))
                .with("expand-sz-a", Value::ExpandSz("a".to_string()))
                .with("multi-sz", Value::MultiSz(vec![]))
                .with("multi-sz-a", Value::MultiSz(vec!["a".to_string()]))
                .with("multi-sz-a-b", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                .with("qword-0", Value::Qword(0))
                .with("qword-255", Value::Qword(255))
                .with("sz-a", Value::Sz("a".to_string()))
                .with(r#"sz-sp\ec"ial"#, Value::Sz(r#"sp\ec"ial"#.to_string())),
        );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\Software\regashii\complex]
"binary-a"=hex:61
"binary-a-b"=hex:61,62
"dword-0"=dword:00000000
"dword-255"=dword:000000ff
"expand-sz"=hex(2):00,00
"expand-sz-a"=hex(2):61,00,00,00
"multi-sz"=hex(7):00,00
"multi-sz-a"=hex(7):61,00,00,00,00,00
"multi-sz-a-b"=hex(7):61,00,00,00,62,00,00,00,00,00
"qword-0"=hex(b):00,00,00,00,00,00,00,00
"qword-255"=hex(b):ff,00,00,00,00,00,00,00
"sz-a"="a"
"sz-sp\\ec\"ial"="sp\\ec\"ial"
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn complex_regedit4() {
        let registry = Registry::deserialize_file("tests/complex-regedit4.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit4).with(
            r"HKEY_CURRENT_USER\Software\regashii\complex",
            Key::new()
                .with("binary-a", Value::Binary(vec![0x61]))
                .with("binary-a-b", Value::Binary(vec![0x61, 0x62]))
                .with("dword-0", Value::Dword(0))
                .with("dword-255", Value::Dword(255))
                .with("expand-sz", Value::ExpandSz("".to_string()))
                .with("expand-sz-a", Value::ExpandSz("a".to_string()))
                .with("multi-sz", Value::MultiSz(vec![]))
                .with("multi-sz-a", Value::MultiSz(vec!["a".to_string()]))
                .with("multi-sz-a-b", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                .with("qword-0", Value::Qword(0))
                .with("qword-255", Value::Qword(255))
                .with("sz-a", Value::Sz("a".to_string()))
                .with(r#"sz-sp\ec"ial"#, Value::Sz(r#"sp\ec"ial"#.to_string())),
        );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
REGEDIT4

[HKEY_CURRENT_USER\Software\regashii\complex]
"binary-a"=hex:61
"binary-a-b"=hex:61,62
"dword-0"=dword:00000000
"dword-255"=dword:000000ff
"expand-sz"=hex(2):00
"expand-sz-a"=hex(2):61,00
"multi-sz"=hex(7):00
"multi-sz-a"=hex(7):61,00,00
"multi-sz-a-b"=hex(7):61,00,62,00,00
"qword-0"=hex(b):00,00,00,00,00,00,00,00
"qword-255"=hex(b):ff,00,00,00,00,00,00,00
"sz-a"="a"
"sz-sp\\ec\"ial"="sp\\ec\"ial"
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn japanese_regedit5() {
        let registry = Registry::deserialize_file("tests/japanese-regedit5.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit5).with(
            r"HKEY_CURRENT_USER\Software\regashii\日本語",
            Key::new()
                .with("expand-sz", Value::ExpandSz("あ".to_string()))
                .with("multi-sz-1", Value::MultiSz(vec!["あ".to_string()]))
                .with("multi-sz-2", Value::MultiSz(vec!["あ".to_string(), "い".to_string()]))
                .with("sz-あ", Value::Sz("あ".to_string())),
        );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\Software\regashii\日本語]
"expand-sz"=hex(2):42,30,00,00
"multi-sz-1"=hex(7):42,30,00,00,00,00
"multi-sz-2"=hex(7):42,30,00,00,44,30,00,00,00,00
"sz-あ"="あ"
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn japanese_regedit4() {
        let registry = Registry::new(Format::Regedit4).with(
            r"HKEY_CURRENT_USER\Software\regashii\japanese",
            Key::new()
                .with("expand-sz", Value::ExpandSz("あ".to_string()))
                .with("multi-sz-1", Value::MultiSz(vec!["あ".to_string()]))
                .with("multi-sz-2", Value::MultiSz(vec!["あ".to_string(), "い".to_string()]))
                .with("sz", Value::Sz("あ".to_string())),
        );

        assert_eq!(
            r#"
REGEDIT4

[HKEY_CURRENT_USER\Software\regashii\japanese]
"expand-sz"=hex(2):42,00
"multi-sz-1"=hex(7):42,00,00
"multi-sz-2"=hex(7):42,00,44,00,00
"sz"="?"
"#
            .trim_start(),
            registry.serialize()
        );
    }

    #[test]
    fn multi_sz_separation_regedit5() {
        let registry = Registry::deserialize_file("tests/multi-sz-separation-regedit5.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit5).with(
            r"HKEY_CURRENT_USER\Software\regashii\multi-sz-separation",
            Key::new()
                .with("a", Value::MultiSz(vec!["a".to_string()]))
                .with("a-_", Value::MultiSz(vec!["a".to_string()]))
                .with("a-b", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                .with("a-b-_", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                .with("a-_-b", Value::MultiSz(vec!["a".to_string(), "b".to_string()])),
        );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\Software\regashii\multi-sz-separation]
"a"=hex(7):61,00,00,00,00,00
"a-_"=hex(7):61,00,00,00,00,00
"a-_-b"=hex(7):61,00,00,00,62,00,00,00,00,00
"a-b"=hex(7):61,00,00,00,62,00,00,00,00,00
"a-b-_"=hex(7):61,00,00,00,62,00,00,00,00,00
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn multi_sz_separation_regedit4() {
        let registry = Registry::deserialize_file("tests/multi-sz-separation-regedit4.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit4).with(
            r"HKEY_CURRENT_USER\Software\regashii\multi-sz-separation",
            Key::new()
                .with("a", Value::MultiSz(vec!["a".to_string()]))
                .with("a-_", Value::MultiSz(vec!["a".to_string()]))
                .with("a-b", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                .with("a-b-_", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                .with("a-_-b", Value::MultiSz(vec!["a".to_string(), "b".to_string()])),
        );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
REGEDIT4

[HKEY_CURRENT_USER\Software\regashii\multi-sz-separation]
"a"=hex(7):61,00,00
"a-_"=hex(7):61,00,00
"a-_-b"=hex(7):61,00,62,00,00
"a-b"=hex(7):61,00,62,00,00
"a-b-_"=hex(7):61,00,62,00,00
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn repetition_regedit5() {
        let registry = Registry::deserialize_file("tests/repetition-regedit5.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit5)
            .with(
                r"HKEY_CURRENT_USER\Software\regashii\repeated-kEy",
                Key::replaced()
                    .with("bar", Value::Sz("2".to_string()))
                    .with("baz", Value::Sz("3".to_string())),
            )
            .with(
                r"HKEY_CURRENT_USER\Software\regashii\repeated-value",
                Key::new().with("fOo", Value::Sz("3".to_string())),
            );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
Windows Registry Editor Version 5.00

[-HKEY_CURRENT_USER\Software\regashii\repeated-kEy]

[HKEY_CURRENT_USER\Software\regashii\repeated-kEy]
"bar"="2"
"baz"="3"

[HKEY_CURRENT_USER\Software\regashii\repeated-value]
"fOo"="3"
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn irregular_regedit5() {
        let registry = Registry::deserialize_file("tests/irregular-regedit5.reg").unwrap();

        let deserialized = Registry::new(Format::Regedit5).with(
            r#"HKEY_CURRENT_USER\Software\regashii\irre]";gular"#,
            Key::new()
                .with_addendum("addendum".to_string())
                .with(ValueName::Default, Value::Sz("".to_string()))
                .with(
                    "dword",
                    Value::Hex {
                        kind: Kind::Dword,
                        bytes: vec![0],
                    },
                )
                .with("sz-line-break", Value::Sz("a\nb".to_string()))
                .with("hex-continuations", Value::Binary(vec![0, 1, 2]))
                .with(
                    "hex-continuations-with-long-name ............................................ end",
                    Value::Binary(vec![
                        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
                        26, 27, 28, 29, 30, 31,
                    ]),
                )
                .with("semi;colons", Value::Sz("and\"quotes;".to_string())),
        );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
Windows Registry Editor Version 5.00

[HKEY_CURRENT_USER\Software\regashii\irre]";gular] addendum
@=""
"dword"=hex(4):00
"hex-continuations"=hex:00,01,02
"hex-continuations-with-long-name ............................................ end"=hex:00,\
  01,02,03,04,05,06,07,08,09,0a,0b,0c,0d,0e,0f,10,11,12,13,14,15,16,17,18,19,\
  1a,1b,1c,1d,1e,1f
"semi;colons"="and\"quotes;"
"sz-line-break"=hex(1):61,00,0a,00,62,00,00,00
"#
            .trim_start(),
            deserialized.serialize()
        );
    }

    #[test]
    fn wine_v2() {
        let registry = Registry::deserialize_file("tests/wine-v2.reg").unwrap();

        let deserialized = Registry::new(Format::Wine2)
            .with_wine_option(wine::GlobalOption::Arch("win32".to_string()))
            .with(
                r"Software\regashii\wine",
                Key::new()
                    .with_addendum("1729718050".to_string())
                    .with_wine_option(wine::KeyOption::Time("1db259080cb98c4".to_string()))
                    .with_wine_option(wine::KeyOption::Class("foo".to_string()))
                    .with(ValueName::Default, Value::Sz("".to_string()))
                    .with("binary-a", Value::Binary(vec![0x61]))
                    .with("dword-1", Value::Dword(1))
                    .with("multi-sz-a-b", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                    .with("expand-sz-a", Value::ExpandSz("a".to_string()))
                    .with("sz-a", Value::Sz("a".to_string()))
                    .with("sz-empty", Value::Sz("".to_string()))
                    .with("sz-str", Value::Sz("x".to_string())),
            )
            .with(
                r"Software\regashii\日本語",
                Key::new().with("sz-あ", Value::Sz("あ".to_string())),
            )
            .with(
                r"Software\regashii\wine-br[a]cket",
                Key::new().with(ValueName::Default, Value::Sz("test".to_string())),
            )
            .with(
                r"Software\regashii\wine-link",
                Key::new()
                    .with_wine_option(wine::KeyOption::Link)
                    .with("SymbolicLinkValue", Value::Sz("foo".to_string())),
            );

        assert_eq!(deserialized, registry);

        assert_eq!(
            r#"
WINE REGISTRY Version 2
#arch=win32

[Software\\regashii\\wine] 1729718050
#time=1db259080cb98c4
#class="foo"
@=""
"binary-a"=hex:61
"dword-1"=dword:00000001
"expand-sz-a"=str(2):"a"
"multi-sz-a-b"=str(7):"a\0b"
"sz-a"="a"
"sz-empty"=""
"sz-str"="x"

[Software\\regashii\\wine-br\[a\]cket]
@="test"

[Software\\regashii\\wine-link]
#link
"SymbolicLinkValue"="foo"

[Software\\regashii\\\x65e5\x672c\x8a9e]
"sz-\x3042"="\x3042"
"#
            .trim_start(),
            deserialized.serialize()
        );
    }
}
