use std::path::Path;

use utf16string::{LittleEndian, WString};

use crate::{error, Format};

const BOM_LE: &str = "\u{feff}";
const BOM_BE: &str = "\u{ffef}";
pub const SZ_INVALID_CHARS: &[char] = &['\n', '\r'];

pub fn utf16_bytes_to_str(bytes: Vec<u8>) -> Option<String> {
    let ws = WString::<LittleEndian>::from_utf16(bytes).ok()?;
    Some(ws.to_utf8().trim_end_matches('\0').to_string())
}

pub fn ascii_bytes_to_str(bytes: Vec<u8>) -> Option<String> {
    Some(String::from_utf8(bytes).ok()?.trim_end_matches('\0').to_string())
}

pub fn str_to_utf16_bytes(data: &str) -> Vec<u8> {
    let ws = WString::<LittleEndian>::from(data.trim_end_matches('\0'));
    let mut bytes = ws.into_bytes();
    bytes.push(0);
    bytes.push(0);
    bytes
}

pub fn str_to_ascii_bytes(data: &str) -> Vec<u8> {
    let mut bytes: Vec<_> = data.trim_end_matches('\0').chars().map(|c| c as u8).collect();
    bytes.push(0);
    bytes
}

pub fn read_bytes(bytes: Vec<u8>) -> Option<String> {
    if let Ok(raw) = String::from_utf8(bytes.clone()) {
        return Some(raw);
    }

    if let Ok(raw) = WString::from_utf16le(bytes.clone()) {
        let utf8 = raw.to_utf8();
        return Some(utf8.strip_prefix(BOM_LE).map(|x| x.to_string()).unwrap_or(utf8));
    }

    if let Ok(raw) = WString::from_utf16be(bytes) {
        let utf8 = raw.to_utf8();
        return Some(utf8.strip_prefix(BOM_BE).map(|x| x.to_string()).unwrap_or(utf8));
    }

    None
}

pub fn write_file<P: AsRef<Path>>(file: P, content: String, format: Format) -> Result<(), error::Write> {
    match format {
        Format::Regedit4 => {
            std::fs::write(file, content)?;
        }
        Format::Regedit5 => {
            let content = format!("{BOM_LE}{content}");
            let utf16 = WString::<LittleEndian>::from(&content);
            std::fs::write(file, utf16.as_bytes())?;
        }
    }

    Ok(())
}
