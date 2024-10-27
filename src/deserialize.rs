use crate::{error, wine, Format, Key, KeyKind, KeyName, Kind, RawValue, ValueName};
use once_cell::sync::Lazy;
use regex::Regex;

use std::sync::atomic::{AtomicBool, Ordering};

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take_till, take_till1, take_while, take_while1, take_while_m_n},
    character::complete::{char, hex_digit1, line_ending, none_of, space0, space1},
    combinator::{eof, opt, value},
    multi::{fold_many0, many1, separated_list0},
    sequence::{delimited, terminated, tuple},
    AsChar, IResult,
};

pub fn unescape_wine_unicode(raw: &str) -> String {
    if !raw.contains(r"\x") {
        return raw.to_string();
    }

    static UNICODE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"(?xs)
            (\\x (?<escape> [0-9a-fA-F]{4}) )
            | (?<other> . )
        "#,
        )
        .unwrap()
    });

    let mut points = vec![];
    for capture in UNICODE.captures_iter(raw) {
        if let Some(raw_code) = capture.name("escape") {
            let Ok(code) = u16::from_str_radix(raw_code.as_str(), 16) else {
                continue;
            };
            points.push(code);
        } else if let Some(other) = capture.name("other") {
            points.extend(other.as_str().encode_utf16());
        }
    }
    String::from_utf16_lossy(&points)
}

pub fn registry(raw: &str) -> Result<(Format, Vec<Element>), error::Deserialize> {
    match elements(raw) {
        Ok((_, (format, elements))) => match format {
            RawFormat::Valid(format) => Ok((format, elements)),
            RawFormat::Invalid(format) => Err(error::Deserialize::UnknownFormat(format)),
        },
        Err(_) => Err(error::Deserialize::Malformed),
    }
}

fn ws<'a, F, O, E: nom::error::ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: 'a,
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(space0, inner, space0)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawFormat {
    Valid(Format),
    Invalid(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Element {
    Key(KeyName, Key),
    Value(ValueName, RawValue),
    WineGlobalOption(wine::GlobalOption),
    WineKeyOption(wine::KeyOption),
    Comment,
    Blank,
}

pub fn elements(input: &str) -> IResult<&str, (RawFormat, Vec<Element>)> {
    let (input, format) = self::format(input)?;

    let RawFormat::Valid(valid_format) = format else {
        return Ok((input, (format, vec![])));
    };

    let found_key = AtomicBool::new(false);

    let (input, elements) = fold_many0(
        |x| element(x, valid_format, found_key.load(Ordering::Relaxed)),
        Vec::new,
        |mut acc, element| {
            match &element {
                Element::Key { .. } => {
                    found_key.store(true, Ordering::Relaxed);
                }
                Element::Blank => {
                    return acc;
                }
                _ => {}
            }

            acc.push(element);
            acc
        },
    )(input)?;

    Ok((input, (format, elements)))
}

pub fn element(input: &str, format: Format, found_key: bool) -> IResult<&str, Element> {
    let key = |x| {
        key(x, format).map(|(input, output)| match output {
            Some((x, y)) => (input, Element::Key(x, y)),
            None => (input, Element::Blank),
        })
    };
    let named_value = |x| {
        named_value(x, format).map(|(input, output)| match output {
            Some((x, y)) => (input, Element::Value(x, y)),
            None => (input, Element::Blank),
        })
    };
    let comment = |x| comment(x).map(|(input, _)| (input, Element::Comment));
    let blank = |x| blank(x).map(|(input, _)| (input, Element::Blank));

    let (input, element) = if format.is_wine() {
        if found_key {
            let wine_key_option = |x| wine_key_option(x).map(|(input, output)| (input, Element::WineKeyOption(output)));
            alt((named_value, key, wine_key_option, comment, blank))(input)?
        } else {
            let wine_global_option =
                |x| wine_global_option(x).map(|(input, output)| (input, Element::WineGlobalOption(output)));
            alt((named_value, key, wine_global_option, comment, blank))(input)?
        }
    } else {
        alt((named_value, key, comment, blank))(input)?
    };

    Ok((input, element))
}

pub fn format(input: &str) -> IResult<&str, RawFormat> {
    let tail = || alt((tag(" "), tag("\t"), tag("\n"), tag("\r"), tag(";")));

    let (input, format) = opt(terminated(tag(Format::REGEDIT4), tail()))(input)?;
    if format.is_some() {
        return Ok((input, RawFormat::Valid(Format::Regedit4)));
    }

    let (input, format) = opt(terminated(tag(Format::REGEDIT5), tail()))(input)?;
    if format.is_some() {
        return Ok((input, RawFormat::Valid(Format::Regedit5)));
    }

    let (input, format) = opt(terminated(tag(Format::WINE2), tail()))(input)?;
    if format.is_some() {
        return Ok((input, RawFormat::Valid(Format::Wine2)));
    }

    let (input, format) = take_till(|x| matches!(x, '\n' | '\r'))(input)?;
    Ok((input, RawFormat::Invalid(format.to_string())))
}

pub fn comment(input: &str) -> IResult<&str, &str> {
    let (input, _) = char(';')(input)?;
    let (input, comment) = take_while(|x| x != '\n' && x != '\r')(input)?;

    Ok((input, comment))
}

pub fn blank(input: &str) -> IResult<&str, &str> {
    let (input, blank) = take_till1(|x| matches!(x, '[' | '@' | '"' | '#' | ';'))(input)?;

    Ok((input, blank))
}

pub fn key(input: &str, format: Format) -> IResult<&str, Option<(KeyName, Key)>> {
    let (input, _) = char('[')(input)?;

    let (input, space) = opt(alt((char(' '), char(']'))))(input)?;
    if space.is_some() {
        return Ok((input, None));
    }

    let (input, deleted) = opt(char('-'))(input)?;

    let (input, space) = opt(alt((char(' '), char(']'))))(input)?;
    if space.is_some() {
        return Ok((input, None));
    }

    let (input_out, line) = take_till(|x| x == '\n' || x == '\r')(input)?;

    let (name, tail) = match line.rfind("]") {
        Some(i) => (&line[..i], &line[i + 1..]),
        None => return Ok((input_out, None)),
    };

    let name = if format.is_wine() {
        let (_input, name) = escaped_transform(
            none_of("\\"),
            '\\',
            alt((
                value("\\", tag("\\")),
                value("[", tag("[")),
                value("]", tag("]")),
                value("\\x", tag("x")),
            )),
        )(name)?;
        unescape_wine_unicode(&name)
    } else {
        name.to_string()
    };

    let addendum = tail.split(';').next().unwrap().trim().to_string();
    let addendum = (!addendum.is_empty()).then_some(addendum);

    let key = Key {
        kind: if deleted.is_some() {
            KeyKind::Delete
        } else {
            KeyKind::Add
        },
        addendum,
        ..Default::default()
    };

    Ok((input_out, Some((KeyName::new(name), key))))
}

pub fn named_value(input: &str, format: Format) -> IResult<&str, Option<(ValueName, RawValue)>> {
    let (input, _) = space0(input)?;
    let (input, name) = value_name(input, format)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = value_data(input, format)?;
    let (input, _) = space0(input)?;

    // Regedit won't import the value if there's garbage at the end of the line.
    let (input, _) = opt(comment)(input)?;
    let (input, eol) = opt(line_ending)(input)?;
    let (input, eof) = opt(eof)(input)?;
    if eol.is_none() && eof.is_none() {
        return Ok((input, None));
    }

    Ok((input, Some((name, value))))
}

pub fn value_name(input: &str, format: Format) -> IResult<&str, ValueName> {
    let (input, _) = space0(input)?;
    if format.is_wine() {
        alt((default_value_name, specific_value_name_wine))(input)
    } else {
        alt((default_value_name, specific_value_name))(input)
    }
}

fn default_value_name(input: &str) -> IResult<&str, ValueName> {
    let (input, _) = char('@')(input)?;

    Ok((input, ValueName::Default))
}

fn specific_value_name(input: &str) -> IResult<&str, ValueName> {
    let (input, read) = delimited(
        char('"'),
        escaped_transform(
            none_of("\\\""),
            '\\',
            alt((value("\\", tag("\\")), value("\"", tag("\"")))),
        ),
        char('"'),
    )(input)?;

    Ok((input, ValueName::Named(read.to_string())))
}

fn specific_value_name_wine(input: &str) -> IResult<&str, ValueName> {
    let (input, read) = delimited(
        char('"'),
        escaped_transform(
            none_of("\\\""),
            '\\',
            alt((
                value("\\", tag("\\")),
                value("\"", tag("\"")),
                value("\n", tag("n")),
                value("\r", tag("r")),
                value("\0", tag("0")),
                value("\\x", tag("x")),
            )),
        ),
        char('"'),
    )(input)?;

    Ok((input, ValueName::Named(unescape_wine_unicode(&read))))
}

pub fn value_data(input: &str, format: Format) -> IResult<&str, RawValue> {
    let (input, value) = if format.is_wine() {
        alt((
            value_delete,
            value_dword,
            value_sz_wine,
            value_sz_empty,
            value_hex,
            value_str,
            value_str_empty,
        ))(input)
    } else {
        alt((value_delete, value_dword, value_sz, value_sz_empty, value_hex))(input)
    }?;

    let value = match value {
        RawValue::Delete => RawValue::Delete,
        RawValue::Dword(data) => RawValue::Dword(data),
        RawValue::Sz(data) => {
            let data = if format.is_wine() {
                unescape_wine_unicode(&data)
            } else {
                data
            };
            RawValue::Sz(data)
        }
        RawValue::Hex { kind, bytes } => RawValue::Hex { kind, bytes },
        RawValue::Str { kind, data } => {
            let data = if format.is_wine() {
                unescape_wine_unicode(&data)
            } else {
                data
            };
            RawValue::Str { kind, data }
        }
    };

    Ok((input, value))
}

fn value_delete(input: &str) -> IResult<&str, RawValue> {
    let (input, _) = char('-')(input)?;

    Ok((input, RawValue::Delete))
}

fn value_dword(input: &str) -> IResult<&str, RawValue> {
    let (input, _) = tag("dword:")(input)?;
    let (input, _) = space0(input)?;

    let (input, read) = take_while_m_n(8, 8, |x: char| x.is_ascii_hexdigit())(input)?;

    let value = u32::from_str_radix(read, 16).unwrap();

    Ok((input, RawValue::Dword(value)))
}

fn value_sz(input: &str) -> IResult<&str, RawValue> {
    let (input, read) = delimited(
        char('"'),
        escaped_transform(
            none_of("\\\""),
            '\\',
            alt((value("\\", tag("\\")), value("\"", tag("\"")))),
        ),
        char('"'),
    )(input)?;

    Ok((input, RawValue::Sz(read.to_string())))
}

fn value_sz_empty(input: &str) -> IResult<&str, RawValue> {
    let (input, _) = ws(tag("\"\""))(input)?;

    Ok((input, RawValue::Sz(String::new())))
}

fn value_sz_wine(input: &str) -> IResult<&str, RawValue> {
    let (input, read) = delimited(
        char('"'),
        escaped_transform(
            none_of("\\\""),
            '\\',
            alt((
                value("\\", tag("\\")),
                value("\"", tag("\"")),
                value("\n", tag("n")),
                value("\r", tag("r")),
                value("\0", tag("0")),
                value("\\x", tag("x")),
            )),
        ),
        char('"'),
    )(input)?;

    Ok((input, RawValue::Sz(read.to_string())))
}

fn value_hex(input: &str) -> IResult<&str, RawValue> {
    let (input, _) = tag("hex")(input)?;

    let (input, kind) = opt(delimited(char('('), hex_digit1, char(')')))(input)?;
    let kind = kind
        .and_then(|kind| u8::from_str_radix(kind, 16).ok())
        .map(Kind::from)
        .unwrap_or(Kind::Binary);

    let (input, _) = char(':')(input)?;
    let (input, _) = space0(input)?;

    let (input, bytes) = separated_list0(
        tuple((
            space0,
            char(','),
            space0,
            opt(tuple((
                char('\\'),
                space0,
                take_while(|x| !matches!(x, '\n' | '\r')),
                space0,
                many1(line_ending),
                space1,
            ))),
        )),
        take_while_m_n(2, 2, |x: char| x.is_ascii_hexdigit()),
    )(input)?;

    let bytes = bytes
        .into_iter()
        .filter_map(|x| u8::from_str_radix(x, 16).ok())
        .collect();

    Ok((input, RawValue::Hex { kind, bytes }))
}

fn value_str(input: &str) -> IResult<&str, RawValue> {
    let (input, _) = tag("str")(input)?;

    let (input, kind) = opt(delimited(char('('), hex_digit1, char(')')))(input)?;
    let kind = kind
        .and_then(|kind| u8::from_str_radix(kind, 16).ok())
        .map(Kind::from)
        .unwrap_or(Kind::Sz);

    let (input, _) = char(':')(input)?;
    let (input, _) = space0(input)?;

    let (input, data) = delimited(
        char('"'),
        escaped_transform(
            none_of("\\\""),
            '\\',
            alt((
                value("\\", tag("\\")),
                value("\"", tag("\"")),
                value("\n", tag("n")),
                value("\r", tag("r")),
                value("\0", tag("0")),
                value("\\x", tag("x")),
            )),
        ),
        char('"'),
    )(input)?;

    Ok((input, RawValue::Str { kind, data }))
}

fn value_str_empty(input: &str) -> IResult<&str, RawValue> {
    let (input, _) = tag("str")(input)?;

    let (input, kind) = opt(delimited(char('('), hex_digit1, char(')')))(input)?;
    let kind = kind
        .and_then(|kind| u8::from_str_radix(kind, 16).ok())
        .map(Kind::from)
        .unwrap_or(Kind::Sz);

    let (input, _) = char(':')(input)?;

    let (input, _) = ws(tag("\"\""))(input)?;

    Ok((
        input,
        RawValue::Str {
            kind,
            data: String::new(),
        },
    ))
}

pub fn wine_global_option(input: &str) -> IResult<&str, wine::GlobalOption> {
    let (input, _) = char('#')(input)?;

    let (input, arch) = opt(tag("arch="))(input)?;
    if arch.is_some() {
        let (input, arch) = take_while1(|x| !matches!(x, '\n' | '\r'))(input)?;
        return Ok((input, wine::GlobalOption::Arch(arch.to_string())));
    }

    Ok((input, wine::GlobalOption::Other(input.to_string())))
}

pub fn wine_key_option(input: &str) -> IResult<&str, wine::KeyOption> {
    let (input, _) = char('#')(input)?;

    let (input, class) = opt(tag("class=\""))(input)?;
    if class.is_some() {
        let (input, class) = take_while1(|x: char| x != '"')(input)?;
        let (input, _) = char('"')(input)?;
        return Ok((input, wine::KeyOption::Class(class.to_string())));
    }

    let (input, time) = opt(tag("time="))(input)?;
    if time.is_some() {
        let (input, time) = take_while1(AsChar::is_hex_digit)(input)?;
        return Ok((input, wine::KeyOption::Time(time.to_string())));
    }

    let (input, link) = opt(tag("link"))(input)?;
    if link.is_some() {
        return Ok((input, wine::KeyOption::Link));
    }

    Ok((input, wine::KeyOption::Other(input.to_string())))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Kind;
    use pretty_assertions::assert_eq;
    use test_case::test_case;

    #[test_case("Windows Registry Editor Version 5.00; blah", Format::Regedit5 ; "comment after format")]
    fn format_normalization(input: &str, output: Format) {
        assert_eq!(output, registry(input).unwrap().0)
    }

    #[test_case("[HKEY_CURRENT_USER] ; blah", vec![
        Element::Key(KeyName::new("HKEY_CURRENT_USER"), Key::new()),
    ] ; "comment after key")]
    #[test_case("; blah", vec![Element::Comment] ; "whole line comment")]
    #[test_case(r#""a;b"="x;y";foo"#, vec![
        Element::Value(ValueName::named("a;b"), RawValue::Sz("x;y".to_string())),
    ] ; "quoted semicolons")]
    #[test_case(r#""a;b\"c"="x\"y;z";foo"#, vec![
        Element::Value(ValueName::named("a;b\"c"), RawValue::Sz("x\"y;z".to_string())),
    ] ; "quoted semicolons plus escaped quotes")]
    #[test_case("\"foo\"=hex:00,\\\n  01,\\\n  02", vec![
        Element::Value(ValueName::named("foo"), RawValue::Hex { kind: Kind::Binary, bytes: vec![0, 1, 2] }),
    ] ; "concatenated lines")]
    #[test_case("\"foo\"=hex:00,\\ ; blah\n  01; blah", vec![
        Element::Value(ValueName::named("foo"), RawValue::Hex { kind: Kind::Binary, bytes: vec![0, 1] }),
    ] ; "concatenated line with comment")]
    #[test_case("\"x\"=dword:00000000\n\"y\"=dword:00000001\\\n\"z\"=dword:00000002", vec![
        Element::Value(ValueName::named("x"), RawValue::Dword(0)),
        Element::Value(ValueName::named("z"), RawValue::Dword(2)),
    ] ; "garbage after value data")]
    #[test_case("[foo;bar]", vec![
        Element::Key(KeyName::new("foo;bar"), Key::new()),
    ] ; "semicolon in key name")]
    #[test_case("[foo]bar]", vec![
        Element::Key(KeyName::new("foo]bar"), Key::new()),
    ] ; "bracket in key name")]
    #[test_case("[foo]bar;];", vec![
        Element::Key(KeyName::new("foo]bar;"), Key::new()),
    ] ; "semicolon and bracket in key name")]
    #[test_case("[foo\"bar]\n\"baz;\"=hex:00", vec![
        Element::Key(KeyName::new("foo\"bar"), Key::new()),
        Element::Value(ValueName::named("baz;"), RawValue::Hex { kind: Kind::Binary, bytes: vec![0] }),
    ] ; "quote in key name")]
    fn element_normalization(input: &str, output: Vec<Element>) {
        assert_eq!(
            output,
            registry(&format!("Windows Registry Editor Version 5.00\n{input}"))
                .unwrap()
                .1
        )
    }

    #[test_case("[foo]", "foo", Key::new() ; "simple")]
    #[test_case("[foo ]", "foo ", Key::new() ; "inner trailing space")]
    #[test_case("[[baz]]", "[baz]", Key::new() ; "extra brackets")]
    #[test_case("[foo] ; bar", "foo", Key::new() ; "comment")]
    #[test_case("[foo] bar", "foo", Key::new().with_addendum("bar".to_string()) ; "addendum")]
    #[test_case("[foo] bar ; baz", "foo", Key::new().with_addendum("bar".to_string()) ; "addendum with comment")]
    #[test_case(r"[foo\bar]", r"foo\bar", Key::new() ; "one backslash")]
    #[test_case(r"[foo\\bar]", r"foo\bar", Key::new() ; "multiple backslashes")]
    fn valid_keys(raw: &str, name: &str, parsed: Key) {
        assert_eq!(
            Some((KeyName(name.to_string()), parsed)),
            key(raw, Format::Regedit5).unwrap().1
        );
    }

    #[test_case("[]" ; "blank add key")]
    #[test_case("[-]" ; "blank delete key")]
    #[test_case("[ foo]" ; "inner leading space")]
    fn invalid_keys(raw: &str) {
        assert_eq!(None, key(raw, Format::Regedit5).unwrap().1);
    }

    #[test_case(r#"foo\bar"#, r#"[foo\\bar]"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"[foo"bar]"# ; "quote")]
    #[test_case("foo\0bar", "[foo\0bar]" ; "null")]
    #[test_case("fooあbar", "[fooあbar]" ; "Unicode")]
    fn escaped_keys_regedit5(unescaped: &str, raw: &str) {
        assert_eq!(unescaped, key(raw, Format::Regedit5).unwrap().1.unwrap().0 .0.as_str());
    }

    #[test_case(r#"foo\bar"#, r#"[foo\\bar]"# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#"[foo"bar]"# ; "quote")]
    #[test_case(r#"fo[o]bar"#, r#"[fo\[o\]bar]"# ; "bracket")]
    #[test_case("fooあbar", r"[foo\x3042bar]" ; "Unicode")]
    #[test_case(r"Control Panel\International\🌎🌏🌍", r"[Control Panel\\International\\\xd83c\xdf0e\xd83c\xdf0f\xd83c\xdf0d]" ; "surrogate pair")]
    fn escaped_keys_wine(unescaped: &str, raw: &str) {
        assert_eq!(unescaped, key(raw, Format::Wine2).unwrap().1.unwrap().0 .0.as_str());
    }

    #[test_case("@=\"a\"", ValueName::Default, RawValue::Sz("a".to_string()) ; "simple")]
    #[test_case("  @ = \"b\" ", ValueName::Default, RawValue::Sz("b".to_string()) ; "whitespace")]
    #[test_case("\"eq=s\"=\"EQ=S\"", ValueName::Named("eq=s".to_string()), RawValue::Sz("EQ=S".to_string()) ; "quoted equal signs")]
    fn valid_named_values(raw: &str, name: ValueName, parsed: RawValue) {
        assert_eq!(Some((name, parsed)), named_value(raw, Format::Regedit5).unwrap().1);
    }

    #[test_case("@", ValueName::Default ; "default")]
    #[test_case("  @ ", ValueName::Default ; "default whitespace")]
    #[test_case("\"foo\"", ValueName::Named("foo".to_string()) ; "sz simple")]
    #[test_case("\"eq=s\"", ValueName::Named("eq=s".to_string()) ; "sz inner equal signs")]
    #[test_case(r#""sp\\ec\"ial""#, ValueName::Named(r#"sp\ec"ial"#.to_string()) ; "sz escaped characters")]
    fn valid_value_names(raw: &str, parsed: ValueName) {
        assert_eq!(parsed, value_name(raw, Format::Regedit5).unwrap().1);
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
    fn valid_value_data(raw: &str, parsed: RawValue) {
        assert_eq!(parsed, value_data(raw, Format::Regedit5).unwrap().1);
    }

    #[test_case("str:\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::Sz } ; "str sz")]
    #[test_case("str(2):\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::ExpandSz } ; "str expand")]
    #[test_case("str(7):\"foo\"", RawValue::Str { data: "foo".to_string(), kind: Kind::MultiSz } ; "str multi")]
    fn valid_value_data_wine(raw: &str, parsed: RawValue) {
        assert_eq!(parsed, value_data(raw, Format::Wine2).unwrap().1);
    }

    #[test_case("str(0):\"foo\"" ; "str invalid")]
    fn invalid_value_data(raw: &str) {
        assert!(value_data(raw, Format::Regedit5).is_err());
    }

    #[test_case(r"", r#""""# ; "empty")]
    #[test_case(r#"foo\bar"#, r#""foo\\bar""# ; "regular backslash")]
    #[test_case(r#"foo"bar"#, r#""foo\"bar""# ; "quote")]
    fn escaped_value_data_regedit5(unescaped: &str, raw: &str) {
        assert_eq!(
            RawValue::Sz(unescaped.to_string()),
            value_data(raw, Format::Regedit5).unwrap().1
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
    fn escaped_value_data_wine(unescaped: &str, raw: &str) {
        assert_eq!(
            RawValue::Sz(unescaped.to_string()),
            value_data(raw, Format::Wine2).unwrap().1
        );
    }

    #[test_case("#arch=win32", wine::GlobalOption::Arch("win32".to_string()) ; "arch")]
    #[test_case("#foo", wine::GlobalOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_global_options(raw: &str, parsed: wine::GlobalOption) {
        assert_eq!(parsed, wine_global_option(raw).unwrap().1);
    }

    #[test_case("#time=1f", wine::KeyOption::Time("1f".to_string()) ; "time")]
    #[test_case("#class=\"foo\"", wine::KeyOption::Class("foo".to_string()) ; "class")]
    #[test_case("#link", wine::KeyOption::Link ; "link")]
    #[test_case("#foo", wine::KeyOption::Other("foo".to_string()) ; "other")]
    fn valid_wine_key_options(raw: &str, parsed: wine::KeyOption) {
        assert_eq!(parsed, wine_key_option(raw).unwrap().1);
    }
}
