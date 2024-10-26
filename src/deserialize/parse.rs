use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take_while_m_n},
    character::complete::{char, hex_digit1, none_of, space0},
    combinator::{opt, value},
    multi::separated_list0,
    sequence::delimited,
    IResult,
};

use crate::{deserialize::unescape_wine_unicode, wine, Format, Key, KeyKind, KeyName, Kind, RawValue, ValueName};

pub fn ws<'a, F, O, E: nom::error::ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: 'a,
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(space0, inner, space0)
}

pub fn key(input: &str, format: Format) -> IResult<&str, Option<(KeyName, Key)>> {
    let (input, _) = space0(input)?;
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

    let (name, addendum) = match input.rfind("]") {
        Some(i) => (&input[..i], input[i + 1..].trim()),
        None => (input, ""),
    };

    let (input, name) = if format.is_wine() {
        let (input, name) = escaped_transform(
            none_of("\\"),
            '\\',
            alt((
                value("\\", tag("\\")),
                value("[", tag("[")),
                value("]", tag("]")),
                value("\\x", tag("x")),
            )),
        )(name)?;
        (input, unescape_wine_unicode(&name))
    } else {
        (input, name.to_string())
    };

    let addendum = (!addendum.is_empty()).then(|| addendum.to_string());

    let key = Key {
        kind: if deleted.is_some() {
            KeyKind::Delete
        } else {
            KeyKind::Add
        },
        addendum,
        ..Default::default()
    };

    Ok((input, Some((KeyName::new(name), key))))
}

pub fn named_value(input: &str, format: Format) -> IResult<&str, (ValueName, RawValue)> {
    let (input, _) = space0(input)?;
    let (input, name) = value_name(input, format)?;
    let (input, _) = ws(char('='))(input)?;
    let (input, value) = value_data(input, format)?;
    let (input, _) = space0(input)?;

    Ok((input, (name, value)))
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

    let (input, bytes) = separated_list0(char(','), ws(take_while_m_n(2, 2, |x: char| x.is_ascii_hexdigit())))(input)?;
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
        return Ok((input, wine::GlobalOption::Arch(input.to_string())));
    }

    Ok((input, wine::GlobalOption::Other(input.to_string())))
}

pub fn wine_key_option(input: &str) -> IResult<&str, wine::KeyOption> {
    let (input, _) = char('#')(input)?;

    let (input, class) = opt(tag("class=\""))(input)?;
    if class.is_some() {
        let class = match input.strip_suffix('"') {
            Some(input) => input,
            None => input,
        };
        return Ok((input, wine::KeyOption::Class(class.to_string())));
    }

    let (input, time) = opt(tag("time="))(input)?;
    if time.is_some() {
        return Ok((input, wine::KeyOption::Time(input.to_string())));
    }

    let (input, link) = opt(tag("link"))(input)?;
    if link.is_some() {
        return Ok((input, wine::KeyOption::Link));
    }

    Ok((input, wine::KeyOption::Other(input.to_string())))
}
