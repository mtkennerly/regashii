use regashii::{error, Format, Key, Kind, Registry, Value, ValueName};

fn main() -> Result<(), error::Write> {
    let registry = Registry::new(Format::Regedit5)
        .with(
            r"HKEY_CURRENT_USER\Software\regashii",
            Key::new()
                .with(ValueName::Default, Value::Sz("string".to_string()))
                .with("expand", Value::ExpandSz("%PATH%".to_string()))
                .with("multi", Value::MultiSz(vec!["a".to_string(), "b".to_string()]))
                .with("dword", Value::Dword(254))
                .with("dword (BE)", Value::DwordBigEndian(255))
                .with("qword", Value::Qword(256))
                .with("binary", Value::Binary(vec![100]))
                .with(
                    "arbitrary",
                    Value::Hex {
                        kind: Kind::ResourceList,
                        bytes: vec![ /* ... */ ],
                    },
                ),
        )
        .with(r"HKEY_CURRENT_USER\Software\regashii\deleted", Key::Delete);

    println!("{}", registry.serialize());
    registry.serialize_file("examples/write.reg")?;

    Ok(())
}
