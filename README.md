# Regashii

Regashii is a Rust crate that lets you (de)serialize Windows Regedit `*.reg` files.

* [Examples](https://github.com/mtkennerly/regashii/tree/master/examples)
* [Documentation](https://docs.rs/crate/regashii/latest)

## Design
* This crate aims to be as tolerant as Regedit itself.
  Regedit will generally ignore lines that it can't handle
  and even ignore garbage in some positions (e.g., after a key name).
  When you parse a `*.reg` file with this crate,
  the output should include the keys/values that Regedit would actually import.
* On the other hand, if Regedit would ignore some text,
  then this crate won't include it in the parsed output.
  This crate does not preserve formatting or comments.
* This crate can (de)serialize from string data,
  or it can read/write the files for you.
  Regedit uses UTF-8 or UTF-16 depending on the export format,
  which this crate automatically handles accordingly.
* This crate attempts to handle various edge cases in a way compatible with Regedit.
  This includes:

  * Preserving invalid values that Regedit is willing to import
    (e.g., a `hex(4)` dword value with the wrong number of bytes)
  * Serializing `sz` strings values as `hex(1)` bytes if they contain illegal characters
  * Handling strings which may or may not be null-terminated

  If you find an edge case that's handled poorly, please report it.

## Sample
### Read
```rust
use regashii::{Key, Registry, Value};

let registry = Registry::deserialize_file("sample.reg").unwrap();
for (key_name, key) in registry.keys() {
    match key {
        Key::Delete => {
            // On import, Regedit would delete this key
        },
        Key::Add(values) => {
            // On import, Regedit would add this key and its values
            for (value_name, value) in values {
                match value {
                    Value::Delete => { /* This value would be deleted */ },
                    Value::Sz(string) => { /* This value would be added */ }
                    _ => { /* and so on */},
                }
            }
        },
        Key::Replace(values) => {
            // On import, Regedit would delete and re-add this key and its values
            for (value_name, value) in values {
                // ...
            }
        },
    }
}
```

### Write
```rust
use regashii::{Format, Key, Registry, Value, ValueName};

let registry = Registry::new(Format::Regedit5)
    .with(
        r"HKEY_CURRENT_USER\Software\Foo",
        Key::new()
            .with(ValueName::Default, Value::Sz("some string".to_string()))
            .with("this is a dword", Value::Dword(255)),
    );

registry.serialize_file("sample.reg").unwrap();
```
