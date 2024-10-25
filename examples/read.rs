//! This example reads a registry file and prints some of its content.

use regashii::{error, KeyKind, Registry};

fn main() -> Result<(), error::Read> {
    let registry = Registry::deserialize_file("tests/simple-regedit5.reg")?;

    for (key_name, key) in registry.keys() {
        match key.kind() {
            KeyKind::Delete => {
                println!("\n{key_name:?}  (delete)");
            }
            KeyKind::Add => {
                println!("\n{key_name:?}  (add)");
                for (value_name, value) in key.values() {
                    println!("  {value_name:?} = {value:?}");
                }
            }
            KeyKind::Replace => {
                println!("\n{key_name:?}  (replace)");
                for (value_name, value) in key.values() {
                    println!("  {value_name:?} = {value:?}");
                }
            }
        }
    }

    Ok(())
}
