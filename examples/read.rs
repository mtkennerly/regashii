//! This example reads a registry file and prints some of its content.

use regashii::{error, Key, Registry};

fn main() -> Result<(), error::Read> {
    let registry = Registry::deserialize_file("tests/simple-regedit5.reg")?;

    for (key_name, key) in registry.keys() {
        match key {
            Key::Delete => {
                println!("\n{key_name:?}  (delete)");
            }
            Key::Add { values, .. } => {
                println!("\n{key_name:?}  (add)");
                for (value_name, value) in values {
                    println!("  {value_name:?} = {value:?}");
                }
            }
            Key::Replace { values, .. } => {
                println!("\n{key_name:?}  (replace)");
                for (value_name, value) in values {
                    println!("  {value_name:?} = {value:?}");
                }
            }
        }
    }

    Ok(())
}
