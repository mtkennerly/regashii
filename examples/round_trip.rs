//! This example reads a registry file and writes it back out.
//! You can use this to see how the formatting is normalized and comments are removed.

use regashii::Registry;

fn main() {
    let mut args = std::env::args();
    args.next();
    let input_file = args.next().expect("pass a file name on the command line");
    let output_file = args.next().unwrap_or_else(|| "examples/round_trip.reg".to_string());

    let registry = Registry::deserialize_file(input_file).unwrap();
    registry.serialize_file(output_file).unwrap();
}
