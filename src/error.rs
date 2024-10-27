/// Error when deserializing a string
#[derive(Debug, thiserror::Error)]
pub enum Deserialize {
    #[error("Malformed")]
    Malformed,
    #[error("Unknown format: {0}")]
    UnknownFormat(String),
}

/// Error when reading and deserializing a file.
#[derive(Debug, thiserror::Error)]
pub enum Read {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error("Unsupported encoding")]
    UnsupportedEncoding,
    #[error(transparent)]
    Deserialize(#[from] Deserialize),
}

/// Error when serializing and writing to a file.
#[derive(Debug, thiserror::Error)]
pub enum Write {
    #[error(transparent)]
    Io(#[from] std::io::Error),
}
