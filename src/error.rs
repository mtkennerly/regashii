/// Error when deserializing a string
#[derive(Debug, thiserror::Error)]
pub enum Deserialize {
    /// The registry content begins with an unknown format header.
    #[error("Unknown format: {0}")]
    UnknownFormat(String),
    /// Parsing failed for some other reason.
    /// This shouldn't normally happen, since the parsing tries to be forgiving.
    /// If you run into this, please open a ticket with the input that triggered it.
    #[error("Unparsable")]
    Unparsable,
}

/// Error when reading and deserializing a file.
#[derive(Debug, thiserror::Error)]
pub enum Read {
    /// Unable to perform file operation.
    #[error(transparent)]
    Io(#[from] std::io::Error),
    /// File uses an unsupported encoding.
    #[error("Unsupported encoding")]
    UnsupportedEncoding,
    /// Unable to deserialize the file content.
    #[error(transparent)]
    Deserialize(#[from] Deserialize),
}

/// Error when serializing and writing to a file.
#[derive(Debug, thiserror::Error)]
pub enum Write {
    /// Unable to perform file operation.
    #[error(transparent)]
    Io(#[from] std::io::Error),
}
