/// Wine global options.
/// These are represented as lines beginning with `#`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum GlobalOption {
    Arch(String),
    Other(String),
}

/// Wine key-level options.
/// These are represented as lines beginning with `#`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum KeyOption {
    Time(u64),
    Class(String),
    Link,
    Other(String),
}
