## v0.3.0 (2025-03-26)

* Added:
  * `Registry::apply`
  * `Registry::remove`
  * `Registry::remove_wine_option`
  * `Key::remove`
  * `Key::remove_wine_option`
* Fixed:
  * Wine global options were serialized (if set) even in non-Wine formats.
    This issue did not occur with key options.

## v0.2.0 (2024-10-27)

* Added:
  * Support for the `WINE REGISTRY Version 2` format.
    This includes Wine's global options (e.g., `#arch=win32`),
    key-level options (e.g., `#link`),
    and string values written as `"name"=str:"data"`.
  * Support for addenda after key names (e.g., `[foo] 123`).
    Regedit ignores this, but Wine uses it to store the modified time.
  * `error::Deserialize::Unparsable`, `Registry::format`, `Format::raw`, and `KeyName::raw`,
* Fixed:
  * Extra backslashes in key paths are now ignored and normalized.
  * Quotes and semicolons in key names no longer corrupt the deserialization.
  * Empty string values are now preserved when deserializing.
  * Parsing performance would degrade as the size of the input increased.
    On each key/value insert,
    the library would iterate through all keys/values found so far
    to check for a potential case-insensitive match.
    Now, the library caches the case-normalized form on insert,
    so that later lookups are O(1).
* Changed:
  * Parsing is now much faster due to moving away from regular expressions.
    In release mode, the library can parse a 520 MB full registry export in 5 seconds,
    which would have taken 20 seconds previously
    (even with the key lookup fix mentioned above).
  * `Key` is now a struct instead of an enum.
  * Values serialized as hexadecimal bytes (e.g., `Kind::Binary`)
    are now split onto multiple lines when they're long enough.
  * Renamed `Registry::update` to `Registry::insert`,
    `Key::update` to `Key::insert`,
    and added underscores to the `Format` string constants.
* Removed:
  * `Registry::keys_mut` because it would interfere with the case-normalized lookup optimizations.
  * Dependency on `once_cell`, using `std::sync::LazyLock` instead.

## v0.1.0 (2024-05-29)

* Initial release.
