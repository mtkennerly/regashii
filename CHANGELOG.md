## Unreleased

* Added:
  * Support for addenda after key names (e.g., `[foo] 123`).
    Regedit ignores this, but Wine uses it to store the modified time.
  * Support for the `WINE REGISTRY Version 2` format.
    This includes Wine's global options (e.g., `#arch=win32`),
    key-level options (e.g., `#link`),
    and string values written as `"name"=str:"data"`.
* Fixed:
  * Extra backslashes in key paths are now ignored and normalized.
  * Quotes and semicolons in key names no longer corrupt the deserialization.
* Changed:
  * The variants of `Key` now name their fields.
  * Values serialized as hexadecimal bytes (e.g., `Kind::Binary`)
    are now split onto multiple lines when they're long enough.

## v0.1.0 (2024-05-29)

* Initial release.
