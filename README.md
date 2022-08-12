# conbor

Simple, opinionated GPLv3 C++20 CBOR encoder and decoder.

Built largely around concepts, hence the name.  Should be useable similarly to
[Rust's Serde](https://serde.rs/).  You can use the `to_cbor` and `from_cbor`
functions to encode and decode any appropriate type, and you can define these
for your own types.

Also included is a generic `conbor::Value` type.

This operates in exceptional and exceptionless varieties.  All `to_cbor` and
`from_cbor` methods natively return a `conbor::Result` type, and the naked
non-`conbor::Result` variants throw an exception automatically if it's
encountered.
