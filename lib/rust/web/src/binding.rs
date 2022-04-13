//! Parent module for web API bindings. It contains both native, WASM bindings and mock ones, which
//! allow compilation of the API to native code without throwing panics in order for it to be useful
//! in native tests.


// ==============
// === Export ===
// ==============

pub mod mock;
pub mod wasm;
