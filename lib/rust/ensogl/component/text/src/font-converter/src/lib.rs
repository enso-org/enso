use enso_prelude::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn main() {
    init_tracing(TRACE);
    event!(WARN, "Hello, world!");
}
