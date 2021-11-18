use crate::binding::emscripten_get_value_from_memory;
use crate::prelude::*;
use wasm_bindgen::JsValue;



// ================================
// === EmscriptenRepresentation ===
// ================================

/// Trait of type having its representation in emscripten API
///
/// The _emscirpten API_ is a set of functions that are put to library by emscripten SDK, the
/// especially useful is a function reading value from given address in `msdfgen` library memory
/// (we cannot do it directly, because each wasm module have separate address space)
pub trait EmscriptenRepresentation: Sized {
    const EMSCRIPTEN_SIZE_IN_BYTES: usize;
    const EMSCRIPTEN_TYPE_NAME: &'static str;

    /// Convert from JsValue returned from emscripten API
    fn from_js_value(js_value: JsValue) -> Option<Self>;

    /// Read value from address in `msdfgen` library memory
    fn read_from_emscripten_memory(address: usize) -> Option<Self> {
        let js_value = emscripten_get_value_from_memory(address, Self::EMSCRIPTEN_TYPE_NAME);
        Self::from_js_value(js_value)
    }
}

impl EmscriptenRepresentation for f32 {
    const EMSCRIPTEN_SIZE_IN_BYTES: usize = 4;
    const EMSCRIPTEN_TYPE_NAME: &'static str = "float";
    fn from_js_value(js_value: JsValue) -> Option<Self> {
        js_value.as_f64().map(|f| f as f32)
    }
}

impl EmscriptenRepresentation for f64 {
    const EMSCRIPTEN_SIZE_IN_BYTES: usize = 8;
    const EMSCRIPTEN_TYPE_NAME: &'static str = "double";
    fn from_js_value(js_value: JsValue) -> Option<Self> {
        js_value.as_f64()
    }
}



// =======================
// === ArrayMemoryView ===
// =======================

/// View of array in `msdfgen` library memory
#[derive(Debug)]
pub struct ArrayMemoryView<F: EmscriptenRepresentation> {
    begin_address: usize,
    end_address:   usize,
    type_marker:   std::marker::PhantomData<F>,
}

/// Iterator over values in `msdfgen` library memory
///
/// It cannot outlives view from which was created, because one might expect, that data may be freed
/// by library once view is destroyed
#[derive(Clone, Copy, Debug)]
pub struct ArrayMemoryViewIterator<'a, F: EmscriptenRepresentation> {
    next_read_address: usize,
    end_address:       usize,
    view_lifetime:     std::marker::PhantomData<&'a ArrayMemoryView<F>>,
}

impl<F: EmscriptenRepresentation> ArrayMemoryView<F> {
    /// Create view from first element's address and array size
    pub fn new(address: usize, size: usize) -> ArrayMemoryView<F> {
        let size_in_bytes = size * F::EMSCRIPTEN_SIZE_IN_BYTES;
        ArrayMemoryView {
            begin_address: address,
            end_address:   address + size_in_bytes,
            type_marker:   std::marker::PhantomData,
        }
    }

    /// Create an empty view
    pub fn empty() -> ArrayMemoryView<F> {
        ArrayMemoryView {
            begin_address: 0,
            end_address:   0,
            type_marker:   std::marker::PhantomData,
        }
    }

    /// Iterator over elements
    pub fn iter(&self) -> ArrayMemoryViewIterator<F> {
        ArrayMemoryViewIterator {
            next_read_address: self.begin_address,
            end_address:       self.end_address,
            view_lifetime:     std::marker::PhantomData,
        }
    }
}

impl<'a, F: EmscriptenRepresentation> Iterator for ArrayMemoryViewIterator<'a, F> {
    type Item = F;
    fn next(&mut self) -> Option<Self::Item> {
        let has_element = self.next_read_address < self.end_address;
        has_element.and_option_from(|| {
            let current_value = F::read_from_emscripten_memory(self.next_read_address).unwrap();
            self.next_read_address += F::EMSCRIPTEN_SIZE_IN_BYTES;
            Some(current_value)
        })
    }
}
