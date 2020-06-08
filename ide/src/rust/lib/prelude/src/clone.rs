
use crate::*;
pub use shapely::CloneRef;



// ================
// === CloneRef ===
// ================

/// Clone for internal-mutable structures. This trait can be implemented only if mutating one
/// structure will be reflected in all of its clones. Please note that it does not mean that all the
/// fields needs to provide internal mutability as well. For example, a structure can remember it's
/// creation time and store it as `f32`. As long as it cannot be mutated, the structure can
/// implement `CloneRef`. In order to guide the auto-deriving mechanism, it is advised to wrap all
/// immutable fields in the `Immutable` newtype.
pub trait CloneRef: Sized {
    fn clone_ref(&self) -> Self;
}

#[macro_export]
macro_rules! impl_clone_ref_as_clone {
    ([$($bounds:tt)*] $($toks:tt)*) => {
        impl <$($bounds)*> CloneRef for $($toks)* {
            fn clone_ref(&self) -> Self {
                self.clone()
            }
        }
    };

    ($($toks:tt)*) => {
        impl CloneRef for $($toks)* {
            fn clone_ref(&self) -> Self {
                self.clone()
            }
        }
    };
}

impl_clone_ref_as_clone!(());
impl_clone_ref_as_clone!(f32);
impl_clone_ref_as_clone!(f64);
impl_clone_ref_as_clone!(i32);
impl_clone_ref_as_clone!(i64);
impl_clone_ref_as_clone!(usize);
impl_clone_ref_as_clone!([T] PhantomData<T>);
impl_clone_ref_as_clone!([T:?Sized] Rc<T>);
impl_clone_ref_as_clone!([T:?Sized] Weak<T>);

impl_clone_ref_as_clone!(wasm_bindgen::JsValue);
impl_clone_ref_as_clone!(web_sys::HtmlDivElement);
impl_clone_ref_as_clone!(web_sys::HtmlElement);
impl_clone_ref_as_clone!(web_sys::Performance);
impl_clone_ref_as_clone!(web_sys::WebGl2RenderingContext);
impl_clone_ref_as_clone!(web_sys::HtmlCanvasElement);
impl_clone_ref_as_clone!(web_sys::EventTarget);
