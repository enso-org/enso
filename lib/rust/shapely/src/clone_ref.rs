// ==============
// === Export ===
// ==============

pub use crate::entry_point;
pub use crate::CloneRef;
pub use crate::NoCloneBecauseOfCustomDrop;



// ================
// === CloneRef ===
// ================

/// Clone for internal-mutable structures. This trait can be implemented only if mutating one
/// structure will be reflected in all of its clones. Please note that it does not mean that all the
/// fields needs to provide internal mutability as well. For example, a structure can remember it's
/// creation time and store it as `f32`. As long as it cannot be mutated, the structure can
/// implement `CloneRef`. In order to guide the auto-deriving mechanism, it is advised to wrap all
/// immutable fields in the `Immutable` newtype.
pub trait CloneRef: Sized + Clone {
    fn clone_ref(&self) -> Self;
}


// === Macros ===

#[macro_export]
macro_rules! impl_clone_ref_as_clone {
    ([$($bounds:tt)*] $($toks:tt)*) => {
        impl <$($bounds)*> CloneRef for $($toks)* {
            fn clone_ref(&self) -> Self {
                self.clone()
            }
        }

        impl <$($bounds)*> From<&$($toks)*> for $($toks)* {
            fn from(t:&$($toks)*) -> Self {
                t.clone_ref()
            }
        }
    };

    ($($toks:tt)*) => {
        impl CloneRef for $($toks)* {
            fn clone_ref(&self) -> Self {
                self.clone()
            }
        }

        impl From<&$($toks)*> for $($toks)* {
            fn from(t:&$($toks)*) -> Self {
                t.clone_ref()
            }
        }
    };
}

#[macro_export]
macro_rules! impl_clone_ref_as_clone_no_from {
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


// === Prim Impls ===

impl_clone_ref_as_clone_no_from!(());
impl_clone_ref_as_clone_no_from!(f32);
impl_clone_ref_as_clone_no_from!(f64);
impl_clone_ref_as_clone_no_from!(i32);
impl_clone_ref_as_clone_no_from!(i64);
impl_clone_ref_as_clone_no_from!(u32);
impl_clone_ref_as_clone_no_from!(u64);
impl_clone_ref_as_clone_no_from!(usize);
impl_clone_ref_as_clone_no_from!(std::any::TypeId);
impl_clone_ref_as_clone_no_from!([T] std::marker::PhantomData<T>);
impl_clone_ref_as_clone_no_from!([T:?Sized] std::rc::Rc<T>);
impl_clone_ref_as_clone_no_from!([T:?Sized] std::rc::Weak<T>);

impl_clone_ref_as_clone_no_from!(wasm_bindgen::JsValue);
impl_clone_ref_as_clone_no_from!(web_sys::Element);
impl_clone_ref_as_clone_no_from!(web_sys::HtmlDivElement);
impl_clone_ref_as_clone_no_from!(web_sys::HtmlElement);
impl_clone_ref_as_clone_no_from!(web_sys::Performance);
impl_clone_ref_as_clone_no_from!(web_sys::WebGl2RenderingContext);
impl_clone_ref_as_clone_no_from!(web_sys::HtmlCanvasElement);
impl_clone_ref_as_clone_no_from!(web_sys::EventTarget);


// === Option ===

impl<T: CloneRef> CloneRef for Option<T> {
    fn clone_ref(&self) -> Self {
        self.as_ref().map(|t| t.clone_ref())
    }
}
