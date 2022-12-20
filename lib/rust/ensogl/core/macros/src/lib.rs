#![feature(proc_macro_span)]

mod optimize_shape_def;

mod prelude {
    pub use enso_macro_utils::repr;
    pub use proc_macro2::Span;
    pub use proc_macro2::TokenStream;
    pub use quote::quote;
}

#[proc_macro_attribute]
pub fn optimize_shape_def(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    optimize_shape_def::run(attr, input)
}
