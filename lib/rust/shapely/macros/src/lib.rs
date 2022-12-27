//! This crate defines a custom derive macro `Iterator`. Should not be used
//! directly, but only through `enso-shapely` crate, as it provides utilities
//! necessary for the generated code to compile.

// === Features ===
#![feature(exact_size_is_empty)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]



extern crate proc_macro;

mod derive_clone_ref;
mod derive_entry_point;
mod derive_for_each_variant;
mod derive_iterator;
mod derive_no_clone;
mod gen;
mod overlappable;
mod tagged_enum;

mod prelude {
    pub use enso_macro_utils::repr;
    pub use proc_macro2::Span;
    pub use proc_macro2::TokenStream;
    pub use quote::quote;
}

use crate::derive_iterator::IsMut;

/// For `struct Foo<T>` or `enum Foo<T>` provides:
/// * `IntoIterator` implementations for `&'t Foo<T>`, `iter` and `into_iter`
/// methods.
///
/// The iterators will:
/// * for structs: go over each field that declared type is same as the struct's last type
///   parameter.
/// * enums: delegate to current constructor's nested value's iterator.
///
/// Enums are required to use only a single element tuple-like variant. This
/// limitation should be lifted in the future.
///
/// Any dependent type stored in struct, tuple or wrapped in enum should have
/// dependency only in its last type parameter. All dependent types that are not
/// tuples nor directly the yielded type, are required to provide `iter` method
/// that returns a compatible iterator (possible also derived).
///
/// Caller must have the following features enabled:
/// ```
/// #![feature(generators)]
/// #![feature(type_alias_impl_trait)]
/// ```
///
/// When used on type that takes no type parameters, like `struct Foo`, does
/// nothing but yields no errors.
#[proc_macro_derive(Iterator)]
pub fn derive_iterator(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_iterator::derive(input, IsMut::Immutable)
}

/// Same as `derive(Iterator)` but generates mutable iterator.
///
/// It is separate, as some types allow deriving immutable iterator but ont the
/// mutable one.
#[proc_macro_derive(IteratorMut)]
pub fn derive_iterator_mut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_iterator::derive(input, IsMut::Mutable)
}

/// Derives `CloneRef` implementation for given type. It performs `clone_ref` on every member
/// field. The input type must implement `Clone` and its every field must implement `CloneRef`.
///
/// For generic types no bounds are introduced in the generated implementation. To customize this
/// behavior user might add `#[clone_ref(bound="…")]` attribute. Then the generated implementation
/// will use the provided bounds.
///
/// Moreover, for a given struct `X` this macro generates also `impl From<&X> for X` which uses
/// `CloneRef` under the hood. The semantics of `CloneRef` makes each object to naturally provide
/// transformation from reference to an owned type.
///
/// Supported inputs are structs (unit, named, unnamed), enums (with unit, named, unnamed and no
/// variants at all). Unions are currently not supported.
#[proc_macro_derive(CloneRef, attributes(clone_ref))]
pub fn derive_clone_ref(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_clone_ref::derive(input)
}

/// Makes sure that the structure does not derive [`Clone`] and that it implements custom [`Drop`]
/// implementation.
///
/// For the given input
/// ```text
/// #[derive(NoCloneBecauseOfCustomDrop)]
/// struct Test {}
/// ```
///
/// The following output will be generated:
/// ```text
/// struct Test {}
/// impl !Clone for Test {}
//  impl ImplementsDrop for Test {}
/// ```
#[proc_macro_derive(NoCloneBecauseOfCustomDrop)]
pub fn derive_no_clone(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_no_clone::derive(input)
}

/// Implements the `ForEachVariant` derive macro which creates a helper for iterating over each
/// variant of an enum at compile time. The derive panics if used on non-enum types.
///
/// The derive creates a macro (hereafter called loop-macro) named `for_each_NAME_variant` where
/// `NAME` is replaced with the name of the enum converted to snake case. The loop-macro takes a
/// name of another macro (hereafter called iterator-macro) as an argument followed by a
/// parenthesized list of extra arguments. The loop-macro expands to a call of the iterator-macro
/// with a list of comma-separated names of the enum variants wrapped in square brackets, followed
/// by the extra arguments defined above.
///
/// For example, the following code:
/// ```no_compile
/// #[derive(ForEachVariant)]
/// pub enum FooBar {
///     Foo,
///     Bar,
/// }
/// ```
/// results in the following macro being defined:
/// ```
/// #[macro_export]
/// macro_rules! for_each_foo_bar_variant {
///     ( $f:ident($( $args:tt )*) ) => { $f!([Foo, Bar] $($args)*) }
/// }
///
/// pub(crate) use for_each_foo_bar_variant;
/// ```
#[proc_macro_derive(ForEachVariant)]
pub fn derive_for_each_variant(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_for_each_variant::derive(input)
}

/// Exposes the function as an application entry point. Entry points are alternative application
/// running modes that you can access by adding `?entry=` to the end of the application URL. If no
/// explicit name is provided to this macro (as an argument), the crate name will be used as the
/// entry point name.
#[proc_macro_attribute]
pub fn entry_point(
    args: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    derive_entry_point::derive(args, item)
}

#[allow(missing_docs)]
#[proc_macro_attribute]
pub fn overlappable(
    attrs: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    overlappable::overlappable(attrs, input)
}

/// Transforms Rust enums into enums where each variant is a separate type. It also implements
/// several traits (such as conversions between variants and the enum type) and defines utility
/// functions, such as constructors. See [`tagged_enum::run`] to learn more.
#[proc_macro_attribute]
pub fn tagged_enum(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    tagged_enum::run(attr, input)
}

/// A macro allowing generation of variety of things. For now it supports a few cases, however, it
/// is meant to be a hub for other extensions.
///
///
/// # Update and set functions generation.
/// This macro allows generation of `update` and `set` function given its `modify` version. Let's
/// consider the following usage:
///
/// ```text
/// #[gen(update, set)]
/// fn modify_column_count(&self, f: impl FnOnce(&mut Option<usize>)) -> &Self {
///     ...
//  }
/// ```
/// 
/// This macro will generate the following code:
/// ```text
/// fn modify_column_count(&self, f: impl FnOnce(&mut Option<usize>)) -> &Self {
///     ...
/// }
/// fn update_column_count(&self, f: impl FnOnce(Option<usize>) -> Option<usize>) -> &Self {
///     self.modify_column_count(|t| *t = f(*t))
/// }
/// fn set_column_count(&self, v: impl Into<Option<usize>>) -> &Self {
///     self.modify_column_count(|t| *t = v.into())
/// }
/// ```
/// 
///
/// ## Hierarchical update and set functions generation.
///
/// In case the modify field type is `Vector2`, `Vector3`, or `Vector4`, the `update` and `set`
/// functions will be generated for the `x`, `y`, `z`, and `w` fields as well. Consider the
/// following example:
/// ```text
/// #[enso_shapely::gen(update, set)]
/// fn modify_position(&self, f: impl FnOnce(&mut Vector2<f32>)) -> &Self {
///     self.display_object().modify_auto_layout(|l| f(&mut l.columns_and_rows_count.x));
///     self
/// }
/// ```
/// 
/// The following output will be generated:
/// ```text
/// fn modify_position(&self, f: impl FnOnce(&mut Vector2<f32>)) -> &Self {
///     self.display_object().modify_auto_layout(|l| f(&mut l.columns_and_rows_count.x));
///     self
/// }
/// fn update_position(&self, f: impl FnOnce(Vector2<f32>) -> Vector2<f32>) -> &Self {
///     self.modify_position(|t| *t = f(*t))
/// }
/// fn set_position(&self, v: impl Into<Vector2<f32>>) -> &Self {
///     self.modify_position(|t| *t = v.into())
/// }
/// #[enso_shapely::gen(update, set)]
/// fn modify_position_x(&self, f: impl FnOnce(&mut f32)) -> &Self {
///     self.modify_position(|t| f(&mut t.x))
/// }
/// #[enso_shapely::gen(update, set)]
/// fn modify_position_y(&self, f: impl FnOnce(&mut f32)) -> &Self {
///     self.modify_position(|t| f(&mut t.y))
/// }
/// ```
/// 
/// You can skip generation of field modifiers by providing the `skip_fields` argument to the macro:
/// ```text
/// #[enso_shapely::gen(update, set, skip_fields)]
/// fn modify_position(&self, f: impl FnOnce(&mut Vector2<f32>)) -> &Self {
///     self.display_object().modify_auto_layout(|l| f(&mut l.columns_and_rows_count.x));
///     self
/// }
/// ```
/// 
///
/// ## Custom conversions
/// By default, the `set` and `update` methods will use `Into` trait to convert the resulting value
/// to the expected one. This behavior can be overriden by setting `trait` and `fn` properties of
/// the `set` argument. For example, the following code:
/// ```text
/// #[enso_shapely::gen(update, set(trait = "IntoVectorTrans2<Unit>", fn = "into_vector_trans()"))]
/// fn modify_gap(&self, f: impl FnOnce(&mut Vector2<Unit>)) -> &Self {
///     self.display_object().modify_auto_layout(|l| f(&mut l.gap));
///     self
/// }
/// ```
/// 
/// Will result in the following code:
/// ```text
/// fn modify_gap(&self, f: impl FnOnce(&mut Vector2<Unit>)) -> &Self {
///     self.display_object().modify_auto_layout(|l| f(&mut l.gap));
///     self
/// }
/// fn update_gap<Out: IntoVectorTrans2<Unit>>(
///     &self,
///     f: impl FnOnce(Vector2<Unit>) -> Out,
/// ) -> &Self {
///     self.modify_gap(|t| *t = f(*t).into_vector_trans())
/// }
/// fn set_gap(&self, v: impl IntoVectorTrans2<Unit>) -> &Self {
///     self.modify_gap(|t| *t = v.into_vector_trans())
/// }
/// #[enso_shapely::gen(update, set)]
/// fn modify_gap_x(&self, f: impl FnOnce(&mut Unit)) -> &Self {
///     self.modify_gap(|t| f(&mut t.x))
/// }
/// #[enso_shapely::gen(update, set)]
/// fn modify_gap_y(&self, f: impl FnOnce(&mut Unit)) -> &Self {
///     self.modify_gap(|t| f(&mut t.y))
/// }
/// ```
/// 
///
///
/// # Debugging the macro
/// You can use the `debug` parameter to cause the macro to panic and print to the console its
/// generated code. In order to do so, simply use the `debug` parameter among other ones:
/// ```text
/// #[gen(debug, update, set)]
/// fn modify_column_count(&self, f: impl FnOnce(&mut Option<usize>)) -> &Self {
///     ...
//  }
/// ```
#[proc_macro_attribute]
pub fn gen(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    gen::run(attr, input)
}
