//! This module defines an abstraction for all types which can be used as GLSL code values.

use crate::prelude::*;

use crate::data::color;
use crate::math::topology::unit::Unit;
use crate::system::gpu::shader::glsl::Glsl;
use crate::system::gpu::types::*;

use nalgebra::Scalar;
use std::ops::*;


// ======================
// === VarInitializer ===
// ======================

/// A trait implemented on types which can be used to construct Var<T>. For example,
/// `Var<i32>` can be constructed from `i32`, `Glsl` code, or just from `&str` for convenient usage.
/// Please refer to `Var` documentation to understand what it is all about.
pub trait VarInitializer<T> = VarInitializerMarker<T> + Into<Glsl>;

/// Marker trait for `VarInitializer`.
pub trait VarInitializerMarker<T> {}


// === Instances ===

impl<T> VarInitializerMarker<Var<T>> for Glsl    {}
impl<T> VarInitializerMarker<Var<T>> for &Glsl   {}
impl<T> VarInitializerMarker<Var<T>> for String  {}
impl<T> VarInitializerMarker<Var<T>> for &String {}
impl<T> VarInitializerMarker<Var<T>> for &str    {}
impl<T> VarInitializerMarker<Var<T>> for  T      {}
impl<T> VarInitializerMarker<Var<T>> for &T      {}

impl<E1,E2,T> VarInitializerMarker<Var<color::Rgba<E1,T>>> for color::Rgb<E2,T>
    where E1:color::RgbStandard, E2:color::RgbStandard, T:color::Component {}

impl<E1,E2,T> VarInitializerMarker<Var<color::Rgba<E1,T>>> for color::Rgba<E2,T>
    where E1:color::RgbStandard, E2:color::RgbStandard, T:color::Component {}

impl<E,T,G> VarInitializerMarker<Var<color::Rgba<E,T>>> for color::SdfSampler<G>
    where E:color::RgbStandard, T:color::Component {}

impl<T,U,V> VarInitializerMarker<Var<Unit<T,Anything,V>>> for Unit<T,U,V> where {}

impl<T,S1,S2> VarInitializerMarker<Var<Vector2<T>>> for (S1,S2)
    where T:Scalar, S1:VarInitializerMarkerNested<Var<T>>, S2:VarInitializerMarkerNested<Var<T>> {}


// === Nested ===

/// Marker trait for nested cases of `VarInitializer`.
pub trait VarInitializerMarkerNested<T> {}

impl<T,S> VarInitializerMarkerNested<T>      for S where S:VarInitializerMarker<T> {}
impl<T>   VarInitializerMarkerNested<Var<T>> for  Var<T> {}
impl<T>   VarInitializerMarkerNested<Var<T>> for &Var<T> {}



// ===========
// === Var ===
// ===========

/// Var contains either the value it is parametrized with or its GLSL representation.
///
/// It is widely used to define shapes. For example, you may want to draw a circle which radius
/// depend on `time : Var<f32>`. However, the time is not defined in Rust, it is a variable which
/// lives in GLSL and is passed as uniform to shaders. Thus `time` is defined as
/// `Var::Dynamic("time".into())`. The idea here is that all operations, like `time * 2.0` should
/// work no matter if `time` was defined as `GLSL` code or normal Rust value.
#[derive(Clone,Debug,Display)]
pub enum Var<T> {
    /// Static value.
    Static(T),

    /// Dynamic value expressed as GLSL code.
    Dynamic (Glsl),
}

// === Constructors ===

impl<T,S> From<T> for Var<S>
    where T : VarInitializer<Var<S>> {
    default fn from(t:T) -> Self {
        Self::Dynamic(t.into())
    }
}

impl<T> From<T> for Var<T>
where T : VarInitializer<Var<T>> {
    fn from(t:T) -> Self {
        Self::Static(t)
    }
}

// === Conversions ===

impls! {[T:Clone] From<&Var<T>> for Var<T> { |t| t.clone() }}

impls! {[T:RefInto<Glsl>] From<&Var<T>> for Glsl { |t|
    match t {
        Var::Static  (s) => s.into(),
        Var::Dynamic (s) => s.clone(),
    }
}}

impls! {[T:Into<Glsl>] From<Var<T>> for Glsl { |t|
    match t {
        Var::Static  (s) => s.into(),
        Var::Dynamic (s) => s,
    }
}}



// ==================
// === Operations ===
// ==================

impl<T> Abs for Var<T>
where T:Abs {
    fn abs(&self) -> Self {
        match self {
            Self::Static  (t) => Var::Static(t.abs()),
            Self::Dynamic (t) => Var::Dynamic(format!("abs({})",t).into())
        }
    }
}

impl<T:Scalar> HasComponents for Var<Vector2<T>> {
    type Component = Var<T>;
}

impl<T:Scalar> HasComponents for Var<Vector3<T>> {
    type Component = Var<T>;
}

impl<T:Scalar> Dim1 for Var<Vector2<T>> {
    fn x(&self) -> Var<T> {
        match self {
            Self::Static  (t) => Var::Static(t.x),
            Self::Dynamic (t) => Var::Dynamic(format!("{}.x",t).into())
        }
    }
}

impl<T:Scalar> Dim2 for Var<Vector2<T>> {
    fn y(&self) -> Var<T> {
        match self {
            Self::Static  (t) => Var::Static(t.y),
            Self::Dynamic (t) => Var::Dynamic(format!("{}.y",t).into())
        }
    }
}

impl<T:Scalar> Dim1 for Var<Vector3<T>> {
    fn x(&self) -> Var<T> {
        match self {
            Self::Static  (t) => Var::Static(t.x),
            Self::Dynamic (t) => Var::Dynamic(format!("{}.x",t).into())
        }
    }
}

impl<T:Scalar> Dim2 for Var<Vector3<T>> {
    fn y(&self) -> Var<T> {
        match self {
            Self::Static  (t) => Var::Static(t.y),
            Self::Dynamic (t) => Var::Dynamic(format!("{}.y",t).into())
        }
    }
}

impl<T:Scalar> Dim3 for Var<Vector3<T>> {
    fn z(&self) -> Var<T> {
        match self {
            Self::Static  (t) => Var::Static(t.z),
            Self::Dynamic (t) => Var::Dynamic(format!("{}.z",t).into())
        }
    }
}



// =================
// === Operators ===
// =================

// The whole code in this section defines operators such as `+`, `-`, or `*` for the `Var<T>` type.
// Unfortunately, due to lack of abstractions over references and values, we have to generate
// hundreds of lines of boring code.

macro_rules! define_operator_newtype {
    ( $name:ident $fn:ident $base:ident where [$($bounds:tt)*] {
        |$v_lhs:ident, $v_rhs:ident| $($body:tt)*
    } ) => {
        impl<'t,B,A> $name<&'t $base<B>> for &'t $base<A>
        where &'t A : $name<&'t B>, $($bounds)* {
            type Output = $base<<&'t A as $name<&'t B>>::Output>;
            fn $fn(self, rhs:&'t $base<B>) -> Self::Output {
                let f = move |$v_lhs:&'t $base<A>, $v_rhs:&'t $base<B>| { $($body)* };
                f(self,rhs)
            }
        }

        impl<'t,B,A> $name<&'t $base<B>> for $base<A>
        where A : $name<&'t B>, $($bounds)* {
            type Output = $base<<A as $name<&'t B>>::Output>;
            fn $fn(self, rhs:&'t $base<B>) -> Self::Output {
                let f = move |$v_lhs:$base<A>, $v_rhs:&'t $base<B>| { $($body)* };
                f(self,rhs)
            }
        }

        impl<'t,B,A> $name<$base<B>> for &'t $base<A>
        where &'t A : $name<B>, $($bounds)* {
            type Output = $base<<&'t A as $name<B>>::Output>;
            fn $fn(self, rhs:$base<B>) -> Self::Output {
                let f = move |$v_lhs:&'t $base<A>, $v_rhs:$base<B>| { $($body)* };
                f(self,rhs)
            }
        }

        impl<B,A> $name<$base<B>> for $base<A>
        where A : $name<B>, $($bounds)* {
            type Output = $base<<A as $name<B>>::Output>;
            fn $fn(self, rhs:$base<B>) -> Self::Output {
                let f = move |$v_lhs:$base<A>, $v_rhs:$base<B>| { $($body)* };
                f(self,rhs)
            }
        }
    }
}

macro_rules! define_shape_data_operator {
    ( $name:ident $fn:ident ($opr:tt) where $bounds:tt ) => {
        define_operator_newtype! { $name $fn Var where $bounds {
            |lhs,rhs| {
                match lhs {
                    Var::Static(lhs) => match rhs {
                        Var::Static(rhs) => Var::Static(lhs $opr rhs),
                        _ => {
                            let code = format!("{}({},{})",stringify!($fn),lhs.glsl(),rhs.glsl());
                            Var::Dynamic(code.into())
                        }
                    },
                    _ => {
                        let code = format!("{}({},{})",stringify!($fn),lhs.glsl(),rhs.glsl());
                        Var::Dynamic(code.into())
                    }
                }
            }
        }}
    }
}

macro_rules! define_shape_data_prim_operator {
    ( $name:ident $fn:ident ($opr:tt) for $target:ident where [$($bounds:tt)*] ) => {
        impl<A> $name<$target> for Var<A>
        where A: $name<$target>, $($bounds)* {
            type Output = Var<<A as $name<$target>>::Output>;
            default fn $fn(self, rhs: $target) -> Self::Output {
                let f = move |lhs: Var<A>, rhs: $target| {
                    match lhs {
                        Var::Static(lhs) => Var::Static(lhs $opr rhs),
                        _ => {
                            let code = format!("{}({},{})",stringify!($fn),lhs.glsl(),rhs.glsl());
                            Var::Dynamic(code.into())
                        }
                    }
                };
                f(self, rhs)
            }
        }

        impl<'t,A> $name<$target> for &'t Var<A>
        where &'t A: $name<$target>, $($bounds)* {
            type Output = Var<<&'t A as $name<$target>>::Output>;
            default fn $fn(self, rhs: $target) -> Self::Output {
                let f = move |lhs: &'t Var<A>, rhs: $target| {
                    match lhs {
                        Var::Static(lhs) => Var::Static(lhs $opr rhs),
                        _ => {
                            let code = format!("{}({},{})",stringify!($fn),lhs.glsl(),rhs.glsl());
                            Var::Dynamic(code.into())
                        }
                    }
                };
                f(self, rhs)
            }
        }
    }
}

define_shape_data_operator!      { Add add (+)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_operator!      { Sub sub (-)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_operator!      { Mul mul (*)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_operator!      { Div div (/)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_prim_operator! { Div div (/) for f32 where [A:RefInto<Glsl>] }
define_shape_data_prim_operator! { Mul mul (*) for f32 where [A:RefInto<Glsl>] }

impl<T> Neg for Var<T>
where T : Neg + RefInto<Glsl> {
    type Output = Var<<T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        match self {
            Var::Static(t)  => Var::Static(-t),
            Var::Dynamic(t) => Var::Dynamic(iformat!("neg({t})").into()),
        }
    }
}

impl<'t,T> Neg for &'t Var<T>
    where &'t T : Neg + Into<Glsl> {
    type Output = Var<<&'t T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        match self {
            Var::Static(t)  => Var::Static(-t),
            Var::Dynamic(t) => Var::Dynamic(iformat!("neg({t})").into()),
        }
    }
}


// === String Operators ===

macro_rules! define_shape_data_string_operator {
    ( $name:ident $fn:ident ($opr:tt) ) => {
        define_shape_data_string_operator_ref!    { $name $fn ($opr) for str }
        define_shape_data_string_operator_no_ref! { $name $fn ($opr) for String }
        define_shape_data_string_operator_no_ref! { $name $fn ($opr) for CowString }
    }
}

macro_rules! define_shape_data_string_operator_ref {
    ( $name:ident $fn:ident ($opr:tt) for $target:ident ) => {
        impl<'t,A> $name<&'t $target> for &'t Var<A>
            where A : RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs: &'t $target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }

        impl<'t,A> $name<&'t $target> for Var<A>
            where A : RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs: &'t $target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }

        impl<'t,A> $name<&'t Var<A>> for &'t $target
            where A : Display + RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs: &'t Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }

        impl<'t,A> $name<Var<A>> for &'t $target
            where A : Display + RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs: Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }
    }
}

macro_rules! define_shape_data_string_operator_no_ref {
    ( $name:ident $fn:ident ($opr:tt) for $target:ident ) => {
        impl<'t,A> $name<$target> for &'t Var<A>
            where A : RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs:$target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }

        impl<A> $name<$target> for Var<A>
            where A : RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs:$target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }

        impl<'t,A> $name<&'t Var<A>> for $target
            where A : Display + RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs:&'t Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }

        impl<A> $name<Var<A>> for $target
            where A : Display + RefInto<Glsl> {
            type Output = Var<A>;
            fn $fn(self, rhs:Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})",stringify!($fn),self.glsl(),rhs).into())
            }
        }
    }
}

define_shape_data_string_operator! { Add add (+) }
define_shape_data_string_operator! { Sub sub (-) }
define_shape_data_string_operator! { Mul mul (*) }
define_shape_data_string_operator! { Div div (/) }
