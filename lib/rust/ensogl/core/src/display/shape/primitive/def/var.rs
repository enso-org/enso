//! This module defines an abstraction for all types which can be used as GLSL code values.

use crate::prelude::*;
use crate::system::gpu::types::*;
use std::ops::*;

use crate::data::color;
use crate::display::shape::primitive::def::unit::PixelDistance;
use crate::system::gpu::shader::glsl::Glsl;

use nalgebra::Scalar;



// ======================
// === VarInitializer ===
// ======================

/// A trait implemented on types which can be used to construct Var<T>. For example,
/// `Var<i32>` can be constructed from `i32`, `Glsl` code, or just from `&str` for convenient usage.
/// Please refer to `Var` documentation to understand what it is all about.
pub trait VarInitializer<T> = VarInitializerMarker<T> + Into<Glsl>;

/// Marker trait for `VarInitializer`.
#[marker]
pub trait VarInitializerMarker<T> {}


// === Instances ===

impl<T> VarInitializerMarker<Var<T>> for Glsl {}
impl<T> VarInitializerMarker<Var<T>> for &Glsl {}
impl<T> VarInitializerMarker<Var<T>> for String {}
impl<T> VarInitializerMarker<Var<T>> for &String {}
impl<T> VarInitializerMarker<Var<T>> for &str {}
impl<T> VarInitializerMarker<Var<T>> for T {}
impl<T> VarInitializerMarker<Var<T>> for &T {}

impl VarInitializerMarker<Var<color::Rgba>> for color::Rgb {}

impl VarInitializerMarker<Var<color::Rgba>> for color::Rgba {}

impl<G> VarInitializerMarker<Var<color::Rgba>> for color::gradient::SdfSampler<G> {}

impl<T, S1, S2> VarInitializerMarker<Var<Vector2<T>>> for (S1, S2)
where
    T: Scalar,
    S1: VarInitializerMarkerNested<Var<T>>,
    S2: VarInitializerMarkerNested<Var<T>>,
{
}



// === Nested ===

/// Marker trait for nested cases of `VarInitializer`.
pub trait VarInitializerMarkerNested<T> {}

impl<T, S> VarInitializerMarkerNested<T> for S where S: VarInitializerMarker<T> {}
impl<T> VarInitializerMarkerNested<Var<T>> for Var<T> {}
impl<T> VarInitializerMarkerNested<Var<T>> for &Var<T> {}



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
#[derive(Clone, Debug, Display)]
pub enum Var<T> {
    /// Static value.
    Static(T),

    /// Dynamic value expressed as GLSL code.
    Dynamic(Glsl),
}

// === Constructors ===

impl Var<Pixels> {
    /// Get the current shape's sizes.
    pub fn canvas_size() -> Var<Vector2<Pixels>> {
        "input_size".into()
    }
}

impl Var<color::Rgba> {
    /// Build a color from its components.
    pub fn rgba(
        r: impl Into<Var<f32>>,
        g: impl Into<Var<f32>>,
        b: impl Into<Var<f32>>,
        a: impl Into<Var<f32>>,
    ) -> Var<color::Rgba> {
        format!(
            "srgba({},{},{},{})",
            r.into().glsl(),
            g.into().glsl(),
            b.into().glsl(),
            a.into().glsl()
        )
        .into()
    }
}

impl<T, S> From<T> for Var<S>
where T: VarInitializer<Var<S>>
{
    default fn from(t: T) -> Self {
        Self::Dynamic(t.into())
    }
}

impl<T> From<T> for Var<T>
where T: VarInitializer<Var<T>>
{
    fn from(t: T) -> Self {
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
where T: Abs
{
    fn abs(&self) -> Self {
        match self {
            Self::Static(t) => Var::Static(t.abs()),
            Self::Dynamic(t) => Var::Dynamic(format!("abs({})", t).into()),
        }
    }
}

impl<T> Min for Var<T>
where T: Min + Into<Glsl>
{
    fn min(a: Self, b: Self) -> Self {
        match (a, b) {
            (Var::Static(a), Var::Static(b)) => Var::Static(Min::min(a, b)),
            (a, b) => {
                let a: Glsl = a.into();
                let b: Glsl = b.into();
                Var::Dynamic(format!("min({},{})", a.glsl(), b.glsl()).into())
            }
        }
    }
}

impl<T> Max for Var<T>
where T: Max + Into<Glsl>
{
    fn max(a: Self, b: Self) -> Self {
        match (a, b) {
            (Var::Static(a), Var::Static(b)) => Var::Static(Max::max(a, b)),
            (a, b) => {
                let a: Glsl = a.into();
                let b: Glsl = b.into();
                Var::Dynamic(format!("max({},{})", a.glsl(), b.glsl()).into())
            }
        }
    }
}

macro_rules! dim_impl {
    ($trait:ident for $tp:ty {
        type $dim_ty_name:ident = $dim_ty:ty;
        args = [$($single_arg:ident),* $(,)?];
        $(swizzling = $multi_ty:ident [$($multi_arg:ident),* $(,)?];)*
    }) => {
        impl<T: Scalar + Copy> $trait for $tp {
            type $dim_ty_name = $dim_ty;
            $(
                fn $single_arg(&self) -> Self::Dim1Type {
                    match self {
                        Self::Static(t) => Var::Static(t.$single_arg.clone()),
                        Self::Dynamic(t) => {
                            let code = format!("{}.{}", t, stringify!($single_arg));
                            Var::Dynamic(code.into())
                        }
                    }
                }
            )*
            $($(
                fn $multi_arg(&self) -> Self::$multi_ty {
                    match self {
                        Self::Static(t) => Var::Static(t.$multi_arg()),
                        Self::Dynamic(t) => {
                            let code = format!("{}.{}", t, stringify!($multi_arg));
                            Var::Dynamic(code.into())
                        }
                    }
                }
            )*)*
        }
    };
}

dim_impl! ( Dim1 for Var<Vector2<T>> {
    type Dim1Type = Var<T>;
    args = [x];
});

dim_impl! ( Dim2 for Var<Vector2<T>> {
    type Dim2Type = Var<Vector2<T>>;
    args = [y];
    swizzling = Dim2Type [xx, xy, yx, yy];
});

dim_impl! ( Dim1 for Var<Vector3<T>> {
    type Dim1Type = Var<T>;
    args = [x];
});

dim_impl! ( Dim2 for Var<Vector3<T>> {
    type Dim2Type = Var<Vector2<T>>;
    args = [y];
    swizzling = Dim2Type [xx, xy, yx, yy];
});

dim_impl! ( Dim3 for Var<Vector3<T>> {
    type Dim3Type = Var<Vector3<T>>;
    args = [z];
    swizzling = Dim2Type [zz, xz, zx, yz, zy];
    swizzling = Dim3Type [
        xxx, xxy, xxz, xyx, xyy, xyz, xzx, xzy, xzz, yxx, yxy, yxz, yyx, yyy, yyz, yzx, yzy, yzz,
        zxx, zxy, zxz, zyx, zyy, zyz, zzx, zzy, zzz
    ];
});

dim_impl! ( Dim1 for Var<Vector4<T>> {
    type Dim1Type = Var<T>;
    args = [x];
});

dim_impl! ( Dim2 for Var<Vector4<T>> {
    type Dim2Type = Var<Vector2<T>>;
    args = [y];
    swizzling = Dim2Type [xx, xy, yx, yy];
});

dim_impl! ( Dim3 for Var<Vector4<T>> {
    type Dim3Type = Var<Vector3<T>>;
    args = [z];
    swizzling = Dim2Type [zz, xz, zx, yz, zy];
    swizzling = Dim3Type [
        xxx, xxy, xxz, xyx, xyy, xyz, xzx, xzy, xzz, yxx, yxy, yxz, yyx, yyy, yyz, yzx, yzy, yzz,
        zxx, zxy, zxz, zyx, zyy, zyz, zzx, zzy, zzz
    ];
});

dim_impl! ( Dim4 for Var<Vector4<T>> {
    type Dim4Type = Var<Vector4<T>>;
    args = [w];
    swizzling = Dim2Type [ww, xw, wx, yw, wy, zw, wz];
    swizzling = Dim3Type [
        xxw, xyw, xzw, yxw, yyw, yzw, zxw, zyw, zzw, xwx, xwy, xwz, ywx, ywy, ywz, zwx, zwy, zwz,
        wxx, wxy, wxz, wyx, wyy, wyz, wzx, wzy, wzz
    ];
    swizzling = Dim4Type [
        xxxx, xxxy, xxxz, xxxw, xxyx, xxyy, xxyz, xxyw, xxzx, xxzy, xxzz, xxzw, xxwx, xxwy, xxwz,
        xxww, xyxx, xyxy, xyxz, xyxw, xyyx, xyyy, xyyz, xyyw, xyzx, xyzy, xyzz, xyzw, xywx, xywy,
        xywz, xyww, xzxx, xzxy, xzxz, xzxw, xzyx, xzyy, xzyz, xzyw, xzzx, xzzy, xzzz, xzzw, xzwx,
        xzwy, xzwz, xzww, xwxx, xwxy, xwxz, xwxw, xwyx, xwyy, xwyz, xwyw, xwzx, xwzy, xwzz, xwzw,
        xwwx, xwwy, xwwz, xwww, yxxx, yxxy, yxxz, yxxw, yxyx, yxyy, yxyz, yxyw, yxzx, yxzy, yxzz,
        yxzw, yxwx, yxwy, yxwz, yxww, yyxx, yyxy, yyxz, yyxw, yyyx, yyyy, yyyz, yyyw, yyzx, yyzy,
        yyzz, yyzw, yywx, yywy, yywz, yyww, yzxx, yzxy, yzxz, yzxw, yzyx, yzyy, yzyz, yzyw, yzzx,
        yzzy, yzzz, yzzw, yzwx, yzwy, yzwz, yzww, ywxx, ywxy, ywxz, ywxw, ywyx, ywyy, ywyz, ywyw,
        ywzx, ywzy, ywzz, ywzw, ywwx, ywwy, ywwz, ywww, zxxx, zxxy, zxxz, zxxw, zxyx, zxyy, zxyz,
        zxyw, zxzx, zxzy, zxzz, zxzw, zxwx, zxwy, zxwz, zxww, zyxx, zyxy, zyxz, zyxw, zyyx, zyyy,
        zyyz, zyyw, zyzx, zyzy, zyzz, zyzw, zywx, zywy, zywz, zyww, zzxx, zzxy, zzxz, zzxw, zzyx,
        zzyy, zzyz, zzyw, zzzx, zzzy, zzzz, zzzw, zzwx, zzwy, zzwz, zzww, zwxx, zwxy, zwxz, zwxw,
        zwyx, zwyy, zwyz, zwyw, zwzx, zwzy, zwzz, zwzw, zwwx, zwwy, zwwz, zwww, wxxx, wxxy, wxxz,
        wxxw, wxyx, wxyy, wxyz, wxyw, wxzx, wxzy, wxzz, wxzw, wxwx, wxwy, wxwz, wxww, wyxx, wyxy,
        wyxz, wyxw, wyyx, wyyy, wyyz, wyyw, wyzx, wyzy, wyzz, wyzw, wywx, wywy, wywz, wyww, wzxx,
        wzxy, wzxz, wzxw, wzyx, wzyy, wzyz, wzyw, wzzx, wzzy, wzzz, wzzw, wzwx, wzwy, wzwz, wzww,
        wwxx, wwxy, wwxz, wwxw, wwyx, wwyy, wwyz, wwyw, wwzx, wwzy, wwzz, wwzw, wwwx, wwwy, wwwz,
        wwww
    ];
});

impl PixelDistance for Var<Vector2<f32>> {
    type Output = Var<Vector2<Pixels>>;
    fn px(&self) -> Self::Output {
        match self {
            Self::Static(t) => Var::Static(Vector2(t.x.pixels(), t.y.pixels())),
            Self::Dynamic(t) => Var::Dynamic(t.clone()),
        }
    }
}

impl PixelDistance for Var<Vector3<f32>> {
    type Output = Var<Vector3<Pixels>>;
    fn px(&self) -> Self::Output {
        match self {
            Self::Static(t) => Var::Static(Vector3(t.x.pixels(), t.y.pixels(), t.z.pixels())),
            Self::Dynamic(t) => Var::Dynamic(t.clone()),
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
    };
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

define_shape_data_operator! { Add add (+)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_operator! { Sub sub (-)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_operator! { Mul mul (*)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_operator! { Div div (/)         where [A:RefInto<Glsl>, B:RefInto<Glsl>] }
define_shape_data_prim_operator! { Div div (/) for f32 where [A:RefInto<Glsl>] }
define_shape_data_prim_operator! { Mul mul (*) for f32 where [A:RefInto<Glsl>] }
define_shape_data_prim_operator! { Sub sub (-) for f32 where [A:RefInto<Glsl>] }
define_shape_data_prim_operator! { Rem rem (%) for f32 where [A:RefInto<Glsl>] }

impl<T> Neg for Var<T>
where T: Neg + RefInto<Glsl>
{
    type Output = Var<<T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        match self {
            Var::Static(t) => Var::Static(-t),
            Var::Dynamic(t) => Var::Dynamic(iformat!("neg({t})").into()),
        }
    }
}

impl<'t, T> Neg for &'t Var<T>
where &'t T: Neg + Into<Glsl>
{
    type Output = Var<<&'t T as Neg>::Output>;
    fn neg(self) -> Self::Output {
        match self {
            Var::Static(t) => Var::Static(-t),
            Var::Dynamic(t) => Var::Dynamic(iformat!("neg({t})").into()),
        }
    }
}


// === String Operators ===

macro_rules! define_shape_data_string_operator {
    ( $name:ident $fn:ident ($opr:tt) ) => {
        define_shape_data_string_operator_ref! { $name $fn ($opr) for str }
        define_shape_data_string_operator_no_ref! { $name $fn ($opr) for String }
        define_shape_data_string_operator_no_ref! { $name $fn ($opr) for CowString }
    };
}

macro_rules! define_shape_data_string_operator_ref {
    ( $name:ident $fn:ident ($opr:tt) for $target:ident ) => {
        impl<'t, A> $name<&'t $target> for &'t Var<A>
        where A: RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: &'t $target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }

        impl<'t, A> $name<&'t $target> for Var<A>
        where A: RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: &'t $target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }

        impl<'t, A> $name<&'t Var<A>> for &'t $target
        where A: Display + RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: &'t Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }

        impl<'t, A> $name<Var<A>> for &'t $target
        where A: Display + RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }
    };
}

macro_rules! define_shape_data_string_operator_no_ref {
    ( $name:ident $fn:ident ($opr:tt) for $target:ident ) => {
        impl<'t, A> $name<$target> for &'t Var<A>
        where A: RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: $target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }

        impl<A> $name<$target> for Var<A>
        where A: RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: $target) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }

        impl<'t, A> $name<&'t Var<A>> for $target
        where A: Display + RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: &'t Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }

        impl<A> $name<Var<A>> for $target
        where A: Display + RefInto<Glsl>
        {
            type Output = Var<A>;
            fn $fn(self, rhs: Var<A>) -> Self::Output {
                Var::Dynamic(format!("{}({},{})", stringify!($fn), self.glsl(), rhs).into())
            }
        }
    };
}

define_shape_data_string_operator! { Add add (+) }
define_shape_data_string_operator! { Sub sub (-) }
define_shape_data_string_operator! { Mul mul (*) }
define_shape_data_string_operator! { Div div (/) }


// =========================
// === Utility Functions ===
// =========================

impl Var<f32> {
    /// Perform Hermite interpolation between two values.
    pub fn smoothstep(&self, e1: impl RefInto<Glsl>, e2: impl RefInto<Glsl>) -> Self {
        let e1 = e1.glsl();
        let e2 = e2.glsl();
        match self {
            Var::Static(t) => Var::Dynamic(iformat!("smoothstep({e1},{e2},{t})").into()),
            Var::Dynamic(t) => Var::Dynamic(iformat!("smoothstep({e1},{e2},{t})").into()),
        }
    }
}

impl Var<f32> {
    /// Linearly interpolate between two values.
    pub fn mix(&self, e1: impl RefInto<Glsl>, e2: impl RefInto<Glsl>) -> Self {
        let e1 = e1.glsl();
        let e2 = e2.glsl();
        match self {
            Var::Static(t) => Var::Dynamic(iformat!("mix({e1},{e2},{t})").into()),
            Var::Dynamic(t) => Var::Dynamic(iformat!("mix({e1},{e2},{t})").into()),
        }
    }
}



// ===============================
// === Trigonometric Functions ===
// ===============================

impl<T> Sin for Var<T>
where T: Sin<Output = T>
{
    type Output = Var<T>;
    fn sin(&self) -> Self {
        match self {
            Self::Static(t) => Var::Static(t.sin()),
            Self::Dynamic(t) => Var::Dynamic(format!("sin({})", t).into()),
        }
    }
}

impl<T> Asin for Var<T>
where T: Asin<Output = T>
{
    type Output = Var<T>;
    fn asin(&self) -> Self {
        match self {
            Self::Static(t) => Var::Static(t.asin()),
            Self::Dynamic(t) => Var::Dynamic(format!("asin({})", t).into()),
        }
    }
}


impl<T> Cos for Var<T>
where T: Cos<Output = T>
{
    type Output = Var<T>;
    fn cos(&self) -> Self {
        match self {
            Self::Static(t) => Var::Static(t.cos()),
            Self::Dynamic(t) => Var::Dynamic(format!("cos({})", t).into()),
        }
    }
}

impl<T> Acos for Var<T>
where T: Acos<Output = T>
{
    type Output = Var<T>;
    fn acos(&self) -> Self {
        match self {
            Self::Static(t) => Var::Static(t.acos()),
            Self::Dynamic(t) => Var::Dynamic(format!("acos({})", t).into()),
        }
    }
}



// ===================
// === Square Root ===
// ===================

impl<T> Sqrt for Var<T>
where T: Sqrt<Output = T>
{
    type Output = Var<T>;
    fn sqrt(&self) -> Self {
        match self {
            Self::Static(t) => Var::Static(t.sqrt()),
            Self::Dynamic(t) => Var::Dynamic(format!("sqrt({})", t).into()),
        }
    }
}



// =============
// === Clamp ===
// =============

impl<T> Clamp for Var<T>
where T: Clamp<Output = T> + Into<Glsl>
{
    type Output = Var<T>;
    fn clamp(self, lower: Var<T>, upper: Var<T>) -> Var<T> {
        use Var::Dynamic;
        use Var::Static;

        match (self, lower, upper) {
            (Static(value), Static(lower), Static(upper)) => Static(value.clamp(lower, upper)),
            (value, lower, upper) => {
                let value: Glsl = value.into();
                let lower: Glsl = lower.into();
                let upper: Glsl = upper.into();
                Dynamic(format!("clamp({},{},{})", value.glsl(), lower.glsl(), upper.glsl()).into())
            }
        }
    }
}



// ==============
// === Signum ===
// ==============

impl<T> Signum for Var<T>
where T: Signum<Output = T>
{
    type Output = Var<T>;
    fn signum(self) -> Self {
        match self {
            Self::Static(t) => Var::Static(t.signum()),
            Self::Dynamic(t) => Var::Dynamic(format!("sign({})", t).into()),
        }
    }
}



// ============================
// === Conversion Functions ===
// ============================
// TODO this needs to be revisited with a more generic solution

impl From<Var<Radians>> for Var<f32> {
    fn from(other: Var<Radians>) -> Self {
        match other {
            Var::Static(t) => Var::Static(t.value),
            Var::Dynamic(t) => Var::Dynamic(glsl::rad_to_f32(&t.glsl())),
        }
    }
}

impl From<Var<f32>> for Var<Radians> {
    fn from(other: Var<f32>) -> Self {
        match other {
            Var::Static(t) => Var::Static(Radians::from(t)),
            Var::Dynamic(t) => Var::Dynamic(glsl::f32_to_rad(&t.glsl())),
        }
    }
}

impl From<Var<f32>> for Var<Degrees> {
    fn from(other: Var<f32>) -> Self {
        match other {
            Var::Static(t) => Var::Static(Degrees::from(t)),
            Var::Dynamic(t) => Var::Dynamic(glsl::f32_to_deg(&t.glsl())),
        }
    }
}

impl From<Var<Degrees>> for Var<f32> {
    fn from(other: Var<Degrees>) -> Self {
        match other {
            Var::Static(t) => Var::Static(t.value),
            Var::Dynamic(t) => Var::Dynamic(glsl::deg_to_f32(&t.glsl())),
        }
    }
}

impl From<Var<Pixels>> for Var<f32> {
    fn from(other: Var<Pixels>) -> Self {
        match other {
            Var::Static(t) => Var::Static(t.value),
            Var::Dynamic(t) => Var::Dynamic(t),
        }
    }
}

impl From<Var<f32>> for Var<Pixels> {
    fn from(other: Var<f32>) -> Self {
        match other {
            Var::Static(t) => Var::Static(Pixels::from(t)),
            Var::Dynamic(t) => Var::Dynamic(t),
        }
    }
}

impl From<Var<Vector4<f32>>> for Var<color::Rgba> {
    fn from(other: Var<Vector4<f32>>) -> Self {
        match other {
            Var::Static(t) => Var::Static(color::Rgba::new(t.x, t.y, t.z, t.w)),
            Var::Dynamic(t) => Var::Dynamic(format!("srgba({0}.x,{0}.y,{0}.z,{0}.w)", t).into()),
        }
    }
}

impl Var<color::Rgba> {
    /// Return the color with the given alpha value.
    pub fn with_alpha(self, alpha: &Var<f32>) -> Self {
        match (self, alpha) {
            (Var::Static(t), Var::Static(alpha)) =>
                Var::Static(color::Rgba::new(t.data.red, t.data.green, t.data.blue, *alpha)),
            (t, alpha) => {
                let t = t.glsl();
                let alpha = alpha.glsl();
                let var = format!("srgba({0}.raw.x,{0}.raw.y,{0}.raw.z,{1})", t, alpha);
                Var::Dynamic(var.into())
            }
        }
    }

    /// Return the color with the alpha value replaced by multiplying the colors' alpha value with
    /// the given alpha value.
    pub fn multiply_alpha(self, alpha: &Var<f32>) -> Self {
        match (self, alpha) {
            (Var::Static(t), Var::Static(alpha)) => {
                let var =
                    color::Rgba::new(t.data.red, t.data.green, t.data.blue, *alpha * t.data.alpha);
                Var::Static(var)
            }
            (t, alpha) => {
                let t = t.glsl();
                let alpha = alpha.glsl();
                let var = format!("srgba({0}.raw.x,{0}.raw.y,{0}.raw.z,{0}.raw.w*{1})", t, alpha);
                Var::Dynamic(var.into())
            }
        }
    }

    /// Transform to LinearRgba.
    pub fn into_linear(self) -> Var<color::LinearRgba> {
        match self {
            Var::Static(c) => Var::Static(c.into()),
            Var::Dynamic(c) => Var::Dynamic(format!("rgba({0})", c.glsl()).into()),
        }
    }
}
