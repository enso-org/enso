//! This module defines `Components`, a wrapper over generic color component representation.

use crate::prelude::*;
use enso_generics::*;
use nalgebra::Scalar;
use nalgebra::Vector3;
use nalgebra::Vector4;



// =================
// === Component ===
// =================

/// Wrapper for tuple containing color components. For most components without alpha it is 3 values
/// tuple. Alpha is always stored as the last component.
#[derive(Clone, Copy, Debug)]
#[allow(missing_docs)]
pub struct Components<T> {
    pub tuple: T,
}

/// Smart constructor.
#[allow(non_snake_case)]
pub fn Components<T>(tuple: T) -> Components<T> {
    Components { tuple }
}

impl<T> Components<T> {
    /// Constructor.
    pub fn new(tuple: T) -> Self {
        Self { tuple }
    }
}

impl<T> Deref for Components<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.tuple
    }
}



// =========================
// === HasComponentsRepr ===
// =========================

/// Component tuple representation associated to the given type.
#[allow(missing_docs)]
pub trait HasComponentsRepr {
    type ComponentsRepr;
}

/// Type-level accessor of component tuple representation of a type.
pub type ComponentsReprOf<T> = <T as HasComponentsRepr>::ComponentsRepr;

/// Type-level accessor of `Component` representation of a type.
pub type ComponentsOf<T> = Components<ComponentsReprOf<T>>;


// === Generics ===

impl<T: KnownLast> KnownLast for Components<T> {
    type Last = Last<T>;
}
impl<T: KnownInit> KnownInit for Components<T> {
    type Init = Components<Init<T>>;
}

impl<T, X> PushBack<X> for Components<T>
where T: PushBack<X>
{
    type Output = Components<<T as PushBack<X>>::Output>;
    fn push_back(self, t: X) -> Self::Output {
        Components(self.tuple.push_back(t))
    }
}

impl<T> PopBack for Components<T>
where T: PopBack
{
    fn pop_back(self) -> (Self::Last, Self::Init) {
        let (last, init) = self.tuple.pop_back();
        let init = Components(init);
        (last, init)
    }
}



// ====================
// === ComponentMap ===
// ====================

/// Allows mapping over `f32` components.
#[allow(missing_docs)]
pub trait ComponentMap {
    fn map<F: Fn(f32) -> f32>(&self, f: F) -> Self;
}

/// Trait for converting a type to its component representation.
pub trait ToComponents = Sized + HasComponentsRepr + Into<ComponentsOf<Self>>;

/// Trait for a component representation to the given type.
pub trait FromComponents = Sized + HasComponentsRepr where ComponentsOf<Self>: Into<Self>;

/// Trait allowing two way conversion of types and their component representations.
pub trait HasComponents: ToComponents + FromComponents {
    /// Convert components to the given type.
    fn from_components(components: ComponentsOf<Self>) -> Self {
        components.into()
    }

    /// Convert the type to its component representation.
    fn into_components(self) -> ComponentsOf<Self> {
        self.into()
    }
}
impl<T> HasComponents for T where T: ToComponents + FromComponents {}

/// Convert components to the given type.
pub fn from_components<T: FromComponents>(components: ComponentsOf<T>) -> T {
    components.into()
}



// =================
// === Operators ===
// =================

macro_rules! define_operators_for_components {
    ($($toks:tt)*) => {
        define_operators_for_component_tuple! { [f32 f32 f32]     [0 1 2]   $($toks)* }
        define_operators_for_component_tuple! { [f32 f32 f32 f32] [0 1 2 3] $($toks)* }
    }
}

macro_rules! define_operators_for_component_tuple {
    ($comps:tt $nums:tt $($name:ident :: $fn:ident),*) => {$(
        define_operator_for_component_tuple! { $comps $nums $name $fn }
    )*}
}

macro_rules! define_operator_for_component_tuple {
    ([$($comp:ident)*] [$($num:tt)*] $name:ident $fn:ident) => {
        impl $name<f32> for Components<( $($comp),* )> {
            type Output = Components<( $($comp),* )>;
            fn $fn(self, r:f32) -> Self::Output {
                Components((  $((self.$num).$fn(r)),* ))
            }
        }

        impl $name<Components<( $($comp),* )>> for Components<( $($comp),* )> {
            type Output = Components<( $($comp),* )>;
            fn $fn(self, r:Components<( $($comp),* )>) -> Self::Output {
                Components((  $((self.$num).$fn(r.$num)),* ))
            }
        }
    }
}

macro_rules! define_operators_for_component_refs {
    ($($name:ident :: $fn:ident),*) => {$(
        impl<T> $name<&f32> for Components<T>
        where Components<T> : Copy + $name<f32,Output=Components<T>> {
            type Output = Components<T>;
            fn $fn(self, r:&f32) -> Self::Output {
                self.$fn(*r)
            }
        }

        impl<T> $name<f32> for &Components<T>
        where Components<T> : Copy + $name<f32,Output=Components<T>> {
            type Output = Components<T>;
            fn $fn(self, r:f32) -> Self::Output {
                (*self).$fn(r)
            }
        }

        impl<T> $name<&f32> for &Components<T>
        where Components<T> : Copy + $name<f32,Output=Components<T>> {
           type Output = Components<T>;
            fn $fn(self, r:&f32) -> Self::Output {
                (*self).$fn(*r)
            }
        }

        impl<T> $name<&Components<T>> for Components<T>
        where Components<T> : Copy + $name<Components<T>,Output=Components<T>> {
            type Output = Components<T>;
            fn $fn(self, r:&Components<T>) -> Self::Output {
                self.$fn(*r)
            }
        }

        impl<T> $name<Components<T>> for &Components<T>
        where Components<T> : Copy + $name<Components<T>,Output=Components<T>>{
            type Output = Components<T>;
            fn $fn(self, r:Components<T>) -> Self::Output {
                (*self).$fn(r)
            }
        }

        impl<T> $name<&Components<T>> for &Components<T>
        where Components<T> : Copy + $name<Components<T>,Output=Components<T>> {
            type Output = Components<T>;
            fn $fn(self, r:&Components<T>) -> Self::Output {
                (*self).$fn(*r)
            }
        }
    )*}
}

define_operators_for_components! { Add::add, Sub::sub, Mul::mul, Div::div }
define_operators_for_component_refs! { Add::add, Sub::sub, Mul::mul, Div::div }



// ==========================
// === Vector Conversions ===
// ==========================

impl<T: Scalar> HasComponentsRepr for Vector2<T> {
    type ComponentsRepr = (T, T);
}
impl<T: Scalar> HasComponentsRepr for Vector3<T> {
    type ComponentsRepr = (T, T, T);
}
impl<T: Scalar> HasComponentsRepr for Vector4<T> {
    type ComponentsRepr = (T, T, T, T);
}

impl<T: Scalar> From<Vector2<T>> for ComponentsOf<Vector2<T>> {
    fn from(t: Vector2<T>) -> Self {
        Components((t.x.clone(), t.y.clone()))
    }
}

impl<T: Scalar> From<ComponentsOf<Vector2<T>>> for Vector2<T> {
    fn from(value: ComponentsOf<Vector2<T>>) -> Self {
        Vector2::new(value.0.clone(), value.1.clone())
    }
}

impl<T: Scalar> From<Vector3<T>> for ComponentsOf<Vector3<T>> {
    fn from(t: Vector3<T>) -> Self {
        Components((t.x.clone(), t.y.clone(), t.z.clone()))
    }
}

impl<T: Scalar> From<ComponentsOf<Vector3<T>>> for Vector3<T> {
    fn from(value: ComponentsOf<Vector3<T>>) -> Self {
        Vector3::new(value.0.clone(), value.1.clone(), value.2.clone())
    }
}

impl<T: Scalar> From<Vector4<T>> for ComponentsOf<Vector4<T>> {
    fn from(t: Vector4<T>) -> Self {
        Components((t.x.clone(), t.y.clone(), t.z.clone(), t.w.clone()))
    }
}

impl<T: Scalar> From<ComponentsOf<Vector4<T>>> for Vector4<T> {
    fn from(value: ComponentsOf<Vector4<T>>) -> Self {
        Vector4::new(value.0.clone(), value.1.clone(), value.2.clone(), value.3.clone())
    }
}
