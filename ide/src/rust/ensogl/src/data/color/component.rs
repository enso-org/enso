//! This module defines `Components`, a wrapper over generic color component representation.

use crate::prelude::*;
use enso_generics::*;
use nalgebra::Vector3;
use nalgebra::Vector4;



// =================
// === Component ===
// =================

/// Wrapper for tuple containing color components. For most components without alpha it is 3 values
/// tuple. Alpha is always stored as the last component.
#[derive(Clone,Copy,Debug)]
pub struct Components<T>(pub T);

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

impl<T:KnownLast> KnownLast for Components<T> { type Last = Last<T>; }
impl<T:KnownInit> KnownInit for Components<T> { type Init = Components<Init<T>>; }

impl<T,X> PushBack<X> for Components<T>
    where T:PushBack<X> {
    type Output = Components<<T as PushBack<X>>::Output>;
    fn push_back(self, t:X) -> Self::Output {
        Components(self.0.push_back(t))
    }
}

impl<T> PopBack for Components<T>
    where T:PopBack {
    fn pop_back(self) -> (Self::Last,Self::Init) {
        let (last,init) = self.0.pop_back();
        let init = Components(init);
        (last,init)
    }
}


// === Vector Conversions ===

impl<T:nalgebra::Scalar> Into<Vector3<T>> for Components<(T,T,T)> {
    fn into(self) -> Vector3<T> {
        let tuple = self.0;
        Vector3::new(tuple.0,tuple.1,tuple.2)
    }
}

impl<T:nalgebra::Scalar> Into<Vector4<T>> for Components<(T,T,T,T)> {
    fn into(self) -> Vector4<T> {
        let tuple = self.0;
        Vector4::new(tuple.0,tuple.1,tuple.2,tuple.3)
    }
}



// ====================
// === ComponentMap ===
// ====================

/// Allows mapping over `f32` components.
#[allow(missing_docs)]
pub trait ComponentMap {
    fn map<F:Fn(f32)->f32>(&self, f:F) -> Self;
}

/// Trait for converting a type to its component representation.
pub trait ToComponents = Sized + HasComponentsRepr + Into<ComponentsOf<Self>>;

/// Trait for a component representation to the given type.
pub trait FromComponents = Sized + HasComponentsRepr where ComponentsOf<Self> : Into<Self>;

/// Trait allowing two way conversion of types and their component representations.
pub trait HasComponents : ToComponents + FromComponents {
    /// Convert components to the given type.
    fn from_components(components:ComponentsOf<Self>) -> Self {
        components.into()
    }

    /// Convert the type to its component representation.
    fn into_components(self) -> ComponentsOf<Self> {
        self.into()
    }
}
impl<T> HasComponents for T where T : ToComponents + FromComponents {}

/// Convert components to the given type.
pub fn from_components<T:FromComponents>(components:ComponentsOf<T>) -> T {
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
                let t = self.0;
                Components((  $((t.$num).$fn(r)),* ))
            }
        }

        impl $name<Components<( $($comp),* )>> for Components<( $($comp),* )> {
            type Output = Components<( $($comp),* )>;
            fn $fn(self, r:Components<( $($comp),* )>) -> Self::Output {
                let t = self.0;
                let r = r.0;
                Components((  $((t.$num).$fn(r.$num)),* ))
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

define_operators_for_components!     { Add::add, Sub::sub, Mul::mul, Div::div }
define_operators_for_component_refs! { Add::add, Sub::sub, Mul::mul, Div::div }
