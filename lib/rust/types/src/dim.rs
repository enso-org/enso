//! Definition of Dim* traits. These traits define common operations on types that represent
//! dimensional transformations. For example, they define getters, such as `xy()`, or setters, such
//! as `set_xy(...)` on 2-dimensional types. Moreover, this module implements these traits for
//! suitable nalgebra's Vector types.

use enso_prelude::*;

use nalgebra;
use nalgebra::Scalar;
use nalgebra::Vector2;
use nalgebra::Vector3;
use nalgebra::Vector4;



// ===================
// === Dim* Traits ===
// ===================

macro_rules! gen_dim_trait {
    ([$prev_dim:tt] $dim:tt
        $( $name:ident $swizzling_dim:tt [$($dim_ix:tt)*] [$($dim_ord:tt)*] )*
    ) => { paste! {
        #[doc = "Component swizzling getters for "]
        #[doc = stringify!($dim)]
        #[doc = "`-dimensional types."]
        #[allow(missing_docs)]
        pub trait [<Dim $dim>] : [<Dim $prev_dim>] {
            type [<Dim $dim Type>];
            $(
                fn $name(&self) -> Self::[<Dim $swizzling_dim Type>];
            )*
        }
    }};
}

/// An abstract, 0-dimensional type. It is defined to simplify macro definition.
pub trait Dim0 {}
impl<T> Dim0 for T {}

crate::with_swizzling_for_dim!(1, gen_dim_trait, 0);
crate::with_swizzling_for_dim!(2, gen_dim_trait, 1);
crate::with_swizzling_for_dim!(3, gen_dim_trait, 2);
crate::with_swizzling_for_dim!(4, gen_dim_trait, 3);



// =========================
// === DimSetter* Traits ===
// =========================

macro_rules! gen_dim_setter_trait {
    ($dim:tt
        $( $name:ident $swizzling_dim:tt [$($dim_ix:tt)*] [$($dim_ord:tt)*] )*
    ) => { paste! {
        #[doc = "Component swizzling setters for "]
        #[doc = stringify!($dim)]
        #[doc = "`-dimensional types."]
        #[allow(missing_docs)]
        pub trait [<Dim Setter $dim>] : [<Dim $dim>] {
            $(
                fn [<set_$name>](&mut self, value: Self::[<Dim $swizzling_dim Type>]);
            )*
        }
    }};
}

crate::with_swizzling_for_dim_unique!(1, gen_dim_setter_trait);
crate::with_swizzling_for_dim_unique!(2, gen_dim_setter_trait);
crate::with_swizzling_for_dim_unique!(3, gen_dim_setter_trait);
crate::with_swizzling_for_dim_unique!(4, gen_dim_setter_trait);



// =====================================
// === Getters for nalgebra::Vector* ===
// =====================================

macro_rules! gen_dim_impl_for_vector {
    ([$vec:tt] $dim:tt $( $name:ident $swizzling_dim:tt [$($dim_ix:tt)*] [$($dim_ord:tt)*] )*) => {
        paste! {
            impl<T: Scalar + Copy> [<Dim $dim>] for $vec<T> {
                type [<Dim $dim Type>] = [<Vector $dim>]<T>;
                $(
                    fn $name(&self) -> Self::[<Dim $swizzling_dim Type>] {
                        gen_dim_impl_for_vector_body!{self, $swizzling_dim, [$($dim_ix)*]}
                    }
                )*
            }
        }
    };
}

macro_rules! gen_dim_impl_for_vector_body {
    ($this:tt, $swizzling_dim:tt, [$dim_ix:tt]) => {
        $this[$dim_ix]
    };
    ($this:tt, $swizzling_dim:tt, [$($dim_ix:tt),*]) => { paste! {
        Self::[<Dim $swizzling_dim Type>]::new( $( $this[$dim_ix] ),* )
    }};
}

/// A one dimensional vector, being just a value. It is used mostly to simplify macro definitions.
pub type Vector1<T> = T;

crate::with_swizzling_for_dim!(1, gen_dim_impl_for_vector, Vector2);
crate::with_swizzling_for_dim!(2, gen_dim_impl_for_vector, Vector2);

crate::with_swizzling_for_dim!(1, gen_dim_impl_for_vector, Vector3);
crate::with_swizzling_for_dim!(2, gen_dim_impl_for_vector, Vector3);
crate::with_swizzling_for_dim!(3, gen_dim_impl_for_vector, Vector3);

crate::with_swizzling_for_dim!(1, gen_dim_impl_for_vector, Vector4);
crate::with_swizzling_for_dim!(2, gen_dim_impl_for_vector, Vector4);
crate::with_swizzling_for_dim!(3, gen_dim_impl_for_vector, Vector4);
crate::with_swizzling_for_dim!(4, gen_dim_impl_for_vector, Vector4);



// =====================================
// === Setters for nalgebra::Vector* ===
// =====================================

macro_rules! gen_dim_setter_impl_for_vector {
    ([$vec:tt] $dim:tt $( $name:ident $swizzling_dim:tt [$($dim_ix:tt)*] [$($dim_ord:tt)*] )*) => {
        paste! {
            impl<T: Scalar + Copy> [<Dim Setter $dim>] for $vec<T> {
                $(
                    fn [<set_ $name>](&mut self, value: Self::[<Dim $swizzling_dim Type>]) {
                        gen_dim_setter_impl_for_vector_body!{self, value, $swizzling_dim, [$($dim_ix)*], [$($dim_ord)*]}
                    }
                )*
            }
        }
    };
}

macro_rules! gen_dim_setter_impl_for_vector_body {
    ($this:tt, $value:tt, $swizzling_dim:tt, [$dim_ix:tt], [$($dim_ord:tt),*]) => {
        $this[$dim_ix] = $value;
    };
    ($this:tt, $value:tt, $swizzling_dim:tt, [$($dim_ix:tt),*], [$($dim_ord:tt),*]) => { paste! {
        $( $this[$dim_ix] = $value[$dim_ord]; )*
    }};
}

crate::with_swizzling_for_dim_unique!(1, gen_dim_setter_impl_for_vector, Vector2);
crate::with_swizzling_for_dim_unique!(2, gen_dim_setter_impl_for_vector, Vector2);

crate::with_swizzling_for_dim_unique!(1, gen_dim_setter_impl_for_vector, Vector3);
crate::with_swizzling_for_dim_unique!(2, gen_dim_setter_impl_for_vector, Vector3);
crate::with_swizzling_for_dim_unique!(3, gen_dim_setter_impl_for_vector, Vector3);

crate::with_swizzling_for_dim_unique!(1, gen_dim_setter_impl_for_vector, Vector4);
crate::with_swizzling_for_dim_unique!(2, gen_dim_setter_impl_for_vector, Vector4);
crate::with_swizzling_for_dim_unique!(3, gen_dim_setter_impl_for_vector, Vector4);
crate::with_swizzling_for_dim_unique!(4, gen_dim_setter_impl_for_vector, Vector4);
