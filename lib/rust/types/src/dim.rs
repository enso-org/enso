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

macro_rules! gen_dim_x_trait {
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

        impl<T: [<Dim $dim>]> [<Dim $dim>] for Rc<T> {
            type [<Dim $dim Type>] = <T as [<Dim $dim>]>::[<Dim $dim Type>];
            $(
                fn $name(&self) -> Self::[<Dim $swizzling_dim Type>] {
                    (**self).$name()
                }
            )*
        }

        impl<T: Copy + [<Dim $dim>]> [<Dim $dim>] for Cell<T> {
            type [<Dim $dim Type>] = <T as [<Dim $dim>]>::[<Dim $dim Type>];
            $(
                fn $name(&self) -> Self::[<Dim $swizzling_dim Type>] {
                    self.get().$name()
                }
            )*
        }

        impl<T: [<Dim $dim>]> [<Dim $dim>] for RefCell<T> {
            type [<Dim $dim Type>] = <T as [<Dim $dim>]>::[<Dim $dim Type>];
            $(
                fn $name(&self) -> Self::[<Dim $swizzling_dim Type>] {
                    self.borrow().$name()
                }
            )*
        }

        #[doc = "Component swizzling getters for "]
        #[doc = stringify!($dim)]
        #[doc = "`-dimensional types."]
        #[allow(missing_docs)]
        pub trait [<DimRef $dim>] : [<DimRef $prev_dim>] {
            type [<Dim $dim Type>];
            $(
                fn $name(&self) -> &Self::[<Dim $swizzling_dim Type>];
            )*
        }

        impl<T: [<DimRef $dim>]> [<DimRef $dim>] for Rc<T> {
            type [<Dim $dim Type>] = <T as [<DimRef $dim>]>::[<Dim $dim Type>];
            $(
                fn $name(&self) -> &Self::[<Dim $swizzling_dim Type>] {
                    (**self).$name()
                }
            )*
        }
    }};
}

/// An abstract, 0-dimensional type. It is defined to simplify macro definition.
pub trait Dim0 {}
impl<T> Dim0 for T {}

/// An abstract, 0-dimensional type. It is defined to simplify macro definition.
pub trait DimRef0 {}
impl<T> DimRef0 for T {}

crate::with_swizzling_for_dim!(1, gen_dim_x_trait, 0);
crate::with_swizzling_for_dim!(2, gen_dim_x_trait, 1);
crate::with_swizzling_for_dim!(3, gen_dim_x_trait, 2);
crate::with_swizzling_for_dim!(4, gen_dim_x_trait, 3);



// =========================
// === DimSetter* Traits ===
// =========================

macro_rules! gen_dim_x_setter_trait {
    ($dim:tt
        $( $name:ident $swizzling_dim:tt [$($dim_ix:tt)*] [$($dim_ord:tt)*] )*
    ) => { paste! {
        #[doc = "Component swizzling setters for "]
        #[doc = stringify!($dim)]
        #[doc = "`-dimensional types."]
        #[allow(missing_docs)]
        pub trait [<DimSetter $dim>] : [<Dim $dim>] {
            $(
                fn [<set_ $name>](&mut self, value: Self::[<Dim $swizzling_dim Type>]);

                fn [<update_ $name>](
                    &mut self,
                    f: impl FnOnce(Self::[<Dim $swizzling_dim Type>]) ->
                       Self::[<Dim $swizzling_dim Type>]
                ) {
                    self.[<set_ $name>](f(self.$name()))
                }

                fn [<modify_ $name>](
                    &mut self, f: impl FnOnce(&mut Self::[<Dim $swizzling_dim Type>]))
                {
                    let mut value = self.$name();
                    f(&mut value);
                    self.[<set_ $name>](value)
                }
            )*
        }

        #[doc = "Component swizzling setters for "]
        #[doc = stringify!($dim)]
        #[doc = "`-dimensional types with interior mutability pattern."]
        #[allow(missing_docs)]
        pub trait [<DimSetterInterior $dim>] : [<Dim $dim>] {
            $(
                fn [<set_$name>](&self, value: Self::[<Dim $swizzling_dim Type>]);

                fn [<update_ $name>](
                    &self,
                    f: impl FnOnce(Self::[<Dim $swizzling_dim Type>]) ->
                       Self::[<Dim $swizzling_dim Type>]
                ) {
                    self.[<set_ $name>](f(self.$name()))
                }

                fn [<modify_ $name>](
                    &self, f: impl FnOnce(&mut Self::[<Dim $swizzling_dim Type>]))
                {
                    let mut value = self.$name();
                    f(&mut value);
                    self.[<set_ $name>](value)
                }
            )*
        }

        impl<T: [<DimSetterInterior $dim>]> [<DimSetterInterior $dim>] for Rc<T> {
            $(
                fn [<set_$name>](&self, value: Self::[<Dim $swizzling_dim Type>]) {
                    (**self).[<set_$name>](value);
                }
            )*
        }

        impl<T: Copy + [<DimSetter $dim>]> [<DimSetterInterior $dim>] for Cell<T> {
            $(
                fn [<set_$name>](&self, value: Self::[<Dim $swizzling_dim Type>]) {
                    let mut t = self.get();
                    t.[<set_$name>](value);
                    self.set(t)
                }
            )*
        }

        impl<T: [<DimSetter $dim>]> [<DimSetterInterior $dim>] for RefCell<T> {
            $(
                fn [<set_$name>](&self, value: Self::[<Dim $swizzling_dim Type>]) {
                    self.borrow_mut().[<set_$name>](value);
                }
            )*
        }
    }};
}

crate::with_swizzling_for_dim_unique!(1, gen_dim_x_setter_trait);
crate::with_swizzling_for_dim_unique!(2, gen_dim_x_setter_trait);
crate::with_swizzling_for_dim_unique!(3, gen_dim_x_setter_trait);
crate::with_swizzling_for_dim_unique!(4, gen_dim_x_setter_trait);



// =====================================
// === Getters for nalgebra::Vector* ===
// =====================================

macro_rules! gen_dim_x_impl_for_vector {
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

crate::with_swizzling_for_dim!(1, gen_dim_x_impl_for_vector, Vector2);
crate::with_swizzling_for_dim!(2, gen_dim_x_impl_for_vector, Vector2);

crate::with_swizzling_for_dim!(1, gen_dim_x_impl_for_vector, Vector3);
crate::with_swizzling_for_dim!(2, gen_dim_x_impl_for_vector, Vector3);
crate::with_swizzling_for_dim!(3, gen_dim_x_impl_for_vector, Vector3);

crate::with_swizzling_for_dim!(1, gen_dim_x_impl_for_vector, Vector4);
crate::with_swizzling_for_dim!(2, gen_dim_x_impl_for_vector, Vector4);
crate::with_swizzling_for_dim!(3, gen_dim_x_impl_for_vector, Vector4);
crate::with_swizzling_for_dim!(4, gen_dim_x_impl_for_vector, Vector4);



// =====================================
// === Setters for nalgebra::Vector* ===
// =====================================

macro_rules! gen_dim_x_setter_impl_for_vector {
    ([$vec:tt] $dim:tt $( $name:ident $swizzling_dim:tt [$($dim_ix:tt)*] [$($dim_ord:tt)*] )*) => {
        paste! {
            impl<T: Scalar + Copy> [<DimSetter $dim>] for $vec<T> {
                $(
                    fn [<set_ $name>](&mut self, value: Self::[<Dim $swizzling_dim Type>]) {
                        gen_dim_x_setter_impl_for_vector_body!
                            {self, value, $swizzling_dim, [$($dim_ix)*], [$($dim_ord)*]}
                    }
                )*
            }
        }
    };
}

macro_rules! gen_dim_x_setter_impl_for_vector_body {
    ($this:tt, $value:tt, $swizzling_dim:tt, [$dim_ix:tt], [$($dim_ord:tt),*]) => {
        $this[$dim_ix] = $value;
    };
    ($this:tt, $value:tt, $swizzling_dim:tt, [$($dim_ix:tt),*], [$($dim_ord:tt),*]) => { paste! {
        $( $this[$dim_ix] = $value[$dim_ord]; )*
    }};
}

crate::with_swizzling_for_dim_unique!(1, gen_dim_x_setter_impl_for_vector, Vector2);
crate::with_swizzling_for_dim_unique!(2, gen_dim_x_setter_impl_for_vector, Vector2);

crate::with_swizzling_for_dim_unique!(1, gen_dim_x_setter_impl_for_vector, Vector3);
crate::with_swizzling_for_dim_unique!(2, gen_dim_x_setter_impl_for_vector, Vector3);
crate::with_swizzling_for_dim_unique!(3, gen_dim_x_setter_impl_for_vector, Vector3);

crate::with_swizzling_for_dim_unique!(1, gen_dim_x_setter_impl_for_vector, Vector4);
crate::with_swizzling_for_dim_unique!(2, gen_dim_x_setter_impl_for_vector, Vector4);
crate::with_swizzling_for_dim_unique!(3, gen_dim_x_setter_impl_for_vector, Vector4);
crate::with_swizzling_for_dim_unique!(4, gen_dim_x_setter_impl_for_vector, Vector4);



// ==========================
// === Getters for Tuples ===
// ==========================

macro_rules! gen_dim_x_impl_for_tuple {
    ([$vec:tt] $dim:tt $( $name:ident $swizzling_dim:tt [$($dim_ix:tt)*] [$($dim_ord:tt)*] )*) => {
        paste! {
            impl<T: Copy> [<Dim $dim>] for $vec<T> {
                type [<Dim $dim Type>] = [<Tuple $dim>]<T>;
                $(
                    fn $name(&self) -> Self::[<Dim $swizzling_dim Type>] {
                        gen_dim_impl_for_tuple_body!{self, $swizzling_dim, [$($dim_ix)*]}
                    }
                )*
            }
        }
    };
}

macro_rules! gen_dim_impl_for_tuple_body {
    ($this:tt, $swizzling_dim:tt, [$($dim_ix:tt),*]) => {
        ( $( $this.$dim_ix ),* )
    };
}

type Tuple1<T> = T;
type Tuple2<T> = (T, T);
type Tuple3<T> = (T, T, T);
type Tuple4<T> = (T, T, T, T);

crate::with_swizzling_for_dim!(1, gen_dim_x_impl_for_tuple, Tuple2);
crate::with_swizzling_for_dim!(2, gen_dim_x_impl_for_tuple, Tuple2);

crate::with_swizzling_for_dim!(1, gen_dim_x_impl_for_tuple, Tuple3);
crate::with_swizzling_for_dim!(2, gen_dim_x_impl_for_tuple, Tuple3);
crate::with_swizzling_for_dim!(3, gen_dim_x_impl_for_tuple, Tuple3);

crate::with_swizzling_for_dim!(1, gen_dim_x_impl_for_tuple, Tuple4);
crate::with_swizzling_for_dim!(2, gen_dim_x_impl_for_tuple, Tuple4);
crate::with_swizzling_for_dim!(3, gen_dim_x_impl_for_tuple, Tuple4);
crate::with_swizzling_for_dim!(4, gen_dim_x_impl_for_tuple, Tuple4);



// ==========================
// === Generic Dim Traits ===
// ==========================

/// A typed dimension. Used mostly with combination with the [`Dim`] and [`DimSetter`] traits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct X;

/// A typed dimension. Used mostly with combination with the [`Dim`] and [`DimSetter`] traits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Y;

/// A typed dimension. Used mostly with combination with the [`Dim`] and [`DimSetter`] traits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Z;

/// A typed dimension. Used mostly with combination with the [`Dim`] and [`DimSetter`] traits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct W;

/// Component getter for the given dimension.
#[allow(missing_docs)]
pub trait Dim<D>: Dim1 {
    fn get_dim(&self, dim: D) -> Self::Dim1Type;
}

/// Component getter for the given dimension.
#[allow(missing_docs)]
pub trait DimRef<D> {
    type Output;
    fn get_dim(&self, dim: D) -> &Self::Output;
}

/// Component getter for the given dimension.
#[allow(missing_docs)]
pub trait DimMut<D>: DimRef<D> {
    fn get_dim_mut(&mut self, dim: D) -> &mut Self::Output;
}

/// Component setter for the given dimension.
#[allow(missing_docs)]
pub trait DimSetter<D>: Dim<D> {
    fn set_dim(&mut self, dim: D, value: Self::Dim1Type);
    /// Set dimension value, returning `true` only if the value was changed.
    fn set_dim_checked(&mut self, dim: D, value: Self::Dim1Type) -> bool;
    fn update_dim(&mut self, dim: D, f: impl FnOnce(Self::Dim1Type) -> Self::Dim1Type)
    where D: Copy {
        self.set_dim(dim, f(self.get_dim(dim)));
    }
    fn modify_dim(&mut self, dim: D, f: impl FnOnce(&mut Self::Dim1Type))
    where D: Copy {
        let mut value = self.get_dim(dim);
        f(&mut value);
        self.set_dim(dim, value);
    }
}

/// Component setter for the given dimension for structs with interior mutability pattern.
#[allow(missing_docs)]
pub trait DimSetterInterior<D>: Dim<D> {
    fn set_dim(&self, dim: D, value: Self::Dim1Type);
    fn update_dim(&self, dim: D, f: impl FnOnce(Self::Dim1Type) -> Self::Dim1Type)
    where D: Copy {
        self.set_dim(dim, f(self.get_dim(dim)));
    }
    fn modify_dim(&self, dim: D, f: impl FnOnce(&mut Self::Dim1Type))
    where D: Copy {
        let mut value = self.get_dim(dim);
        f(&mut value);
        self.set_dim(dim, value);
    }
}



// ======================================================
// === Generic Dim traits impls for nalgebra::Vector* ===
// ======================================================

macro_rules! gen_dim_impl_for_vector {
    ($vec:ident, $($dim:ident),*) => {
        paste! { $(
            impl<T: Scalar + Copy> Dim<[<$dim:upper>]> for $vec<T> {
                fn get_dim(&self, _dim: [<$dim:upper>]) -> Self::Dim1Type {
                    self.$dim()
                }
            }

            impl<T: Scalar + Copy> DimSetter<[<$dim:upper>]> for $vec<T> {
                fn set_dim(&mut self, _dim: [<$dim:upper>], value: Self::Dim1Type) {
                    self.[<set_ $dim>](value);
                }

                fn set_dim_checked(&mut self, _dim: [<$dim:upper>], value: Self::Dim1Type) -> bool {
                    if self.$dim() == value {
                        false
                    } else {
                        self.[<set_ $dim>](value);
                        true
                    }
                }
            }
        )*}
    };
}

gen_dim_impl_for_vector!(Vector2, x, y);
gen_dim_impl_for_vector!(Vector3, x, y, z);
gen_dim_impl_for_vector!(Vector4, x, y, z, w);



// =====================================================
// === Generic Dim traits impls for Cell and RefCell ===
// =====================================================

impl<D: Copy, T: Copy + Dim<D>> Dim<D> for Cell<T> {
    fn get_dim(&self, dim: D) -> Self::Dim1Type {
        self.get().get_dim(dim)
    }
}

impl<D: Copy, T: Copy + Dim<D>> Dim<D> for RefCell<T> {
    fn get_dim(&self, dim: D) -> Self::Dim1Type {
        self.borrow().get_dim(dim)
    }
}

impl<D: Copy, T: Copy + DimSetter<D>> DimSetterInterior<D> for Cell<T> {
    fn set_dim(&self, dim: D, value: Self::Dim1Type) {
        let mut cell_value = self.get();
        cell_value.set_dim(dim, value);
        self.set(cell_value);
    }
}

impl<D: Copy, T: Copy + DimSetter<D>> DimSetterInterior<D> for RefCell<T> {
    fn set_dim(&self, dim: D, value: Self::Dim1Type) {
        self.borrow_mut().set_dim(dim, value);
    }
}


// ===========================================
// === Generic Dim traits impls for tuples ===
// ===========================================

macro_rules! gen_dim_impl_for_tuple {
    (($($t:tt),*) $dim:tt $dim_num:tt) => {
        paste! {
            impl<$([<T $t>]),*> DimRef<$dim> for ($([<T $t>],)*) {
                type Output = [<T $dim_num>];
                fn get_dim(&self, _dim: $dim) -> &Self::Output {
                    &self.$dim_num
                }
            }

            impl<$([<T $t>]),*> DimMut<$dim> for ($([<T $t>],)*) {
                fn get_dim_mut(&mut self, _dim: $dim) -> &mut Self::Output {
                    &mut self.$dim_num
                }
            }
        }
    };
}

gen_dim_impl_for_tuple!((0, 1) X 0);
gen_dim_impl_for_tuple!((0, 1) Y 1);
