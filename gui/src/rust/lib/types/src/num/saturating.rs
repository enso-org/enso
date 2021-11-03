//! Definition of numeric types whose operations are saturating by default.

use crate::algebra::*;

macro_rules! def_saturating_unsigned_int {
    ($($name:ident($prim:ident)),* $(,)?) => {$(
        /// Saturating version of $prim.
        #[derive(Clone,Copy,Debug,Default,Eq,Hash,Ord,PartialEq,PartialOrd)]
        pub struct $name {
            /// Underlying primitive type.
            pub raw : $prim,
        }

        impl std::ops::Deref for $name {
            type Target = $prim;
            fn deref(&self) -> &Self::Target {
                &self.raw
            }
        }

        impl std::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.raw
            }
        }

        impl From<$prim>  for $name { fn from(raw:$prim)  -> Self { Self{raw} } }
        impl From<&$prim> for $name { fn from(raw:&$prim) -> Self { (*raw).into() } }
        impl From<$name>  for $prim { fn from(num:$name)  -> Self { num.raw } }
        impl From<&$name> for $prim { fn from(num:&$name) -> Self { num.raw } }

        crate::impl_T_x_T_to_T!{Mul::mul as saturating_mul for $name {raw}}
        crate::impl_T_x_T_to_T!{Add::add as saturating_add for $name {raw}}
        crate::impl_T_x_T_to_T!{Sub::sub as saturating_sub for $name {raw}}
        crate::impl_T_x_T_to_T!{Div::div for $name {raw}}

        crate::impl_T_x_S_to_T!{Mul::mul [$prim] as saturating_mul for $name {raw}}
        crate::impl_T_x_S_to_T!{Add::add [$prim] as saturating_add for $name {raw}}
        crate::impl_T_x_S_to_T!{Sub::sub [$prim] as saturating_sub for $name {raw}}
        crate::impl_T_x_S_to_T!{Div::div [$prim] for $name {raw}}

        crate::impl_T_x_T_to_T!{SaturatingMul::saturating_mul for $name {raw}}
        crate::impl_T_x_T_to_T!{SaturatingAdd::saturating_add for $name {raw}}
        crate::impl_T_x_T_to_T!{SaturatingSub::saturating_sub for $name {raw}}
    )*}
}

def_saturating_unsigned_int! {
    SaturatingU8(u8),
    SaturatingU16(u16),
    SaturatingU32(u32),
    SaturatingU64(u64),
    SaturatingU128(u128),
    SaturatingUsize(usize),
}
