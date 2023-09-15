//! This module defines utilities for working with ZST.

use super::std_reexports::*;

use crate::ZST;

use derivative::Derivative;



// ===================
// === ZST ===
// ===================

/// Replacement for the default phantom data, that provides a default type argument.
pub type PhantomData0<T = ()> = ZST<T>;

/// The following `ZST` implementations allow each argument to be non Sized. Unfortunately, this is
/// not equivalent to `ZST<(T1,T2,...)>`, as tuple requires each arg to implement `Sized`.
pub type PhantomData2<T1, T2> = ZST<(ZST<T1>, ZST<T2>)>;
pub type PhantomData3<T1, T2, T3> = PhantomData2<PhantomData2<T1, T2>, ZST<T3>>;
pub type PhantomData4<T1, T2, T3, T4> = PhantomData2<PhantomData3<T1, T2, T3>, ZST<T4>>;
pub type PhantomData5<T1, T2, T3, T4, T5> = PhantomData2<PhantomData4<T1, T2, T3, T4>, ZST<T5>>;
pub type PhantomData6<T1, T2, T3, T4, T5, T6> =
    PhantomData2<PhantomData5<T1, T2, T3, T4, T5>, ZST<T6>>;
pub type PhantomData7<T1, T2, T3, T4, T5, T6, T7> =
    PhantomData2<PhantomData6<T1, T2, T3, T4, T5, T6>, ZST<T7>>;
pub type PhantomData8<T1, T2, T3, T4, T5, T6, T7, T8> =
    PhantomData2<PhantomData7<T1, T2, T3, T4, T5, T6, T7>, ZST<T8>>;
pub type PhantomData9<T1, T2, T3, T4, T5, T6, T7, T8, T9> =
    PhantomData2<PhantomData8<T1, T2, T3, T4, T5, T6, T7, T8>, ZST<T9>>;



// ===================
// === WithPhantom ===
// ===================

/// A wrapper adding a phantom type to a structure.
#[derive(Derivative)]
#[derivative(Clone(bound = "T:Clone"))]
#[derivative(Default(bound = "T:Default"))]
#[derivative(Debug(bound = "T:Debug"))]
pub struct WithPhantom<T, P = ()> {
    pub without_phantom: T,
    phantom:             ZST<P>,
}

impl<T, P> WithPhantom<T, P> {
    pub fn new(without_phantom: T) -> Self {
        let phantom = ZST();
        Self { without_phantom, phantom }
    }
}



// ==========================
// === PhantomConversions ===
// ==========================

/// A utility for easy driving of type-level computations from value level. Often we've got some
/// type level relations, like a few singleton types, and for each such type we've got an associated
/// value. For example, we can define types `Int` and `Float` and associate with them
/// `WebGlContext::Int` and `WebGlContext::Float` constants encoded as `GlEnum`. In order to convert
/// `Int` or `Float` to the `GlEnum` we do not need the instance of the types, only the information
/// what type it was. So we can define:
///
/// ```text
/// impl From<ZST<Int>> for u32 {
///     from(_:ZST<Int>>) {
///         GlEnum(WebGlContext::Int)
///     }
/// }
/// ```
///
/// And use it like:
///
/// ```text
/// let val = GlEnum::from(ZST::<Int>)
/// ```
///
/// Using this utility we can always write the following code instead:
///
/// ```text
/// let val = GlEnum::phantom_from::<Int>()
/// ```
pub trait PhantomConversions: Sized {
    fn phantom_into<P>() -> P
    where Self: PhantomInto<P> {
        ZST::<Self>().into()
    }
    fn phantom_from<P: PhantomInto<Self>>() -> Self {
        ZST::<P>().into()
    }
}
impl<T> PhantomConversions for T {}

/// Like `Into` but for phantom types.
pub trait PhantomInto<T> = where ZST<Self>: Into<T>;
