// This module contains dead code. Its purpose is making sure that it compiles
#![allow(dead_code)]

use enso_prelude::*;

#[derive(Clone,CloneRef)] struct StructUnit;

#[derive(Clone,CloneRef)] struct StructUnnamedEmpty();

#[derive(Clone,CloneRef)] struct StructUnnamed(Rc<i32>,Rc<String>);

#[derive(Clone,CloneRef)] struct StructNamedEmpty{}

#[derive(Clone,CloneRef)] struct StructNamed{named0:Rc<i32>,named1:Rc<String>}

#[derive(Clone,CloneRef)] enum EnumEmpty {}

#[derive(Clone,CloneRef)] enum Enum {
    Unit,
    NamedEmpty {},
    Named {named0:Rc<i32>,named1:Rc<String>},
    UnnamedEmpty(),
    Unnamed(Rc<i32>,Rc<String>),
}

#[derive(CloneRef,Derivative)]
#[derivative(Clone(bound=""))]
struct StructUnnamedUnbound<T>(Rc<T>);

#[derive(CloneRef,Clone)]
#[clone_ref(bound="T:CloneRef")]
struct StructUnnamedBound<T>(T);

#[derive(CloneRef,Clone)]
#[clone_ref(bound="T:CloneRef,U:CloneRef")]
struct StructUnnamedBoundTwoPatams<T,U>(T,U);

#[derive(Clone,CloneRef)]
#[clone_ref(bound="T:Clone+Display")]
struct StructBoundGeneric<T:Display>(Rc<T>);

#[derive(CloneRef,Derivative)]
#[derivative(Clone(bound=""))]
// Note: CloneRef "knows" about `Display` bound.
struct StructGenericLifetime<'t>(PhantomData<&'t String>);

#[derive(CloneRef,Derivative)]
#[derivative(Clone(bound=""))]
struct StructWhereClause<T>(Rc<T>) where T:Debug;

#[derive(CloneRef,Clone)]
#[clone_ref(bound="T:CloneRef")]
// Here derive macro must correctly merge user-provided bound, generics list bound and where clause.
struct StructVariousBounds<T:Display>(T) where T:Debug;
