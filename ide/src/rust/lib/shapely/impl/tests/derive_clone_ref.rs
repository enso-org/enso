// This module contains dead code. Its purpose is making sure that it compiles
#![allow(dead_code)]

use enso_prelude::*;

use shapely::*;

#[derive(Clone,CloneRef)] struct StructUnit;

#[derive(Clone,CloneRef)] struct StructUnnamedEmpty();

#[derive(Clone,CloneRef)] struct StructUnnamed(Rc<i32>,Rc<String>);

#[derive(Clone,CloneRef)] struct StructNamedEmpty{}

#[derive(Clone,CloneRef)] struct StructNamed{named0:Rc<i32>,named1:Rc<String>}

#[derive(Clone,CloneRef)] enum EnumEmpty {}

#[derive(Clone,CloneRef)] enum Enum {
    VariantUnit,
    VariantNamedEmpty {},
    VariantNamed {named0:Rc<i32>,named1:Rc<String>},
    VariantUnnamedEmpty(),
    VariantUnnamed(Rc<i32>,Rc<String>),
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
