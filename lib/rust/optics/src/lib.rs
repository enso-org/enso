#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(clippy::option_map_unit_fn)]

use enso_prelude::*;

// ================
// === TypeList ===
// ================

trait HList {}
impl HList for Nil {}
impl<Head,Tail> HList for Cons<Head,Tail> where Head:?Sized, Tail:?Sized {}

struct Nil;
struct Cons<Head,Tail>(PhantomData2<Head,Tail>) where Head:?Sized, Tail:?Sized;

// === Instances ===

impl<Head:?Sized, Tail:?Sized>
Cons<Head,Tail> {
    pub fn new() -> Self { Self::default() }
}

impl<Head:?Sized, Tail:?Sized>
Default for Cons<Head,Tail> {
    fn default() -> Self { Self(default()) }
}

// === Append ===

type Append<El, Els> = <Els as Appendable<El>>::Result;
trait Appendable<T:?Sized> { type Result; }

impl<T:?Sized>
Appendable<T> for Nil {
    type Result = Cons<T, Nil>;
}

impl<T:?Sized, Head:?Sized, Tail:?Sized+Appendable<T>>
Appendable<T> for Cons<Head, Tail> {
    type Result = Cons<Head, Append<T, Tail>>;
}

// =============
// === Field ===
// =============

type Field<T, Field> = <T as HasField<Field>>::Result;
trait HasField<Field> { type Result; }

// ==============
// === Getter ===
// ==============

trait Getter<T>: HasField<T> {
    fn get     (&    self) -> &    Field<Self, T>;
    fn get_mut (&mut self) -> &mut Field<Self, T>;
}

trait OptGetter<T>: HasField<T> {
    fn get     (&    self) -> Option <&    Field<Self, T>>;
    fn get_mut (&mut self) -> Option <&mut Field<Self, T>>;
}

// ================
// === Resolver ===
// ================

// === FieldResolver ===

type NestedField<T, Path> = <Path as FieldResolver<T>>::Result;
trait FieldResolver<T: ?Sized> { type Result; }

impl<T>
FieldResolver<T> for Nil {
    type Result = T;
}

impl<Head, Tail, T>
FieldResolver<T> for Cons<Head, Tail>
where T: HasField<Head>, Tail: FieldResolver<Field<T, Head>> {
    type Result = NestedField<Field<T, Head>, Tail>;
}

// === Resolver ===

trait Resolver<T>: FieldResolver<T> {
    fn resolve     (t: &    T) -> &    NestedField<T, Self>;
    fn resolve_mut (t: &mut T) -> &mut NestedField<T, Self>;
}

trait OptResolver<T>: FieldResolver<T> {
    fn resolve     (t: &    T) -> Option<&    NestedField<T, Self>>;
    fn resolve_mut (t: &mut T) -> Option<&mut NestedField<T, Self>>;
}

impl<T> Resolver<T> for Nil {
    fn resolve     (t: &    T) -> &    NestedField<T, Self> { t }
    fn resolve_mut (t: &mut T) -> &mut NestedField<T, Self> { t }
}

impl<T> OptResolver<T> for Nil {
    fn resolve     (t: &    T) -> Option<&    NestedField<T, Self>> { Some(t) }
    fn resolve_mut (t: &mut T) -> Option<&mut NestedField<T, Self>> { Some(t) }
}

impl<Head:'static,Tail,T> Resolver<T> for Cons<Head, Tail>
where T: Getter<Head>, Tail: Resolver<Field<T, Head>> {
    fn resolve(t: &T) -> &NestedField<T, Self> {
        let head = Getter::<Head>::get(t);
        <Tail as Resolver<Field<T, Head>>>::resolve(head)
    }
    fn resolve_mut(t: &mut T) -> &mut NestedField<T, Self> {
        let head = Getter::<Head>::get_mut(t);
        <Tail as Resolver<Field<T, Head>>>::resolve_mut(head)
    }
}

impl<Head:'static,Tail,T> OptResolver<T> for Cons<Head, Tail>
    where T: OptGetter<Head>, Tail: OptResolver<Field<T, Head>> {
    fn resolve(t: &T) -> Option<&NestedField<T, Self>> {
        OptGetter::<Head>::get(t)
            .and_then(|t| <Tail as OptResolver<Field<T, Head>>>::resolve(t))
    }
    fn resolve_mut(t: &mut T) -> Option<&mut NestedField<T, Self>> {
        OptGetter::<Head>::get_mut(t)
            .and_then(|t| <Tail as OptResolver<Field<T, Head>>>::resolve_mut(t))
    }
}

// ============
// === Lens ===
// ============

struct Lens    <Src,Tgt,Path>(PhantomData3<Src,Tgt,Path>);
struct OptLens <Src,Tgt,Path>(PhantomData3<Src,Tgt,Path>);

impl<Src,Tgt,Path> Copy  for Lens<Src,Tgt,Path> {}
impl<Src,Tgt,Path> Clone for Lens<Src,Tgt,Path> {
    fn clone(&self) -> Self { Lens::new() }
}

impl<Src,Tgt,Path> Copy  for OptLens<Src,Tgt,Path> {}
impl<Src,Tgt,Path> Clone for OptLens<Src,Tgt,Path> {
    fn clone(&self) -> Self { OptLens::new() }
}

impl<Src,Tgt,Path> OptLens<Src,Tgt,Path> {
    pub fn resolve(self, t: &Src) -> Option<&NestedField<Src, Path>>
    where Path: OptResolver<Src> {
        <Path as OptResolver<Src>>::resolve(t)
    }
    pub fn resolve_mut(self, t: &mut Src) -> Option<&mut NestedField<Src, Path>>
    where Path: OptResolver<Src> {
        <Path as OptResolver<Src>>::resolve_mut(t)
    }
}

impl<Src,Tgt,Path> Lens<Src,Tgt,Path> {
    pub fn resolve(self, t: &Src) -> &NestedField<Src, Path>
    where Path: Resolver<Src> {
        <Path as Resolver<Src>>::resolve(t)
    }
}

impl<Src,Tgt,Path> Lens<Src,Tgt,Path> {
    pub fn new() -> Self { default() }
}

impl<Src,Tgt,Path> Default for Lens<Src,Tgt,Path> {
    fn default() -> Self { Self(default()) }
}

impl<Src,Tgt,Path> OptLens<Src,Tgt,Path> {
    pub fn new() -> Self { default() }
}

impl<Src,Tgt,Path> Default for OptLens<Src,Tgt,Path> {
    fn default() -> Self { Self(default()) }
}

struct BoundLens<'t,Src,Tgt,Path> {
    target: &'t Src,
    lens: Lens<Src,Tgt,Path>,
}

struct BoundLensMut<'t,Src,Tgt,Path> {
    target: &'t mut Src,
    lens: Lens<Src,Tgt,Path>,
}

struct BoundOptLens<'t,Src,Tgt,Path> {
    target: &'t Src,
    lens: OptLens<Src,Tgt,Path>,
}

struct BoundOptLensMut<'t,Src,Tgt,Path> {
    target: &'t mut Src,
    lens: OptLens<Src,Tgt,Path>,
}

impl<'t,Src,Tgt,Path> BoundLens<'t,Src,Tgt,Path> {
    pub fn new(target: &'t Src) -> Self {
        let lens = Lens::new();
        Self { target, lens }
    }

    pub fn read(&self) -> &NestedField<Src,Path>
    where Path: Resolver<Src> {
        self.lens.resolve(self.target)
    }
}

impl<'t,Src,Tgt,Path> BoundOptLens<'t,Src,Tgt,Path> {
    pub fn new(target: &'t Src) -> Self {
        let lens = OptLens::new();
        Self { target, lens }
    }

    pub fn read(&self) -> Option<&NestedField<Src, Path>>
    where Path: OptResolver<Src> {
        self.lens.resolve(self.target)
    }
}

impl<'t,Src:Clone,Tgt,Path> BoundOptLens<'t,Src,Tgt,Path> {
    pub fn write(&self, val: NestedField<Src, Path>)
    where for<'a> Path: OptResolver<Src>,
          OptLens<Src,Tgt,Path>: Copy {
        let mut a = (*self.target).clone();
        a.lens_mut().unsafe_repath::<Path>().set(val);
    }
}

impl<'t,Src,Tgt,Path> BoundOptLensMut<'t,Src,Tgt,Path> {
    pub fn new(target: &'t mut Src) -> Self {
        let lens = OptLens::new();
        Self { target, lens }
    }

    pub fn get(&mut self) -> Option<&mut NestedField<Src, Path>>
    where Path: OptResolver<Src> {
        self.lens.resolve_mut(self.target)
    }

    pub fn set(&mut self, val: NestedField<Src, Path>)
    where Path: OptResolver<Src> {
        let r = self.get();
        r.map(|s| *s = val);
    }

    pub fn unsafe_repath<Path2>(self) -> BoundOptLensMut<'t,Src,Tgt,Path2> {
        BoundOptLensMut {
            target: self.target,
            lens: OptLens::new(),
        }
    }
}

////////////////////////////////////////
////////////////////////////////////////
////////////////////////////////////////


trait HasLens where Self:Sized {
    fn lens(&self) -> BoundOptLens<'_, Self, Self, Nil> {
        BoundOptLens::new(self)
    }
    fn lens_mut(&mut self) -> BoundOptLensMut<'_, Self, Self, Nil> {
        BoundOptLensMut::new(self)
    }
}
impl<T> HasLens for T {}

macro_rules! mk_lens_field_decl {
($struct_name:ident<$($param:ident),*>{$field_name:ident : $field_type:ty}) => {
paste::item! {
    // struct FIELD_bar;
    struct [<FIELD_ $field_name>];

    // impl HasField<FIELD_bar> for Foo<> {
    //     type Result = Bar;
    // }
    impl<$($param),*> HasField<[<FIELD_ $field_name>]> for $struct_name<$($param),*> {
    type Result = $field_type;
    }
}}}

macro_rules! mk_lenses_for {
($struct_name:ident<$($param:ident),*>{$field_name:ident : $field_type:ty}) => {
paste::item! {

    mk_lens_field_decl!($struct_name<$($param),*>{$field_name:$field_type});

    // impl Getter<FIELD_bar> for Foo {
    //     fn get(&self) -> &Field<Self, FIELD_bar> {
    //         &self.bar
    //     }
    //     fn get_mut(&mut self) -> &mut Field<Self, FIELD_bar> {
    //         &mut self.bar
    //     }
    // }
    impl<$($param),*> Getter<[<FIELD_ $field_name>]> for $struct_name<$($param),*> {
        fn get(&self) -> &Field<Self, [<FIELD_ $field_name>]> {
            &self.$field_name
        }
        fn get_mut(&mut self) -> &mut Field<Self, [<FIELD_ $field_name>]> {
            &mut self.$field_name
        }
    }

    // impl OptGetter<FIELD_bar> for Foo {
    //     fn get(&self) -> Option<&Field<Self, FIELD_bar>> {
    //         Some(&self.bar)
    //     }
    //     fn get_mut(&mut self) -> Option<&mut Field<Self, FIELD_bar>> {
    //         Some(&mut self.bar)
    //     }
    // }
    impl<$($param),*> OptGetter<[<FIELD_ $field_name>]> for $struct_name<$($param),*> {
        fn get(&self) -> Option<&Field<Self, [<FIELD_ $field_name>]>> {
            Some(&self.$field_name)
        }
        fn get_mut(&mut self)
        -> Option<&mut Field<Self, [<FIELD_ $field_name>]>> {
            Some(&mut self.$field_name)
        }
    }

    // impl<'lens_lifetime, LENS_BEGIN, LENS_NEW_PATH>
    // BoundOptLensMut<'lens_lifetime, LENS_BEGIN, Foo, LENS_NEW_PATH>
    // where LENS_NEW_PATH: Appendable<FIELD_bar> {
    //     fn bar(self)
    //     -> BoundOptLensMut
    //     < 'lens_lifetime
    //     , LENS_BEGIN
    //     , Bar
    //     , Append<FIELD_bar
    //     , LENS_NEW_PATH>
    //     > {
    //         BoundOptLensMut::new(self.target)
    //     }
    // }
    impl<'lens_lifetime, LENS_BEGIN, LENS_NEW_PATH>
    BoundOptLensMut<'lens_lifetime, LENS_BEGIN, $struct_name, LENS_NEW_PATH>
    where LENS_NEW_PATH: Appendable<[<FIELD_ $field_name>]> {
        fn $field_name(self)
        -> BoundOptLensMut
        < 'lens_lifetime
        , LENS_BEGIN
        , Field<$struct_name
        ,[<FIELD_ $field_name>]>
        , Append<[<FIELD_ $field_name>]
        , LENS_NEW_PATH>
        > {
            BoundOptLensMut::new(self.target)
        }
    }

    impl<'lens_lifetime, LENS_BEGIN, LENS_NEW_PATH>
    BoundOptLens<'lens_lifetime, LENS_BEGIN, $struct_name, LENS_NEW_PATH>
    where LENS_NEW_PATH: Appendable<[<FIELD_ $field_name>]> {
        fn $field_name(self)
        -> BoundOptLens
        < 'lens_lifetime
        , LENS_BEGIN
        , Field<$struct_name
        ,[<FIELD_ $field_name>]>
        , Append<[<FIELD_ $field_name>]
        , LENS_NEW_PATH>
        > {
            BoundOptLens::new(self.target)
        }
    }
}};

($struct_name:ident<$($param:ident),*> :: $cons_name:ident {$field_name:ident : $field_type:ty}) => {
paste::item! {

    mk_lens_field_decl!($struct_name<$($param),*>{$field_name:$field_type});

    // impl OptGetter<FIELD_bar> for Foo {
    //     fn get(&self) -> Option<&Field<Self, FIELD_bar>> {
    //         Some(&self.bar)
    //     }
    //     fn get_mut(&mut self) -> Option<&mut Field<Self, FIELD_bar>> {
    //         Some(&mut self.bar)
    //     }
    // }
    impl<$($param),*> OptGetter<[<FIELD_ $field_name>]> for $struct_name<$($param),*> {
        fn get(&self) -> Option<&Field<Self, [<FIELD_ $field_name>]>> {
            match self {
                $cons_name(ref $field_name) => Some($field_name),
                _                           => None
            }
        }
        fn get_mut(&mut self)
        -> Option<&mut Field<Self, [<FIELD_ $field_name>]>> {
            match self {
                $cons_name(ref mut $field_name) => Some($field_name),
                _                               => None
            }
        }
    }

    // impl<'lens_lifetime, LENS_BEGIN, LENS_NEW_PATH>
    // BoundOptLensMut<'lens_lifetime, LENS_BEGIN, Foo, LENS_NEW_PATH>
    // where LENS_NEW_PATH: Appendable<FIELD_bar> {
    //     fn bar(self)
    //     -> BoundOptLensMut
    //     < 'lens_lifetime
    //     , LENS_BEGIN
    //     , Field<FIELD_Bar, Foo<>>
    //     , Append<FIELD_bar, LENS_NEW_PATH>
    //     > {
    //         BoundOptLensMut::new(self.target)
    //     }
    // }
    impl<'lens_lifetime, LENS_BEGIN, LENS_NEW_PATH, $($param),*>
    BoundOptLensMut<'lens_lifetime, LENS_BEGIN, $struct_name<$($param),*>, LENS_NEW_PATH>
    where LENS_NEW_PATH: Appendable<[<FIELD_ $field_name>]> {
        fn $cons_name(self)
        -> BoundOptLensMut
        < 'lens_lifetime
        , LENS_BEGIN
        , Field<$struct_name<$($param),*>,[<FIELD_ $field_name>]>
        , Append<[<FIELD_ $field_name>], LENS_NEW_PATH>
        > {
            BoundOptLensMut::new(self.target)
        }
    }

    impl<'lens_lifetime, LENS_BEGIN, LENS_NEW_PATH, $($param),*>
    BoundOptLens<'lens_lifetime, LENS_BEGIN, $struct_name<$($param),*>, LENS_NEW_PATH>
    where LENS_NEW_PATH: Appendable<[<FIELD_ $field_name>]> {
        fn $cons_name(self)
        -> BoundOptLens
        < 'lens_lifetime
        , LENS_BEGIN
        , Field<$struct_name<$($param),*>,[<FIELD_ $field_name>]>
        , Append<[<FIELD_ $field_name>], LENS_NEW_PATH>
        > {
            BoundOptLens::new(self.target)
        }
    }
}}}

macro_rules! lens {
    ($base:ident . $($seg:ident).*) => {
        $base.lens().$($seg()).*
    }
}

macro_rules! lens_mut {
    ($base:ident . $($seg:ident).*) => {
        $base.lens_mut().$($seg()).*
    }
}


mk_lenses_for!(Option<T>::Some{val: T});


#[cfg(test)]
mod tests {
    use super::*;



    #[derive(Debug, PartialEq, Eq)]
    struct Foo {
        bar: Bar,
    }
    mk_lenses_for!(Foo<>{bar: Bar});

    #[derive(Debug, PartialEq, Eq)]
    struct Bar {
        baz: Option<Baz>,
    }
    mk_lenses_for!(Bar<>{baz: Option<Baz>});

    #[derive(Debug, PartialEq, Eq)]
    struct Baz {
        qux: Option<Qux>,
    }
    mk_lenses_for!(Baz<>{qux: Option<Qux>});

    #[derive(Debug, PartialEq, Eq)]
    struct Qux {
        quxx: String,
    }
    mk_lenses_for!(Qux<>{quxx: String});


    #[test]
    fn deeply_nested() {
        let mut foo = Foo {
            bar: Bar {
                baz: Some(Baz {
                    qux: Some(Qux {
                        quxx: "nice".to_owned(),
                    }),
                }),
            },
        };

        lens_mut!(foo.bar.baz.Some.qux.Some.quxx).set("Hello".into());
        assert_eq!(foo, Foo {
            bar: Bar {
                baz: Some(Baz {
                    qux: Some(Qux {
                        quxx: "Hello".to_owned(),
                    }),
                }),
            },
        })
    }
}

pub fn main() {
    let mut val: Option<Option<i32>> = Some(Some(17));

    val.lens_mut().Some().Some().set(10);
    let lens1 = val.lens().Some().Some();
    let mut val2 = *lens1.target;
    let lm = val2.lens_mut();
    let mut lm2 = BoundOptLensMut {
        target: lm.target,
        lens: lens1.lens,
    };
    lm2.set(9);
    println!("{:?}", val.lens_mut().Some().Some().get());
    println!("{:?}", val2.lens_mut().Some().Some().get());
}
