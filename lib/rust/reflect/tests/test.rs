// The type definitions in this crate exercise `#[derive(Reflect)]`.

// === Non-Standard Linter Configuration ===
#![allow(dead_code)]

use enso_reflect as reflect;
use enso_reflect_macros::Reflect;



#[derive(Reflect)]
struct Foo;

#[derive(Reflect)]
struct Bar {
    bar: Foo,
}

#[derive(Reflect)]
enum Baz {
    Bar(Bar),
    Baz,
}

#[derive(Reflect)]
struct Quux<T> {
    _quux: T,
}

#[derive(Reflect)]
pub struct Code<'s> {
    pub _repr: std::borrow::Cow<'s, str>,
}

#[test]
fn test() {
    use reflect::Reflect;
    let _type = Baz::reflect();
}
