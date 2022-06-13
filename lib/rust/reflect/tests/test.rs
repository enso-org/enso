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
    quux: T,
}

#[derive(Reflect)]
pub struct Code<'s> {
    pub repr: std::borrow::Cow<'s, str>,
}

#[test]
fn test() {
    use reflect::Reflect;
    let type_ = Baz::reflect();
}
