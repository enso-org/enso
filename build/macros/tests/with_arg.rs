use enso_build_base::prelude::*;

use itertools::Itertools;



#[derive(enso_build_macros::Arg)]
pub enum Foo {
    Bar,
    BarBaz(String),
    HogeHoge(OsString),
    // #[arg(format = ToString::to_string)]
    // TaraPon(u32),
}

#[test]
fn test_argument_formatting() {
    let bar = Foo::Bar;
    assert_eq!(bar.into_iter().collect_vec(), vec![OsString::from("--bar")]);

    let bar_baz = Foo::BarBaz("foo".into());
    assert_eq!(bar_baz.into_iter().collect_vec(), vec![
        OsString::from("--bar-baz"),
        OsString::from("foo")
    ]);

    let hoge_hoge = Foo::HogeHoge(OsString::from("foo"));
    assert_eq!(hoge_hoge.into_iter().collect_vec(), vec![
        OsString::from("--hoge-hoge"),
        OsString::from("foo")
    ]);
}
