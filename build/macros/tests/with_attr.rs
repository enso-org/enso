// use enso_build_base::prelude::*;
//
// use itertools::Itertools;
//

// #[derive(enso_build_macros::Arg)]
// pub enum Foo {
//     Bar,
//     #[arg(format = ToString::to_string)]
//     TaraPon(u32),
// }
//
// #[test]
// fn test_argument_formatting() {
//     let bar = Foo::Bar;
//     assert_eq!(bar.into_iter().collect_vec(), vec![OsString::from("--bar")]);
//
//     let tara_pon = Foo::TaraPon(42);
//     assert_eq!(tara_pon.into_iter().collect_vec(), vec![
//         OsString::from("--tara-pon"),
//         OsString::from("42")
//     ]);
// }
