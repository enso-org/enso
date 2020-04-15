use parser::Parser;
use enso_prelude::default;
use ast::HasIdMap;



#[ignore]
#[test]
fn parsing_main_with_id_map() {
    const CASES : &[&str] = &
        [ "main =\n    2 + 2"
        , "main =   \n \n    2 + 2\n    foo = bar \n    baz"
        ];

    for case in CASES {
        let parser = Parser::new().unwrap();
        let ast1 = parser.parse(case.to_string(),default()).unwrap();
        let id_map = ast1.id_map();
        let ast2 = parser.parse(case.to_string(),id_map).unwrap();
        assert_eq!(ast1,ast2)
    }
}
