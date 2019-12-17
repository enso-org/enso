use parser::Parser;
use wasm_bindgen_test::{wasm_bindgen_test_configure, wasm_bindgen_test};
use parser::api::Error::ParsingError;

wasm_bindgen_test_configure!(run_in_browser);


#[wasm_bindgen_test]
fn web_test() {
   let mut parser = Parser::new_or_panic();
   
   let mut parse = |input| {
      match parser.parse(String::from(input)) {
         Err(ParsingError(str)) => str,
         _ => panic!("Not implemented.")
      }
   };
   
   assert_eq!(parse(""), "\
      {\"shape\":{\"Module\":{\"lines\":[{\"elem\":null,\"off\":0}]}}\
      ,\"span\":0}"
   );
}