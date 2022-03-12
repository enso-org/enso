use enso_automata::dfa;
use enso_automata::dfa::ParserRunner;

include!("../dist/lexer.rs");

pub fn main() {
    let input = "abc";
    let mut parser = ParserRunner::new(input);
    println!("{:?}", parser.current_input);
    step(&mut parser);
    println!("{:?}", parser.current_input);
    step(&mut parser);
    println!("{:?}", parser.current_input);
    step(&mut parser);
    println!("{:?}", parser.current_input);

    // dfa::example_parser();

    // parser
}
