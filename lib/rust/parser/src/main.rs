use enso_automata::dfa;
use enso_automata::dfa::ParserRunner;
use enso_automata::dfa::P1;

include!("../dist/lexer.rs");

pub fn main() {
    let input = "aab";
    let runner = ParserRunner::new(input);
    let mut parser = P1 { runner };
    println!("{:?}", parser.runner.current_input);
    step(&mut parser);
    println!("{:?}", parser.runner.current_input);
    step(&mut parser);
    println!("{:?}", parser.runner.current_input);
    step(&mut parser);
    println!("{:?}", parser.runner.current_input);
    step(&mut parser);
    println!("{:?}", parser.runner.current_input);

    // dfa::example_parser();

    // parser
}
