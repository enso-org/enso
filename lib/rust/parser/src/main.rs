use enso_automata::dfa::Parser;

include!("../dist/lexer.rs");

pub fn main() {
    let input = "abc";
    let mut parser = Parser::new(input);
    println!("{:?}", parser.current_input);
    step(&mut parser);
    println!("{:?}", parser.current_input);
    step(&mut parser);
    println!("{:?}", parser.current_input);
    step(&mut parser);
    println!("{:?}", parser.current_input);

    // parser
}
