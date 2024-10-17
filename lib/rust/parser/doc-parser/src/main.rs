//! Prints a debug representation of Enso documentation found in the given Enso source file(s).

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]

use enso_doc_parser::*;
use enso_parser::prelude::*;

use enso_parser::syntax::tree::DocComment;
use enso_parser::syntax::tree::TextElement;



// ====================================
// === Debug Representation Printer ===
// ====================================

fn main() {
    let args = std::env::args().skip(1);
    if args.len() == 0 {
        use std::io::Read;
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).unwrap();
        check_doc_parse("<stdin>", input.as_str());
    } else {
        args.for_each(|path| check_doc_parse(&path, &std::fs::read_to_string(&path).unwrap()));
    }
}

/// Print the token for the input file.
fn check_doc_parse(filename: &str, code: &str) {
    println!("File: {filename}");
    let docs = extract_docs(filename, code);
    for doc in &docs {
        for token in parse(doc) {
            println!("{token:?}");
        }
    }
}

/// Extract docs from the input file.
fn extract_docs(_filename: &str, mut code: &str) -> Vec<String> {
    if let Some((_meta, code_)) = enso_parser::metadata::parse(code) {
        code = code_;
    }
    let ast = enso_parser::Parser::new().parse_module(code);
    let docs = RefCell::new(vec![]);
    ast.visit_trees(|tree| match &tree.variant {
        enso_parser::syntax::tree::Variant::Documented(doc) => {
            docs.borrow_mut().push(doc.documentation.clone());
        }
        enso_parser::syntax::tree::Variant::CaseOf(case_of) => {
            for case in case_of.cases.iter().filter_map(|c| c.case.as_ref()) {
                docs.borrow_mut().extend(case.documentation.clone());
            }
        }
        _ => {}
    });
    docs.take().iter().map(content).collect()
}

/// Return the contents of the comment, with leading whitespace, the `##` token, and following
/// empty lines removed; newlines will be normalized.
pub fn content(node: &DocComment) -> String {
    let mut buf = String::new();
    for element in &node.elements {
        match element {
            TextElement::Section { text } => buf.push_str(&text.code.repr),
            TextElement::Newline { .. } => buf.push('\n'),
            TextElement::Escape {
                token:
                    token @ enso_parser::syntax::token::TextEscape {
                        variant: enso_parser::syntax::token::variant::TextEscape { value },
                        ..
                    },
            } => {
                if let Some(c) = value.to_char() {
                    buf.push(c);
                } else {
                    // Invalid escape character, or unpaired surrogate that can't be represented in
                    // a Rust string.
                    buf.push_str(**token.code)
                }
            }
            // Unreachable.
            TextElement::Splice { .. } => continue,
        }
    }
    buf
}

/// Lex the given documentation, and return the sequence of tokens.
fn parse(input: &str) -> Vec<Token> {
    let mut docs = TokenCollector::<IgnoredLocation>::default();
    let mut lexer = Lexer::default();
    for (line_number, line) in input.trim_start().lines().enumerate() {
        let location = Location::start_of_line(line_number);
        let line = Span { location, text: line };
        lexer.line::<IgnoredLocation>(line, &mut docs);
    }
    lexer.finish(&mut docs);
    docs.tokens
}



// =======================
// === Token Collector ===
// =======================

/// Token consumer that reifies the sequence of tokens for debugging and tests.
#[derive(Default, Debug)]
struct TokenCollector<L> {
    tokens:        Vec<Token>,
    location_type: ZST<L>,
}

#[derive(Debug)]
enum Token {
    Tag { tag: Tag, description: String },
    EnterMarkedSection { mark: Mark, header: String },
    EnterKeyedSection { header: String },
    Start(ScopeType),
    End(ScopeType),
    StartQuote,
    EndQuote,
    Text(String),
    RawLine(String),
}

impl<L> TokenConsumer<L> for TokenCollector<L> {
    fn tag(&mut self, tag: Tag, description: Option<Span<L>>) {
        self.tokens.push(Token::Tag {
            tag,
            description: description.map(String::from).unwrap_or_default(),
        })
    }

    fn enter_marked_section(&mut self, mark: Mark, header: Option<Span<L>>) {
        self.tokens.push(Token::EnterMarkedSection {
            mark,
            header: header.map(String::from).unwrap_or_default(),
        })
    }

    fn enter_keyed_section(&mut self, header: Span<L>) {
        self.tokens.push(Token::EnterKeyedSection { header: header.into() })
    }

    fn text(&mut self, text: Span<L>) {
        match self.tokens.last_mut() {
            Some(Token::Text(current)) => {
                current.push(' ');
                current.push_str(text.text.as_ref())
            }
            _ => self.tokens.push(Token::Text(text.text.into())),
        }
    }

    fn start_list(&mut self) {
        self.tokens.push(Token::Start(ScopeType::List));
    }

    fn start_list_item(&mut self) {
        self.tokens.push(Token::Start(ScopeType::ListItem));
    }

    fn start_paragraph(&mut self) {
        self.tokens.push(Token::Start(ScopeType::Paragraph));
    }

    fn start_raw(&mut self) {
        self.tokens.push(Token::Start(ScopeType::Raw));
    }

    fn start_quote(&mut self) {
        self.tokens.push(Token::StartQuote);
    }

    fn end_quote(&mut self) {
        self.tokens.push(Token::EndQuote);
    }

    fn whitespace(&mut self) {
        self.tokens.push(Token::Text(" ".to_owned()));
    }

    fn raw_line(&mut self, text: Span<L>) {
        self.tokens.push(Token::RawLine(text.text.into()));
    }

    fn end(&mut self, scope: ScopeType) {
        self.tokens.push(Token::End(scope));
    }
}
