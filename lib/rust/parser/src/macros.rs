//! The macro system for the Enso parser.

pub mod builder;
pub mod definition;
pub mod literal;
pub mod registry;

use crate::prelude::*;
use crate::prelude::logger::*;

use crate::macros::definition::Definition;
use crate::prelude::lexer::token;
use crate::macros::registry::Registry;



// ================
// === Resolver ===
// ================

/// The Enso macro resolver.
#[derive(Clone,Debug,PartialEq)]
pub struct Resolver<Logger> {
    /// The macro registry.
    registry : Registry,
    /// The logger for the macro resolver.
    logger : Logger
}

impl<Logger:AnyLogger<Owned=Logger>> Resolver<Logger> {
    /// Constructor.
    pub fn new(macros:Vec<Definition>, parent_logger:&Logger) -> Self {
        let registry = Registry::from(macros);
        let logger = <Logger>::sub(parent_logger,"Resolver");
        Self{logger,registry}
    }

    /// Define the macro described by `definition` in the macro resolver `self`.
    pub fn define_macro(&mut self, definition:Definition) {
        self.registry.insert(definition)
    }

    /// Execute the macro resolver, returning the chunked token stream.
    pub fn run(&mut self, _tokens:token::Stream) -> Vec<Chunk> {
        trace!(self.logger,"Executing macro resolver.");
        unimplemented!()
    }
}



// =============
// === Chunk ===
// =============

/// A temporary structure used for writing the results of the macro resolver prior to clarifying the
/// design for the new AST.
#[derive(Clone,Debug,Default,PartialEq)]
pub struct Chunk {
    tokens : token::Stream,
}


// === Trait Impls ===

impl From<token::Stream> for Chunk {
    fn from(tokens:token::Stream) -> Self {
        Chunk{tokens}
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
#[allow(dead_code)]
mod tests {
    use super::*;
    use enso_logger::enabled::Logger;
    use crate::macros::definition::Section;
    use crate::macros::literal::Literal;

    // === Utilities ===

    /// A default set of macros for the tests.
    fn default_macros() -> Vec<Definition> {
        vec![
            Definition::new("if_then",vec![
                Section::new(Literal::variable("if")),
                Section::new(Literal::variable("then")),
            ])
        ]
    }

    /// Parse the input code.
    fn parse(code:impl Str, macros:Vec<Definition>) -> Vec<Chunk> {
        let logger       = Logger::new("Enso.Macro");
        let mut resolver = Resolver::new(macros,&logger);
        let tokens       = lexer::run(code.into().as_str()).tokens;
        resolver.run(tokens)
    }


    // === The Tests ===

    // #[test]
    // fn test_run() {
    //     let input  = "if a then if b then c else d else e";
    //     let result = parse(input,default_macros());
    //     assert!(!result.is_empty())
    // }
}
