//! The macro system for the Enso parser.

pub mod r#match;
pub mod definition;
pub mod literal;
pub mod registry;

use crate::prelude::*;
use crate::prelude::logger::*;

use crate::macros::definition::Definition;
use crate::prelude::lexer::token;
use crate::macros::registry::Registry;
use crate::macros::r#match::{Match, Ast};
use crate::macros::literal::Literal;
use crate::prelude::lexer::token::Token;


// ================
// === Resolver ===
// ================

/// The Enso macro resolver.
#[derive(Clone,Debug,PartialEq)]
pub struct Resolver<Logger> {
    /// The macro registry.
    registry : Registry,
    // /// The stack of pending macro matches.
    // builder_stack : Vec<Match>,
    // /// Whether or not the resolver's process is at the start of the line.
    // at_line_start : bool,
    /// The logger for the macro resolver.
    logger : Logger
}

impl<Logger:AnyLogger<Owned=Logger>> Resolver<Logger> {
    /// Constructor.
    pub fn new(macros:Vec<Definition>, parent_logger:&Logger) -> Self {
        let registry      = Registry::from(macros);
        // let builder_stack = default();
        // let at_line_start = true;
        let logger        = <Logger>::sub(parent_logger,"Resolver");
        Self{registry/*,builder_stack,at_line_start*/,logger}
    }

    /// Define the macro described by `definition` in the macro resolver `self`.
    pub fn define_macro(&mut self, definition:Definition) {
        debug!(self.logger,"Define Macro: {&definition:?}.");
        self.registry.insert(definition)
    }

    /// Get a reference to the current builder.
    pub fn current_builder<'a>(&self, builder_stack:&'a Vec<Match>) -> Option<&'a Match> {
        builder_stack.last()
    }

    /// Get a mutable reference to the current builder.
    pub fn current_builder_mut<'a>(&self, builder_stack:&'a mut Vec<Match>) -> Option<&'a mut Match> {
        builder_stack.last_mut()
    }

    /// Push `builder` onto the builder stack.
    pub fn push_builder(&self, builder_stack:&mut Vec<Match>, builder:Match) {
        debug!(self.logger,"Push Builder onto Stack: {&builder:?}.");
        builder_stack.push(builder)
    }

    /// Pop the top builder off the stack.
    pub fn pop_builder(&self, builder_stack:&mut Vec<Match>) -> Option<Match> {
        let builder = builder_stack.pop();
        debug!(self.logger,"Pop Builder from Stack: {&builder:?}.");
        builder
    }

    /// Push `tree` onto the tree stack.
    pub fn push_tree<'a>(&self, tree_stack:&mut Vec<&'a registry::Tree>, tree:&'a registry::Tree) {
        debug!(self.logger,"Push Tree onto Stack: {tree:?}.");
        tree_stack.push(tree);
    }

    /// Pop the top tree off the stack.
    pub fn pop_tree<'a>(&self, tree_stack:&mut Vec<&'a registry::Tree>) -> Option<&'a registry::Tree> {
        let tree = tree_stack.pop();
        debug!(self.logger,"Pop Tree from Stack: {tree:?}.");
        tree
    }

    // /// Execute the macro resolver, returning the chunked token stream.
    // pub fn run(&self, tokens:token::Stream) -> Vec<Match> {
    //     trace!(self.logger,"Executing macro resolver.");
    //     // let mut at_line_start = true;
    //     let mut builder_stack = Vec::new();
    //     let mut tree_stack    = Vec::new();
    //     tree_stack.push(self.registry.root());
    //     builder_stack.push(Match::top_level());
    //
    //     for token in tokens.into_iter() {
    //         debug!(self.logger,"At token: {&token:?}");
    //         let offset = token.offset;
    //         let length = token.length;
    //         match Literal::try_from(token.shape) {
    //             Ok(lit) => {
    //                 let tree = self.find_tree(&lit,&tree_stack);
    //                 match tree {
    //                     Some((t,cont)) => {
    //                         trace!(self.logger,"Tree found for literal {&lit:?}.");
    //                         if cont == ContinueMacro::Yes {
    //                             trace!(self.logger,"Continuing to build macro.");
    //                             self.pop_tree(&mut tree_stack);
    //                             let current_builder = builder_stack.last_mut().expect("Must be present");
    //                             current_builder.add_segment(offset,lit);
    //                         } else {
    //                             trace!(self.logger,"Beginning new macro.");
    //                             let mut builder = Match::default();
    //                             builder.add_segment(offset,lit);
    //                             self.push_builder(&mut builder_stack,builder);
    //                         }
    //                         if let Some(def) = t.get_value(&[]) {
    //                             let current_builder = builder_stack.last_mut().expect("Must be present");
    //                             current_builder.set_definition(def.clone());
    //                         }
    //                         self.push_tree(&mut tree_stack,t);
    //                     },
    //                     None => {
    //                         trace!(self.logger,"No tree found for literal {&lit:?}.");
    //                         let current_builder = builder_stack.last_mut().expect("Must be present");
    //                         let token           = Ast::Token(Token::new(lit.into(),length,offset));
    //                         current_builder.add_token(token);
    //                     }
    //                 }
    //             },
    //             Err(shape) => {
    //                 match shape {
    //                     token::Shape::Block{..} => todo!("Blocks not implemented yet."),
    //                     _                       => todo!("Accumulate"),
    //                 }
    //             }
    //         }
    //     }
    //     builder_stack
    // }

    /// Execute the macro resolver, returning the chunked token stream.
    pub fn run(&self, tokens:token::Stream) -> Vec<Match> {
        trace!(self.logger,"Executing macro resolver.");
        // let mut at_line_start = true;
        let mut builder_stack = Vec::new();
        let mut tree_stack    = Vec::new();
        tree_stack.push(self.registry.root());
        builder_stack.push(Match::top_level());

        for token in tokens.into_iter() {
            debug!(self.logger,"At token: {&token:?}");
            let offset = token.offset;
            let length = token.length;
        }
        builder_stack
    }

    /// Find a tree for `literal` in the currently available trees.
    fn find_tree<'a>
    ( &self
      , literal : &Literal
      , trees   : &Vec<&'a registry::Tree>
    ) -> Option<(&'a registry::Tree,ContinueMacro)> {
        for (ix, tree) in trees.iter().rev().enumerate() {
            let result = tree.get_at_current_level(&literal);
            if result.is_some() {
                let continue_macro = if ix == 0 && trees.len() > 1 {
                    ContinueMacro::Yes
                } else { ContinueMacro::No };
                return result.map(|t| (t,continue_macro))
            }
        }
        None
    }
}

/// Whether or not the match is a continuation of the macro.
#[derive(Copy,Clone,Debug,Eq,PartialEq)]
enum ContinueMacro {
    Yes,
    No
}


/*
if a then if b then c else d

[root]
:< if
[then...,root]
:< a
:< then
[else...,root]
:< if
[then...,else...,root]
:< b
:< then
[else..,else..,root]
:< c
:< else
[else..,root]
:< d
:<<<
if a then (if b then c else d)
 */



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
            Definition::new("if_then_else",vec![
                Section::new(Literal::variable("if")),
                Section::new(Literal::variable("then")),
                Section::new(Literal::variable("else")),
            ]),
            Definition::new("if_then",vec![
                Section::new(Literal::variable("if")),
                Section::new(Literal::variable("then")),
            ])
        ]
    }

    /// Parse the input code.
    fn parse(code:impl Str, macros:Vec<Definition>) -> Vec<Match> {
        let logger   = Logger::new("Enso.Macro");
        let resolver = Resolver::new(macros,&logger);
        let tokens   = lexer::run(code.into().as_str()).tokens;
        resolver.run(tokens)
    }


    // === The Tests ===

    #[test]
    fn test_run() {
        // let input  = "if a then if b then c else d else e";
        let input  = "if a then b else c";
        let result = parse(input,default_macros());
        assert!(!result.is_empty())
    }
}
