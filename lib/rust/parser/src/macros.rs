//! The macro system for the Enso parser.

pub mod definition;
pub mod literal;
pub mod registry;

use crate::prelude::*;
use crate::prelude::logger::*;

use crate::macros::definition::Definition;
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
        let registry      = Registry::from(macros);
        let logger        = <Logger>::sub(parent_logger,"Resolver");
        Self{registry,logger}
    }

    /// Define the macro described by `definition` in the macro resolver `self`.
    pub fn define_macro(&mut self, definition:Definition) {
        debug!(self.logger,"Define Macro: {&definition:?}.");
        self.registry.insert(definition)
    }
}
