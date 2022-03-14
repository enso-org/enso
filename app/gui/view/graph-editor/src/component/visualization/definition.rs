//! Visualization definition abstraction.

use crate::data::*;
use crate::prelude::*;

use crate::visualization;

use ensogl::display::Scene;
use std::fmt::Formatter;
use visualization::java_script;



// =================
// === Signature ===
// =================

/// General information about a visualization.
#[derive(Clone, CloneRef, Debug, Eq, Hash, PartialEq, Shrinkwrap)]
#[allow(missing_docs)]
pub struct Signature {
    #[shrinkwrap(main_field)]
    pub path:         visualization::Path,
    pub input_type:   enso::Type,
    pub input_format: Rc<visualization::data::Format>,
}

impl Signature {
    /// Constructor.
    pub fn new(
        path: impl Into<visualization::Path>,
        input_type: impl Into<enso::Type>,
        input_format: impl Into<visualization::data::Format>,
    ) -> Self {
        let path = path.into();
        let input_type = input_type.into();
        let input_format = input_format.into();
        let input_format = Rc::new(input_format);

        Self { path, input_type, input_format }
    }

    /// Constructor of signature valid for any Enso type.
    pub fn new_for_any_type(
        path: impl Into<visualization::Path>,
        input_format: impl Into<visualization::data::Format>,
    ) -> Self {
        let input_type = enso::Type::any();
        Self::new(path, input_type, input_format)
    }
}



// ==================
// === Definition ===
// ==================

/// Generic definition of a visualization. Provides information about the visualization `Signature`,
/// and a way to create new instances.
#[derive(Clone, CloneRef, Derivative)]
#[derivative(Debug)]
#[allow(missing_docs)]
pub struct Definition {
    pub signature:   Signature,
    #[derivative(Debug = "ignore")]
    pub constructor: Rc<dyn Fn(&Scene) -> InstantiationResult>,
}

impl Definition {
    /// Constructor.
    pub fn new<F>(signature: impl Into<Signature>, constructor: F) -> Self
    where F: 'static + Fn(&Scene) -> InstantiationResult {
        let signature = signature.into();
        let constructor = Rc::new(constructor);
        Self { signature, constructor }
    }

    /// Creates a new instance of the visualization.
    pub fn new_instance(&self, scene: &Scene) -> InstantiationResult {
        (self.constructor)(scene)
    }

    /// Get the path identifying this visualization definition.
    pub fn path(&self) -> visualization::Path {
        self.signature.path.clone_ref()
    }
}


// === Result ===

/// Result of the attempt to instantiate a `Visualization` from a `Definition`.
pub type InstantiationResult = Result<visualization::Instance, InstantiationError>;


// === Errors ===

/// Indicates that instantiating a `Visualisation` from a `Definition` has failed.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum InstantiationError {
    ConstructorError(java_script::instance::Error),
}

impl Display for InstantiationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InstantiationError::ConstructorError(value) => f.write_fmt(format_args!(
                "Could not construct visualisation because of error: {:?}",
                value
            )),
        }
    }
}
