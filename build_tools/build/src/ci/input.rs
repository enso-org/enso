use crate::version::nightly::YdocVariant;
use crate::version::promote::Designation;

use ide_ci::actions::workflow::definition::WorkflowDispatchInput;
use strum::IntoEnumIterator;



pub mod name {

    pub const DESIGNATOR: &str = "designator";
    pub const YDOC: &str = "ydoc";
}

pub fn designator() -> WorkflowDispatchInput {
    WorkflowDispatchInput::new_choice(
        "What kind of release should be promoted.",
        true,
        Designation::iter().map(|d| d.as_ref().to_string()),
        None::<String>,
    )
    .unwrap()
}

pub fn ydoc() -> WorkflowDispatchInput {
    WorkflowDispatchInput::new_choice(
        "What kind of Ydoc image to build.",
        false,
        YdocVariant::iter().map(|v| v.as_ref().to_string()),
        Some(YdocVariant::Nodejs.as_ref()),
    )
    .unwrap()
}
