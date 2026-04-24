use crate::version::promote::Designation;

use ide_ci::actions::workflow::definition::WorkflowDispatchInput;
use strum::IntoEnumIterator;

pub mod name {
    pub const DESIGNATOR: &str = "designator";
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
