//! UI components that allows picking a number or range through mouse interaction. We have a number
//! picker that allows to pick a number in a range and a range picker that allows to pick a range.
//!
//! Both share the same model (i.e., arrangement of shapes), but they have different logic about
//! using the features of the shapes making up the model and handling interactions. The base model
//! is implemented in `model.rs` and the logic for the number and range picker are placed in
//! `number.rs` and `range.rs` respectively. The rest of the sub-modules contain utilities.
//!
//! The only things exposed form this module are the `NumberPicker`, `NumberRangePicker`, their
//! FRPs and the `Bounds` struct.

pub(crate) mod bounds;
pub(crate) mod model;

mod decimal_aligned;
mod frp;
mod number;
mod range;
mod shape;

use ensogl_core::application;
use ensogl_core::application::Application;

pub use bounds::Bounds;
pub(crate) use frp::*;
use model::*;



// =====================
// === Number Picker ===
// =====================

/// UI component for selecting a number. Looks like a rounded node that has the selected value as
/// text representation in the center. The background shows the selected value visually in the
/// range that the value can be picked from (e.g., 0..255) by by filling in the proportion of the
/// background that corresponds to the value relative in the range, for example, 0.0 would be not
/// filled in, 128.0 would be about halfway filled in, and 128.0 would be completely filled in.
/// The value can be changed by clicking and dragging on the shape.
pub type NumberPicker = crate::component::Component<Model, number::Frp>;

impl application::View for NumberPicker {
    fn label() -> &'static str {
        "NumberPicker"
    }
    fn new(app: &Application) -> Self {
        NumberPicker::new(app)
    }
    fn app(&self) -> &Application {
        &self.app
    }
}



// ===========================
// === Number Range Picker ===
// ===========================

/// UI component for selecting a ranger. Looks like a rounded node that shows a textual
/// representation of the range bounds at the left and right end of the shape. The background shows
/// the selected value visually in the by displaying a track within the range of permissible values
/// that the range bounds can be selected from. For example, if the allowed range is 0.0..1.0
/// a selected range of 0..0.5 would show the track covering the left half of the background, a
/// selected range of 0.25..0.75 would show the track centered on the background, and 0.5..1.0
/// would show the track covering the right half of the background. The selected range can be
/// changed by clicking and dragging the track, which changes the whole range, but preserves the
/// width, or the individual edges of the track which changes just the respective end of the range.
pub type NumberRangePicker = crate::component::Component<Model, range::Frp>;

impl application::View for NumberRangePicker {
    fn label() -> &'static str {
        "RangePicker"
    }
    fn new(app: &Application) -> Self {
        NumberRangePicker::new(app)
    }
    fn app(&self) -> &Application {
        &self.app
    }
}
