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

#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]


// ==============
// === Export ===
// ==============

pub mod bounds;
pub mod model;



mod decimal_aligned;
mod frp;
mod number;
mod range;
mod shape;

/// Commonly used types and functions.
pub mod prelude {
    pub use ensogl_core::prelude::*;
}

use prelude::*;

use crate::model::*;

use ensogl_core::application;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::display::shape::StyleWatchFrp;

pub use crate::bounds::Bounds;
pub use crate::frp::*;



// =====================
// === Number Picker ===
// =====================

/// UI component for selecting a number. Looks like a rounded node that has the selected value as
/// text representation in the center. The background shows the selected value visually in the
/// range that the value can be picked from (e.g., 0..255) by by filling in the proportion of the
/// background that corresponds to the value relative in the range, for example, 0.0 would be not
/// filled in, 128.0 would be about halfway filled in, and 128.0 would be completely filled in.
/// The value can be changed by clicking and dragging on the shape.
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct NumberPicker {
    /// Public FRP api of the Component.
    pub frp: Rc<number::Frp>,
    #[display_object]
    model:   Rc<Model>,
}

impl NumberPicker {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
        let frp = number::Frp::default();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        frp.init(app, &model, &style);
        let frp = Rc::new(frp);
        Self { frp, model }
    }
}

impl Deref for NumberPicker {
    type Target = number::Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl FrpNetworkProvider for NumberPicker {
    fn network(&self) -> &enso_frp::Network {
        self.frp.network()
    }
}

impl application::View for NumberPicker {
    fn label() -> &'static str {
        "NumberPicker"
    }

    fn new(app: &Application) -> Self {
        NumberPicker::new(app)
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
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct NumberRangePicker {
    /// Public FRP api of the Component.
    pub frp: Rc<range::Frp>,
    #[display_object]
    model:   Rc<Model>,
}

impl NumberRangePicker {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let model = Rc::new(Model::new(app));
        let frp = range::Frp::default();
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        frp.init(app, &model, &style);
        let frp = Rc::new(frp);
        Self { frp, model }
    }
}

impl Deref for NumberRangePicker {
    type Target = range::Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl FrpNetworkProvider for NumberRangePicker {
    fn network(&self) -> &enso_frp::Network {
        self.frp.network()
    }
}

impl application::View for NumberRangePicker {
    fn label() -> &'static str {
        "RangePicker"
    }

    fn new(app: &Application) -> Self {
        NumberRangePicker::new(app)
    }
}
