//! This module contains GLSL code builder. It allows translating complex vector shapes to the GLSL
//! code.

use crate::prelude::*;

use crate::display::shape::cached::CACHED_TEXTURE_MAX_DISTANCE;
use crate::display::shape::primitive::def::primitive;
use crate::display::shape::primitive::glsl::codes;
use crate::display::shape::primitive::shader::overload;
use crate::display::symbol::shader::builder::CodeTemplate;

use super::canvas;
use super::canvas::Canvas;



// ===============
// === Builder ===
// ===============

// === GLSL Sources ===

/// Common GLSL functions for all sprite types.
pub const GLSL_PRELUDE: &str = include_str!("../glsl/prelude.glsl");
const MATH: &str = include_str!("../glsl/math.glsl");
const COLOR: &str = include_str!("../glsl/color.glsl");
const DEBUG: &str = include_str!("../glsl/debug.glsl");
const SHAPE: &str = include_str!("../glsl/shape.glsl");
const FRAGMENT_RUNNER: &str = include_str!("../glsl/fragment_runner.glsl");


// === Definition ===

// TODO: Consider removing this struct and moving the utils to functions.
/// GLSL code builder.
#[derive(Clone, Copy, Debug)]
pub struct Builder {}

impl Builder {
    /// Returns the final GLSL code. If `pointer_events_enabled` is set to false, the generated
    /// shape will be transparent for pointer events and will pass them trough.
    #[profile(Detail)]
    pub fn run<S: canvas::Draw>(shape: &S, pointer_events_enabled: bool) -> CodeTemplate {
        let mut canvas = Canvas::default();
        let shape_ref = shape.draw(&mut canvas);
        let shape_header = header("Shape Definition");
        canvas.add_current_function_code_line(format!("return {};", shape_ref.getter()));
        canvas.submit_shape_constructor("run");
        let shape_def = overload::allow_overloading(&canvas.to_glsl());
        let code = [GLSL_BOILERPLATE.as_str(), "", &shape_header, &shape_def].join("\n\n");
        let main =
            format!("bool pointer_events_enabled = {pointer_events_enabled};\n{FRAGMENT_RUNNER}");

        CodeTemplate::new(code, main, "")
    }
}


// == Utils ===

/// Defines glsl comment being a pretty printed header of a code section.
fn header(label: &str) -> String {
    let border_len = label.len() + 8;
    let border = "=".repeat(border_len);
    format!("// {border}\n// === {label} ===\n// {border}")
}


// == GLSL Boilerplate ==

lazy_static! {
    /// A common preamble used to start every shader program.
    static ref GLSL_BOILERPLATE: String = gen_glsl_boilerplate();
}

fn glsl_codes() -> String {
    let codes = codes::DisplayModes::all();
    let header = header("Codes");
    let display_modes = codes
        .iter()
        .map(|code| format!("const int {} = {};", code.name().to_uppercase(), code.value()))
        .join("\n");
    let error_codes =
        format!("const int ID_ENCODING_OVERFLOW_ERROR = {};", codes::ID_ENCODING_OVERFLOW_ERROR);
    format!("{header}\n\n{display_modes}\n{error_codes}")
}

fn glsl_constants() -> String {
    let codes = glsl_codes();
    format!("{codes}\n\nconst float CACHED_SHAPE_MAX_DISTANCE = {CACHED_TEXTURE_MAX_DISTANCE:?};")
}

/// The GLSL common code and shared constants (including debug codes).
pub fn glsl_prelude_and_constants() -> String {
    let constants = glsl_constants();
    format!("{GLSL_PRELUDE}\n\n{constants}")
}

fn gen_glsl_boilerplate() -> String {
    let redirections = overload::builtin_redirections();
    let math = overload::allow_overloading(MATH);
    let color = overload::allow_overloading(COLOR);
    let debug = overload::allow_overloading(DEBUG);
    let shape = overload::allow_overloading(SHAPE);
    let codes_and_prelude = glsl_prelude_and_constants();
    let defs_header = header("SDF Primitives");
    let sdf_defs = overload::allow_overloading(&primitive::all_shapes_glsl_definitions());
    [
        redirections.as_str(),
        codes_and_prelude.as_str(),
        math.as_str(),
        color.as_str(),
        debug.as_str(),
        shape.as_str(),
        defs_header.as_str(),
        sdf_defs.as_str(),
    ]
    .join("\n\n")
}
