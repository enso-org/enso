//! This module contains GLSL code builder. It allows translating complex vector shapes to the GLSL
//! code.

use crate::prelude::*;

use crate::display::shape::primitive::def::primitive;
use crate::display::shape::primitive::shader::overload;
use crate::display::symbol::shader::builder::CodeTemplate;
use super::canvas;
use super::canvas::Canvas;



// ===============
// === Builder ===
// ===============

// === GLSL Sources ===

const MATH            :&str = include_str!("../glsl/math.glsl");
const COLOR           :&str = include_str!("../glsl/color.glsl");
const DEBUG           :&str = include_str!("../glsl/debug.glsl");
const SHAPE           :&str = include_str!("../glsl/shape.glsl");
const FRAGMENT_RUNNER :&str = include_str!("../glsl/fragment_runner.glsl");


// === Definition ===

// TODO: Consider removing this struct and moving the utils to functions.
/// GLSL code builder.
#[derive(Clone,Copy,Debug)]
pub struct Builder {}

impl Builder {
    /// Returns the final GLSL code. If `pointer_events_enabled` is set to false, the generated
    /// shape will be transparent for pointer events and will pass them trough.
    pub fn run<S:canvas::Draw>(shape:&S, pointer_events_enabled:bool) -> CodeTemplate {
        let sdf_defs     = primitive::all_shapes_glsl_definitions();
        let mut canvas   = Canvas::default();
        let shape_ref    = shape.draw(&mut canvas);
        let defs_header  = header("SDF Primitives");
        let shape_header = header("Shape Definition");
        canvas.add_current_function_code_line(iformat!("return {shape_ref.getter()};"));
        canvas.submit_shape_constructor("run");
        let defs = iformat!("{defs_header}\n\n{sdf_defs}\n\n\n\n{shape_header}\n\n{canvas.to_glsl()}");

        let redirections = overload::builtin_redirections();
        let math         = overload::allow_overloading(MATH);
        let color        = overload::allow_overloading(COLOR);
        let debug        = overload::allow_overloading(DEBUG);
        let shape        = overload::allow_overloading(SHAPE);

        let defs = overload::allow_overloading(&defs);
        let code = format!("{}\n\n{}\n\n{}\n\n{}\n\n{}\n\n{}",redirections,math,color,debug,shape,defs);
        let main = format!("bool pointer_events_enabled = {};\n{}",pointer_events_enabled,FRAGMENT_RUNNER);

        CodeTemplate::new(code,main,"")
    }
}


// == Utils ===

/// Defines glsl comment being a pretty printed header of a code section.
fn header(label:&str) -> String {
    let border_len = label.len() + 8;
    let border     = "=".repeat(border_len);
    iformat!("// {border}\n// === {label} ===\n// {border}")
}
