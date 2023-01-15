//! Utilities to create consistent shadows for UI components.

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

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;
use ensogl_core::system::web::traits::*;

use ensogl_core::data::color;
use ensogl_core::display::shape::AnyShape;
use ensogl_core::display::style;
use ensogl_core::display::DomSymbol;
use ensogl_core::frp;
use ensogl_hardcoded_theme as theme;



/// Defines the appearance of a shadow
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub struct Parameters {
    pub base_color: Var<color::Rgba>,
    pub fading:     Var<color::Rgba>,
    pub size:       Var<f32>,
    pub spread:     Var<f32>,
    pub exponent:   Var<f32>,
    pub offset_x:   Var<f32>,
    pub offset_y:   Var<f32>,
}

/// Loads shadow parameters from the given style, at the given path. The structure of the style
/// definition should be analogous to that at `ensogl_hardcoded_theme::shadow`.
pub fn parameters_from_style_path(style: &StyleWatch, path: impl Into<style::Path>) -> Parameters {
    let path: style::Path = path.into();
    Parameters {
        base_color: style.get_color(&path).into(),
        fading:     style.get_color(path.sub("fading")).into(),
        size:       style.get_number(path.sub("size")).into(),
        spread:     style.get_number(path.sub("spread")).into(),
        exponent:   style.get_number(path.sub("exponent")).into(),
        offset_x:   style.get_number(path.sub("offset_x")).into(),
        offset_y:   style.get_number(path.sub("offset_y")).into(),
    }
}

/// Utility method to retrieve the size of the shadow. can be used to determine shape padding etc.
pub fn size(style: &StyleWatch) -> Var<f32> {
    let parameters = parameters_from_style_path(style, theme::shadow);
    parameters.size
}

/// Return a shadow for the given shape. Exact appearance will depend on the theme parameters.
pub fn from_shape(base_shape: AnyShape, style: &StyleWatch) -> AnyShape {
    let alpha = Var::<f32>::from(1.0);
    from_shape_with_alpha(base_shape, &alpha, style)
}

/// Return a shadow for the given shape. Exact appearance will depend on the theme parameters.
/// The color will be multiplied with the given alpha value, which is useful for fade-in/out
/// animations.
pub fn from_shape_with_alpha(
    base_shape: AnyShape,
    alpha: &Var<f32>,
    style: &StyleWatch,
) -> AnyShape {
    let parameters = parameters_from_style_path(style, theme::shadow);
    from_shape_with_parameters_and_alpha(base_shape, parameters, alpha)
}

/// Return a shadow for the given shape and shadow parameters.
pub fn from_shape_with_parameters(base_shape: AnyShape, parameters: Parameters) -> AnyShape {
    let alpha = Var::<f32>::from(1.0);
    from_shape_with_parameters_and_alpha(base_shape, parameters, &alpha)
}

/// Return a shadow for the given shape, shadow parameters and alpha. As in `from_shape_with_alpha`,
/// the shadow colors will be multiplied with the given alpha value.
pub fn from_shape_with_parameters_and_alpha(
    base_shape: AnyShape,
    parameters: Parameters,
    alpha: &Var<f32>,
) -> AnyShape {
    let grow = parameters.size.clone();
    let shadow = base_shape.grow(grow);
    let shadow = shadow.translate((parameters.offset_x.px(), parameters.offset_y.px()));

    let base_color = parameters.base_color.multiply_alpha(alpha);

    let fading_color = parameters.fading.multiply_alpha(alpha);

    let shadow_color = color::gradient::Linear::<Var<color::LinearRgba>>::new(
        fading_color.into_linear(),
        base_color.into_linear(),
    );
    let shadow_color = shadow_color
        .sdf_sampler()
        .size(parameters.size)
        .spread(parameters.spread)
        .exponent(parameters.exponent);
    let shadow = shadow.fill(shadow_color);
    shadow.into()
}

/// Add a theme defined box shadow to the given `DomSymbol`.
pub fn add_to_dom_element(element: &DomSymbol, style: &StyleWatch) {
    let off_x = style.get_number(theme::shadow::offset_x);
    let off_y = -style.get_number(theme::shadow::offset_y);
    let alpha = style.get_number(ensogl_hardcoded_theme::shadow::html::alpha);
    let blur = style.get_number(ensogl_hardcoded_theme::shadow::html::blur);
    let spread = style.get_number(ensogl_hardcoded_theme::shadow::html::spread);
    let shadow = format!("{off_x}px {off_y}px {blur}px {spread}px rgba(0,0,0,{alpha})");
    element.dom().set_style_or_warn("box-shadow", shadow);
}


/// Provides FRP endpoints for the parameters that define the appearance of a shadow
#[derive(Debug, Clone, CloneRef)]
#[allow(missing_docs)]
pub struct ParametersFrp {
    pub base_color: frp::Sampler<color::Rgba>,
    pub fading:     frp::Sampler<color::Rgba>,
    pub size:       frp::Sampler<f32>,
    pub spread:     frp::Sampler<f32>,
    pub exponent:   frp::Sampler<f32>,
    pub offset_x:   frp::Sampler<f32>,
    pub offset_y:   frp::Sampler<f32>,
}

/// Return FRP endpoints for the parameters that define a shadow.
pub fn frp_from_style(style: &StyleWatchFrp, path: impl Into<style::Path>) -> ParametersFrp {
    let path: style::Path = path.into();
    ParametersFrp {
        base_color: style.get_color(&path),
        fading:     style.get_color(path.sub("fading")),
        size:       style.get_number(path.sub("size")),
        spread:     style.get_number(path.sub("spread")),
        exponent:   style.get_number(path.sub("exponent")),
        offset_x:   style.get_number(path.sub("offset_x")),
        offset_y:   style.get_number(path.sub("offset_y")),
    }
}
