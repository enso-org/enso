//! Utilities to create consistent shadows for UI components.
use crate::prelude::*;

use ensogl_core::data::color;
use ensogl_core::display::DomSymbol;
use ensogl_core::display::shape::*;
use ensogl_core::display::shape::AnyShape;
use ensogl_core::system::web::StyleSetter;
use ensogl_theme as theme;



/// Return a shadow for the given shape. Exact appearance will depends on the theme parameters.
pub fn from_shape(base_shape:AnyShape, style:&StyleWatch) -> AnyShape {
    let alpha         = Var::<f32>::from(1.0);
    from_shape_with_alpha(base_shape,&alpha,style)
}

/// Return a shadow for the given shape. Exact appearance will depends on the theme parameters.
/// The color will be multiplied with the given alpha value, which is useful for fade-in/out
/// animations.
pub fn from_shape_with_alpha(base_shape:AnyShape,alpha:&Var<f32>,style:&StyleWatch) -> AnyShape {
    let shadow_size   = style.get_number(theme::shadow::size);
    let shadow_spread = style.get_number(theme::shadow::spread);
    let shadow_off_x  = style.get_number(theme::shadow::offset_x).px();
    let shadow_off_y  = style.get_number(theme::shadow::offset_y).px();

    let shadow_grow   = Var::<f32>::from(shadow_size);
    let shadow        = base_shape.grow(shadow_grow);
    let shadow        = shadow.translate((shadow_off_x,shadow_off_y));

    let base_color    = style.get_color(theme::shadow);
    let base_color    = Var::<color::Rgba>::from(base_color);
    let base_color    = base_color.multiply_alpha(&alpha);

    let fading_color  = style.get_color(theme::shadow::fading);
    let fading_color  = Var::<color::Rgba>::from(fading_color);
    let fading_color  = fading_color.multiply_alpha(&alpha);

    let exp           = style.get_number(theme::shadow::exponent);

    let shadow_color  = color::gradient::Linear::<Var<color::LinearRgba>>
    ::new(fading_color.into_linear(),base_color.into_linear());
    let shadow_color  = shadow_color.sdf_sampler().size(shadow_size)
        .spread(shadow_spread).exponent(exp);
    let shadow        = shadow.fill(shadow_color);
    shadow.into()
}

/// Add a theme defined box shadow to the given `DomSymbol`.
pub fn add_to_dom_element(element:&DomSymbol, style:&StyleWatch,logger:&Logger) {
    let off_x  = style.get_number(theme::shadow::offset_x);
    let off_y  = -style.get_number(theme::shadow::offset_y);
    let alpha  = style.get_number(ensogl_theme::shadow::html::alpha);
    let blur   = style.get_number(ensogl_theme::shadow::html::blur);
    let spread = style.get_number(ensogl_theme::shadow::html::spread);
    let shadow = format!("{}px {}px {}px {}px rgba(0,0,0,{})",off_x,off_y,blur,spread,alpha);
    element.dom().set_style_or_warn("box-shadow",shadow,&logger);
}
