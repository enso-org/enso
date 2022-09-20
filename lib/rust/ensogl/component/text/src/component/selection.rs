//! Provides a text cursor and selection component.

use crate::prelude::*;
use ensogl_core::display::shape::*;

use enso_frp as frp;
use ensogl_core::application::command::FrpNetworkProvider;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::system::gpu::shader::glsl::traits::IntoGlsl;
use ensogl_core::Animation;
use ensogl_core::DEPRECATED_Animation;
// FIXME: circular dep?
use crate::font::glyph::WeakGlyph;

const DEBUG_SLOWDOWN: bool = true;


// ==============
// === Cursor ===
// ==============

const CURSOR_PADDING: f32 = 4.0;
const CURSOR_WIDTH: f32 = 2.0;
const CURSOR_ALPHA: f32 = 0.8;
const CURSORS_SPACING: f32 = 1.0;
const SELECTION_ALPHA: f32 = 0.3;
const SELECTION_CORNER_RADIUS: f32 = 2.0;
const BLINK_SLOPE_IN_DURATION: f32 = 200.0;
const BLINK_SLOPE_OUT_DURATION: f32 = 200.0;
const BLINK_ON_DURATION: f32 = 300.0;
const BLINK_OFF_DURATION: f32 = 300.0;
const BLINK_PERIOD: f32 =
    BLINK_SLOPE_IN_DURATION + BLINK_SLOPE_OUT_DURATION + BLINK_ON_DURATION + BLINK_OFF_DURATION;



/// Text cursor and selection shape definition. If the shape is narrow, it is considered a cursor,
/// and thus, it blinks.
///
/// ## Blinking Implementation
///
/// The blinking alpha is a time-dependent function which starts as a fully opaque value and
/// changes periodically. The `start_time` parameter is set to the current time after each cursor
/// operation, which makes cursor visible during typing and after position change.
///
/// ```compile_fail
/// |
/// |    on         off
/// | <------>   <------->
/// | --------.             .--------.             .-...
/// |          \           /          \           /
/// |           '---------'            '---------'
/// |         <->         <->
/// |      slope_out   slope_in
/// |                                              time
/// |-------------------------------------------------->
/// start time
/// ```
pub mod shape {
    use super::*;

    ensogl_core::define_shape_system! {
        pointer_events = false;
        (style:Style, selection:f32, start_time:f32, letter_width:f32, color_rgb:Vector3<f32>) {
            let width_abs      = Var::<f32>::from("abs(input_size.x)");
            let height         = Var::<f32>::from("input_size.y");
            let rect_width     = width_abs - 2.0 * CURSOR_PADDING;
            let rect_height    = height    - 2.0 * CURSOR_PADDING;
            let time           = Var::<f32>::from("input_time");
            let one            = Var::<f32>::from(1.0);
            let time           = time - start_time;
            let on_time        = BLINK_ON_DURATION + BLINK_SLOPE_OUT_DURATION;
            let off_time       = on_time + BLINK_OFF_DURATION;
            let sampler        = time % BLINK_PERIOD;
            let slope_out      = sampler.smoothstep(BLINK_ON_DURATION,on_time);
            let slope_in       = sampler.smoothstep(off_time,BLINK_PERIOD);
            let blinking_alpha = (one - slope_out + slope_in) * CURSOR_ALPHA;
            let sel_width      = &rect_width - CURSOR_WIDTH;
            let alpha_weight   = sel_width.smoothstep(0.0,letter_width);
            let alpha          = alpha_weight.mix(blinking_alpha,SELECTION_ALPHA);
            let shape          = Rect((1.px() * rect_width,1.px() * rect_height));
            let shape          = shape.corners_radius(SELECTION_CORNER_RADIUS.px());
            let color          = format!("srgba({}.x,{}.y,{}.z,{})",color_rgb,color_rgb
                ,color_rgb,alpha.glsl());
            let shape          = shape.fill(color);
            shape.into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_color (color::Rgb),
        set_ascender (f32),
        set_descender (f32),
        set_attached_glyphs (Rc<Vec<WeakGlyph>>),
    }

    Output {
        right_side_of_last_attached_glyph (f32),
    }
}



// =================
// === Selection ===
// =================

/// Visual representation of text cursor and text selection.
///
/// ## Implementation Notes
/// Selection contains a `right_side` display object which is always placed on its right side. It is
/// used for smooth glyph animation. For example, after several glyphs were selected and removed,
/// the selection will gradually shrink. Making all following glyphs children of the `right_side`
/// object will make the following glyphs  animate while the selection is shrinking.
#[derive(Clone, CloneRef)]
pub struct Selection {
    pub frp:        Frp,
    display_object: display::object::Instance,
    pub right_side: display::object::Instance,
    shape_view:     shape::View,
    pub position:   DEPRECATED_Animation<Vector2>,
    pub width:      DEPRECATED_Animation<f32>,
    pub ascender:   Animation<f32>,
    pub descender:  Animation<f32>,
    pub edit_mode:  Rc<Cell<bool>>,
}

impl Debug for Selection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Selection")
    }
}

impl Deref for Selection {
    type Target = shape::View;
    fn deref(&self) -> &Self::Target {
        &self.shape_view
    }
}

impl Selection {
    /// Constructor.
    pub fn new(edit_mode: bool) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let display_object = display::object::Instance::new();
        let right_side = display::object::Instance::new();
        let shape_view = shape::View::new();
        let position = DEPRECATED_Animation::new(&network);
        let width = DEPRECATED_Animation::new(&network);
        let ascender = Animation::new(&network);
        let descender = Animation::new(&network);
        let edit_mode = Rc::new(Cell::new(edit_mode));

        let spring_factor = if DEBUG_SLOWDOWN { 0.1 } else { 1.0 };

        position.update_spring(|spring| spring * spring_factor);
        width.update_spring(|spring| spring * spring_factor);

        frp::extend! { network
            ascender.target <+ frp.set_ascender;
            descender.target <+ frp.set_descender;
            _eval <- all_with(&ascender.value, &descender.value,
                f!([shape_view](ascender,descender) {
                    let height = ascender - descender;
                    shape_view.set_position_y(height / 2.0 + descender);
                    shape_view.size.modify(|t| Vector2(t.x, CURSOR_PADDING * 2.0 + height));
                })
            );

            line_width_can_change <- any_(&frp.set_attached_glyphs, &position.value);
            rhs_last_glyph <- frp.set_attached_glyphs.sample(&line_width_can_change).map(f!([display_object, right_side](glyphs) {
                if let Some(glyph) = glyphs.last().and_then(|glyph| glyph.upgrade()) {
                    let glyph_right_x = glyph.position().x + glyph.width();
                    let origin_x = display_object.position().x + right_side.position().x;
                    origin_x + glyph_right_x
                } else {
                    0.0
                }
            }));
            frp.private.output.right_side_of_last_attached_glyph <+ rhs_last_glyph.on_change();
        }

        Self {
            display_object,
            right_side,
            shape_view,
            position,
            width,
            ascender,
            descender,
            edit_mode,
            frp,
        }
        .init()
    }

    fn init(self) -> Self {
        let network = self.frp.network();
        let view = &self.shape_view;
        let object = &self.display_object;
        let right_side = &self.right_side;
        let shape_view = &self.shape_view;
        self.add_child(view);
        self.add_child(right_side);
        frp::extend! { network
            _eval <- all_with(&self.position.value,&self.width.value,
                f!([view,object,right_side](p,width){
                    let side       = width.signum();
                    let abs_width  = width.abs();
                    let width      = max(CURSOR_WIDTH, abs_width - CURSORS_SPACING);
                    let view_width = CURSOR_PADDING * 2.0 + width;
                    let view_x     = (abs_width/2.0) * side;
                    object.set_position_xy(*p);
                    right_side.set_position_x(abs_width/2.0);
                    view.size.modify(|t| Vector2(view_width,t.y));
                    view.set_position_x(view_x);
                })
            );
            eval self.frp.set_color((color) shape_view.color_rgb.set(color.into()));
        }
        self
    }

    pub fn flip_sides(&self) {
        let width = self.width.target_value();
        self.position.set_value(self.position.value() + Vector2(width, 0.0));
        self.position.set_target_value(self.position.target_value() + Vector2(width, 0.0));

        self.width.set_value(-self.width.value());
        self.width.set_target_value(-self.width.target_value());
    }
}

impl display::Object for Selection {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
