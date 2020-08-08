//! The text area implementation. It serves the purpose of single and multi-line text labels and
//! text editors.

use crate::prelude::*;

use crate::buffer::data::unit::*;
use crate::buffer::Transform;
use crate::buffer;
use crate::typeface::glyph::Glyph;
use crate::typeface::glyph;
use crate::typeface::pen;
use crate::typeface;

use enso_frp as frp;
use enso_frp::io::keyboard::Key;
use ensogl::application::Application;
use ensogl::application::shortcut;
use ensogl::application;
use ensogl::data::color;
use ensogl::display::Attribute;
use ensogl::display::Buffer;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::Sprite;
use ensogl::display;
use ensogl::gui::component::Animation;
use ensogl::gui::component;
use ensogl::gui;
use ensogl::system::web::clipboard;
use ensogl::system::gpu::shader::glsl::traits::IntoGlsl;



// =================
// === Constants ===
// =================

/// Record separator ASCII code. Used for separating of copied strings. It is defined as the `\RS`
/// escape code (`x1E`) (https://en.wikipedia.org/wiki/ASCII).
pub const RECORD_SEPARATOR : &str = "\x1E";



// ==================
// === Frp Macros ===
// ==================

// FIXME: these are generic FRP utilities. To be refactored out after the API settles down.
// FIXME: the same are defined in text/view
macro_rules! define_frp {
    (
        $(Commands {$commands_name : ident})?
        Input  { $($in_field  : ident : $in_field_type  : ty),* $(,)? }
        Output { $($out_field : ident : $out_field_type : ty),* $(,)? }
    ) => {
        /// Frp network and endpoints.
        #[derive(Debug,Clone,CloneRef)]
        #[allow(missing_docs)]
        pub struct Frp {
            pub network : frp::Network,
            pub input   : FrpInputs,
            pub output  : FrpOutputs,
        }

        impl Frp {
            /// Constructor.
            pub fn new(network:frp::Network, input:FrpInputs, output:FrpOutputs) -> Self {
                Self {network,input,output}
            }
        }

        /// Frp inputs.
        #[derive(Debug,Clone,CloneRef)]
        #[allow(missing_docs)]
        pub struct FrpInputs {
            $(pub command : $commands_name,)?
            $(pub $in_field : frp::Source<$in_field_type>),*
        }

        impl FrpInputs {
            /// Constructor.
            pub fn new(network:&frp::Network) -> Self {
                $(
                    #[allow(non_snake_case)]
                    let $commands_name = $commands_name::new(network);
                )?
                frp::extend! { network
                    $($in_field <- source();)*
                }
                Self { $(command:$commands_name,)? $($in_field),* }
            }
        }

        /// Frp output setters.
        #[derive(Debug,Clone,CloneRef)]
        pub struct FrpOutputsSetter {
            $($out_field : frp::Any<$out_field_type>),*
        }

        /// Frp outputs.
        #[derive(Debug,Clone,CloneRef)]
        #[allow(missing_docs)]
        pub struct FrpOutputs {
            setter           : FrpOutputsSetter,
            $(pub $out_field : frp::Stream<$out_field_type>),*
        }

        impl FrpOutputsSetter {
            /// Constructor.
            pub fn new(network:&frp::Network) -> Self {
                frp::extend! { network
                    $($out_field <- any(...);)*
                }
                Self {$($out_field),*}
            }
        }

        impl FrpOutputs {
            /// Constructor.
            pub fn new(network:&frp::Network) -> Self {
                let setter = FrpOutputsSetter::new(network);
                $(let $out_field = setter.$out_field.clone_ref().into();)*
                Self {setter,$($out_field),*}
            }
        }
    };
}



// ==================
// === Background ===
// ==================

/// Canvas node shape definition.
pub mod background {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style, selection:f32) {
            let out = Rect((1000.px(),1000.px())).fill(color::Rgba::new(1.0,1.0,1.0,0.05));
            out.into()
        }
    }
}



// ==============
// === Cursor ===
// ==============

const LINE_VERTICAL_OFFSET     : f32 = 4.0;
const CURSOR_PADDING           : f32 = 4.0;
const CURSOR_WIDTH             : f32 = 2.0;
const CURSOR_ALPHA             : f32 = 0.8;
const CURSORS_SPACING          : f32 = 1.0;
const SELECTION_ALPHA          : f32 = 0.3;
const SELECTION_CORNER_RADIUS  : f32 = 2.0;
const BLINK_SLOPE_IN_DURATION  : f32 = 200.0;
const BLINK_SLOPE_OUT_DURATION : f32 = 200.0;
const BLINK_ON_DURATION        : f32 = 300.0;
const BLINK_OFF_DURATION       : f32 = 300.0;
const BLINK_PERIOD             : f32 =
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
pub mod selection {
    use super::*;
    ensogl::define_shape_system! {
        (style:Style, selection:f32, start_time:f32, letter_width:f32) {
            let width_abs      = Var::<f32>::from("abs(input_size.x)");
            let rect_width     = width_abs - 2.0 * CURSOR_PADDING;
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
            let shape          = Rect((1.px() * rect_width,LINE_HEIGHT.px()));
            let shape          = shape.corners_radius(SELECTION_CORNER_RADIUS.px());
            let shape          = shape.fill(format!("srgba(1.0,1.0,1.0,{})",alpha.glsl()));
            shape.into()
        }
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
#[derive(Clone,CloneRef,Debug)]
pub struct Selection {
    logger         : Logger,
    display_object : display::object::Instance,
    right_side     : display::object::Instance,
    shape_view     : component::ShapeView<selection::Shape>,
    network        : frp::Network,
    position       : Animation<Vector2>,
    width          : Animation<f32>,
    edit_mode      : Rc<Cell<bool>>,
}

impl Deref for Selection {
    type Target = component::ShapeView<selection::Shape>;
    fn deref(&self) -> &Self::Target {
        &self.shape_view
    }
}

impl Selection {
    /// Constructor.
    pub fn new(logger:impl AnyLogger, scene:&Scene, edit_mode:bool) -> Self {
        let logger         = Logger::sub(logger,"selection");
        let display_object = display::object::Instance::new(&logger);
        let right_side     = display::object::Instance::new(&logger);
        let network        = frp::Network::new();
        let shape_view     = component::ShapeView::<selection::Shape>::new(&logger,scene);
        let position       = Animation::new(&network);
        let width          = Animation::new(&network);
        let edit_mode      = Rc::new(Cell::new(edit_mode));
        let debug          = false; // Change to true to slow-down movement for debug purposes.
        let spring_factor  = if debug { 0.1 } else { 1.5 };
        position . update_spring (|spring| spring * spring_factor);
        width    . update_spring (|spring| spring * spring_factor);
        Self {logger,display_object,right_side,shape_view,network,position,width,edit_mode} . init()
    }

    fn init(self) -> Self {
        let network    = &self.network;
        let view       = &self.shape_view;
        let object     = &self.display_object;
        let right_side = &self.right_side;
        self.add_child(view);
        view.add_child(right_side);
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
                    view.shape.sprite.size.set(Vector2(view_width,20.0));
                    view.set_position_x(view_x);
                })
            );
        }
        self
    }

    fn flip_sides(&self) {
        let width    = self.width.target_value();
        self.position.set_value(self.position.value() + Vector2(width,0.0));
        self.position.set_target_value(self.position.target_value() + Vector2(width,0.0));

        self.width.set_value(-self.width.value());
        self.width.set_target_value(-self.width.target_value());
    }
}

impl display::Object for Selection {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ====================
// === SelectionMap ===
// ====================

/// Mapping between selection id, `Selection`, and text location.
#[derive(Clone,Debug,Default)]
pub struct SelectionMap {
    id_map       : HashMap<usize,Selection>,
    location_map : HashMap<usize,HashMap<Column,usize>>
}



// ============
// === Line ===
// ============

/// Visual line representation.
///
/// **Design Notes**
/// The `divs` and `centers` are kept as vectors for performance reasons. Especially, when clicking
/// inside of the text area, it allows us to binary search the place of the mouse pointer.
#[derive(Debug)]
pub struct Line {
    display_object : display::object::Instance,
    glyphs         : Vec<Glyph>,
    divs           : Vec<f32>,
    centers        : Vec<f32>,
}

impl Line {
    fn new(logger:impl AnyLogger) -> Self {
        let logger         = Logger::sub(logger,"line");
        let display_object = display::object::Instance::new(logger);
        let glyphs         = default();
        let divs           = default();
        let centers        = default();
        Self {display_object,glyphs,divs,centers}
    }

    /// Set the division points (offsets between letters). Also updates center points.
    fn set_divs(&mut self, divs:Vec<f32>) {
        let div_iter         = divs.iter();
        let div_iter_skipped = divs.iter().skip(1);
        self.centers         = div_iter.zip(div_iter_skipped).map(|(t,s)|(t+s)/2.0).collect();
        self.divs = divs;
    }

    fn div_index_close_to(&self, offset:f32) -> usize {
        self.centers.binary_search_by(|t|t.partial_cmp(&offset).unwrap()).unwrap_both()
    }

    fn div_by_column(&self, column:Column) -> f32 {
        let ix = column.as_usize().min(self.divs.len() - 1);
        self.divs[ix]
    }

    fn resize_with(&mut self, size:usize, cons:impl Fn()->Glyph) {
        let display_object = self.display_object().clone_ref();
        self.glyphs.resize_with(size,move || {
            let glyph = cons();
            display_object.add_child(&glyph);
            glyph
        });
    }
}

impl display::Object for Line {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =============
// === Lines ===
// =============

/// Set of all visible lines.
#[derive(Clone,CloneRef,Debug,Default)]
struct Lines {
    rc : Rc<RefCell<Vec<Line>>>
}

impl Lines {
    /// The number of visible lines.
    pub fn count(&self) -> usize {
        self.rc.borrow().len()
    }

    /// Resize the line container and use the provided function to construct missing elements.
    pub fn resize_with(&self, size:usize, cons:impl Fn(usize)->Line) {
        let vec    = &mut self.rc.borrow_mut();
        let mut ix = vec.len();
        vec.resize_with(size,|| {
            let line = cons(ix);
            ix += 1;
            line
        })
    }
}



// ===========
// === FRP ===
// ===========

ensogl::def_command_api! { Commands
    /// Insert character of the last pressed key at every cursor.
    insert_char_of_last_pressed_key,
    /// Increase the indentation of all lines containing cursors.
    increase_indentation,
    /// Decrease the indentation of all lines containing cursors.
    decrease_indentation,
    /// Removes the character on the left of every cursor.
    delete_left,
    /// Removes the word on the left of every cursor.
    delete_word_left,
    /// Set the text cursor at the mouse cursor position.
    set_cursor_at_mouse_position,
    /// Add a new cursor at the mouse cursor position.
    add_cursor_at_mouse_position,
    /// Remove all cursors.
    remove_all_cursors,
    /// Start changing the shape of the newest selection with the mouse position.
    start_newest_selection_end_follow_mouse,
    /// Stop changing the shape of the newest selection with the mouse position.
    stop_newest_selection_end_follow_mouse,
    /// Move the cursor to the left by one character.
    cursor_move_left,
    /// Move the cursor to the right by one character.
    cursor_move_right,
    /// Move the cursor to the left by one word.
    cursor_move_left_word,
    /// Move the cursor to the right by one word.
    cursor_move_right_word,
    /// Move the cursor down one line.
    cursor_move_down,
    /// Move the cursor up one line.
    cursor_move_up,
    /// Extend the cursor selection to the left by one character.
    cursor_select_left,
    /// Extend the cursor selection to the right by one character.
    cursor_select_right,
    /// Extend the cursor selection down one line.
    cursor_select_down,
    /// Extend the cursor selection up one line.
    cursor_select_up,
    /// Extend the cursor selection to the left by one word.
    cursor_select_left_word,
    /// Extend the cursor selection to the right by one word.
    cursor_select_right_word,
    /// Select all characters.
    select_all,
    /// Select the word at cursor position.
    select_word_at_cursor,
    /// Discard all but the first selection.
    keep_first_selection_only,
    /// Discard all but the last selection.
    keep_last_selection_only,
    /// Discard all but the first selection and convert it to cursor.
    keep_first_cursor_only,
    /// Discard all but the last selection and convert it to cursor.
    keep_last_cursor_only,
    /// Discard all but the newest selection.
    keep_newest_selection_only,
    /// Discard all but the oldest selection.
    keep_oldest_selection_only,
    /// Discard all but the newest selection and convert it to cursor.
    keep_newest_cursor_only,
    /// Discard all but the oldest selection and convert it to cursor.
    keep_oldest_cursor_only,
    /// Set the oldest selection end to mouse position.
    set_newest_selection_end_to_mouse_position,
    /// Set the newest selection end to mouse position.
    set_oldest_selection_end_to_mouse_position,
    /// Undo the last operation.
    undo,
    /// Redo the last operation.
    redo,
    /// Copy selected text to clipboard,
    copy,
    /// Paste selected text from clipboard,
    paste,
}

impl application::command::CommandApi for Area {
    fn command_api_docs() -> Vec<application::command::EndpointDocs> {
        Commands::command_api_docs()
    }

    fn command_api(&self) -> Vec<application::command::CommandEndpoint> {
        self.frp.input.command.command_api()
    }
}

define_frp! {
    Commands { Commands }
    Input {
        paste_string : String,
    }
    Output {
        mouse_cursor_style : gui::cursor::Style,
    }
}



// ============
// === Area ===
// ============

/// Hardcoded line height. To be generalized in the future.
pub const LINE_HEIGHT : f32 = 14.0;

/// The visual text area implementation.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Area {
    data    : AreaData,
    pub frp : Frp,
}

impl Deref for Area {
    type Target = AreaData;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl Area {
    /// Consturctor.
    pub fn new(app:&Application) -> Self {
        let network = frp::Network::new();
        let data    = AreaData::new(app,&network);
        let output  = FrpOutputs::new(&network);
        let frp     = Frp::new(network,data.frp_inputs.clone_ref(),output);
        Self {data,frp} . init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let mouse   = &self.scene.mouse.frp;
        let model   = &self.data;
        let input   = &model.frp_inputs;
        let cmd     = &input.command;
        let bg      = &self.background;
        let pos     = Animation :: <Vector2> :: new(&network);
        pos.update_spring(|spring| spring*2.0);

        frp::extend! { network
            cursor_over  <- bg.events.mouse_over.constant(gui::cursor::Style::new_text_cursor());
            cursor_out   <- bg.events.mouse_out.constant(gui::cursor::Style::default());
            mouse_cursor <- any(cursor_over,cursor_out);
            self.frp.output.setter.mouse_cursor_style <+ mouse_cursor;

            mouse_on_set_cursor <- mouse.position.sample(&cmd.set_cursor_at_mouse_position);
            mouse_on_add_cursor <- mouse.position.sample(&cmd.add_cursor_at_mouse_position);

            selecting <- bool
                ( &cmd.stop_newest_selection_end_follow_mouse
                , &cmd.start_newest_selection_end_follow_mouse
                );

            eval mouse_on_set_cursor ([model](screen_pos) {
                let location = model.get_in_text_location(*screen_pos);
                model.frp.set_cursor.emit(location);
            });

            eval mouse_on_add_cursor ([model](screen_pos) {
                let location = model.get_in_text_location(*screen_pos);
                model.frp.add_cursor.emit(location);
            });

            _eval <- model.frp.output.selection_edit_mode.map2
                (&model.scene.frp.frame_time,f!([model](selections,time) {
                        model.redraw(); // FIXME: added for undo redo. Should not be needed.
                        model.on_modified_selection(selections,*time,true)
                    }
            ));

            _eval <- model.frp.output.selection_non_edit_mode.map2
                (&model.scene.frp.frame_time,f!([model](selections,time) {
                    model.redraw(); // FIXME: added for undo redo. Should not be needed.
                    model.on_modified_selection(selections,*time,false)
                }
            ));

            set_sel_end_1 <- mouse.position.gate(&selecting);
            set_sel_end_2 <- mouse.position.sample(&cmd.set_newest_selection_end_to_mouse_position);
            set_newest_selection_end <- any(&set_sel_end_1,&set_sel_end_2);

            eval set_newest_selection_end([model](screen_pos) {
                let location = model.get_in_text_location(*screen_pos);
                model.frp.set_newest_selection_end.emit(location);
            });




            // === Copy / Paste ===

            copy_sels      <- cmd.copy.map(f_!(model.selections_contents()));
            all_empty_sels <- copy_sels.map(|s|s.iter().all(|t|t.is_empty()));
            line_sel_mode  <- copy_sels.gate(&all_empty_sels);

            eval_ line_sel_mode (model.frp.cursors_select.emit(Some(Transform::Line)));
            non_line_sel_mode_sels <- copy_sels.gate_not(&all_empty_sels);
            line_sel_mode_sels     <- line_sel_mode.map(f_!(model.selections_contents()));
            sels                   <- any(&line_sel_mode_sels,&non_line_sel_mode_sels);
            eval sels ((s) model.copy(s));
            eval_ cmd.paste (model.paste());
            eval input.paste_string ((s) model.frp.paste.emit(model.decode_paste(s)));



            eval_ model.frp.output.text_changed (model.redraw());

            eval_ cmd.remove_all_cursors (model.frp.remove_all_cursors.emit(()));

            eval_ cmd.keep_first_selection_only (model.frp.keep_first_selection_only.emit(()));
            eval_ cmd.keep_last_selection_only  (model.frp.keep_last_selection_only.emit(()));
            eval_ cmd.keep_first_cursor_only     (model.frp.keep_first_cursor_only.emit(()));
            eval_ cmd.keep_last_cursor_only      (model.frp.keep_last_cursor_only.emit(()));

            eval_ cmd.keep_newest_selection_only (model.frp.keep_newest_selection_only.emit(()));
            eval_ cmd.keep_oldest_selection_only (model.frp.keep_oldest_selection_only.emit(()));
            eval_ cmd.keep_newest_cursor_only     (model.frp.keep_newest_cursor_only.emit(()));
            eval_ cmd.keep_oldest_cursor_only     (model.frp.keep_oldest_cursor_only.emit(()));

            eval_ cmd.cursor_move_left  (model.frp.cursors_move.emit(Some(Transform::Left)));
            eval_ cmd.cursor_move_right (model.frp.cursors_move.emit(Some(Transform::Right)));
            eval_ cmd.cursor_move_up    (model.frp.cursors_move.emit(Some(Transform::Up)));
            eval_ cmd.cursor_move_down  (model.frp.cursors_move.emit(Some(Transform::Down)));

            eval_ cmd.cursor_move_left_word  (model.frp.cursors_move.emit(Some(Transform::LeftWord)));
            eval_ cmd.cursor_move_right_word (model.frp.cursors_move.emit(Some(Transform::RightWord)));

            eval_ cmd.cursor_select_left  (model.frp.cursors_select.emit(Some(Transform::Left)));
            eval_ cmd.cursor_select_right (model.frp.cursors_select.emit(Some(Transform::Right)));
            eval_ cmd.cursor_select_up    (model.frp.cursors_select.emit(Some(Transform::Up)));
            eval_ cmd.cursor_select_down  (model.frp.cursors_select.emit(Some(Transform::Down)));

            eval_ cmd.cursor_select_left_word  (model.frp.cursors_select.emit(Some(Transform::LeftWord)));
            eval_ cmd.cursor_select_right_word (model.frp.cursors_select.emit(Some(Transform::RightWord)));

            eval_ cmd.select_all            (model.frp.cursors_select.emit(Some(Transform::All)));
            eval_ cmd.select_word_at_cursor (model.frp.cursors_select.emit(Some(Transform::Word)));

            eval_ cmd.delete_left      (model.frp.delete_left.emit(()));
            eval_ cmd.delete_word_left (model.frp.delete_word_left.emit(()));

            eval_ cmd.undo (model.frp.undo.emit(()));
            eval_ cmd.redo (model.frp.redo.emit(()));

            key_on_char_to_insert <- model.scene.keyboard.frp.on_pressed.sample
                (&cmd.insert_char_of_last_pressed_key);
            char_to_insert <= key_on_char_to_insert.map(|key| {
                match key {
                    Key::Character(s) => Some(s.clone()),
                    Key::Enter        => Some("\n".into()),
                    _                 => None
                }
            });
            eval char_to_insert ((s) model.frp.insert.emit(s));
        }
        self
    }
}



// ================
// === AreaData ===
// ================

/// Internal representation of `Area`.
#[derive(Clone,CloneRef,Debug)]
pub struct AreaData {
    scene          : Scene,
    logger         : Logger,
    frp_inputs     : FrpInputs,
    buffer         : buffer::View,
    display_object : display::object::Instance,
    glyph_system   : glyph::System,
    lines          : Lines,
    selection_map  : Rc<RefCell<SelectionMap>>,
    background     : component::ShapeView<background::Shape>,
}

impl Deref for AreaData {
    type Target = buffer::View;
    fn deref(&self) -> &Self::Target {
        &self.buffer
    }
}

impl AreaData {
    /// Constructor.
    pub fn new
    (app:&Application, network:&frp::Network) -> Self {
        let scene          = app.display.scene().clone_ref();
        let logger         = Logger::new("text_area");
        let bg_logger      = Logger::sub(&logger,"background");
        let selection_map  = default();
        let background     = component::ShapeView::<background::Shape>::new(&bg_logger,&scene);
        let fonts          = scene.extension::<typeface::font::Registry>();
        let font           = fonts.load("DejaVuSansMono");
        let glyph_system   = typeface::glyph::System::new(&scene,font);
        let display_object = display::object::Instance::new(&logger);
        let glyph_system   = glyph_system.clone_ref();
        let buffer         = default();
        let lines          = default();
        let frp_inputs     = FrpInputs::new(network);
        display_object.add_child(&background);

        // FIXME: Hardcoded position. To be refactored in the future PRs.
        // FIXME: Should be resolved as part of https://github.com/enso-org/ide/issues/462
        background.shape.sprite.size.set(Vector2(150.0,100.0));
        background.mod_position(|p| p.x += 50.0);

        let shape_system = scene.shapes.shape_system(PhantomData::<selection::Shape>);
        shape_system.shape_system.set_pointer_events(false);
        Self {scene,logger,frp_inputs,display_object,glyph_system,buffer,lines,selection_map
             ,background}.init()
    }

    fn on_modified_selection(&self, selections:&buffer::selection::Group, time:f32, do_edit:bool) {
        {
        let mut selection_map     = self.selection_map.borrow_mut();
        let mut new_selection_map = SelectionMap::default();
        for sel in selections {
            let sel        = self.buffer.snap_selection(*sel);
            let id         = sel.id;
            let start_line = sel.start.line.as_usize();
            let end_line   = sel.end.line.as_usize();
            let min_pos_x  = self.lines.rc.borrow()[start_line].div_by_column(sel.start.column);
            let max_pos_x  = self.lines.rc.borrow()[end_line].div_by_column(sel.end.column);
            let logger     = Logger::sub(&self.logger,"cursor");
            let min_pos_y  = -LINE_HEIGHT/2.0 - LINE_HEIGHT * start_line as f32;
            let pos        = Vector2(min_pos_x,min_pos_y);
            let width      = max_pos_x - min_pos_x;
            let selection  = match selection_map.id_map.remove(&id) {
                Some(selection) => {
                    let select_left  = selection.width.simulator.target_value() < 0.0;
                    let select_right = selection.width.simulator.target_value() > 0.0;
                    let tgt_pos_x    = selection.position.simulator.target_value().x;
                    let tgt_width    = selection.width.simulator.target_value();
                    let mid_point    = tgt_pos_x + tgt_width / 2.0;
                    let go_left      = pos.x < mid_point;
                    let go_right     = pos.x > mid_point;
                    let need_flip    = (select_left && go_left) || (select_right && go_right);
                    if width == 0.0 && need_flip { selection.flip_sides() }
                    selection.position.set_target_value(pos);
                    selection
                }
                None => {
                    let selection = Selection::new(&logger,&self.scene,do_edit);
                    selection.shape.sprite.size.set(Vector2(4.0,20.0)); // FIXME hardcoded values
                    selection.shape.letter_width.set(7.0); // FIXME hardcoded values
                    self.add_child(&selection);
                    selection.position.set_target_value(pos);
                    selection.position.skip();
                    let selection_network = &selection.network;
                    let model = self.clone_ref(); // FIXME: memory leak. To be fixed with the below note.
                    frp::extend! { selection_network
                        // FIXME[WD]: This is ultra-slow. Redrawing all glyphs on each
                        //            animation frame. Multiple times, once per cursor.
                        eval_ selection.position.value (model.redraw());
                    }
                    selection
                }
            };

            selection.width.set_target_value(width);
            selection.edit_mode.set(do_edit);
            selection.shape.start_time.set(time);
            new_selection_map.id_map.insert(id,selection);
            new_selection_map.location_map.entry(start_line).or_default().insert(sel.start.column,id);
        }
        *selection_map = new_selection_map;
        }
        self.redraw()
    }

    /// Transforms screen position to the object (display object) coordinate system.
    fn to_object_space(&self, screen_pos:Vector2) -> Vector2 {
        let origin_world_space = Vector4(0.0,0.0,0.0,1.0);
        let origin_clip_space  = self.scene.camera().view_projection_matrix() * origin_world_space;
        let inv_object_matrix  = self.transform_matrix().try_inverse().unwrap();

        let shape        = self.scene.frp.shape.value();
        let clip_space_z = origin_clip_space.z;
        let clip_space_x = origin_clip_space.w * 2.0 * screen_pos.x / shape.width;
        let clip_space_y = origin_clip_space.w * 2.0 * screen_pos.y / shape.height;
        let clip_space   = Vector4(clip_space_x,clip_space_y,clip_space_z,origin_clip_space.w);
        let world_space  = self.scene.camera().inversed_view_projection_matrix() * clip_space;
        (inv_object_matrix * world_space).xy()
    }

    fn get_in_text_location(&self, screen_pos:Vector2) -> Location {
        let object_space = self.to_object_space(screen_pos);
        let line_index   = (-object_space.y / LINE_HEIGHT) as usize;
        let line_index   = std::cmp::min(line_index,self.lines.count() - 1);
        let div_index    = self.lines.rc.borrow()[line_index].div_index_close_to(object_space.x);
        let line         = line_index.into();
        let column       = div_index.into();
        Location(line,column)
    }

    fn init(self) -> Self {
        self.redraw();
        self
    }

    // FIXME: make private
    /// Redraw the text.
    pub fn redraw(&self) {
        let lines      = self.buffer.view_lines();
        let line_count = lines.len();
        self.lines.resize_with(line_count,|ix| self.new_line(ix));
        for (view_line_index,content) in lines.into_iter().enumerate() {
            self.redraw_line(view_line_index,content)
        }
    }

    fn redraw_line(&self, view_line_index:usize, content:String) {
        let cursor_map = self.selection_map.borrow()
            .location_map.get(&view_line_index).cloned().unwrap_or_default();
        let line           = &mut self.lines.rc.borrow_mut()[view_line_index];
        let line_object    = line.display_object().clone_ref();
        let line_range     = self.buffer.byte_range_of_view_line_index_snapped(view_line_index.into());
        let mut line_style = self.buffer.sub_style(line_range.start .. line_range.end).iter();

        let mut pen         = pen::Pen::new(&self.glyph_system.font);
        let mut divs        = vec![];
        let mut byte_offset = 0.column();
        let mut last_cursor = None;
        let mut last_cursor_origin = default();
        line.resize_with(content.chars().count(),||self.glyph_system.new_glyph());
        for (glyph,chr) in line.glyphs.iter_mut().zip(content.chars()) {

            let style     = line_style.next().unwrap_or_default();
            let chr_size  = style.size.raw;
            let info      = pen.advance(chr,chr_size);
            let chr_bytes : Bytes = info.char.len_utf8().into();
            line_style.drop(chr_bytes - 1.bytes());
            let glyph_info   = self.glyph_system.font.get_glyph_info(info.char);
            let size         = glyph_info.scale.scale(chr_size);
            let glyph_offset = glyph_info.offset.scale(chr_size);
            let glyph_x      = info.offset + glyph_offset.x;
            let glyph_y      = glyph_offset.y;
            glyph.set_position_xy(Vector2(glyph_x,glyph_y));
            glyph.set_char(info.char);
            glyph.set_color(style.color);
            glyph.size.set(size);

            cursor_map.get(&byte_offset).for_each(|id| {
                self.selection_map.borrow().id_map.get(id).for_each(|cursor| {
                    if cursor.edit_mode.get() {
                        let pos_y          = LINE_HEIGHT/2.0 - LINE_VERTICAL_OFFSET;
                        last_cursor        = Some(cursor.clone_ref());
                        last_cursor_origin = Vector2(info.offset,pos_y);
                    }
                });
            });

            match &last_cursor {
                None         => line_object.add_child(glyph),
                Some(cursor) => {
                    cursor.right_side.add_child(glyph);
                    glyph.mod_position_xy(|p| p - last_cursor_origin);
                },
            }
            divs.push(info.offset);
            byte_offset += 1.column();
        }

        divs.push(pen.advance_final());
        line.set_divs(divs);
    }

    fn new_line(&self, index:usize) -> Line {
        let line     = Line::new(&self.logger);
        let y_offset = - ((index + 1) as f32) * LINE_HEIGHT + LINE_VERTICAL_OFFSET;
        line.set_position_y(y_offset);
        self.add_child(&line);
        line
    }

    fn copy(&self, selections:&[String]) {
        let encoded = match selections {
            []  => "".to_string(),
            [s] => s.clone(),
            lst => lst.join(RECORD_SEPARATOR),
        };
        clipboard::write_text(encoded);
    }

    fn paste(&self) {
        let paste_string = self.frp_inputs.paste_string.clone_ref();
        clipboard::read_text(move |t|{
            paste_string.emit(t);
        });
    }

    fn decode_paste(&self, encoded:&str) -> Vec<String> {
        encoded.split(RECORD_SEPARATOR).map(|s|s.into()).collect()
    }
}

impl display::Object for AreaData {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl display::Object for Area {
    fn display_object(&self) -> &display::object::Instance {
        self.data.display_object()
    }
}

impl application::command::FrpNetworkProvider for Area {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::command::Provider for Area {
    fn label() -> &'static str {
        "TextArea"
    }
}

impl application::View for Area {
    fn new(app:&Application) -> Self {
        Area::new(app)
    }
}

// FIXME[WD]: Some of the shortcuts are commented out and some are assigned to strange key bindings
//            because the shortcut manager is broken. To be fixed after fixing the shortcut manager.
//            Moreover, the lines should fit 100 chars after introducing the new cmd engine.
//            Should be resolved as part of https://github.com/enso-org/ide/issues/713
impl application::shortcut::DefaultShortcutProvider for Area {
    fn default_shortcuts() -> Vec<shortcut::Shortcut> {
        use enso_frp::io::mouse;
//        vec! [ Self::self_shortcut(shortcut::Action::press (&[],&[mouse::PrimaryButton]), "set_cursor_at_mouse_position")
//        ]
        vec! [ Self::self_shortcut(shortcut::Action::press   (&[Key::ArrowLeft]                       , shortcut::Pattern::Any) , "cursor_move_left"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::ArrowRight]                      , shortcut::Pattern::Any) , "cursor_move_right"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::ArrowUp]                         , shortcut::Pattern::Any) , "cursor_move_up"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::ArrowDown]                       , shortcut::Pattern::Any) , "cursor_move_down"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::ArrowLeft]             , shortcut::Pattern::Any) , "cursor_move_left_word"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::ArrowRight]            , shortcut::Pattern::Any) , "cursor_move_right_word"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Shift,Key::ArrowLeft]            , shortcut::Pattern::Any) , "cursor_select_left"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Shift,Key::ArrowRight]           , shortcut::Pattern::Any) , "cursor_select_right"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::Shift,Key::ArrowLeft]  , shortcut::Pattern::Any) , "cursor_select_left_word"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::Shift,Key::ArrowRight] , shortcut::Pattern::Any) , "cursor_select_right_word"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Shift,Key::ArrowUp]              , shortcut::Pattern::Any) , "cursor_select_up"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Shift,Key::ArrowDown]            , shortcut::Pattern::Any) , "cursor_select_down"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Backspace]                       , shortcut::Pattern::Any) , "delete_left"),
//               Self::self_shortcut(shortcut::Action::press   (&[Key::Tab]                             , shortcut::Pattern::Any) , "increase_indentation"),
//               Self::self_shortcut(shortcut::Action::press   (&[Key::Shift,Key::Tab]                  , shortcut::Pattern::Any) , "decrease_indentation"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::Backspace]             , shortcut::Pattern::Any) , "delete_word_left"),
//               Self::self_shortcut(shortcut::Action::press   (&[Key::Escape]                          , shortcut::Pattern::Any) , "keep_oldest_cursor_only"),
               Self::self_shortcut(shortcut::Action::press   (shortcut::Pattern::Any,&[])                                       , "insert_char_of_last_pressed_key"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Shift],&[mouse::PrimaryButton])                            , "set_newest_selection_end_to_mouse_position"),
               Self::self_shortcut(shortcut::Action::double_press (&[],&[mouse::PrimaryButton])                                 , "select_word_at_cursor"),
               Self::self_shortcut(shortcut::Action::press   (&[],&[mouse::PrimaryButton])                                      , "set_cursor_at_mouse_position"),
               Self::self_shortcut(shortcut::Action::press   (&[],&[mouse::PrimaryButton])                                      , "start_newest_selection_end_follow_mouse"),
               Self::self_shortcut(shortcut::Action::release (&[],&[mouse::PrimaryButton])                                      , "stop_newest_selection_end_follow_mouse"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta],&[mouse::PrimaryButton])                             , "add_cursor_at_mouse_position"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta],&[mouse::PrimaryButton])                             , "start_newest_selection_end_follow_mouse"),
               Self::self_shortcut(shortcut::Action::release (&[Key::Meta],&[mouse::PrimaryButton])                             , "stop_newest_selection_end_follow_mouse"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::Character("a".into())],&[])                      , "select_all"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::Character("c".into())],&[])                      , "copy"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Meta,Key::Character("v".into())],&[])                      , "paste"),
//               Self::self_shortcut(shortcut::Action::release (&[Key::Meta,Key::Character("z".into())],&[])                      , "undo"),
//               Self::self_shortcut(shortcut::Action::release (&[Key::Meta,Key::Character("y".into())],&[])                      , "redo"),
//               Self::self_shortcut(shortcut::Action::release (&[Key::Meta,Key::Shift,Key::Character("z".into())],&[])           , "redo"),
//                Self::self_shortcut(shortcut::Action::press   (&[Key::Escape]                          , shortcut::Pattern::Any) , "undo"),
               Self::self_shortcut(shortcut::Action::press   (&[Key::Escape]                          , shortcut::Pattern::Any) , "paste"),
        ]
    }
}

// TODO[WD]: The following shortcuts are missing and its worth adding them in the future:
// undo, redo, multicursor up/down, check with different views, scrolling, parens, lines indent
