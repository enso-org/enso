//! Definition of the Edge component.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::bounding_box::BoundingBox;
use ensogl::data::color;
use ensogl::define_endpoints_2;
use ensogl::display;
use ensogl::display::scene::Scene;
use ensogl_hardcoded_theme as theme;



mod inputs;
mod layout;
mod render;
mod state;

use inputs::Inputs;
use layout::EndPoint;
use render::ShapeParent;
use render::Shapes;
use state::*;



// ===========
// === FRP ===
// ===========

define_endpoints_2! {
    Input {
        /// The width and height of the source node in pixels.
        source_size(Vector2),
        /// The location of the center of the target node's input port.
        target_position(Vector2),
        /// Whether the target end of the edge is attached to a node (If `false`, it is being
        /// dragged by the mouse.)
        target_attached(bool),
        /// Whether the source end of the edge is attached to a node (If `false`, it is being
        /// dragged by the mouse.)
        source_attached(bool),
        set_disabled(bool),
        /// The typical color of the node; also used to derive the focus color.
        set_color(color::Lcha),
    }
    Output {
        /// The mouse has clicked to detach the source end of the edge.
        source_click(),
        /// The mouse has clicked to detach the target end of the edge.
        target_click(),
    }
}



// ============
// === Edge ===
// ============

/// Edge definition.
#[derive(AsRef, Clone, CloneRef, Debug, Deref)]
pub struct Edge {
    #[deref]
    frp:   Frp,
    model: Rc<EdgeModel>,
}

impl AsRef<Edge> for Edge {
    fn as_ref(&self) -> &Self {
        self
    }
}

impl Edge {
    /// Constructor.
    #[profile(Detail)]
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(EdgeModel::new(&app.display.default_scene));
        let network = &frp.network;
        let display_object = &model.display_object;
        let output = &frp.private.output;

        let edge_color = color::Animation::new(network);
        let mouse_move = display_object.on_event::<mouse::Move>();
        let mouse_down = display_object.on_event::<mouse::Down>();
        let mouse_out = display_object.on_event::<mouse::Out>();

        frp::extend! { network
            // Setters.
            eval frp.target_position ((t) model.inputs.set_target_position(ParentCoords(*t)));
            eval frp.source_attached ((t) model.inputs.set_source_attached(*t));
            eval frp.target_attached ((t) model.inputs.set_target_attached(*t));
            eval frp.source_size ((t) model.inputs.set_source_size(*t));
            eval frp.set_disabled ((t) model.inputs.set_disabled(*t));

            // Mouse events.
            eval mouse_move ([model] (e) {
                let pos = model.screen_pos_to_scene_pos(e.client_centered());
                model.inputs.set_mouse_position(pos);
            });
            eval_ mouse_out (model.inputs.clear_focus.set(true));
            eval mouse_down ([model, output] (e) {
                let pos = model.screen_pos_to_scene_pos(e.client_centered());
                let pos = model.scene_pos_to_parent_pos(pos);
                match model.closer_end(pos) {
                    Some(EndPoint::Source) => output.target_click.emit(()),
                    Some(EndPoint::Target) => output.source_click.emit(()),
                    // Ignore click events that were delivered to our display object inaccurately.
                    None => (),
                }
            });

            // Colors.
            edge_color.target <+ frp.set_color;
            eval edge_color.value ((color) model.inputs.set_color(color.into()));

            // Invalidation.
            redraw_needed <- any(...);
            redraw_needed <+ frp.target_position.constant(());
            redraw_needed <+ frp.source_attached.constant(());
            redraw_needed <+ frp.target_attached.constant(());
            redraw_needed <+ frp.source_size.constant(());
            redraw_needed <+ frp.set_disabled.constant(());
            redraw_needed <+ mouse_move.constant(());
            redraw_needed <+ mouse_out.constant(());
            redraw_needed <+ edge_color.value.constant(());
            redraw_needed <+ display_object.on_transformed.constant(());
            redraw <- redraw_needed.debounce();
            eval_ redraw (model.redraw());
        }
        Self { model, frp }
    }

    /// Return the FRP network.
    pub fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl display::Object for Edge {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}



// =================
// === EdgeModel ===
// =================

/// Internal data of `Edge`
#[derive(Debug)]
struct EdgeModel {
    /// The parent display object of all the edge's parts.
    display_object: display::object::Instance,
    /// The [`Scene`], needed for coordinate conversions and special layer assignments.
    scene:          Scene,
    /// The raw inputs the state is computed from.
    inputs:         Inputs,
    /// The state, as of the last redraw.
    state:          RefCell<Option<State>>,
    /// The currently-rendered shapes implementing the state.
    shapes:         Shapes,
}

impl EdgeModel {
    /// Constructor.
    #[profile(Debug)]
    pub fn new(scene: &Scene) -> Self {
        let display_object = display::object::Instance::new_named("Edge");
        let scene = scene.clone_ref();
        scene.layers.main_edges_level.add(&display_object);
        Self { display_object, scene, inputs: default(), state: default(), shapes: default() }
    }

    /// Redraws the connection.
    #[profile(Detail)]
    pub fn redraw(&self) {
        let state = self.calculate_state();
        self.apply_state(&state);
        self.state.set(state);
    }

    fn calculate_state(&self) -> State {
        if self.inputs.clear_focus.take() {
            self.inputs.hover_position.take();
        }
        let target_offset = self.target_offset();
        let target_attached = self.inputs.target_attached.get();
        let source_attached = self.inputs.source_attached.get();
        let layout = layout::layout(self.source_half_width(), target_offset, target_attached);
        let is_attached = target_attached && source_attached;
        let focus_split = is_attached
            .then(|| {
                // Pointer targets are updated by an asynchronous process, independent of pointer
                // movement detection. As a result, we can receive mouse events when the pointer is
                // not within the bounding box of any of our shapes, in which case `find_position`
                // here will return `None`. We treat it the same way as a
                // `mouse::Out` event.
                self.inputs.hover_position.get().and_then(|position| {
                    let position = self.scene_pos_to_parent_pos(position);
                    let source_height = self.inputs.source_size.get().y();
                    layout::find_position(position, &layout, source_height, render::HOVER_WIDTH)
                })
            })
            .flatten();
        let styles = StyleWatch::new(&self.scene.style_sheet);
        let normal_color = if self.inputs.disabled.get() {
            styles.get_color(theme::code::syntax::disabled)
        } else {
            self.inputs.color.get()
        };
        let bg_color = styles.get_color(theme::application::background);
        let focused_color = color::mix(bg_color, normal_color, 0.25);
        let (source_color, target_color) = match focus_split.map(|split| split.closer_end) {
            Some(EndPoint::Target) => (focused_color, normal_color),
            Some(EndPoint::Source) => (normal_color, focused_color),
            None => (normal_color, normal_color),
        };
        State {
            layout,
            colors: Colors { source_color, target_color },
            is_attached: IsAttached { is_attached },
            focus_split: FocusSplit { focus_split },
        }
    }

    fn apply_state(&self, state: &State) {
        let StateUpdate { layout, colors, is_attached, focus_split } =
            state.compare(&self.state.borrow());
        let display_object_dirty = None
            .or(any(layout, is_attached).changed(
                |(Layout { corners, .. }, IsAttached { is_attached, .. })| {
                    let hover_corners = is_attached.then_some(&corners[..]).unwrap_or_default();
                    self.shapes.redraw_hover_sections(self, hover_corners)
                },
            ))
            .or(any4(layout, colors, focus_split, is_attached).changed(
                |(
                    Layout { corners, arrow, .. },
                    Colors { source_color, target_color, .. },
                    FocusSplit { focus_split, .. },
                    IsAttached { is_attached, .. },
                )| {
                    self.shapes.redraw_sections(self, render::RedrawSections {
                        corners,
                        source_color: *source_color,
                        target_color: *target_color,
                        focus_split: *focus_split,
                        is_attached: *is_attached,
                    });
                    self.shapes.redraw_dataflow_arrow(self, render::RedrawDataflowArrow {
                        arrow:        *arrow,
                        source_color: *source_color,
                        target_color: *target_color,
                        focus_split:  *focus_split,
                        is_attached:  *is_attached,
                    });
                },
            ))
            .or(any(layout, colors).changed(
                |(Layout { target_attachment, .. }, Colors { target_color, .. })| {
                    self.shapes.redraw_target_attachment(self, *target_attachment, *target_color);
                },
            ))
            .is_some();
        if display_object_dirty {
            // Force layout update of this object's children. Because edge positions are computed
            // based on node positions, `redraw` must be run after the layout has been updated.
            // Updating the layouts of modified edges a second time later in the frame avoids
            // latency when edge children are modified.
            //
            // FIXME: Find a better solution to fix this issue. We either need a layout that can
            //  depend on other arbitrary position, or we need the layout update to be multi-stage.
            self.display_object.update(&self.scene);
        }
    }
}


// === Low-level operations ===

impl EdgeModel {
    fn source_half_width(&self) -> f32 {
        self.inputs.source_size.get().x() / 2.0
    }

    fn screen_pos_to_scene_pos(&self, screen_pos: Vector2) -> SceneCoords {
        let screen_pos_3d = Vector3(screen_pos.x(), screen_pos.y(), 0.0);
        SceneCoords(self.scene.screen_to_scene_coordinates(screen_pos_3d).xy())
    }

    fn scene_pos_to_parent_pos(&self, scene_pos: SceneCoords) -> ParentCoords {
        ParentCoords(*scene_pos - self.display_object.xy())
    }

    fn closer_end(&self, pos: ParentCoords) -> Option<EndPoint> {
        let state = self.state.borrow();
        let state = state.as_ref()?;
        let source_height = self.inputs.source_size.get().y();
        layout::find_position(pos, &state.layout, source_height, render::HOVER_WIDTH)
            .map(|split| split.closer_end)
    }

    fn target_offset(&self) -> Vector2 {
        *self.inputs.target_position.get() - self.display_object.xy()
    }
}


// === Trait implementations ===

impl display::Object for EdgeModel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl ShapeParent for EdgeModel {
    fn scene(&self) -> &Scene {
        &self.scene
    }
}



// ==========================
// === Coordinate systems ===
// ==========================

mod coords {
    use super::*;

    /// Marker for coordinates relative to the origin of the parent display object.
    #[derive(Debug, Copy, Clone, PartialEq, Default)]
    pub struct ParentOrigin;

    /// Marker for coordinates relative to the origin of the scene.
    #[derive(Debug, Copy, Clone, PartialEq, Default)]
    pub struct SceneOrigin;

    /// Coordinates marked to identify different coordinate spaces.
    #[derive(Debug, Copy, Clone, PartialEq, Default, Deref)]
    pub struct Coords<Space, Number: Copy + Debug + PartialEq + 'static = f32> {
        #[deref]
        coords: Vector2<Number>,
        space:  PhantomData<*const Space>,
    }

    pub type ParentCoords = Coords<ParentOrigin>;
    pub type SceneCoords = Coords<SceneOrigin>;

    #[allow(non_snake_case)]
    pub fn ParentCoords(coords: Vector2) -> ParentCoords {
        Coords { coords, space: default() }
    }
    #[allow(non_snake_case)]
    pub fn SceneCoords(coords: Vector2) -> SceneCoords {
        Coords { coords, space: default() }
    }
}
use coords::*;
