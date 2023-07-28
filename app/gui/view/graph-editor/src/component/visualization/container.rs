//! This module defines the `Container` struct and related functionality. This represent the view
//! a visualization in the graph editor and includes a visual box that contains the visualization,
//! and action bar that allows setting the visualization type.
//!
//! The `[Container]` struct is responsible for managing the visualization and action bar and
//! providing a unified interface to the graph editor. This includes ensuring that the visualization
//! is correctly positioned, sized and layouted in its different [ViewState]s (which include the
//! `Enabled`, `Fullscreen` and `Preview` states). Importantly, this also includes EnsoGL layer
//! management to ensure correct occlusion of the visualization with respect to other scene objects.

// FIXME There is a serious performance problem in this implementation. It assumes that the
// FIXME visualization is a child of the container. However, this is very inefficient. Consider a
// FIXME visualization containing 1M of points. When moving a node (and thus moving a container),
// FIXME this would iterate over 1M of display objects and update their positions. Instead of that,
// FIXME each visualization should be positioned by some wise uniform management, maybe by a
// FIXME separate camera (view?) per visualization? This is also connected to a question how to
// FIXME create efficient dashboard view.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::system::web::traits::*;

use crate::component::visualization::instance::PreprocessorConfiguration;
use crate::data::enso;
use crate::visualization;

use action_bar::ActionBar;
use enso_frp as frp;
use ensogl::application::Application;
use ensogl::control::io::mouse;
use ensogl::data::color::Rgba;
use ensogl::display;
use ensogl::display::scene;
use ensogl::display::scene::Scene;
use ensogl::display::scene::Shape;
use ensogl::display::shape::StyleWatchFrp;
use ensogl::display::DomScene;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::Animation;
use ensogl_component::shadow;
use ensogl_derive_theme::FromTheme;
use ensogl_hardcoded_theme::graph_editor::visualization as theme;


// ==============
// === Export ===
// ==============

pub mod action_bar;
pub mod fullscreen;
pub mod visualization_chooser;



// =================
// === Constants ===
// =================

/// Default width and height of the visualization container.
pub const DEFAULT_SIZE: Vector2 = Vector2(200.0, 200.0);
/// Minimal allowed size of the visualization container.
const MIN_SIZE: Vector2 = Vector2(200.0, 200.0);
/// Maximal allowed size of the visualization container, as percentage of the screen size.
const MAX_PORTION_OF_SCREEN: Vector2 = Vector2(0.8, 0.8);
const CORNER_RADIUS: f32 = super::super::node::CORNER_RADIUS;
const ACTION_BAR_HEIGHT: f32 = 2.0 * CORNER_RADIUS;
/// Whether to reset the manually-resized visualization on opening or not.
const RESET_RESIZING_ON_OPEN: bool = false;



// ======================
// === SelectionStyle ===
// ======================

/// The style parameters of the selected node highlight.
///
/// The highlight looks like a narrow border around the node.
#[derive(Debug, Clone, Copy, Default, FromTheme)]
#[base_path = "theme::selection"]
pub struct SelectionStyle {
    /// Width of the border.
    width: f32,
    /// Color of the border.
    color: Rgba,
}



// ===========
// === Frp ===
// ===========

/// Indicates the visibility state of the visualization.
#[derive(Clone, Copy, Debug, PartialEq, Derivative)]
#[derivative(Default)]
pub enum ViewState {
    /// Visualization is permanently enabled and visible in the graph editor. It is attached to a
    /// single node and can be moved and interacted with when selected.
    Enabled,
    /// Visualization is disabled and hidden in the graph editor.
    #[derivative(Default)]
    Disabled,
    /// Visualization is temporarily enabled and visible in the graph editor. It should be placed
    /// above other scene elements to allow quick inspection.
    Preview,
    /// Visualization is enabled and visible in the graph editor in fullscreen mode. It occludes
    /// the whole graph and can be interacted with.
    Fullscreen,
}

impl ViewState {
    /// Indicates whether the visualization is visible in the graph editor. It is always visible
    /// when not disabled.
    pub fn is_visible(&self) -> bool {
        !matches!(self, ViewState::Disabled)
    }

    /// Indicates whether the visualization is fullscreen mode.
    pub fn is_fullscreen(&self) -> bool {
        matches!(self, ViewState::Fullscreen)
    }
}


ensogl::define_endpoints_2! {
    Input {
        set_view_state      (ViewState),
        set_visualization   (Option<visualization::Definition>),
        cycle_visualization (),
        set_data            (Option<visualization::Data>),
        select              (),
        deselect            (),
        set_size            (Vector2),
        set_vis_input_type  (Option<enso::Type>),
        // Set width of the container, preserving the current height.
        set_width           (f32),
    }
    Output {
        preprocessor   (PreprocessorConfiguration),
        visualization  (Option<visualization::Definition>),
        visualization_path (Option<visualization::Path>),
        size           (Vector2),
        is_selected    (bool),
        vis_input_type (Option<enso::Type>),
        fullscreen     (bool),
        visible        (bool),
        view_state     (ViewState),
    }
}



// ============
// === View ===
// ============

/// View of the visualization container.
///
/// Container has its origin in the top left corner.
#[derive(Debug, display::Object)]
#[allow(missing_docs)]
pub struct View {
    display_object:  display::object::Instance,
    selection:       Rectangle,
    overlay:         Rectangle,
    /// Resize grip is a rectangle with the size of the container but with a slight offset from the
    /// overlay shape so that it extends beyond the container at the bottom and right sides.
    /// The ordering of `overlay`, `selection`, and `resize_grip` is controlled by partition layers
    /// (see [`View::init`]).
    resize_grip:     Rectangle,
    // TODO : We added a HTML background to the `View`, because "shape" background was
    // overlapping the JS visualization. This should be further investigated
    // while fixing rust visualization displaying. (#796)
    background_dom:  DomSymbol,
    scene:           Scene,
    loading_spinner: ensogl_component::spinner::View,
}

impl View {
    /// Constructor.
    pub fn new(scene: Scene) -> Self {
        let display_object = display::object::Instance::new();
        let selection = Rectangle::default().build(|r| {
            r.set_color(Rgba::transparent());
        });
        let overlay = Rectangle::default().build(|r| {
            r.set_color(INVISIBLE_HOVER_COLOR).set_border_color(INVISIBLE_HOVER_COLOR);
        });
        let resize_grip = Rectangle::default().build(|r| {
            r.set_color(INVISIBLE_HOVER_COLOR).set_border_color(INVISIBLE_HOVER_COLOR);
        });
        display_object.add_child(&selection);
        selection.add_child(&overlay);
        overlay.add_child(&resize_grip);
        let div = web::document.create_div_or_panic();
        let background_dom = DomSymbol::new(&div);
        display_object.add_child(&background_dom);
        let loading_spinner = ensogl_component::spinner::View::new();

        Self {
            display_object,
            selection,
            overlay,
            resize_grip,
            background_dom,
            scene,
            loading_spinner,
        }
        .init()
    }

    fn set_layer(&self, layer: visualization::Layer) {
        layer.apply_for_html_component(&self.scene, &self.background_dom);
    }

    fn show_waiting_screen(&self) {
        self.add_child(&self.loading_spinner);
    }

    fn disable_waiting_screen(&self) {
        self.loading_spinner.unset_parent();
    }

    fn set_resize_grip_offset(&self, offset: Vector2) {
        self.resize_grip.set_xy(offset);
    }

    fn set_corner_radius(&self, radius: f32) {
        self.overlay.set_corner_radius(radius);
        self.selection.set_corner_radius(radius);
        let radius = format!("{radius}px");
        self.background_dom.dom().set_style_or_warn("border-radius", radius);
    }

    fn set_background_color(&self, color: Rgba) {
        let bg_color = format!(
            "rgba({},{},{},{})",
            color.red * 255.0,
            color.green * 255.0,
            color.blue * 255.0,
            color.alpha
        );
        self.background_dom.dom().set_style_or_warn("background", bg_color);
    }

    fn init_background(&self) {
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&self.scene.style_sheet);
        let background = &self.background_dom;
        background.dom().set_style_or_warn("width", "0");
        background.dom().set_style_or_warn("height", "0");
        background.dom().set_style_or_warn("z-index", "1");
        background.dom().set_style_or_warn("overflow-y", "auto");
        background.dom().set_style_or_warn("overflow-x", "auto");
        shadow::add_to_dom_element(background, &styles);
    }

    fn init_spinner(&self) {
        let spinner = &self.loading_spinner;
        spinner.scale.set(5.0);
        spinner.rgba.set(Rgba::black().into())
    }

    fn init(self) -> Self {
        self.init_background();
        self.init_spinner();
        self.show_waiting_screen();
        self.set_layer(visualization::Layer::Default);
        self.scene.layers.viz.add(&self);
        self.scene.layers.viz_selection.add(&self.selection);
        self.scene.layers.viz_resize_grip.add(&self.resize_grip);
        self.scene.layers.viz_overlay.add(&self.overlay);
        self
    }
}



// ======================
// === ContainerModel ===
// ======================

/// Internal data of a `Container`.
#[derive(Debug, display::Object)]
#[allow(missing_docs)]
pub struct ContainerModel {
    display_object:     display::object::Instance,
    /// Internal root for all sub-objects. Will be moved when the visualization
    /// container position is changed by dragging.
    drag_root:          display::object::Instance,
    visualization:      RefCell<Option<visualization::Instance>>,
    /// A network containing connection between currently set `visualization` FRP endpoints and
    /// container FRP. We keep a separate network for that, so we can manage life of such
    /// connections reliably.
    vis_frp_connection: RefCell<Option<frp::Network>>,
    scene:              Scene,
    view:               View,
    fullscreen_view:    fullscreen::Panel,
    registry:           visualization::Registry,
    size:               Rc<Cell<Vector2>>,
    action_bar:         ActionBar,
}

impl ContainerModel {
    /// Constructor.
    pub fn new(app: &Application, registry: visualization::Registry) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let drag_root = display::object::Instance::new();
        let visualization = default();
        let vis_frp_connection = default();
        let view = View::new(scene.clone_ref());
        let fullscreen_view = fullscreen::Panel::new(scene);
        let scene = scene.clone_ref();
        let size = default();
        let action_bar = ActionBar::new(app, registry.clone_ref());
        view.add_child(&action_bar);

        Self {
            display_object,
            drag_root,
            visualization,
            vis_frp_connection,
            scene,
            view,
            fullscreen_view,
            registry,
            size,
            action_bar,
        }
        .init()
    }

    fn init(self) -> Self {
        self.display_object.add_child(&self.drag_root);
        self.scene.layers.above_nodes.add(&self.action_bar);
        self.scene.layers.panel.add(&self.fullscreen_view);
        self.update_shape_sizes(ViewState::default());
        self.view.show_waiting_screen();
        self
    }

    fn set_visualization_layer(&self, layer: visualization::Layer) {
        if let Some(vis) = self.visualization.borrow().as_ref() {
            vis.set_layer.emit(layer)
        }
    }
}


// === Private API ===

impl ContainerModel {
    /// Resize the container to the given size. The size is clamped between [`MIN_SIZE`] and
    /// screen_shape * [`MAX_PORTION_OF_SCREEN`].
    fn resize(
        &self,
        mut new_size: Vector2,
        view_state: ViewState,
        screen_shape: &Shape,
    ) -> Vector2 {
        let max_size = Vector2::from(screen_shape).component_mul(&MAX_PORTION_OF_SCREEN);
        new_size.x = new_size.x.clamp(MIN_SIZE.x, max_size.x);
        new_size.y = new_size.y.clamp(MIN_SIZE.y, max_size.y);
        self.update_layout(new_size, view_state);
        new_size
    }

    /// Convert the given position from screen space to object space of the container.
    fn screen_to_object_space(&self, screen_pos: Vector2) -> Vector2 {
        let object = &self.display_object;
        let pos = scene().screen_to_object_space(object, screen_pos);
        pos
    }

    /// Update the selection shape. `value` is a selection width factor in range `[0, 1]`.
    fn set_selection(&self, container_size: Vector2, value: f32, style: &SelectionStyle) {
        let border_width = style.width * value;
        let overall_size = container_size + Vector2(border_width * 2.0, border_width * 2.0);
        if value > 0.0 {
            self.view.selection.set_border_color(style.color);
        } else {
            self.view.selection.set_border_color(Rgba::transparent());
        }
        self.view.selection.set_border_and_inset(border_width);
        self.view.selection.set_size(overall_size);
        self.view.selection.set_xy(-overall_size / 2.0);
    }

    fn apply_view_state(&self, view_state: ViewState) {
        // This is a workaround for #6600. It ensures the action bar is removed
        // and receive no further mouse events.
        if view_state.is_visible() {
            self.view.add_child(&self.action_bar);
        } else {
            self.action_bar.unset_parent();
        }

        // Show or hide the visualization.
        if view_state.is_visible() {
            self.drag_root.add_child(&self.view);
        } else {
            self.drag_root.remove_child(&self.view);
        }

        match view_state {
            ViewState::Enabled => self.enable_default_view(),
            ViewState::Disabled => {}
            ViewState::Preview => self.enable_preview(),
            ViewState::Fullscreen => self.enable_fullscreen(),
        }
    }

    fn set_vis_parents(&self, parent: &dyn display::Object, dom_parent: &DomScene) {
        if let Some(viz) = &*self.visualization.borrow() {
            parent.add_child(viz);
            if let Some(dom) = viz.root_dom() {
                dom_parent.manage(dom);
            }
            viz.inputs.activate.emit(());
        }
    }

    fn enable_fullscreen(&self) {
        self.set_visualization_layer(visualization::Layer::Fullscreen);
        self.set_vis_parents(&self.fullscreen_view, &self.scene.dom.layers.fullscreen_vis)
    }

    fn enable_default_view(&self) {
        self.set_visualization_layer(visualization::Layer::Default);
        self.set_vis_parents(&self.view, &self.scene.dom.layers.back)
    }

    fn enable_preview(&self) {
        self.set_visualization_layer(visualization::Layer::Front);
        self.set_vis_parents(&self.view, &self.scene.dom.layers.front);
    }

    fn set_visualization(
        &self,
        visualization: visualization::Instance,
        preprocessor: &frp::Any<PreprocessorConfiguration>,
        view_state: ViewState,
    ) {
        let size = self.size.get();
        visualization.frp.set_size.emit(size);
        frp::new_network! { vis_frp_connection
            // We need an additional "copy" node here. We create a new network to manage lifetime of
            // connection between `visualization.on_preprocessor_change` and `preprocessor`.
            // However, doing simple `preprocessor <+ visualization.on_preprocessor_change` will not
            // create any node in this network, so in fact it won't manage the connection.
            vis_preprocessor_change <- visualization.on_preprocessor_change.map(|x| x.clone());
            preprocessor            <+ vis_preprocessor_change;
        }
        self.visualization.replace(Some(visualization.clone_ref()));
        self.vis_frp_connection.replace(Some(vis_frp_connection));
        self.apply_view_state(view_state);
        preprocessor.emit(visualization.on_preprocessor_change.value());
    }

    #[profile(Debug)]
    fn set_visualization_data(&self, data: &visualization::Data) {
        self.visualization.borrow().for_each_ref(|vis| vis.send_data.emit(data))
    }

    fn update_shape_sizes(&self, view_state: ViewState) {
        let size = self.size.get();
        self.update_layout(size, view_state);
    }

    fn update_layout(&self, size: Vector2, view_state: ViewState) {
        self.size.set(size);
        let dom = self.view.background_dom.dom();
        let bg_dom = self.fullscreen_view.background_dom.dom();
        if view_state.is_fullscreen() {
            self.view.overlay.set_size(Vector2(0.0, 0.0));
            dom.set_style_or_warn("width", "0");
            dom.set_style_or_warn("height", "0");
            bg_dom.set_style_or_warn("width", format!("{}px", size[0]));
            bg_dom.set_style_or_warn("height", format!("{}px", size[1]));
        } else {
            self.view.overlay.set_size(size);
            self.view.resize_grip.set_size(size);
            self.view.selection.set_size(size);
            self.view.loading_spinner.set_size(size);
            dom.set_style_or_warn("width", format!("{}px", size[0]));
            dom.set_style_or_warn("height", format!("{}px", size[1]));
            bg_dom.set_style_or_warn("width", "0");
            bg_dom.set_style_or_warn("height", "0");
            self.drag_root.set_xy(Vector2(size.x / 2.0, -size.y / 2.0));
        }
        let action_bar_size = if matches!(view_state, ViewState::Enabled) {
            Vector2::new(size.x, ACTION_BAR_HEIGHT)
        } else {
            Vector2::zero()
        };
        self.action_bar.frp.set_size.emit(action_bar_size);
        self.action_bar.set_y((size.y - ACTION_BAR_HEIGHT) / 2.0);

        if view_state.is_visible() && let Some(viz) = &*self.visualization.borrow() {
            viz.frp.set_size.emit(size);
        }
    }

    /// Check if given mouse-event-target means this visualization.
    fn is_this_target(&self, target: scene::PointerTargetId) -> bool {
        self.view.overlay.is_this_target(target)
    }

    #[profile(Debug)]
    fn next_visualization(
        &self,
        current_vis: &Option<visualization::Definition>,
        input_type: &Option<enso::Type>,
    ) -> Option<visualization::Definition> {
        let input_type_or_any = input_type.clone().unwrap_or_else(enso::Type::any);
        let vis_list = self.registry.valid_sources(&input_type_or_any);
        let next_on_list = current_vis.as_ref().and_then(|vis| {
            let mut from_current =
                vis_list.iter().skip_while(|x| vis.signature.path != x.signature.path);
            from_current.nth(1)
        });
        next_on_list.or_else(|| vis_list.first()).cloned()
    }
}



// =================
// === Container ===
// =================

// TODO: Finish the fullscreen management when implementing layout management.

/// Container that wraps a `visualization::Instance` for rendering and interaction in the GUI.
///
/// The API to interact with the visualization is exposed through the `Frp`.
#[derive(Clone, CloneRef, Debug, Deref, display::Object)]
#[allow(missing_docs)]
pub struct Container {
    #[deref]
    #[display_object]
    pub model: Rc<ContainerModel>,
    pub frp:   Frp,
}

impl Container {
    /// Constructor.
    pub fn new(app: &Application, registry: visualization::Registry) -> Self {
        let model = Rc::new(ContainerModel::new(app, registry));
        let frp = Frp::new();
        Self { model, frp }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        let frp = &self.frp.private;
        let input = &frp.input;
        let output = &frp.output;
        let network = &self.frp.network;
        let model = &self.model;
        let scene = &self.model.scene;
        let scene_shape = scene.shape();
        let action_bar = &model.action_bar.frp;
        let registry = &model.registry;
        let selection = Animation::new(network);
        let width_anim = Animation::new(network);
        let style = StyleWatchFrp::new(&app.display.default_scene.style_sheet);
        let selection_style = SelectionStyle::from_theme(network, &style);

        frp::extend! { network
            init <- source_();
            eval input.set_view_state((state) model.apply_view_state(*state));
            output.view_state <+ input.set_view_state.on_change();
            output.fullscreen <+ output.view_state.map(|state| state.is_fullscreen()).on_change();
            output.visible <+ output.view_state.map(|state| state.is_visible()).on_change();

            visualization_not_selected <- input.set_visualization.map(|t| t.is_none());
            input_type_not_set <- input.set_vis_input_type.is_some().not();
            uninitialised <- visualization_not_selected && input_type_not_set;
            set_default_visualization <- uninitialised.on_change().on_true().map(|_| {
                Some(visualization::Registry::default_visualization())
            });
            vis_input_type_changed <- input.set_vis_input_type.on_change();
            vis_input_type_changed_without_selection <-
                vis_input_type_changed.gate(&visualization_not_selected).unwrap();
            set_default_visualization_for_type <- vis_input_type_changed_without_selection.map(f!((tp) {
               registry.default_visualization_for_type(tp)
            }));
            set_default_visualization <- any(
                &set_default_visualization, &set_default_visualization_for_type);


            // === Styles ===

            let corner_radius = style.get_number(theme::corner_radius);
            let background_color = style.get_color(theme::background);
            let grip_offset_x = style.get_number(theme::resize_grip::offset_x);
            let grip_offset_y = style.get_number(theme::resize_grip::offset_y);
            _eval <- corner_radius.all_with(&init, f!((radius, _) model.view.set_corner_radius(*radius)));
            _eval <- background_color.all_with(&init, f!((color, _) model.view.set_background_color(*color)));
            grip_offset <- all_with3(&init, &grip_offset_x, &grip_offset_y, |_, x, y| Vector2(*x, *y));
            eval grip_offset((offset) model.view.set_resize_grip_offset(*offset));


            // === Drag-resize ===

            let on_down = model.view.resize_grip.on_event::<mouse::Down>();
            let on_up = scene.on_event::<mouse::Up>();
            let on_move = scene.on_event::<mouse::Move>();
            on_down <- on_down.gate(&output.visible);
            on_up <- on_up.gate(&output.visible);
            is_down <- bool(&on_up, &on_down);
            on_move_down <- on_move.gate(&is_down);
            glob_pos_on_down <- on_down.map(|event| event.client_centered());
            glob_pos_on_move_down <- on_move_down.map(|event| event.client_centered());
            pos_on_down <- glob_pos_on_down.map(f!((p) model.screen_to_object_space(*p)));
            pos_on_move_down <- glob_pos_on_move_down.map(f!((p) model.screen_to_object_space(*p)));
            pos_diff <- pos_on_move_down.map2(&pos_on_down, |a, b| a - b);
            size_on_drag_start <- output.size.sample(&on_down);
            output.size <+ pos_diff.map4(&size_on_drag_start, &output.view_state, scene_shape,
                f!([model](diff, size, view_state, scene_shape) {
                    let diff = Vector2(diff.x, -diff.y);
                    let new_size = size + diff;
                    model.resize(new_size, *view_state, scene_shape)
                }
            ));


            // === Adjust width to the width of the node ===

            size_was_not_changed_manually <- any(...);
            size_was_not_changed_manually <+ init.constant(true);
            size_was_not_changed_manually <+ pos_diff.filter(|d| *d != Vector2::default()).constant(false);
            size_was_not_changed_manually <+ output.visible.on_change().constant(true);
            width_target <- input.set_width.identity();
            on_visible <- output.visible.on_true();
            width_change_after_open <- width_target.sample(&on_visible);
            width_anim.target <+ width_target.gate(&size_was_not_changed_manually);
            new_size <- width_anim.value.map2(&output.size, |w, s| Vector2(*w, s.y));
            reset_resizing_on_open <- init.constant(RESET_RESIZING_ON_OPEN);
            size_reset <- width_change_after_open.gate(&reset_resizing_on_open).map(|w| Vector2(*w, DEFAULT_SIZE.x));
            size_change <- any3(&new_size, &input.set_size, &size_reset);
            size_change_and_scene_shape <- all(&size_change, scene_shape);
            output.size <+ size_change_and_scene_shape.map2(&output.view_state,
                f!(((new_size, scene_shape), view_state) model.resize(*new_size, *view_state, scene_shape))
            );
        }


        // ===  Visualization Chooser Bindings ===

        frp::extend! { network
            selected_definition <- action_bar.visualization_selection.map(f!([registry](path)
                path.as_ref().and_then(|path| registry.definition_from_path(path))
            ));
            action_bar.hide_icons <+ selected_definition.constant(());
            output.vis_input_type <+ input.set_vis_input_type;
            let chooser = &model.action_bar.visualization_chooser();
            chooser.frp.set_vis_input_type <+ input.set_vis_input_type;
        }


        // === Cycling Visualizations ===

        frp::extend! { network
            vis_after_cycling <- input.cycle_visualization.map3(&output.visualization, &output.vis_input_type,
                f!(((),vis,input_type) model.next_visualization(vis,input_type))
            );
        }


        // === Switching Visualizations ===

        frp::extend! { network
            vis_definition_set <- any(
                input.set_visualization,
                selected_definition,
                vis_after_cycling,
                set_default_visualization);
            new_vis_definition <- vis_definition_set.on_change();
            let preprocessor = &output.preprocessor;
            output.visualization <+ new_vis_definition.map2(&output.view_state, f!(
                [model,action_bar,app,preprocessor](vis_definition, view_state) {

                if let Some(definition) = vis_definition {
                    match definition.new_instance(&app) {
                        Ok(vis)  => {
                            model.set_visualization(vis,&preprocessor, *view_state);
                            let path = Some(definition.signature.path.clone());
                            action_bar.set_selected_visualization.emit(path);
                        },
                        Err(err) => {
                            warn!("Failed to instantiate visualization: {err:?}");
                        },
                    };
                }
                vis_definition.clone()
            }));

            output.visualization_path <+ output.visualization.map(|definition| {
                definition.as_ref().map(|def| def.signature.path.clone_ref())
            });
        }

        // === Visualization Loading Spinner ===

        frp::extend! { network
            eval_ output.visualization ( model.view.show_waiting_screen() );
            eval_ input.set_data ( model.view.disable_waiting_screen() );
        }


        // === Selecting Visualization ===

        frp::extend! { network
            mouse_down_target <- scene.mouse.frp_deprecated.down.map(f_!(scene.mouse.target.get()));
            selected_by_click <= mouse_down_target.map2(&output.view_state, f!([model] (target,view_state){
                let vis        = &model.visualization;
                let activate   = || vis.borrow().as_ref().map(|v| v.activate.clone_ref());
                let deactivate = || vis.borrow().as_ref().map(|v| v.deactivate.clone_ref());
                if model.is_this_target(*target) {
                    if let Some(activate) = activate() {
                        activate.emit(());
                        return Some(true);
                    }
                } else if !view_state.is_fullscreen() {
                    if let Some(deactivate) = deactivate() {
                        deactivate.emit(());
                        return Some(false);
                    }
                }
                None
            }));
            selection_after_click <- selected_by_click.map(|sel| if *sel {1.0} else {0.0});
            selection.target <+ selection_after_click;
            _eval <- selection.value.all_with3(&output.size, &selection_style,
                f!((value, size, style) {
                    model.set_selection(*size, *value, style);
                }
            ));
            is_selected <- selected_by_click || output.fullscreen;
            output.is_selected <+ is_selected.on_change();
        }


        // === Fullscreen View ===

        frp::extend! { network
            enable_fullscreen <- output.fullscreen.on_true();
            disable_fullscreen <- output.fullscreen.on_false();

            fullscreen_enabled_weight  <- enable_fullscreen.constant(1.0);
            fullscreen_disabled_weight <- disable_fullscreen.constant(0.0);
            fullscreen_weight          <- any(fullscreen_enabled_weight,fullscreen_disabled_weight);

            _eval <- fullscreen_weight.all_with4(&output.size,scene_shape,&output.view_state,
                f!([model] (weight,viz_size,scene_size,view_state) {
                    let weight_inv           = 1.0 - weight;
                    let scene_size : Vector2 = scene_size.into();
                    let current_size         = viz_size * weight_inv + scene_size * *weight;
                    model.update_layout(current_size,*view_state);

                    let m1  = model.scene.layers.panel.camera().inversed_view_matrix();
                    let m2  = model.scene.layers.viz.camera().view_matrix();
                    let pos = model.global_position();
                    let pos = Vector4::new(pos.x,pos.y,pos.z,1.0);
                    let pos = m2 * (m1 * pos);
                    let pp = Vector3(pos.x,pos.y,pos.z);
                    let current_pos = pp * weight_inv;
                    model.fullscreen_view.set_position(current_pos);
            }));


            // === Data Update ===

            data <- input.set_data.unwrap();
            has_data <- input.set_data.is_some();
            reset_data <- data.sample(&new_vis_definition).gate(&has_data);
            data_update <- any(&data,&reset_data);
            data_update <- data_update.buffered_gate(&output.visible);
            eval data_update ((t) model.set_visualization_data(t));

        }


        // ===  Action bar actions ===

        frp::extend! { network
            eval_ action_bar.on_container_reset_position(model.drag_root.set_xy(Vector2::zero()));
            drag_action <- app.cursor.frp.scene_position_delta.gate(&action_bar.container_drag_state);
            eval drag_action ((mouse) model.drag_root.update_xy(|pos| pos - mouse.xy()));
        }

        // FIXME[mm]: If we set the size right here, we will see spurious shapes in some
        // computation heavy circumstances (e.g., collapsing nodes #805, or creating an new project
        // #761). This should not happen anyway, but the following is a hotfix to hide the visible
        // behaviour. If we leave use the frp api and don't abort the animation, the size will stay
        // at (0,0) until the animation has run its course and the shape stays invisible during
        // loads.
        //
        // The order of events is like this:
        // * shape gets created (with size 0 or default size).
        // * some load happens in the scene, the shape is visible if it has a non-zero size.
        // * load is resolved and the shape is hidden as it should be.
        // * the animation finishes running and sets the size to the correct size.
        //
        // This is not optimal the optimal solution to this problem, as it also means that we have
        // an animation on an invisible component running.
        self.frp.public.set_size(DEFAULT_SIZE);
        self.frp.public.set_visualization(None);
        init.emit(());
        self
    }

    /// Get the visualization panel view.
    pub fn fullscreen_visualization(&self) -> &fullscreen::Panel {
        &self.model.fullscreen_view
    }
}
