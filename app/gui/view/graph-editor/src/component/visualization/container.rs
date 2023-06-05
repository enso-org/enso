//! This module defines the `Container` struct and related functionality. This represent the view
//! a visualisation in the graph editor and includes a visual box that contains the visualisation,
//! and action bar that allows setting the visualisation type.
//!
//! The `[Container]` struct is responsible for managing the visualisation and action bar and
//! providing a unified interface to the graph editor. This includes ensuring that the visualisation
//! is correctly positioned, sized and layouted in its different [ViewState]s (which include the
//! `Enabled`, `Fullscreen` and `Preview` states). Importantly, this also includes EnsoGL layer
//! management to ensure correct occlusion of the visualisation with respect to other scene objects.

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
use ensogl::data::color::Rgba;
use ensogl::display;
use ensogl::display::scene;
use ensogl::display::scene::Scene;
use ensogl::display::DomScene;
use ensogl::display::DomSymbol;
use ensogl::system::web;
use ensogl::Animation;
use ensogl_component::shadow;


// ==============
// === Export ===
// ==============

pub mod action_bar;
pub mod fullscreen;
pub mod visualization_chooser;



// =================
// === Constants ===
// =================

/// Default width and height of the visualisation container.
pub const DEFAULT_SIZE: (f32, f32) = (200.0, 200.0);
const PADDING: f32 = 20.0;
const CORNER_RADIUS: f32 = super::super::node::CORNER_RADIUS;
const ACTION_BAR_HEIGHT: f32 = 2.0 * CORNER_RADIUS;



// =============
// === Shape ===
// =============

/// Container overlay shape definition. Used to capture events over the visualisation within the
/// container.
pub mod overlay {
    use super::*;

    ensogl::shape! {
        alignment = center;
        (style: Style, radius: f32, roundness: f32, selection: f32) {
            let width         = Var::<Pixels>::from("input_size.x");
            let height        = Var::<Pixels>::from("input_size.y");
            let radius        = 1.px() * &radius;
            let corner_radius = &radius * &roundness;
            let color_overlay = color::Rgba::new(1.0,0.0,0.0,0.000_000_1);
            let overlay       = Rect((&width,&height)).corners_radius(corner_radius);
            let overlay       = overlay.fill(color_overlay);
            let out           = overlay;
            out.into()
        }
    }
}

/// Container's background, including selection.
// TODO[ao] : Currently it does not contain the real background, which is rendered in HTML instead.
//        This should be fixed in https://github.com/enso-org/ide/issues/526
pub mod background {
    use super::*;
    use ensogl_hardcoded_theme::graph_editor::visualization as theme;

    ensogl::shape! {
        alignment = center;
        (style:Style, radius:f32, roundness:f32, selection:f32) {
            let width         = Var::<Pixels>::from("input_size.x");
            let height        = Var::<Pixels>::from("input_size.y");
            let width         = width  - PADDING.px() * 2.0;
            let height        = height - PADDING.px() * 2.0;
            let radius        = 1.px() * &radius;
            let corner_radius = &radius * &roundness;


            // === Selection ===

            let sel_color  = style.get_color(theme::selection);
            let sel_size   = style.get_number(theme::selection::size);
            let sel_offset = style.get_number(theme::selection::offset);

            let sel_width   = &width  - 1.px() + &sel_offset.px() * 2.0 * &selection;
            let sel_height  = &height - 1.px() + &sel_offset.px() * 2.0 * &selection;
            let sel_radius  = &corner_radius + &sel_offset.px();
            let select      = Rect((&sel_width,&sel_height)).corners_radius(sel_radius);

            let sel2_width  = &width  - 2.px() + &(sel_size + sel_offset).px() * 2.0 * &selection;
            let sel2_height = &height - 2.px() + &(sel_size + sel_offset).px() * 2.0 * &selection;
            let sel2_radius = &corner_radius + &sel_offset.px() + &sel_size.px() * &selection;
            let select2     = Rect((&sel2_width,&sel2_height)).corners_radius(sel2_radius);

            let select = select2 - select;
            let select = select.fill(sel_color);

            select.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

/// Indicates the visibility state of the visualisation.
#[derive(Clone, Copy, Debug, PartialEq, Derivative)]
#[derivative(Default)]
pub enum ViewState {
    /// Visualisation is permanently enabled and visible in the graph editor. It is attached to a
    /// single node and can be moved and interacted with when selected.
    Enabled,
    /// Visualisation is disabled and hidden in the graph editor.
    #[derivative(Default)]
    Disabled,
    /// Visualisation is temporarily enabled and visible in the graph editor. It should be placed
    /// above other scene elements to allow quick inspection.
    Preview,
    /// Visualisation is enabled and visible in the graph editor in fullscreen mode. It occludes
    /// the whole graph and can be interacted with.
    Fullscreen,
}

impl ViewState {
    /// Indicates whether the visualisation is visible in the graph editor. It is always visible
    /// when not disabled.
    pub fn is_visible(&self) -> bool {
        !matches!(self, ViewState::Disabled)
    }

    /// Indicates whether the visualisation is fullscreen mode.
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
    }
    Output {
        preprocessor   (PreprocessorConfiguration),
        visualisation  (Option<visualization::Definition>),
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
#[derive(Debug)]
#[allow(missing_docs)]
pub struct View {
    display_object:  display::object::Instance,
    background:      background::View,
    overlay:         overlay::View,
    background_dom:  DomSymbol,
    scene:           Scene,
    loading_spinner: ensogl_component::spinner::View,
}

impl View {
    /// Constructor.
    pub fn new(scene: Scene) -> Self {
        let display_object = display::object::Instance::new();
        let background = background::View::new();
        let overlay = overlay::View::new();
        display_object.add_child(&background);
        display_object.add_child(&overlay);
        let div = web::document.create_div_or_panic();
        let background_dom = DomSymbol::new(&div);
        display_object.add_child(&background_dom);
        let loading_spinner = ensogl_component::spinner::View::new();

        ensogl::shapes_order_dependencies! {
            scene => {
                background -> overlay;
                background -> ensogl_component::spinner;
            }
        };

        Self { display_object, background, overlay, background_dom, scene, loading_spinner }.init()
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

    fn init_background(&self) {
        let background = &self.background_dom;
        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&self.scene.style_sheet);
        let bg_color =
            styles.get_color(ensogl_hardcoded_theme::graph_editor::visualization::background);
        let bg_hex = format!(
            "rgba({},{},{},{})",
            bg_color.red * 255.0,
            bg_color.green * 255.0,
            bg_color.blue * 255.0,
            bg_color.alpha
        );

        // TODO : We added a HTML background to the `View`, because "shape" background was
        // overlapping the JS visualization. This should be further investigated
        // while fixing rust visualization displaying. (#796)
        background.dom().set_style_or_warn("width", "0");
        background.dom().set_style_or_warn("height", "0");
        background.dom().set_style_or_warn("z-index", "1");
        background.dom().set_style_or_warn("overflow-y", "auto");
        background.dom().set_style_or_warn("overflow-x", "auto");
        background.dom().set_style_or_warn("background", bg_hex);
        background.dom().set_style_or_warn("border-radius", "14px");
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
        self
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ======================
// === ContainerModel ===
// ======================

/// Internal data of a `Container`.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct ContainerModel {
    display_object:     display::object::Instance,
    /// Internal root for all sub-objects. Will be moved when the visualisation
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
        self.update_shape_sizes(ViewState::default());
        self.init_corner_roundness();
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

    fn update_layout(&self, size: impl Into<Vector2>, view_state: ViewState) {
        let dom = self.view.background_dom.dom();
        let bg_dom = self.fullscreen_view.background_dom.dom();
        let size = size.into();
        self.size.set(size);
        if view_state.is_fullscreen() {
            self.view.overlay.set_size(Vector2(0.0, 0.0));
            dom.set_style_or_warn("width", "0");
            dom.set_style_or_warn("height", "0");
            bg_dom.set_style_or_warn("width", format!("{}px", size[0]));
            bg_dom.set_style_or_warn("height", format!("{}px", size[1]));
            self.action_bar.frp.set_size.emit(Vector2::zero());
        } else {
            self.view.overlay.radius.set(CORNER_RADIUS);
            self.view.background.radius.set(CORNER_RADIUS);
            self.view.overlay.set_size(size);
            self.view.background.set_size(size + 2.0 * Vector2(PADDING, PADDING));
            self.view.loading_spinner.set_size(size + 2.0 * Vector2(PADDING, PADDING));
            dom.set_style_or_warn("width", format!("{}px", size[0]));
            dom.set_style_or_warn("height", format!("{}px", size[1]));
            bg_dom.set_style_or_warn("width", "0");
            bg_dom.set_style_or_warn("height", "0");

            let action_bar_size = Vector2::new(size.x, ACTION_BAR_HEIGHT);
            self.action_bar.frp.set_size.emit(action_bar_size);
        }

        self.action_bar.set_y((size.y - ACTION_BAR_HEIGHT) / 2.0);

        if let Some(viz) = &*self.visualization.borrow() {
            viz.frp.set_size.emit(size);
        }
    }

    fn init_corner_roundness(&self) {
        self.set_corner_roundness(1.0)
    }

    fn set_corner_roundness(&self, value: f32) {
        self.view.overlay.roundness.set(value);
        self.view.background.roundness.set(value);
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

impl display::Object for ContainerModel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// =================
// === Container ===
// =================

// TODO: Finish the fullscreen management when implementing layout management.

/// Container that wraps a `visualization::Instance` for rendering and interaction in the GUI.
///
/// The API to interact with the visualization is exposed through the `Frp`.
#[derive(Clone, CloneRef, Debug, Derivative, Deref)]
#[allow(missing_docs)]
pub struct Container {
    #[deref]
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

        frp::extend! { network
            eval input.set_view_state((state) model.apply_view_state(*state));
            output.view_state <+ input.set_view_state.on_change();
            output.fullscreen <+ output.view_state.map(|state| state.is_fullscreen()).on_change();
            output.visible <+ output.view_state.map(|state| state.is_visible()).on_change();
            output.size <+ input.set_size.on_change();

            visualisation_not_selected <- input.set_visualization.map(|t| t.is_none());
            input_type_not_set <- input.set_vis_input_type.is_some().not();
            uninitialised <- visualisation_not_selected && input_type_not_set;
            set_default_visualisation <- uninitialised.on_change().on_true().map(|_| {
                Some(visualization::Registry::default_visualisation())
            });
            vis_input_type_changed <- input.set_vis_input_type.on_change();
            vis_input_type_changed_without_selection <-
                vis_input_type_changed.gate(&visualisation_not_selected).unwrap();
            set_default_visualisation_for_type <- vis_input_type_changed_without_selection.map(f!((tp) {
               registry.default_visualization_for_type(tp)
            }));
            set_default_visualisation <- any(
                &set_default_visualisation, &set_default_visualisation_for_type);

        }


        // ===  Visualisation Chooser Bindings ===

        frp::extend! { network
            selected_definition <- action_bar.visualisation_selection.map(f!([registry](path)
                path.as_ref().and_then(|path| registry.definition_from_path(path))
            ));
            action_bar.hide_icons <+ selected_definition.constant(());
            output.vis_input_type <+ input.set_vis_input_type;
            let chooser = &model.action_bar.visualization_chooser();
            chooser.frp.set_vis_input_type <+ input.set_vis_input_type;
        }


        // === Cycling Visualizations ===

        frp::extend! { network
            vis_after_cycling <- input.cycle_visualization.map3(&output.visualisation, &output.vis_input_type,
                f!(((),vis,input_type) model.next_visualization(vis,input_type))
            );
        }


        // === Switching Visualizations ===

        frp::extend! { network
            vis_definition_set <- any(
                input.set_visualization,
                selected_definition,
                vis_after_cycling,
                set_default_visualisation);
            new_vis_definition <- vis_definition_set.on_change();
            let preprocessor = &output.preprocessor;
            output.visualisation <+ new_vis_definition.map2(&output.view_state, f!(
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

            output.visualization_path <+ output.visualisation.map(|definition| {
                definition.as_ref().map(|def| def.signature.path.clone_ref())
            });
        }

        // === Visualisation Loading Spinner ===

        frp::extend! { network
            eval_ output.visualisation ( model.view.show_waiting_screen() );
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
            eval selection.value ((selection) model.view.background.selection.set(*selection));
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
                    model.set_corner_roundness(weight_inv);
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
        self.frp.public.set_size(Vector2(DEFAULT_SIZE.0, DEFAULT_SIZE.1));
        self.frp.public.set_visualization(None);
        self
    }

    /// Get the visualization panel view.
    pub fn fullscreen_visualization(&self) -> &fullscreen::Panel {
        &self.model.fullscreen_view
    }
}

impl display::Object for Container {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
