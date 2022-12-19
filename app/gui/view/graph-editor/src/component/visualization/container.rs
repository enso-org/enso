//! This module defines the `Container` struct and related functionality.

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
use ensogl::display;
use ensogl::display::scene;
use ensogl::display::scene::Scene;
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

ensogl::define_endpoints! {
    Input {
        set_visibility      (bool),
        toggle_visibility   (),
        set_visualization   (Option<visualization::Definition>),
        cycle_visualization (),
        set_data            (visualization::Data),
        select              (),
        deselect            (),
        set_size            (Vector2),
        enable_fullscreen   (),
        disable_fullscreen  (),
        set_vis_input_type  (Option<enso::Type>),
        set_layer           (visualization::Layer),
    }

    Output {
        preprocessor   (PreprocessorConfiguration),
        visualisation  (Option<visualization::Definition>),
        size           (Vector2),
        is_selected    (bool),
        visible        (bool),
        vis_input_type (Option<enso::Type>)
    }
}



// ============
// === View ===
// ============

/// View of the visualization container.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct View {
    display_object: display::object::Instance,

    background:     background::View,
    overlay:        overlay::View,
    background_dom: DomSymbol,
    scene:          Scene,
}

impl View {
    /// Constructor.
    pub fn new(scene: Scene) -> Self {
        let display_object = display::object::Instance::new();
        let background = background::View::new();
        let overlay = overlay::View::new();
        display_object.add_child(&background);
        display_object.add_child(&overlay);

        ensogl::shapes_order_dependencies! {
            scene => {
                background -> overlay;
            }
        };

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape
        // system (#795)
        let styles = StyleWatch::new(&scene.style_sheet);
        let bg_color =
            styles.get_color(ensogl_hardcoded_theme::graph_editor::visualization::background);
        let bg_hex = format!(
            "rgba({},{},{},{})",
            bg_color.red * 255.0,
            bg_color.green * 255.0,
            bg_color.blue * 255.0,
            bg_color.alpha
        );

        let div = web::document.create_div_or_panic();
        let background_dom = DomSymbol::new(&div);
        // TODO : We added a HTML background to the `View`, because "shape" background was
        // overlapping the JS visualization. This should be further investigated
        // while fixing rust visualization displaying. (#796)
        background_dom.dom().set_style_or_warn("width", "0");
        background_dom.dom().set_style_or_warn("height", "0");
        background_dom.dom().set_style_or_warn("z-index", "1");
        background_dom.dom().set_style_or_warn("overflow-y", "auto");
        background_dom.dom().set_style_or_warn("overflow-x", "auto");
        background_dom.dom().set_style_or_warn("background", bg_hex);
        background_dom.dom().set_style_or_warn("border-radius", "14px");
        shadow::add_to_dom_element(&background_dom, &styles);
        display_object.add_child(&background_dom);

        Self { display_object, background, overlay, background_dom, scene }.init()
    }

    fn set_layer(&self, layer: visualization::Layer) {
        layer.apply_for_html_component(&self.scene, &self.background_dom);
    }

    fn init(self) -> Self {
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
    logger:             Logger,
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
    is_fullscreen:      Rc<Cell<bool>>,
    registry:           visualization::Registry,
    size:               Rc<Cell<Vector2>>,
    action_bar:         ActionBar,
}

impl ContainerModel {
    /// Constructor.
    pub fn new(logger: &Logger, app: &Application, registry: visualization::Registry) -> Self {
        let scene = &app.display.default_scene;
        let logger = Logger::new_sub(logger, "visualization_container");
        let display_object = display::object::Instance::new();
        let drag_root = display::object::Instance::new();
        let visualization = default();
        let vis_frp_connection = default();
        let view = View::new(scene.clone_ref());
        let fullscreen_view = fullscreen::Panel::new(&logger, scene);
        let scene = scene.clone_ref();
        let is_fullscreen = default();
        let size = default();
        let action_bar = ActionBar::new(app, registry.clone_ref());
        view.add_child(&action_bar);

        Self {
            logger,
            display_object,
            drag_root,
            visualization,
            vis_frp_connection,
            scene,
            view,
            fullscreen_view,
            is_fullscreen,
            registry,
            size,
            action_bar,
        }
        .init()
    }

    fn init(self) -> Self {
        self.display_object.add_child(&self.drag_root);

        self.update_shape_sizes();
        self.init_corner_roundness();
        // FIXME: These 2 lines fix a bug with display objects visible on stage.
        self.set_visibility(true);
        self.set_visibility(false);
        self
    }

    /// Indicates whether the visualization container is visible and active.
    /// Note: can't be called `is_visible` due to a naming conflict with `display::object::class`.
    pub fn is_active(&self) -> bool {
        self.view.has_parent()
    }
}


// === Private API ===

impl ContainerModel {
    fn set_visibility(&self, visibility: bool) {
        if visibility {
            self.drag_root.add_child(&self.view);
            self.show_visualisation();
        } else {
            self.drag_root.remove_child(&self.view);
        }
    }

    fn enable_fullscreen(&self) {
        self.is_fullscreen.set(true);
        if let Some(viz) = &*self.visualization.borrow() {
            self.fullscreen_view.add_child(viz);
            if let Some(dom) = viz.root_dom() {
                self.scene.dom.layers.fullscreen_vis.manage(dom);
            }
            viz.inputs.activate.emit(());
        }
    }

    fn disable_fullscreen(&self) {
        self.is_fullscreen.set(false);
        if let Some(viz) = &*self.visualization.borrow() {
            self.view.add_child(viz);
            if let Some(dom) = viz.root_dom() {
                self.scene.dom.layers.back.manage(dom);
            }
            viz.inputs.deactivate.emit(());
        }
    }

    fn toggle_visibility(&self) {
        self.set_visibility(!self.is_active())
    }

    fn set_visualization(
        &self,
        visualization: visualization::Instance,
        preprocessor: &frp::Any<PreprocessorConfiguration>,
    ) {
        let size = self.size.get();
        visualization.set_size.emit(size);
        frp::new_network! { vis_frp_connection
            // We need an additional "copy" node here. We create a new network to manage lifetime of
            // connection between `visualization.on_preprocessor_change` and `preprocessor`.
            // However, doing simple `preprocessor <+ visualization.on_preprocessor_change` will not
            // create any node in this network, so in fact it won't manage the connection.
            vis_preprocessor_change <- visualization.on_preprocessor_change.map(|x| x.clone());
            preprocessor            <+ vis_preprocessor_change;
        }
        preprocessor.emit(visualization.on_preprocessor_change.value());
        if self.is_fullscreen.get() {
            self.fullscreen_view.add_child(&visualization)
        } else {
            self.view.add_child(&visualization);
        }
        self.visualization.replace(Some(visualization));
        self.vis_frp_connection.replace(Some(vis_frp_connection));
    }

    fn set_visualization_data(&self, data: &visualization::Data) {
        self.visualization.borrow().for_each_ref(|vis| vis.send_data.emit(data))
    }

    fn update_shape_sizes(&self) {
        let size = self.size.get();
        self.set_size(size);
    }

    fn set_size(&self, size: impl Into<Vector2>) {
        let dom = self.view.background_dom.dom();
        let bg_dom = self.fullscreen_view.background_dom.dom();
        let size = size.into();
        self.display_object().set_size(size);
        if self.is_fullscreen.get() {
            // self.fullscreen_view.background.shape.radius.set(CORNER_RADIUS);
            // self.fullscreen_view.background.shape.sprite.set_size(size);
            // self.view.background.shape.sprite.set_size(zero());
            self.view.overlay.set_size(Vector2(0.0, 0.0));
            dom.set_style_or_warn("width", "0");
            dom.set_style_or_warn("height", "0");
            bg_dom.set_style_or_warn("width", format!("{}px", size[0]));
            bg_dom.set_style_or_warn("height", format!("{}px", size[1]));
            self.action_bar.frp.set_size.emit(Vector2::zero());
        } else {
            // self.view.background.shape.radius.set(CORNER_RADIUS);
            self.view.overlay.radius.set(CORNER_RADIUS);
            self.view.background.radius.set(CORNER_RADIUS);
            self.view.overlay.set_size(size);
            self.view.background.set_size(size + 2.0 * Vector2(PADDING, PADDING));
            dom.set_style_or_warn("width", format!("{}px", size[0]));
            dom.set_style_or_warn("height", format!("{}px", size[1]));
            bg_dom.set_style_or_warn("width", "0");
            bg_dom.set_style_or_warn("height", "0");
            // self.fullscreen_view.background.shape.sprite.set_size(zero());

            let action_bar_size = Vector2::new(size.x, ACTION_BAR_HEIGHT);
            self.action_bar.frp.set_size.emit(action_bar_size);
        }

        self.action_bar.set_y((size.y - ACTION_BAR_HEIGHT) / 2.0);

        if let Some(viz) = &*self.visualization.borrow() {
            viz.set_size.emit(size);
        }
    }

    fn init_corner_roundness(&self) {
        self.set_corner_roundness(1.0)
    }

    fn set_corner_roundness(&self, value: f32) {
        self.view.overlay.roundness.set(value);
        self.view.background.roundness.set(value);
    }

    fn show_visualisation(&self) {
        if let Some(vis) = self.visualization.borrow().as_ref() {
            if self.is_fullscreen.get() {
                self.fullscreen_view.add_child(vis);
            } else {
                self.view.add_child(vis);
            }
        }
    }

    /// Check if given mouse-event-target means this visualization.
    fn is_this_target(&self, target: scene::PointerTargetId) -> bool {
        self.view.overlay.is_this_target(target)
    }

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
#[derive(Clone, CloneRef, Debug, Derivative, Shrinkwrap)]
#[allow(missing_docs)]
pub struct Container {
    #[shrinkwrap(main_field)]
    pub model: Rc<ContainerModel>,
    pub frp:   Frp,
}

impl Container {
    /// Constructor.
    pub fn new(logger: &Logger, app: &Application, registry: visualization::Registry) -> Self {
        let model = Rc::new(ContainerModel::new(logger, app, registry));
        let frp = Frp::new();
        Self { model, frp }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        let frp = &self.frp;
        let network = &self.frp.network;
        let model = &self.model;
        let scene = &self.model.scene;
        let scene_shape = scene.shape();
        let logger = &self.model.logger;
        let action_bar = &model.action_bar.frp;
        let registry = &model.registry;
        let selection = Animation::new(network);

        frp::extend! { network
            eval  frp.set_visibility    ((v) model.set_visibility(*v));
            eval_ frp.toggle_visibility (model.toggle_visibility());
            eval  frp.set_data          ((t) model.set_visualization_data(t));
            frp.source.size    <+ frp.set_size;
            frp.source.visible <+ frp.set_visibility;
            frp.source.visible <+ frp.toggle_visibility.map(f!((()) model.is_active()));
            eval  frp.set_layer         ([model](l) {
                if let Some(vis) = model.visualization.borrow().as_ref() {
                    vis.set_layer.emit(l)
                }
                model.view.set_layer(*l);
            });
        }


        // === Cycling Visualizations ===

        frp::extend! { network
            vis_after_cycling <- frp.cycle_visualization.map3(&frp.visualisation,&frp.vis_input_type,
                f!(((),vis,input_type) model.next_visualization(vis,input_type))
            );
        }


        // === Switching Visualizations ===

        frp::extend! { network
            new_vis_definition <- any(frp.set_visualization,vis_after_cycling);
            let preprocessor   =  &frp.source.preprocessor;
            frp.source.visualisation <+ new_vis_definition.map(f!(
                [model,action_bar,app,logger,preprocessor](vis_definition) {

                if let Some(definition) = vis_definition {
                    match definition.new_instance(&app) {
                        Ok(vis)  => {
                            model.set_visualization(vis,&preprocessor);
                            let path = Some(definition.signature.path.clone());
                            action_bar.set_selected_visualization.emit(path);
                        },
                        Err(err) => {
                            warning!(logger,"Failed to instantiate visualization: {err:?}");
                        },
                    };
                }
                vis_definition.clone()
            }));
        }


        // === Selecting Visualization ===

        frp::extend! { network
            mouse_down_target <- scene.mouse.frp.down.map(f_!(scene.mouse.target.get()));
            selected_by_click <= mouse_down_target.map(f!([model] (target){
                let vis        = &model.visualization;
                let activate   = || vis.borrow().as_ref().map(|v| v.activate.clone_ref());
                let deactivate = || vis.borrow().as_ref().map(|v| v.deactivate.clone_ref());
                if model.is_this_target(*target) {
                    if let Some(activate) = activate() {
                        activate.emit(());
                        return Some(true);
                    }
                } else if !model.is_fullscreen.get() {
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

            selected_by_going_fullscreen <- bool(&frp.disable_fullscreen,&frp.enable_fullscreen);
            selected                     <- any(selected_by_click,selected_by_going_fullscreen);

            is_selected_changed <= selected.map2(&frp.output.is_selected, |&new,&old| {
                (new != old).as_some(new)
            });
            frp.source.is_selected <+ is_selected_changed;
        }


        // === Fullscreen View ===

        frp::extend! { network
            eval_ frp.enable_fullscreen  (model.enable_fullscreen());
            eval_ frp.disable_fullscreen (model.disable_fullscreen());
            fullscreen_enabled_weight  <- frp.enable_fullscreen.constant(1.0);
            fullscreen_disabled_weight <- frp.disable_fullscreen.constant(0.0);
            fullscreen_weight          <- any(fullscreen_enabled_weight,fullscreen_disabled_weight);
            frp.source.size            <+ frp.set_size;

            _eval <- fullscreen_weight.all_with3(&frp.size,scene_shape,
                f!([model] (weight,viz_size,scene_size) {
                    let weight_inv           = 1.0 - weight;
                    let scene_size : Vector2 = scene_size.into();
                    let current_size         = viz_size * weight_inv + scene_size * *weight;
                    model.set_corner_roundness(weight_inv);
                    model.set_size(current_size);

                    let m1  = model.scene.layers.panel.camera().inversed_view_matrix();
                    let m2  = model.scene.layers.viz.camera().view_matrix();
                    let pos = model.global_position();
                    let pos = Vector4::new(pos.x,pos.y,pos.z,1.0);
                    let pos = m2 * (m1 * pos);
                    let pp = Vector3(pos.x,pos.y,pos.z);
                    let current_pos = pp * weight_inv;
                    model.fullscreen_view.set_position(current_pos);
            }));
        }


        // ===  Visualisation chooser frp bindings ===

        frp::extend! { network
            selected_definition  <- action_bar.visualisation_selection.map(f!([registry](path)
                path.as_ref().and_then(|path| registry.definition_from_path(path))
            ));
            eval selected_definition([app,model,logger,preprocessor](definition)  {
                let vis = definition.as_ref().map(|d| d.new_instance(&app));
                match vis {
                    Some(Ok(vis))  => model.set_visualization(vis,&preprocessor),
                    Some(Err(err)) => {
                        warning!(logger,"Failed to instantiate visualisation: {err:?}");
                    },
                    None => warning!(logger,"Invalid visualisation selected."),
                };
            });
            frp.source.visualisation <+ selected_definition;
            on_selected              <- selected_definition.map(|d|d.as_ref().map(|_|())).unwrap();
            eval_ on_selected ( action_bar.hide_icons.emit(()) );
            frp.source.vis_input_type <+ frp.set_vis_input_type;
            eval frp.set_vis_input_type (
                (tp) model.action_bar.visualization_chooser().frp.set_vis_input_type(tp)
            );
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
        frp.set_size.emit(Vector2(DEFAULT_SIZE.0, DEFAULT_SIZE.1));
        frp.set_visualization.emit(Some(visualization::Registry::default_visualisation()));
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
