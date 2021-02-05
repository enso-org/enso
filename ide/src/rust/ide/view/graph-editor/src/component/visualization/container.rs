//! This module defines the `Container` struct and related functionality.

// FIXME There is a serious performance problem in this implementation. It assumes that the
// FIXME visualization is a child of the container. However, this is very inefficient. Consider a
// FIXME visualization containing 1M of points. When moving a node (and thus moving a container),
// FIXME this would iterate over 1M of display objects and update their positions. Instead of that,
// FIXME each visualization should be positioned by some wise uniform management, maybe by a
// FIXME separate camera (view?) per visualization? This is also connected to a question how to
// FIXME create efficient dashboard view.

mod action_bar;
mod visualization_chooser;

use crate::prelude::*;

use crate::data::enso;
use crate::visualization;

use action_bar::ActionBar;
use enso_frp as frp;
use ensogl::data::color;
use ensogl::display::DomSymbol;
use ensogl::display::scene;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::traits::*;
use ensogl::display;
use ensogl::DEPRECATED_Animation;
use ensogl::application::Application;
use ensogl::system::web;
use ensogl::system::web::StyleSetter;
use ensogl_theme as theme;




// =================
// === Constants ===
// =================

/// Default width and height of the visualisation container.
pub const DEFAULT_SIZE  : (f32,f32) = (200.0,200.0);
const CORNER_RADIUS     : f32       = super::super::node::CORNER_RADIUS;
// Note[mm]: at the moment we use a CSS replacement shadow defined in the .visualization class of
// `src/js/lib/content/src/index.html`. While that is in use this shadow is deactivated.
const SHADOW_SIZE       : f32       = 0.0 * super::super::node::SHADOW_SIZE;
const ACTION_BAR_HEIGHT : f32       = 2.0 * CORNER_RADIUS;



// =============
// === Shape ===
// =============

/// Container background shape definition.
///
/// Provides a backdrop and outline for visualisations. Can indicate the selection status of the
/// container.
/// TODO : We do not use backgrounds because otherwise they would overlap JS
///        visualizations. Instead we added a HTML background to the `View`.
///        This should be further investigated while fixing rust visualization displaying. (#526)
pub mod background {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style,selected:f32,radius:f32,roundness:f32) {
            use theme::graph_editor::visualization as visualization_theme;

            let width         = Var::<Pixels>::from("input_size.x");
            let height        = Var::<Pixels>::from("input_size.y");
            let width         = &width  - SHADOW_SIZE.px() * 2.0;
            let height        = &height - SHADOW_SIZE.px() * 2.0;
            let radius        = 1.px() * &radius;
            let color_bg      = style.get_color(visualization_theme::background);
            let corner_radius = &radius * &roundness;
            let background    = Rect((&width,&height)).corners_radius(&corner_radius);
            let background    = background.fill(color::Rgba::from(color_bg));

            // === Shadow ===

            let border_size_f = 16.0;
            let corner_radius = corner_radius*1.75;
            let width         = &width  + SHADOW_SIZE.px() * 2.0;
            let height        = &height + SHADOW_SIZE.px() * 2.0;
            let shadow        = Rect((&width,&height)).corners_radius(&corner_radius).shrink(1.px());
            let base_color    = style.get_color(visualization_theme::shadow);
            let fading_color  = style.get_color(visualization_theme::shadow::fading);
            let exponent      = style.get_number_or(visualization_theme::shadow::exponent,2.0);
            let shadow_color  = color::LinearGradient::new()
                .add(0.0,color::Rgba::from(fading_color).into_linear())
                .add(1.0,color::Rgba::from(base_color).into_linear());
            let shadow_color = color::SdfSampler::new(shadow_color)
                .max_distance(border_size_f)
                .slope(color::Slope::Exponent(exponent));
            let shadow        = shadow.fill(shadow_color);

            let out = shadow + background;
            out.into()
        }
    }
}

/// Container background shape definition.
///
/// Provides a backdrop and outline for visualisations. Can indicate the selection status of the
/// container.
/// TODO : We do not use backgrounds because otherwise they would overlap JS
///        visualizations. Instead we added a HTML background to the `View`.
///        This should be further investigated while fixing rust visualization displaying. (#526)
pub mod fullscreen_background {
    use super::*;

    ensogl::define_shape_system! {
        (style:Style,selected:f32,radius:f32,roundness:f32) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let radius        = 1.px() * &radius;
            let color_path    = theme::graph_editor::visualization::background;
            let color_bg      = style.get_color(color_path);
            let corner_radius = &radius * &roundness;
            let background    = Rect((&width,&height)).corners_radius(&corner_radius);
            let background    = background.fill(color::Rgba::from(color_bg));
            background.into()
        }
    }
}

/// Container overlay shape definition. Used to capture events over the visualisation within the
/// container.
pub mod overlay {
    use super::*;

    ensogl::define_shape_system! {
        (selected:f32,radius:f32,roundness:f32) {
            let width         = Var::<Pixels>::from("input_size.x");
            let height        = Var::<Pixels>::from("input_size.y");
            let radius        = 1.px() * &radius;
            let corner_radius = &radius * &roundness;
            let color_overlay = color::Rgba::new(1.0,0.0,0.0,0.000_000_1);
            let overlay       = Rect((&width,&height)).corners_radius(&corner_radius);
            let overlay       = overlay.fill(color_overlay);
            let out           = overlay;
            out.into()
        }
    }
}



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        set_visibility     (bool),
        toggle_visibility  (),
        set_visualization  (Option<visualization::Definition>),
        set_data           (visualization::Data),
        select             (),
        deselect           (),
        set_size           (Vector2),
        enable_fullscreen  (),
        disable_fullscreen (),
        scene_shape        (scene::Shape),
    }

    Output {
        preprocessor  (enso::Code),
        visualisation (Option<visualization::Definition>),
        size          (Vector2),
        is_selected   (bool),
    }
}



// ============
// === View ===
// ============

/// View of the visualization container.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct View {
    logger         : Logger,
    display_object : display::object::Instance,
    // TODO : We do not use backgrounds because otherwise they would overlap JS
    //        visualizations. Instead we added a HTML background to the `View`.
    //        This should be further investigated while fixing rust visualization displaying. (#526)
    // background     : background::View,
    overlay        : overlay::View,
    background_dom : DomSymbol
}

impl View {
    /// Constructor.
    pub fn new(logger:&Logger, scene:&Scene) -> Self {
        let logger         = Logger::sub(logger,"view");
        let display_object = display::object::Instance::new(&logger);
        let overlay        = overlay::View::new(&logger);
        display_object.add_child(&overlay);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let styles   = StyleWatch::new(&scene.style_sheet);
        let bg_color = styles.get_color(ensogl_theme::graph_editor::visualization::background);
        let bg_color = color::Rgba::from(bg_color);
        let bg_hex   = format!("rgba({},{},{},{})",bg_color.red*255.0,bg_color.green*255.0,bg_color.blue*255.0,bg_color.alpha);

        let shadow_alpha = styles.get_number_or(ensogl_theme::graph_editor::visualization::shadow::html::alpha,0.16);
        let shadow_size  = styles.get_number_or(ensogl_theme::graph_editor::visualization::shadow::html::size,16.0);
        let shadow       = format!("0 0 {}px rgba(0, 0, 0, {})",shadow_size,shadow_alpha);

        let div            = web::create_div();
        let background_dom = DomSymbol::new(&div);
        // TODO : We added a HTML background to the `View`, because "shape" background was overlapping
        //        the JS visualization. This should be further investigated while fixing rust
        //        visualization displaying. (#796)
        background_dom.dom().set_style_or_warn("width"        ,"0"   ,&logger);
        background_dom.dom().set_style_or_warn("height"       ,"0"   ,&logger);
        background_dom.dom().set_style_or_warn("z-index"      ,"1"   ,&logger);
        background_dom.dom().set_style_or_warn("overflow-y"   ,"auto",&logger);
        background_dom.dom().set_style_or_warn("overflow-x"   ,"auto",&logger);
        background_dom.dom().set_style_or_warn("background"   ,bg_hex,&logger);
        background_dom.dom().set_style_or_warn("border-radius","14px",&logger);
        background_dom.dom().set_style_or_warn("box-shadow"   ,shadow,&logger);
        display_object.add_child(&background_dom);

        scene.dom.layers.back.manage(&background_dom);

        Self {logger,display_object,overlay,background_dom} . init(scene)
    }

    fn init(self, scene:&Scene) -> Self {
        scene.layers.viz.add_exclusive(&self);
        self
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ======================
// === FullscreenView ===
// ======================

/// View of the visualization container meant to be used in fullscreen mode. Its components are
/// rendered on top-level layers of the stage.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct FullscreenView {
    logger         : Logger,
    display_object : display::object::Instance,
    // background     : fullscreen_background::View,
    background_dom : DomSymbol
}

impl FullscreenView {
    /// Constructor.
    pub fn new(logger:&Logger, scene:&Scene) -> Self {
        let logger         = Logger::sub(logger,"fullscreen_view");
        let display_object = display::object::Instance::new(&logger);
        let shape_system   = scene.shapes.shape_system(PhantomData::<fullscreen_background::Shape>);
        scene.layers.main.remove_symbol(&shape_system.shape_system.symbol);
        scene.layers.viz_fullscreen.add_symbol_exclusive(&shape_system.shape_system.symbol);

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for shape system (#795)
        let styles   = StyleWatch::new(&scene.style_sheet);
        let bg_color = styles.get_color(ensogl_theme::graph_editor::visualization::background);
        let bg_color = color::Rgba::from(bg_color);
        let bg_hex   = format!("rgba({},{},{},{})",bg_color.red*255.0,bg_color.green*255.0,bg_color.blue*255.0,bg_color.alpha);

        let div            = web::create_div();
        let background_dom = DomSymbol::new(&div);
        // TODO : We added a HTML background to the `View`, because "shape" background was overlapping
        //        the JS visualization. This should be further investigated while fixing rust
        //        visualization displaying. (#796)
        background_dom.dom().set_style_or_warn("width"        ,"0"   ,&logger);
        background_dom.dom().set_style_or_warn("height"       ,"0"   ,&logger);
        background_dom.dom().set_style_or_warn("z-index"      ,"1"   ,&logger);
        background_dom.dom().set_style_or_warn("overflow-y"   ,"auto",&logger);
        background_dom.dom().set_style_or_warn("overflow-x"   ,"auto",&logger);
        background_dom.dom().set_style_or_warn("background"   ,bg_hex,&logger);
        background_dom.dom().set_style_or_warn("border-radius","0"   ,&logger);
        display_object.add_child(&background_dom);
        scene.dom.layers.back.manage(&background_dom);

        Self {logger,display_object,background_dom}
    }
}

impl display::Object for FullscreenView {
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
    logger             : Logger,
    display_object     : display::object::Instance,
    /// Internal root for all sub-objects. Will be moved when the visualisation
    /// container position is changed by dragging.
    drag_root       : display::object::Instance,
    visualization      : RefCell<Option<visualization::Instance>>,
    /// A network containing connection between currently set `visualization` FRP endpoints and
    /// container FRP. We keep a separate network for that, so we can manage life of such
    /// connections reliably.
    vis_frp_connection : RefCell<Option<frp::Network>>,
    scene              : Scene,
    view               : View,
    fullscreen_view    : FullscreenView,
    is_fullscreen      : Rc<Cell<bool>>,
    registry           : visualization::Registry,
    size               : Rc<Cell<Vector2>>,
    action_bar         : ActionBar,
}

impl ContainerModel {
    /// Constructor.
    pub fn new
    (logger:&Logger, app:&Application, registry:visualization::Registry)
    -> Self {
        let scene              = app.display.scene();
        let logger             = Logger::sub(logger,"visualization_container");
        let display_object     = display::object::Instance::new(&logger);
        let drag_root          = display::object::Instance::new(&logger);
        let visualization      = default();
        let vis_frp_connection = default();
        let view               = View::new(&logger,scene);
        let fullscreen_view    = FullscreenView::new(&logger,scene);
        let scene              = scene.clone_ref();
        let is_fullscreen      = default();
        let size               = default();
        let action_bar         = ActionBar::new(&app,registry.clone_ref());
        view.add_child(&action_bar);

        Self {logger,visualization,vis_frp_connection,display_object,drag_root,view,fullscreen_view
            ,scene,is_fullscreen,action_bar,registry,size}.init()
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
    fn set_visibility(&self, visibility:bool) {
        if visibility {
            self.drag_root.add_child(&self.view);
            self.show_visualisation();
            self.scene.add_child(&self.fullscreen_view);
        }
        else {
            self.drag_root.remove_child(&self.view);
            self.scene.remove_child(&self.fullscreen_view);
        }
    }

    fn enable_fullscreen(&self) {
        self.is_fullscreen.set(true);
        if let Some(viz) = &*self.visualization.borrow() {
            self.fullscreen_view.add_child(viz)
        }
    }

    fn toggle_visibility(&self) {
        self.set_visibility(!self.is_active())
    }

    fn set_visualization
    (&self, visualization:visualization::Instance, preprocessor:&frp::Any<enso::Code>) {
        let size = self.size.get();
        visualization.set_size.emit(size);
        frp::new_network! { vis_frp_connection
            // We need an additional "copy" node here. We create a new network to manage lifetime of
            // connection between `visualization.on_preprocessor_change` and `preprocessor`.
            // However, doing simple `preprocessor <+ visualization.on_preprocessor_change` will not
            // create any node in this network, so in fact ot won't manage the connection.
            vis_preprocessor_change <- visualization.on_preprocessor_change.map(|x| x.clone());
            preprocessor            <+ vis_preprocessor_change;
        }
        self.view.add_child(&visualization);
        self.visualization.replace(Some(visualization));
        self.vis_frp_connection.replace(Some(vis_frp_connection));
    }

    fn set_visualization_data(&self, data:&visualization::Data) {
        self.visualization.borrow().for_each_ref(|vis| vis.send_data.emit(data))
    }

    fn update_shape_sizes(&self) {
        let size = self.size.get();
        self.set_size(size);
    }

    fn set_size(&self, size:impl Into<Vector2>) {
        let dom    = self.view.background_dom.dom();
        let bg_dom = self.fullscreen_view.background_dom.dom();
        let size   = size.into();
        self.size.set(size);
        if self.is_fullscreen.get() {
            // self.fullscreen_view.background.shape.radius.set(CORNER_RADIUS);
            // self.fullscreen_view.background.shape.sprite.size.set(size);
            // self.view.background.shape.sprite.size.set(zero());
            self.view.overlay.size.set(zero());
            dom.set_style_or_warn("width" ,"0",&self.logger);
            dom.set_style_or_warn("height","0",&self.logger);
            bg_dom.set_style_or_warn("width", format!("{}px", size[0]), &self.logger);
            bg_dom.set_style_or_warn("height", format!("{}px", size[1]), &self.logger);
            self.action_bar.frp.set_size.emit(Vector2::zero());
        } else {
            // self.view.background.shape.radius.set(CORNER_RADIUS);
            self.view.overlay.radius.set(CORNER_RADIUS);
            // self.view.background.shape.sprite.size.set(size);
            self.view.overlay.size.set(size);
            dom.set_style_or_warn("width" ,format!("{}px",size[0]),&self.logger);
            dom.set_style_or_warn("height",format!("{}px",size[1]),&self.logger);
            bg_dom.set_style_or_warn("width", "0", &self.logger);
            bg_dom.set_style_or_warn("height", "0", &self.logger);
            // self.fullscreen_view.background.shape.sprite.size.set(zero());

            let action_bar_size = Vector2::new(size.x, ACTION_BAR_HEIGHT);
            self.action_bar.frp.set_size.emit(action_bar_size);
        }

        self.action_bar.set_position_y((size.y - ACTION_BAR_HEIGHT) / 2.0);

        if let Some(viz) = &*self.visualization.borrow() {
            viz.set_size.emit(size);
        }
    }

    fn init_corner_roundness(&self) {
        self.set_corner_roundness(1.0)
    }

    fn set_corner_roundness(&self, value:f32) {
        self.view.overlay.roundness.set(value);
        // self.view.background.shape.roundness.set(value);
        // self.fullscreen_view.background.shape.roundness.set(value);
    }

    fn show_visualisation(&self) {
        if let Some(vis) = self.visualization.borrow().as_ref() {
            self.view.add_child(vis);
        }
    }

    /// Check if given mouse-event-target means this visualization.
    fn is_this_target(&self, target:scene::PointerTarget) -> bool {
        self.view.overlay.is_this_target(target)
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
#[derive(Clone,CloneRef,Debug,Derivative,Shrinkwrap)]
#[allow(missing_docs)]
pub struct Container {
    #[shrinkwrap(main_field)]
    pub model : Rc<ContainerModel>,
    pub frp   : Frp,
}

impl Container {
    /// Constructor.
    pub fn new(logger:&Logger,app:&Application,registry:visualization::Registry) -> Self {
        let model = Rc::new(ContainerModel::new(logger,app,registry));
        let frp   = Frp::new();
        Self{model,frp}.init(app)
    }

    fn init(self, app:&Application) -> Self {
        let frp                 = &self.frp;
        let network             = &self.frp.network;
        let model               = &self.model;
        let fullscreen          = DEPRECATED_Animation::new(network);
        let size                = DEPRECATED_Animation::<Vector2>::new(network);
        let fullscreen_position = DEPRECATED_Animation::<Vector3>::new(network);
        let scene               = &self.model.scene;
        let logger              = &self.model.logger;
        let action_bar          = &model.action_bar.frp;
        let registry            = &model.registry;

        frp::extend! { network
            eval  frp.set_visibility    ((v) model.set_visibility(*v));
            eval_ frp.toggle_visibility (model.toggle_visibility());
            let preprocessor = &frp.source.preprocessor;
            frp.source.visualisation <+ frp.set_visualization.map(f!(
                [model,action_bar,scene,logger,preprocessor](vis_definition) {

                if let Some(definition) = vis_definition {
                    match definition.new_instance(&scene) {
                        Ok(vis)  => {
                            model.set_visualization(vis,&preprocessor);
                            let path = Some(definition.signature.path.clone());
                            action_bar.set_selected_visualization.emit(path);
                        },
                        Err(err) => {
                            warning!(logger,"Failed to instantiate visualisation: {err:?}");
                        },
                    };
                }
                vis_definition.clone()
            }));
            eval  frp.set_data          ((t) model.set_visualization_data(t));

            eval_ frp.enable_fullscreen (model.set_visibility(true));
            eval_ frp.enable_fullscreen (model.enable_fullscreen());
            eval_ frp.enable_fullscreen (fullscreen.set_target_value(1.0));
            eval  frp.set_size          ((s) size.set_target_value(*s));

            mouse_down_target <- scene.mouse.frp.down.map(f_!(scene.mouse.target.get()));
            selected <= mouse_down_target.map(f!([model] (target){
                let vis        = &model.visualization;
                let activate   = || vis.borrow().as_ref().map(|v| v.activate.clone_ref());
                let deactivate = || vis.borrow().as_ref().map(|v| v.deactivate.clone_ref());
                if model.is_this_target(*target) {
                    if let Some(activate) = activate() {
                        activate.emit(());
                        return Some(true);
                    }
                } else if let Some(deactivate) = deactivate() {
                    deactivate.emit(());
                    return Some(false);
                }
                None
            }));

            is_selected_changed <= selected.map2(&frp.output.is_selected, |&new,&old| {
                (new != old).as_some(new)
            });
            frp.source.is_selected <+ is_selected_changed;

            _eval <- fullscreen.value.all_with3(&size.value,&frp.scene_shape,
                f!([model] (weight,viz_size,scene_size) {
                    let weight_inv           = 1.0 - weight;
                    let scene_size : Vector2 = scene_size.into();
                    let current_size         = viz_size * weight_inv + scene_size * *weight;
                    model.set_corner_roundness(weight_inv);
                    model.set_size(current_size);

                    let m1  = model.scene.layers.viz_fullscreen.camera().inversed_view_matrix();
                    let m2  = model.scene.layers.viz.camera().view_matrix();
                    let pos = model.global_position();
                    let pos = Vector4::new(pos.x,pos.y,pos.z,1.0);
                    let pos = m2 * (m1 * pos);
                    let pp = Vector3(pos.x,pos.y,pos.z);
                    let current_pos = pp * weight_inv;
                    model.fullscreen_view.set_position(current_pos);
            }));

            eval fullscreen_position.value ((p) model.fullscreen_view.set_position(*p));
        }


        // ===  Visualisation chooser frp bindings ===
        frp::extend! { network
            selected_definition  <- action_bar.visualisation_selection.map(f!([registry](path)
                path.as_ref().map(|path| registry.definition_from_path(path) ).flatten()
            ));
            eval selected_definition([scene,model,logger,preprocessor](definition)  {
                let vis = definition.as_ref().map(|d| d.new_instance(&scene));
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
        }


        // ===  Action bar actions ===
        frp::extend! { network
            eval_ action_bar.on_container_reset_position(model.drag_root.set_position_xy(Vector2::zero()));
            drag_action <- app.cursor.frp.scene_position_delta.gate(&action_bar.container_drag_state);
            eval drag_action ((mouse) model.drag_root.mod_position_xy(|pos| pos - mouse.xy()));
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
        frp.set_size.emit(Vector2(DEFAULT_SIZE.0,DEFAULT_SIZE.1));
        frp.set_visualization.emit(Some(visualization::Registry::default_visualisation()));
        self
    }
}

impl display::Object for Container {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
