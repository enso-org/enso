//! The main view of single project opened in IDE.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::system::web::traits::*;

use crate::code_editor;
use crate::component_browser;
use crate::component_browser::component_list_panel;
use crate::debug_mode_popup;
use crate::debug_mode_popup::DEBUG_MODE_SHORTCUT;
use crate::documentation;
use crate::graph_editor::component::node;
use crate::graph_editor::component::node::Expression;
use crate::graph_editor::component::visualization;
use crate::graph_editor::GraphEditor;
use crate::graph_editor::NodeId;
use crate::open_dialog::OpenDialog;
use crate::searcher;

use enso_config::ARGS;
use enso_frp as frp;
use ensogl::application;
use ensogl::application::shortcut;
use ensogl::application::Application;
use ensogl::display;
use ensogl::system::web;
use ensogl::system::web::dom;
use ensogl::DEPRECATED_Animation;
use ensogl_hardcoded_theme::Theme;
use ide_view_graph_editor::NodeSource;



// =================
// === Constants ===
// =================

/// A time which must pass since last change of expression of node which is the searcher input
/// to send `searcher_input_changed` event. The delay ensures we don't needlessly update Component
/// Browser when user is quickly typing in the expression input.
const INPUT_CHANGE_DELAY_MS: i32 = 200;



// ===========
// === FRP ===
// ===========

/// The parameters of the displayed searcher.
#[derive(Clone, Copy, Debug, Default)]
pub struct SearcherParams {
    /// The node being an Expression Input.
    pub input:       NodeId,
    /// The node being a source for the edited node data - usually it's output shall be a `this`
    /// port for inserted expression.
    pub source_node: Option<NodeSource>,
}

impl SearcherParams {
    fn new_for_new_node(node_id: NodeId, source_node: Option<NodeSource>) -> Self {
        Self { input: node_id, source_node }
    }

    fn new_for_edited_node(node_id: NodeId) -> Self {
        Self { input: node_id, source_node: None }
    }
}

ensogl::define_endpoints! {
    Input {
        /// Open the Open File or Project Dialog.
        show_open_dialog(),
        /// Close the searcher without taking any actions
        close_searcher(),
        /// Close the Open File or Project Dialog without further action
        close_open_dialog(),
        /// Simulates a style toggle press event.
        toggle_style(),
        /// Saves a snapshot of the current state of the project to the VCS.
        save_project_snapshot(),
        /// Undo the last user's action.
        undo(),
        /// Redo the last undone action.
        redo(),
        // Enable Debug Mode of Graph Editor.
        enable_debug_mode(),
        // Disable Debug Mode of Graph Editor.
        disable_debug_mode(),
        /// A set of value updates has been processed and rendered.
        values_updated(),
        /// Interrupt the running program.
        execution_context_interrupt(),
        /// Restart the program execution.
        execution_context_restart(),
    }

    Output {
        searcher                       (Option<SearcherParams>),
        /// The searcher input has changed and the Component Browser content should be refreshed.
        /// Is **not** emitted with every graph's node expression change, only when
        /// [`INPUT_CHANGE_DELAY_MS`] passes since last change, so we won't needlessly update the
        /// Component Browser when user is quickly typing in the input.
        searcher_input_changed         (ImString),
        is_searcher_opened             (bool),
        adding_new_node                (bool),
        old_expression_of_edited_node  (Expression),
        editing_aborted                (NodeId),
        editing_committed_old_searcher (NodeId, Option<searcher::entry::Id>),
        editing_committed              (NodeId, Option<component_list_panel::grid::GroupEntryId>),
        open_dialog_shown              (bool),
        code_editor_shown              (bool),
        style                          (Theme),
        fullscreen_visualization_shown (bool),
        drop_files_enabled             (bool),
        debug_mode                     (bool),
    }
}



// =============
// === Model ===
// =============

/// Common FRP endpoints for both Searcher variants. See [`SearcherVariant`].
#[derive(Clone, CloneRef, Debug)]
struct SearcherFrp {
    editing_committed: frp::Stream,
    is_visible:        frp::Stream<bool>,
    is_empty:          frp::Stream<bool>,
    is_hovered:        frp::Stream<bool>,
}

/// A structure containing the Searcher View: the old Node Searcher of a new Component Browser.
///
/// As Component Browser is unstable, it is available only under the feature flag. Thus the Project
/// View must be able to handle any of those views.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub enum SearcherVariant {
    ComponentBrowser(component_browser::View),
    /// We keep the old searcher in a Rc, as its memory size is much greater than the new one.
    OldNodeSearcher(Rc<searcher::View>),
}

impl SearcherVariant {
    fn new(app: &Application) -> Self {
        if ARGS.enable_new_component_browser {
            Self::ComponentBrowser(app.new_view::<component_browser::View>())
        } else {
            Self::OldNodeSearcher(Rc::new(app.new_view::<searcher::View>()))
        }
    }

    fn frp(&self, project_view_network: &frp::Network) -> SearcherFrp {
        match self {
            SearcherVariant::ComponentBrowser(view) => {
                let grid = &view.model().list.model().grid;
                frp::extend! {project_view_network
                    is_empty <- source::<bool>();
                    editing_committed <- grid.expression_accepted.constant(());
                }
                is_empty.emit(false);
                SearcherFrp {
                    editing_committed,
                    is_visible: view.output.is_visible.clone_ref().into(),
                    is_empty: is_empty.into(),
                    is_hovered: view.output.is_hovered.clone_ref().into(),
                }
            }
            SearcherVariant::OldNodeSearcher(view) => {
                let documentation = view.documentation();
                frp::extend! {project_view_network
                    editing_committed <- view.editing_committed.constant(());
                }
                SearcherFrp {
                    editing_committed,
                    is_visible: view.output.is_visible.clone_ref().into(),
                    is_empty: view.output.is_empty.clone_ref().into(),
                    is_hovered: documentation.frp.is_hovered.clone_ref().into(),
                }
            }
        }
    }

    fn documentation(&self) -> &documentation::View {
        match self {
            SearcherVariant::ComponentBrowser(view) => &view.model().documentation,
            SearcherVariant::OldNodeSearcher(view) => view.documentation(),
        }
    }

    fn show(&self) {
        match self {
            SearcherVariant::ComponentBrowser(view) => view.show(),
            SearcherVariant::OldNodeSearcher(view) => view.show(),
        }
    }

    fn hide(&self) {
        match self {
            SearcherVariant::ComponentBrowser(view) => view.hide(),
            SearcherVariant::OldNodeSearcher(view) => view.hide(),
        }
    }

    fn setup_anchor(&self, network: &frp::Network, anchor: &frp::Stream<Vector2<f32>>) {
        match self {
            SearcherVariant::ComponentBrowser(view) => {
                frp::extend! {network
                    cb_position <- all_with(anchor, &view.expression_input_position, |anchor, pos| anchor - pos);
                    eval cb_position ((pos) view.set_xy(*pos));
                }
            }
            SearcherVariant::OldNodeSearcher(view) => {
                frp::extend! {network
                    searcher_pos <- all_with(anchor, &view.size, |anchor, size| {
                        let x = anchor.x + size.x / 2.0;
                        let y = anchor.y - node::HEIGHT / 2.0 - size.y / 2.0;
                        Vector2(x, y)
                    });
                    eval searcher_pos ((pos) view.set_xy(*pos));
                }
            }
        }
    }
}

impl display::Object for SearcherVariant {
    fn display_object(&self) -> &display::object::Instance {
        match self {
            SearcherVariant::ComponentBrowser(view) => view.display_object(),
            SearcherVariant::OldNodeSearcher(view) => view.display_object(),
        }
    }
}

#[derive(Clone, CloneRef, Debug)]
struct Model {
    app:                    Application,
    display_object:         display::object::Instance,
    /// These buttons are present only in a cloud environment.
    window_control_buttons: Immutable<Option<crate::window_control_buttons::View>>,
    graph_editor:           Rc<GraphEditor>,
    searcher:               SearcherVariant,
    code_editor:            code_editor::View,
    fullscreen_vis:         Rc<RefCell<Option<visualization::fullscreen::Panel>>>,
    open_dialog:            Rc<OpenDialog>,
    debug_mode_popup:       debug_mode_popup::View,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let searcher = SearcherVariant::new(app);
        let graph_editor = app.new_view::<GraphEditor>();
        let code_editor = app.new_view::<code_editor::View>();
        let fullscreen_vis = default();
        let debug_mode_popup = debug_mode_popup::View::new(app);
        let window_control_buttons = ARGS.is_in_cloud.as_some_from(|| {
            let window_control_buttons = app.new_view::<crate::window_control_buttons::View>();
            display_object.add_child(&window_control_buttons);
            scene.layers.panel.add(&window_control_buttons);
            window_control_buttons
        });
        let window_control_buttons = Immutable(window_control_buttons);
        let open_dialog = Rc::new(OpenDialog::new(app));

        display_object.add_child(&graph_editor);
        display_object.add_child(&code_editor);
        display_object.add_child(&searcher);
        display_object.add_child(&debug_mode_popup);
        display_object.remove_child(&searcher);

        let app = app.clone_ref();
        let graph_editor = Rc::new(graph_editor);
        Self {
            app,
            display_object,
            window_control_buttons,
            graph_editor,
            searcher,
            code_editor,
            fullscreen_vis,
            open_dialog,
            debug_mode_popup,
        }
    }

    /// Sets style of IDE to the one defined by parameter `theme`.
    ///
    /// This does not change the EnsoGL theme. Changing it is not supported currently because
    /// the theme is used for shader-precompilation.
    pub fn set_style(&self, theme: Theme) {
        match theme {
            Theme::Light => self.set_light_style(),
            _ => self.set_dark_style(),
        }
    }

    fn set_light_style(&self) {
        self.set_html_style("light-theme");
    }

    fn set_dark_style(&self) {
        self.set_html_style("dark-theme");
    }

    fn set_html_style(&self, style: &'static str) {
        web::document.with_element_by_id_or_warn("root", |root| root.set_class_name(style));
    }

    fn searcher_anchor_next_to_node(&self, node_id: NodeId) -> Vector2<f32> {
        if let Some(node) = self.graph_editor.nodes().get_cloned_ref(&node_id) {
            node.position().xy()
        } else {
            error!("Trying to show searcher under non existing node");
            default()
        }
    }

    /// Update Searcher View - its visibility and position - when edited node changed.
    fn update_searcher_view(
        &self,
        searcher_parameters: Option<SearcherParams>,
        is_searcher_empty: bool,
        searcher_left_top_position: &DEPRECATED_Animation<Vector2<f32>>,
    ) {
        match searcher_parameters {
            Some(SearcherParams { input, .. }) if !is_searcher_empty => {
                self.searcher.show();
                let new_position = self.searcher_anchor_next_to_node(input);
                searcher_left_top_position.set_target_value(new_position);
            }
            _ => {
                self.searcher.hide();
            }
        }
    }

    fn show_fullscreen_visualization(&self, node_id: NodeId) {
        let node = self.graph_editor.nodes().get_cloned_ref(&node_id);
        if let Some(node) = node {
            let visualization =
                node.view.model().visualization.fullscreen_visualization().clone_ref();
            self.display_object.remove_child(&*self.graph_editor);
            self.display_object.add_child(&visualization);
            *self.fullscreen_vis.borrow_mut() = Some(visualization);
        }
    }

    fn hide_fullscreen_visualization(&self) {
        if let Some(visualization) = std::mem::take(&mut *self.fullscreen_vis.borrow_mut()) {
            self.display_object.remove_child(&visualization);
            self.display_object.add_child(&*self.graph_editor);
        }
    }

    fn on_dom_shape_changed(&self, shape: &dom::shape::Shape) {
        // Top buttons must always stay in top-left corner.
        if let Some(window_control_buttons) = &*self.window_control_buttons {
            let pos = Vector2(-shape.width, shape.height) / 2.0;
            window_control_buttons.set_xy(pos);
        }
    }

    fn on_close_clicked(&self) {
        js::close(enso_config::window_app_scope_name);
    }

    fn on_fullscreen_clicked(&self) {
        js::fullscreen();
    }

    fn show_open_dialog(&self) {
        self.display_object.add_child(&*self.open_dialog);
    }

    fn hide_open_dialog(&self) {
        self.display_object.remove_child(&*self.open_dialog);
    }
}



mod js {
    // use super::*;
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen(inline_js = "
    export function close(windowAppScopeConfigName) {
        try { window[windowAppScopeConfigName].close(); }
        catch(e) {
            console.error(`Exception thrown from window.${windowAppScopeConfigName}.close:`,e)
        }
    }")]
    extern "C" {
        #[allow(unsafe_code)]
        pub fn close(window_app_scope_name: &str);
    }


    #[wasm_bindgen(inline_js = "
    export function fullscreen() {
        try {
            if(document.fullscreenElement === null)
                document.documentElement.requestFullscreen()
            else
                document.exitFullscreen()
        } catch (e) {
            console.error('Exception thrown when toggling fullscreen display mode:',e)
        }
    }
    ")]
    extern "C" {
        #[allow(unsafe_code)]
        pub fn fullscreen();
    }
}



// ============
// === View ===
// ============

/// The main view of single project opened in IDE.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct View {
    model:   Model,
    pub frp: Frp,
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl View {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let theme = match ARGS.theme.as_ref() {
            "dark" => Theme::Dark,
            _ => Theme::Light,
        };

        let scene = app.display.default_scene.clone_ref();
        let model = Model::new(app);
        let frp = Frp::new();
        let network = &frp.network;
        let searcher = &model.searcher.frp(network);
        let graph = &model.graph_editor.frp;
        let project_list = &model.open_dialog.project_list;
        let file_browser = &model.open_dialog.file_browser;
        let searcher_anchor = DEPRECATED_Animation::<Vector2<f32>>::new(network);

        // FIXME[WD]: Think how to refactor it, as it needs to be done before model, as we do not
        //   want shader recompilation. Model uses styles already.
        model.set_style(theme);
        // TODO[WD]: This should not be needed after the theme switching issue is implemented.
        //   See: https://github.com/enso-org/ide/issues/795
        let input_change_delay = frp::io::timer::Timeout::new(network);

        if let Some(window_control_buttons) = &*model.window_control_buttons {
            let initial_size = &window_control_buttons.size.value();
            model.graph_editor.input.space_for_window_buttons(initial_size);
            frp::extend! { network
                graph.space_for_window_buttons <+ window_control_buttons.size;
                eval_ window_control_buttons.close      (model.on_close_clicked());
                eval_ window_control_buttons.fullscreen (model.on_fullscreen_clicked());
            }
        }

        let shape = scene.shape().clone_ref();

        frp::extend! { network
            eval shape ((shape) model.on_dom_shape_changed(shape));

            // === Searcher Position and Size ===

            let main_cam = app.display.default_scene.layers.main.camera();
            let searcher_cam = app.display.default_scene.layers.node_searcher.camera();
            let main_cam_frp = &main_cam.frp();
            // We want to:
            // 1. Preserve the zoom factor of the searcher.
            // 2. Keep it directly below edited node at all times.
            // We do that by placing `node_searcher` camera in a position calculated by the
            // following equations:
            // ```
            // xy = main_cam.xy * main_cam.zoom
            // move_to_edited_node = edited_node.xy - edited_node.xy * main_cam.zoom
            // searcher_cam.z = const
            // searcher_cam.xy = xy + move_to_edited_node
            // ```
            // To understand the `move_to_edited_node` equation, consider the following example:
            // If edited_node.x = 100, zoom = 0.1, then the node is positioned at
            // x = 100 * 0.1 = 10 in searcher_cam-space. To compensate for that, we need to move
            // searcher (or rather searcher_cam) by 90 units, so that the node is at x = 100 both
            // in searcher_cam- and in main_cam-space.
            searcher_cam_pos <- all_with3
                (&main_cam_frp.position, &main_cam_frp.zoom, &searcher_anchor.value,
                |&main_cam_pos, &zoom, &searcher_pos| {
                    let preserve_zoom = (main_cam_pos * zoom).xy();
                    let move_to_edited_node = searcher_pos * (1.0 - zoom);
                    preserve_zoom + move_to_edited_node
                });
            eval searcher_cam_pos ((pos) searcher_cam.set_xy(*pos));

            eval searcher.is_visible ([model](is_visible) {
                let is_attached = model.searcher.has_parent();
                if !is_attached && *is_visible {
                    model.display_object.add_child(&model.searcher);
                } else if is_attached && !is_visible {
                    model.display_object.remove_child(&model.searcher);
                }
            });


            // === Closing Searcher

            frp.source.is_searcher_opened <+ frp.searcher.map(|s| s.is_some());
            last_searcher <- frp.searcher.filter_map(|&s| s);

            finished_with_searcher <- graph.node_editing_finished.gate(&frp.is_searcher_opened);
            frp.source.searcher <+ frp.close_searcher.constant(None);
            frp.source.searcher <+ searcher.editing_committed.constant(None);
            frp.source.searcher <+ finished_with_searcher.constant(None);

            aborted_in_searcher <- frp.close_searcher.map2(&last_searcher, |(), &s| s.input);
            frp.source.editing_aborted <+ aborted_in_searcher;
        }

        match &model.searcher {
            SearcherVariant::ComponentBrowser(browser) => {
                let grid = &browser.model().list.model().grid;
                frp::extend! { network
                    committed_in_browser <- grid.expression_accepted.map2(&last_searcher, |&entry, &s| (s.input, Some(entry)));
                    frp.source.editing_committed <+ committed_in_browser;
                    frp.source.editing_committed <+ finished_with_searcher.map(|id| (*id,None));
                }
            }
            SearcherVariant::OldNodeSearcher(searcher) => {
                frp::extend! { network
                    committed_in_searcher <- searcher.editing_committed.map2(&last_searcher, |&entry, &s| (s.input, entry));
                    frp.source.editing_committed_old_searcher <+ committed_in_searcher;
                    frp.source.editing_committed_old_searcher <+ finished_with_searcher.map(|id| (*id,None));
                }
            }
        }

        frp::extend! { network
            committed_in_searcher_event <- searcher.editing_committed.constant(());
            aborted_in_searcher_event <- aborted_in_searcher.constant(());
            graph.stop_editing <+ any(&committed_in_searcher_event, &aborted_in_searcher_event);


            // === Editing ===

            existing_node_edited <- graph.node_being_edited.filter_map(|x| *x).gate_not(&frp.adding_new_node);
            frp.source.searcher <+ existing_node_edited.map(
                |&node| Some(SearcherParams::new_for_edited_node(node))
            );
            searcher_input_change_opt <- graph.node_expression_set.map2(&frp.searcher, |(node_id, expr), searcher| {
                (searcher.as_ref()?.input == *node_id).then(|| expr.clone_ref())
            });
            searcher_input_change <- searcher_input_change_opt.filter_map(|expr| expr.clone());
            input_change_delay.restart <+ searcher_input_change.constant(INPUT_CHANGE_DELAY_MS);
            update_searcher_input_on_commit <- frp.output.editing_committed.constant(());
            input_change_delay.cancel <+ update_searcher_input_on_commit;
            update_searcher_input <- any(&input_change_delay.on_expired, &update_searcher_input_on_commit);
            frp.source.searcher_input_changed <+ searcher_input_change.sample(&update_searcher_input);


            // === Adding Node ===

            node_added_by_user <- graph.node_added.filter(|(_, _, should_edit)| *should_edit);
            searcher_for_adding <- node_added_by_user.map(
                |&(node, src, _)| SearcherParams::new_for_new_node(node, src)
            );
            frp.source.adding_new_node <+ searcher_for_adding.to_true();
            new_node_edited <- graph.node_editing_started.gate(&frp.adding_new_node);
            frp.source.searcher <+ searcher_for_adding.sample(&new_node_edited).map(|&s| Some(s));

            adding_committed_new_searcher <- frp.editing_committed.map(|(id,_)| *id);
            adding_committed_old_searcher <- frp.editing_committed_old_searcher.map(|(id,_)| *id);
            adding_committed <- any(&adding_committed_new_searcher,&adding_committed_old_searcher).gate(&frp.adding_new_node);
            adding_aborted <- frp.editing_aborted.gate(&frp.adding_new_node);
            adding_finished <- any(adding_committed,adding_aborted);
            frp.source.adding_new_node <+ adding_finished.constant(false);
            frp.source.searcher <+ adding_finished.constant(None);

            eval adding_committed ([graph](node) {
                graph.deselect_all_nodes();
                graph.select_node(node);
            });


            // === Searcher Position and Visibility ===

            visibility_conditions <- all(&frp.searcher,&searcher.is_empty);
            _eval                 <- visibility_conditions.map2(&searcher.is_visible,
                f!([model,searcher_anchor]((searcher,is_searcher_empty),is_visible) {
                    model.update_searcher_view(*searcher,*is_searcher_empty,&searcher_anchor);
                    if !is_visible {
                        // Do not animate
                        searcher_anchor.skip();
                    }
                })
            );

            _eval <- graph.output.node_position_set.map2(&frp.searcher,
                f!([searcher_anchor](&(node_id, position), &searcher) {
                    if searcher.map_or(false, |s| s.input == node_id) {
                        searcher_anchor.set_target_value(position);
                    }
                })
            );


            // === Opening Open File or Project Dialog ===

            eval_ frp.show_open_dialog  (model.show_open_dialog());
            project_chosen   <- project_list.chosen_entry.constant(());
            file_chosen      <- file_browser.entry_chosen.constant(());
            mouse_down       <- scene.mouse.frp.down.constant(());
            clicked_on_bg    <- mouse_down.filter(f_!(scene.mouse.target.get().is_background()));
            should_be_closed <- any(frp.close_open_dialog,project_chosen,file_chosen,clicked_on_bg);
            eval_ should_be_closed (model.hide_open_dialog());

            frp.source.open_dialog_shown <+ bool(&should_be_closed,&frp.show_open_dialog);


            // === Style toggle ===

            let style_toggle_ev   = frp.toggle_style.clone_ref();
            style_pressed        <- style_toggle_ev.toggle() ;
            style_was_pressed    <- style_pressed.previous();
            style_press          <- style_toggle_ev.gate_not(&style_was_pressed);
            style_press_on_off   <- style_press.map2(&frp.style, |_,s| match s {
                Theme::Light => Theme::Dark ,
                _            => Theme::Light,
            });
            frp.source.style     <+ style_press_on_off;
            eval frp.style ((style) model.set_style(style.clone()));


            // === Fullscreen Visualization ===

            // TODO[ao]: All DOM elements in visualizations ale displayed below canvas, because
            //     The mouse cursor must be displayed over them. But fullscreen visualization should
            //     be displayed "above" nodes. The workaround is to hide whole graph editor except
            //     fullscreen visualization, and bring it back when fullscreen is closed.
            //
            //     The workaround should be replaced with proper solution being a part of
            //     https://github.com/enso-org/ide/issues/526
            eval  graph.visualization_fullscreen ([model](node_id) {
                if let Some(node_id) = node_id {
                    model.show_fullscreen_visualization(*node_id)
                } else {
                    model.hide_fullscreen_visualization()
                }
            });

            init <- source::<()>();

            // === Disabling Navigation ===

            let documentation = model.searcher.documentation();
            searcher_active <- searcher.is_hovered || documentation.frp.is_selected;
            disable_navigation <- searcher_active || frp.open_dialog_shown;
            graph.set_navigator_disabled <+ disable_navigation;

            // === Disabling Dropping ===

            frp.source.drop_files_enabled <+ init.constant(true);
            frp.source.drop_files_enabled <+ frp.open_dialog_shown.map(|v| !v);

            // === Debug Mode ===

            frp.source.debug_mode <+ bool(&frp.disable_debug_mode, &frp.enable_debug_mode);
            graph.set_debug_mode <+ frp.source.debug_mode;

            model.debug_mode_popup.enabled <+ frp.enable_debug_mode;
            model.debug_mode_popup.disabled <+ frp.disable_debug_mode;
        }
        model.searcher.setup_anchor(network, &searcher_anchor.value);
        init.emit(());

        Self { model, frp }
    }

    /// Graph Editor View.
    pub fn graph(&self) -> &GraphEditor {
        &self.model.graph_editor
    }

    /// Searcher View.
    pub fn searcher(&self) -> &SearcherVariant {
        &self.model.searcher
    }

    /// Code Editor View.
    pub fn code_editor(&self) -> &code_editor::View {
        &self.model.code_editor
    }

    /// Open File or Project Dialog
    pub fn open_dialog(&self) -> &OpenDialog {
        &self.model.open_dialog
    }

    /// Debug Mode Popup
    pub fn debug_mode_popup(&self) -> &debug_mode_popup::View {
        &self.model.debug_mode_popup
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network {
        &self.frp.network
    }
}

impl application::View for View {
    fn label() -> &'static str {
        "ProjectView"
    }

    fn new(app: &Application) -> Self {
        View::new(app)
    }

    fn app(&self) -> &Application {
        &self.model.app
    }

    fn default_shortcuts() -> Vec<application::shortcut::Shortcut> {
        use shortcut::ActionType::*;
        [
            (Press, "!is_searcher_opened", "cmd o", "show_open_dialog"),
            (Press, "is_searcher_opened", "escape", "close_searcher"),
            (Press, "open_dialog_shown", "escape", "close_open_dialog"),
            (Press, "", "cmd alt shift t", "toggle_style"),
            (Press, "", "cmd s", "save_project_snapshot"),
            (Press, "", "cmd z", "undo"),
            (Press, "", "cmd y", "redo"),
            (Press, "", "cmd shift z", "redo"),
            (Press, "!debug_mode", DEBUG_MODE_SHORTCUT, "enable_debug_mode"),
            (Press, "debug_mode", DEBUG_MODE_SHORTCUT, "disable_debug_mode"),
            (Press, "", "cmd shift t", "execution_context_interrupt"),
            (Press, "", "cmd shift r", "execution_context_restart"),
        ]
        .iter()
        .map(|(a, b, c, d)| Self::self_shortcut_when(*a, *c, *d, *b))
        .collect()
    }
}
