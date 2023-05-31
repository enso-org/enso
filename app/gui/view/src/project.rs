//! The main view of single project opened in IDE.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::system::web::traits::*;

use crate::code_editor;
use crate::component_browser;
use crate::component_browser::component_list_panel;
use crate::debug_mode_popup;
use crate::debug_mode_popup::DEBUG_MODE_SHORTCUT;
use crate::graph_editor::component::node::Expression;
use crate::graph_editor::component::visualization;
use crate::graph_editor::GraphEditor;
use crate::graph_editor::NodeId;
use crate::popup;
use crate::project_list::ProjectList;

use enso_config::ARGS;
use enso_frp as frp;
use ensogl::application;
use ensogl::application::shortcut;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::Scene;
use ensogl::system::web;
use ensogl::Animation;
use ensogl_component::text;
use ensogl_component::text::selection::Selection;
use ensogl_hardcoded_theme::Theme;
use ide_view_graph_editor::NodeSource;
use project_view_top_bar::ProjectViewTopBar;


// ==============
// === Export ===
// ==============

pub mod project_view_top_bar;



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
    pub input:           NodeId,
    /// The node being a source for the edited node data - usually it's output shall be a `this`
    /// port for inserted expression.
    pub source_node:     Option<NodeSource>,
    /// A position of the cursor in the input node.
    pub cursor_position: text::Byte,
}

impl SearcherParams {
    fn new_for_new_node(node_id: NodeId, source_node: Option<NodeSource>) -> Self {
        Self { input: node_id, source_node, cursor_position: default() }
    }

    fn new_for_edited_node(node_id: NodeId, cursor_position: text::Byte) -> Self {
        Self { input: node_id, source_node: None, cursor_position }
    }
}

ensogl::define_endpoints! {
    Input {
        /// Open the Open Project Dialog.
        show_project_list(),
        /// Close the Open Project Dialog without further action
        hide_project_list(),
        /// Close the searcher without taking any actions
        close_searcher(),
        /// Simulates a style toggle press event.
        toggle_style(),
        /// Toggles the visibility of private components in the component browser.
        toggle_component_browser_private_entries_visibility(),
        /// Saves a snapshot of the current state of the project to the VCS.
        save_project_snapshot(),
        /// Restores the state of the project to the last snapshot saved to the VCS.
        restore_project_snapshot(),
        /// Undo the last user's action.
        undo(),
        /// Redo the last undone action.
        redo(),
        /// Enable Debug Mode of Graph Editor.
        enable_debug_mode(),
        /// Disable Debug Mode of Graph Editor.
        disable_debug_mode(),
        /// A set of value updates has been processed and rendered.
        values_updated(),
        /// Interrupt the running program.
        execution_context_interrupt(),
        /// Restart the program execution.
        execution_context_restart(),
        toggle_read_only(),
        set_read_only(bool),
    }

    Output {
        searcher                       (Option<SearcherParams>),
        /// The searcher input has changed and the Component Browser content should be refreshed.
        /// Is **not** emitted with every graph's node expression change, only when
        /// [`INPUT_CHANGE_DELAY_MS`] passes since last change, so we won't needlessly update the
        /// Component Browser when user is quickly typing in the input.
        searcher_input_changed         (ImString, Vec<Selection<text::Byte>>),
        is_searcher_opened             (bool),
        adding_new_node                (bool),
        old_expression_of_edited_node  (Expression),
        editing_aborted                (NodeId),
        editing_committed              (NodeId, Option<component_list_panel::grid::GroupEntryId>),
        project_list_shown             (bool),
        code_editor_shown              (bool),
        style                          (Theme),
        fullscreen_visualization_shown (bool),
        drop_files_enabled             (bool),
        debug_mode                     (bool),
        go_to_dashboard_button_pressed (),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
struct Model {
    app:                  Application,
    display_object:       display::object::Instance,
    project_view_top_bar: ProjectViewTopBar,
    graph_editor:         Rc<GraphEditor>,
    searcher:             component_browser::View,
    code_editor:          code_editor::View,
    fullscreen_vis:       Rc<RefCell<Option<visualization::fullscreen::Panel>>>,
    project_list:         Rc<ProjectList>,
    debug_mode_popup:     debug_mode_popup::View,
    popup:                popup::View,
}

impl Model {
    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let searcher = app.new_view::<component_browser::View>();
        let graph_editor = app.new_view::<GraphEditor>();
        let code_editor = app.new_view::<code_editor::View>();
        let fullscreen_vis = default();
        let debug_mode_popup = debug_mode_popup::View::new(app);
        let popup = popup::View::new(app);
        let project_view_top_bar = ProjectViewTopBar::new(app);
        let project_list = Rc::new(ProjectList::new(app));

        display_object.add_child(&graph_editor);
        display_object.add_child(&code_editor);
        display_object.add_child(&searcher);
        display_object.add_child(&debug_mode_popup);
        display_object.add_child(&popup);
        display_object.add_child(&project_view_top_bar);
        display_object.remove_child(&searcher);

        let app = app.clone_ref();
        let graph_editor = Rc::new(graph_editor);
        Self {
            app,
            display_object,
            project_view_top_bar,
            graph_editor,
            searcher,
            code_editor,
            fullscreen_vis,
            project_list,
            debug_mode_popup,
            popup,
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

    fn show_fullscreen_visualization(&self, node_id: NodeId) {
        let node = self.graph_editor.nodes().get_cloned_ref(&node_id);
        if let Some(node) = node {
            let visualization =
                node.view.model().visualization.fullscreen_visualization().clone_ref();
            self.display_object.remove_child(&*self.graph_editor);
            self.display_object.remove_child(&self.project_view_top_bar);
            self.display_object.add_child(&visualization);
            *self.fullscreen_vis.borrow_mut() = Some(visualization);
        }
    }

    fn hide_fullscreen_visualization(&self) {
        if let Some(visualization) = std::mem::take(&mut *self.fullscreen_vis.borrow_mut()) {
            self.display_object.remove_child(&visualization);
            self.display_object.add_child(&*self.graph_editor);
            self.display_object.add_child(&self.project_view_top_bar);
        }
    }

    fn position_project_view_top_bar(
        &self,
        scene_shape: &display::scene::Shape,
        project_view_top_bar_size: Vector2,
    ) {
        let top_left = Vector2(-scene_shape.width, scene_shape.height) / 2.0;
        let project_view_top_bar_origin = Vector2(
            0.0,
            crate::graph_editor::MACOS_TRAFFIC_LIGHTS_VERTICAL_CENTER
                - project_view_top_bar_size.y / 2.0,
        );
        self.project_view_top_bar.set_xy(top_left + project_view_top_bar_origin);
    }

    fn on_close_clicked(&self) {
        js::close(enso_config::window_app_scope_name);
    }

    fn on_fullscreen_clicked(&self) {
        js::fullscreen();
    }

    fn show_project_list(&self) {
        self.display_object.add_child(&*self.project_list);
    }

    fn hide_project_list(&self) {
        self.display_object.remove_child(&*self.project_list);
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
        let theme = match ARGS.groups.feature_preview.options.theme.value.as_ref() {
            "dark" => Theme::Dark,
            _ => Theme::Light,
        };

        let scene = &app.display.default_scene;
        scene.begin_shader_initialization();
        let model = Model::new(app);
        let frp = Frp::new();

        // FIXME[WD]: Think how to refactor it, as it needs to be done before model, as we do not
        //   want shader recompilation. Model uses styles already.
        model.set_style(theme);

        Self { model, frp }
            .init_top_bar_frp(scene)
            .init_graph_editor_frp()
            .init_code_editor_frp()
            .init_searcher_position_frp(scene)
            .init_searcher_input_changes_frp()
            .init_opening_searcher_frp()
            .init_closing_searcher_frp()
            .init_open_projects_dialog_frp(scene)
            .init_style_toggle_frp()
            .init_fullscreen_visualization_frp()
            .init_debug_mode_frp()
    }

    fn init_top_bar_frp(self, scene: &Scene) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let project_view_top_bar = &model.project_view_top_bar;
        frp::extend! { network
            init <- source_();
            let window_control_buttons = &project_view_top_bar.window_control_buttons;
            eval_ window_control_buttons.close (model.on_close_clicked());
            eval_ window_control_buttons.fullscreen (model.on_fullscreen_clicked());
            let go_to_dashboard_button = &project_view_top_bar.go_to_dashboard_button;
            frp.source.go_to_dashboard_button_pressed <+
                go_to_dashboard_button.is_pressed.on_true();

            let project_view_top_bar_display_object = project_view_top_bar.display_object();
            _eval <- all_with3(
                &init,
                scene.shape(),
                &project_view_top_bar_display_object.on_resized,
                f!((_, scene_shape, project_view_top_bar_size)
                   model.position_project_view_top_bar(scene_shape, *project_view_top_bar_size)
                )
            );
            project_view_top_bar_width <-
                project_view_top_bar_display_object.on_resized.map(|new_size| new_size.x);
            self.model.graph_editor.graph_editor_top_bar_offset_x <+ project_view_top_bar_width;
        }
        init.emit(());
        self
    }

    fn init_graph_editor_frp(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let graph = &model.graph_editor;
        let searcher = &model.searcher;
        let documentation = &searcher.model().documentation;

        frp::extend! { network
            // We block graph navigator if it interferes with other panels (searcher, documentation,
            // etc.)
            searcher_active <- searcher.is_hovered || documentation.frp.is_selected;
            disable_navigation <- searcher_active || frp.project_list_shown;
            graph.set_navigator_disabled <+ disable_navigation;

            model.popup.set_label <+ graph.model.breadcrumbs.project_name_error;
            graph.set_read_only <+ frp.set_read_only;
            graph.set_debug_mode <+ frp.source.debug_mode;

            frp.source.fullscreen_visualization_shown <+
                graph.output.visualization_fullscreen.is_some();
        }
        self
    }

    fn init_code_editor_frp(self) -> Self {
        let _network = &self.frp.network;
        frp::extend! { _network
            self.model.code_editor.set_read_only <+ self.frp.set_read_only;
        }
        self
    }

    fn init_searcher_position_frp(self, scene: &Scene) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let main_cam = scene.layers.main.camera();
        let searcher_cam = scene.layers.node_searcher.camera();
        let main_cam_frp = &main_cam.frp();
        let searcher = &self.model.searcher;
        let anchor = Animation::<Vector2<f32>>::new(network);

        frp::extend! { network
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
            searcher_cam_pos <- all_with3(
                &main_cam_frp.position,
                &main_cam_frp.zoom,
                &anchor.value,
                |&main_cam_pos, &zoom, &searcher_pos| {
                    let preserve_zoom = (main_cam_pos * zoom).xy();
                    let move_to_edited_node = searcher_pos * (1.0 - zoom);
                    preserve_zoom + move_to_edited_node
                });
            eval searcher_cam_pos ((pos) searcher_cam.set_xy(*pos));

            // Compute positions. It should be done _before_ showing searcher (or we display it at
            // wrong position).
            input <- frp.searcher.filter_map(|s| Some(s.as_ref()?.input));
            let node_position_set = model.graph_editor.output.node_position_set.clone_ref();
            is_input_position_update <-
                node_position_set.map2(&input, |&(node_id, _), &input_id| node_id == input_id);
            input_position_changed <- is_input_position_update.on_true();
            set_anchor_to_node <- all(input, input_position_changed)._0();
            anchor.target <+ set_anchor_to_node.map(f!((&input) model.searcher_anchor_next_to_node(input)));
            anchor.skip <+ set_anchor_to_node.gate_not(&searcher.is_visible).constant(());
            let searcher_offset = &model.searcher.expression_input_position;
            position <- all_with(&anchor.value, searcher_offset, |anchor, pos| anchor - pos);
            eval position ((pos) model.searcher.set_xy(*pos));

            // Showing searcher.
            searcher.show <+ frp.searcher.is_some().on_true().constant(());
            searcher.hide <+ frp.searcher.is_none().on_true().constant(());
            eval searcher.is_visible ([model](is_visible) {
                let is_attached = model.searcher.has_parent();
                match (is_attached, is_visible) {
                    (false, true) => model.display_object.add_child(&model.searcher),
                    (true, false) => model.display_object.remove_child(&model.searcher),
                    _ => ()
                }
            });
        }
        self
    }

    fn init_opening_searcher_frp(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let graph = &self.model.graph_editor;

        frp::extend! { network
            node_added_by_user <- graph.node_added.filter(|(_, _, should_edit)| *should_edit);
            searcher_for_adding <- node_added_by_user.map(
                |&(node, src, _)| SearcherParams::new_for_new_node(node, src)
            );
            frp.source.adding_new_node <+ searcher_for_adding.to_true();
            new_node_edited <- graph.node_editing_started.gate(&frp.adding_new_node);
            frp.source.searcher <+ searcher_for_adding.sample(&new_node_edited).some();

            edit_which_opens_searcher <-
                graph.node_expression_edited.gate_not(&frp.is_searcher_opened).debounce();
            frp.source.searcher <+ edit_which_opens_searcher.map(|(node_id, _, selections)| {
                let cursor_position = selections.last().map(|sel| sel.end).unwrap_or_default();
                Some(SearcherParams::new_for_edited_node(*node_id, cursor_position))
            });
            frp.source.is_searcher_opened <+ frp.searcher.map(|s| s.is_some());
        }
        self
    }

    fn init_closing_searcher_frp(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let grid = &self.model.searcher.model().list.model().grid;
        let graph = &self.model.graph_editor;

        frp::extend! { network
            last_searcher <- frp.searcher.filter_map(|&s| s);

            node_editing_finished <- graph.node_editing_finished.gate(&frp.is_searcher_opened);
            committed_in_searcher <-
                grid.expression_accepted.map2(&last_searcher, |&entry, &s| (s.input, entry));
            aborted_in_searcher <- frp.close_searcher.map2(&last_searcher, |(), &s| s.input);
            frp.source.editing_committed <+ committed_in_searcher;
            frp.source.editing_committed <+ node_editing_finished.map(|id| (*id,None));
            frp.source.editing_aborted <+ aborted_in_searcher;

            // Should be done before we update `searcher` and `adding_new_node` outputs.
            adding_committed <- committed_in_searcher.gate(&frp.adding_new_node);
            graph.deselect_all_nodes <+ adding_committed.constant(());
            graph.select_node <+ adding_committed._0();

            node_editing_finished_event <- node_editing_finished.constant(());
            committed_in_searcher_event <- committed_in_searcher.constant(());
            aborted_in_searcher_event <- aborted_in_searcher.constant(());
            searcher_should_close <- any(
                node_editing_finished_event,
                committed_in_searcher_event,
                aborted_in_searcher_event
            );
            graph.stop_editing <+ any(&committed_in_searcher_event, &aborted_in_searcher_event);
            frp.source.searcher <+ searcher_should_close.constant(None);
            frp.source.adding_new_node <+ searcher_should_close.constant(false);
        }
        self
    }

    fn init_searcher_input_changes_frp(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let graph = &self.model.graph_editor;
        let input_change_delay = frp::io::timer::Timeout::new(network);

        frp::extend! { network
            searcher_input_change_opt <- graph.node_expression_edited.map2(&frp.searcher,
                |(node_id, expr, selections), searcher| {
                    let input_change = || (*node_id, expr.clone_ref(), selections.clone());
                    (searcher.as_ref()?.input == *node_id).then(input_change)
                }
            );
            input_change <- searcher_input_change_opt.unwrap();
            input_change_delay.restart <+ input_change.constant(INPUT_CHANGE_DELAY_MS);
            update_input_on_commit <- frp.output.editing_committed.constant(());
            input_change_delay.cancel <+ update_input_on_commit;
            update_input <- any(&input_change_delay.on_expired, &update_input_on_commit);
            input_change_and_searcher <-
                all_with(&input_change, &frp.searcher, |c, s| (c.clone(), *s));
            updated_input <- input_change_and_searcher.sample(&update_input);
            input_changed <- updated_input.filter_map(|((node_id, expr, selections), searcher)| {
                let input_change = || (expr.clone_ref(), selections.clone());
                (searcher.as_ref()?.input == *node_id).then(input_change)
            });
            frp.source.searcher_input_changed <+ input_changed;
        }
        self
    }

    fn init_open_projects_dialog_frp(self, scene: &Scene) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let project_list = &model.project_list;
        frp::extend! { network
            eval_ frp.show_project_list  (model.show_project_list());
            project_chosen <- project_list.frp.selected_project.constant(());
            mouse_down <- scene.mouse.frp_deprecated.down.constant(());
            clicked_on_bg <- mouse_down.filter(f_!(scene.mouse.target.get().is_background()));
            should_be_closed <- any(frp.hide_project_list,project_chosen,clicked_on_bg);
            eval_ should_be_closed (model.hide_project_list());
            frp.source.project_list_shown <+ bool(&should_be_closed,&frp.show_project_list);
            frp.source.drop_files_enabled <+ frp.project_list_shown.map(|v| !v);
        }
        frp.source.drop_files_enabled.emit(true);
        self
    }

    fn init_style_toggle_frp(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        frp::extend! {network
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
        }
        self
    }

    fn init_fullscreen_visualization_frp(self) -> Self {
        let network = &self.frp.network;
        let model = &self.model;
        let graph = &self.model.graph_editor;
        frp::extend! { network
            // TODO[ao]: All DOM elements in visualizations are displayed below canvas, because
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
        }
        self
    }

    fn init_debug_mode_frp(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let popup = &self.model.debug_mode_popup;
        frp::extend! { network
            frp.source.debug_mode <+ bool(&frp.disable_debug_mode, &frp.enable_debug_mode);

            popup.enabled <+ frp.enable_debug_mode;
            popup.disabled <+ frp.disable_debug_mode;
        }
        self
    }

    /// Graph Editor View.
    pub fn graph(&self) -> &GraphEditor {
        &self.model.graph_editor
    }

    /// Searcher View.
    pub fn searcher(&self) -> &component_browser::View {
        &self.model.searcher
    }

    /// Code Editor View.
    pub fn code_editor(&self) -> &code_editor::View {
        &self.model.code_editor
    }

    /// Open Project Dialog
    pub fn project_list(&self) -> &ProjectList {
        &self.model.project_list
    }

    /// Debug Mode Popup
    pub fn debug_mode_popup(&self) -> &debug_mode_popup::View {
        &self.model.debug_mode_popup
    }

    /// Pop-up
    pub fn popup(&self) -> &popup::View {
        &self.model.popup
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
            (Press, "!is_searcher_opened", "cmd o", "show_project_list"),
            (Press, "is_searcher_opened", "escape", "close_searcher"),
            (Press, "project_list_shown", "escape", "hide_project_list"),
            (Press, "", "cmd alt shift t", "toggle_style"),
            (Press, "", "cmd alt p", "toggle_component_browser_private_entries_visibility"),
            (Press, "", "cmd s", "save_project_snapshot"),
            (Press, "", "cmd shift r", "restore_project_snapshot"),
            (Press, "", "cmd z", "undo"),
            (Press, "", "cmd y", "redo"),
            (Press, "", "cmd shift z", "redo"),
            (Press, "!debug_mode", DEBUG_MODE_SHORTCUT, "enable_debug_mode"),
            (Press, "debug_mode", DEBUG_MODE_SHORTCUT, "disable_debug_mode"),
            (Press, "", "cmd alt t", "execution_context_interrupt"),
            (Press, "", "cmd alt r", "execution_context_restart"),
            // TODO(#6179): Remove this temporary shortcut when Play button is ready.
            (Press, "", "ctrl shift b", "toggle_read_only"),
        ]
        .iter()
        .map(|(a, b, c, d)| Self::self_shortcut_when(*a, *c, *d, *b))
        .collect()
    }
}
