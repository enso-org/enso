//! The main view of single project opened in IDE.

use crate::prelude::*;
use ensogl::display::shape::*;
use ensogl::system::web::traits::*;

use crate::code_editor;
use crate::component_browser;
use crate::component_browser::component_list_panel;
use crate::graph_editor::component::node::Expression;
use crate::graph_editor::component::visualization;
use crate::graph_editor::GraphEditor;
use crate::graph_editor::NodeId;
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
use ide_view_project_view_top_bar::ProjectViewTopBar;



// ==============
// === Export ===
// ==============



// =================
// === Constants ===
// =================

/// A time which must pass since last change of expression of node which is the searcher input
/// to send `searcher_input_changed` event. The delay ensures we don't needlessly update Component
/// Browser when user is quickly typing in the expression input.
const INPUT_CHANGE_DELAY_MS: u32 = 200;


/// Mitigate limitations of constant strings concatenation.
macro_rules! define_debug_mode_shortcut {
    ($shortcut:literal) => {
        /// A keyboard shortcut used to enable/disable Debug Mode.
        pub const DEBUG_MODE_SHORTCUT: &str = $shortcut;
        const DEBUG_MODE_ENABLED: &str =
            concat!("Debug Mode enabled. To disable, press `", $shortcut, "`.");
    };
}

define_debug_mode_shortcut!("ctrl shift d");


// ===========
// === FRP ===
// ===========

/// The searcher that should be displayed.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub enum SearcherType {
    /// The Searcher with Component Browser.
    #[default]
    ComponentBrowser,
    /// The Searcher with AI completion.
    AiCompletion,
}

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
    /// The type of the searcher.
    pub searcher_type:   SearcherType,
}

impl SearcherParams {
    fn new_for_new_node(
        node_id: NodeId,
        source_node: Option<NodeSource>,
        searcher_type: SearcherType,
    ) -> Self {
        Self { input: node_id, source_node, cursor_position: default(), searcher_type }
    }

    fn new_for_edited_node(
        node_id: NodeId,
        cursor_position: text::Byte,
        searcher_type: SearcherType,
    ) -> Self {
        Self { input: node_id, source_node: None, cursor_position, searcher_type }
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
        /// Show the graph editor.
        show_graph_editor(),
        /// Hide the graph editor.
        hide_graph_editor(),
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
        /// Reload the main module and restart the program execution.
        execution_context_reload_and_restart(),
        toggle_read_only(),
        set_read_only(bool),
        /// Started creation of a new node using the AI searcher.
        start_node_creation_with_ai_searcher(),
        /// Started creation of a new node using the Component Browser.
        start_node_creation_with_component_browser(),
        /// Accepts the currently selected input of the searcher.
        accept_searcher_input(),
    }

    Output {
        /// The type of the searcher currently in use.
        searcher_type                  (SearcherType),
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
        // TODO[MM]: this should not contain the group entry id as that is component browser
        // specific. It should be refactored to be an implementation detail of the component
        // browser.
        editing_committed              (NodeId, Option<component_list_panel::grid::EntryId>),
        project_list_shown             (bool),
        code_editor_shown              (bool),
        style                          (Theme),
        fullscreen_visualization_shown (bool),
        drop_files_enabled             (bool),
        debug_mode                     (bool),
        go_to_dashboard_button_pressed (),
        /// The name of the command currently being handled due to shortcut being pressed.
        current_shortcut               (Option<ImString>),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug, display::Object)]
struct Model {
    display_object:   display::object::Instance,
    top_bar:          ProjectViewTopBar,
    graph_editor:     Rc<GraphEditor>,
    searcher:         component_browser::View,
    code_editor:      code_editor::View,
    fullscreen_vis:   Rc<RefCell<Option<visualization::fullscreen::Panel>>>,
    project_list:     Rc<ProjectList>,
    debug_mode_popup: Rc<crate::notification::View>,
}

impl Model {
    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let searcher = app.new_view::<component_browser::View>();
        let graph_editor = app.new_view::<GraphEditor>();
        let code_editor = app.new_view::<code_editor::View>();
        let fullscreen_vis = default();
        let debug_mode_popup = Rc::new(crate::notification::View::new(app));
        let project_view_top_bar = ProjectViewTopBar::new(app);
        let project_list = Rc::new(ProjectList::new(app));

        display_object.add_child(&graph_editor);
        display_object.add_child(&code_editor);
        display_object.add_child(&searcher);
        display_object.add_child(&project_view_top_bar);
        display_object.remove_child(&searcher);

        let graph_editor = Rc::new(graph_editor);
        Self {
            display_object,
            top_bar: project_view_top_bar,
            graph_editor,
            searcher,
            code_editor,
            fullscreen_vis,
            project_list,
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
        self.graph_editor.model.with_node(node_id, |node| node.position().xy()).unwrap_or_default()
    }

    fn show_fullscreen_visualization(&self, node_id: NodeId) {
        self.graph_editor.model.with_node(node_id, |node| {
            let visualization =
                node.view.model().visualization.fullscreen_visualization().clone_ref();
            self.display_object.remove_child(&*self.graph_editor);
            self.display_object.remove_child(&self.top_bar);
            self.display_object.add_child(&visualization);
            *self.fullscreen_vis.borrow_mut() = Some(visualization);
        });
    }

    fn hide_fullscreen_visualization(&self) {
        if let Some(visualization) = std::mem::take(&mut *self.fullscreen_vis.borrow_mut()) {
            self.display_object.remove_child(&visualization);
            self.display_object.add_child(&*self.graph_editor);
            self.display_object.add_child(&self.top_bar);
        }
    }

    fn position_project_view_top_bar(
        &self,
        scene_shape: &display::scene::Shape,
        project_view_top_bar_size: Vector2,
    ) {
        let top_left = Vector2(-scene_shape.width, scene_shape.height) / 2.0;
        let y = -project_view_top_bar_size.y;
        let x = ARGS.groups.window.options.top_bar_offset.value;
        let project_view_top_bar_origin = Vector2(x as f32, y);
        self.top_bar.set_xy(top_left + project_view_top_bar_origin);
    }

    fn show_project_list(&self) {
        self.display_object.add_child(&*self.project_list);
    }

    fn hide_project_list(&self) {
        self.display_object.remove_child(&*self.project_list);
    }

    fn show_graph_editor(&self) {
        self.display_object.add_child(&*self.graph_editor);
    }

    fn hide_graph_editor(&self) {
        self.display_object.remove_child(&*self.graph_editor);
    }
}



// ============
// === View ===
// ============

/// The main view of single project opened in IDE.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, display::Object)]
pub struct View {
    #[display_object]
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
            .init_start_node_edit_frp()
            .init_top_bar_frp(scene)
            .init_graph_editor_frp()
            .init_code_editor_frp()
            .init_searcher_position_frp(scene)
            .init_searcher_input_handling()
            .init_opening_searcher_frp()
            .init_open_projects_dialog_frp(scene)
            .init_style_toggle_frp()
            .init_fullscreen_visualization_frp()
            .init_debug_mode_frp()
            .init_shortcut_observer(app)
            .init_execution_environment_selector_frp()
    }

    fn init_execution_environment_selector_frp(self) -> Self {
        crate::graph_editor::execution_environment::init_frp(
            &self.model.graph_editor.frp,
            &self.model.top_bar.project_name_with_environment_selector.selector,
        );

        self
    }

    fn init_top_bar_frp(self, scene: &Scene) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let project_view_top_bar = &model.top_bar;
        frp::extend! { network
            init <- source_();

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
            eval_ frp.show_graph_editor(model.show_graph_editor());
            eval_ frp.hide_graph_editor(model.hide_graph_editor());

            // We block graph navigator if it interferes with other panels (searcher, documentation,
            // etc.)
            searcher_active <- searcher.is_hovered || documentation.frp.is_selected;
            disable_navigation <- searcher_active || frp.project_list_shown;
            graph.set_navigator_disabled <+ disable_navigation;

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
            searcher.show <+ frp.searcher.unwrap().map(|params|
                matches!(params.searcher_type, SearcherType::ComponentBrowser)).on_true();
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
            searcher_for_adding <- node_added_by_user.map2(&frp.searcher_type,
                |&(node, src, _), searcher_type| SearcherParams::new_for_new_node(node, src, *searcher_type)
            );
            frp.source.adding_new_node <+ searcher_for_adding.to_true();
            new_node_edited <- graph.node_editing_started.gate(&frp.adding_new_node);
            frp.source.searcher <+ searcher_for_adding.sample(&new_node_edited).some();

            edit_which_opens_searcher <-
                graph.node_expression_edited.gate_not(&frp.is_searcher_opened).debounce();
            frp.source.searcher <+ edit_which_opens_searcher.map2(&frp.searcher_type,
                |(node_id, _, selections), searcher_type| {
                let cursor_position = selections.last().map(|sel| sel.end).unwrap_or_default();
                Some(SearcherParams::new_for_edited_node(*node_id, cursor_position, *searcher_type))
            });
            frp.source.is_searcher_opened <+ frp.searcher.map(|s| s.is_some());
        }
        self
    }

    /// Handles changes to the searcher input and accepting the input.
    fn init_searcher_input_handling(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let grid = &self.model.searcher.model().list.model().grid;
        let graph = &self.model.graph_editor;
        let input_change_delay = frp::io::timer::Timeout::new(network);

        frp::extend! { network

            last_searcher <- frp.searcher.filter_map(|&s| s);

            // === Handling Inputs to the Searcher and Committing Edit ===

            ai_searcher_active <- frp.searcher_type.map(|t| *t == SearcherType::AiCompletion);
            // Note: the "enter" event for the CB searcher is handled in its own view.
            committed_in_ai_searcher <- frp.accept_searcher_input.gate(&ai_searcher_active);
            committed_in_ai_searcher <- committed_in_ai_searcher.map2(&last_searcher, |_, &s| (s.input, None));

            // The searcher will be closed due to accepting the input (e.g., pressing enter).
            committed_in_cb_searcher <-
                grid.expression_accepted.map2(&last_searcher, |&entry, &s| (s.input, entry));

            committed_in_searcher <- any(committed_in_ai_searcher, committed_in_cb_searcher);
            searcher_input_change_opt <- graph.node_expression_edited.map2(&frp.searcher,
                |(node_id, expr, selections), searcher| {
                    let input_change = || (*node_id, expr.clone_ref(), selections.clone());
                    (searcher.as_ref()?.input == *node_id).then(input_change)
                }
            ).on_change();
            input_change <- searcher_input_change_opt.unwrap();
            // We wait with processing an updated input to avoid unnecessary refreshes during
            // typing. This is done by delaying the input change by a short amount of time.
            input_change_delay.restart <+ input_change.constant(INPUT_CHANGE_DELAY_MS);

            // When accepting the input, we need to make sure we correctly process any outstanding
            // key presses. If we don't, we may end up with a stale selection. But we do not want
            // to refresh the searcher if we don't have outstanding key presses, as this will
            // cause the searcher to lose the selected entry and instead select the default entry.

            let needs_refresh = input_change_delay.is_running;
            // We have retained key presses that we need to process.
            update_with_refresh <- committed_in_searcher.gate(&needs_refresh);
            // No key presses retained, accept the selection as is.
            update_without_refresh <- committed_in_searcher.gate_not(&needs_refresh);

            on_update_with_refresh <- update_with_refresh.constant(());
            // We only need to cancel the delay if the timer is running, that is, if we have
            // unprocessed changes and need to refresh.
            input_change_delay.cancel <+ on_update_with_refresh;

            update_input <- any(&input_change_delay.on_expired, &on_update_with_refresh);
            input_change_and_searcher <-
                all_with(&input_change, &frp.searcher, |c, s| (c.clone(), *s));
            updated_input <- input_change_and_searcher.sample(&update_input);
            input_changed <- updated_input.filter_map(|((node_id, expr, selections), searcher)| {
                let input_change = || (expr.clone_ref(), selections.clone());
                (searcher.as_ref()?.input == *node_id).then(input_change)
            });
            frp.source.searcher_input_changed <+ input_changed;

            // Since the input was updated, the selected item might have been replaced, so we need
            // discard it and instead use the currently selected item. Note that this will be the
            // default item, and we loose any selection that was made. But that is fine, as the
            // user was typing and a change to the selection was to be expected.
            frp.source.editing_committed <+ on_update_with_refresh.map3(
                &committed_in_searcher,&grid.active,
                |_, (node_id, _), &entry| (*node_id, entry));

            // If we have no outstanding key presses, we can accept the selection as is.
            frp.source.editing_committed <+ committed_in_searcher.sample(&update_without_refresh);


            // === Closing the Searcher / End of Editing ===

            // The searcher will be closed due to no longer editing the node (e.g., a click
            // on the background of the scene).
            node_editing_finished <- graph.node_editing_finished.gate(&frp.is_searcher_opened);
            aborted_in_searcher <- frp.close_searcher.map2(&last_searcher, |(), &s| s.input);
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
            frp.source.searcher_type <+ searcher_should_close.constant(SearcherType::default());
            frp.source.adding_new_node <+ searcher_should_close.constant(false);
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
            project_chosen <- project_list.grid.entry_selected.constant(());
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

        let mut options = crate::notification::api::UpdateOptions::default();
        options.set_always_present();
        options.set_raw_text_content(DEBUG_MODE_ENABLED);
        options.position = Some(crate::notification::api::Position::BottomRight);
        popup.set_options(options);

        frp::extend! { network
            debug_mode <- bool(&frp.disable_debug_mode, &frp.enable_debug_mode);
            frp.source.debug_mode <+ debug_mode;
            popup.is_enabled <+ debug_mode;
        }
        self
    }

    fn init_start_node_edit_frp(self) -> Self {
        let frp = &self.frp;
        let network = &frp.network;
        let graph_editor = &self.model.graph_editor.frp;
        frp::extend! { network
            // Searcher type to use for node creation.
            // Debounces are needed, because there are shortcut conflits with Component Browser's
            // internals, causing CB being closed immediately after opening.
            // TODO[ao] This is a hotfix, and the proper fix is tracked by
            //          https://github.com/enso-org/enso/issues/7528
            ai_searcher <- frp.start_node_creation_with_ai_searcher.constant(SearcherType::AiCompletion).debounce();
            component_browser_searcher <- frp.start_node_creation_with_component_browser.constant(SearcherType::ComponentBrowser).debounce();
            searcher_type <- any(&ai_searcher, &component_browser_searcher);

            frp.source.searcher_type <+ searcher_type;

            should_not_create_node <- graph_editor.node_editing || graph_editor.read_only;
            should_not_create_node <- should_not_create_node || graph_editor.is_fs_visualization_displayed;
            start_node_creation <- searcher_type.gate_not(&should_not_create_node);
            graph_editor.start_node_creation <+ start_node_creation.constant(());
        }
        self
    }

    fn init_shortcut_observer(self, app: &Application) -> Self {
        let frp = &self.frp;
        frp::extend! { network
            frp.source.current_shortcut <+ app.shortcuts.currently_handled;
        }

        self
    }

    /// Top Bar View.
    pub fn top_bar(&self) -> &ProjectViewTopBar {
        &self.model.top_bar
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

    fn global_shortcuts() -> Vec<application::shortcut::Shortcut> {
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
            // TODO(#7178): Remove this temporary shortcut when the modified-on-disk notification
            // is ready.
            (Press, "", "cmd alt y", "execution_context_reload_and_restart"),
            (Press, "!is_searcher_opened", "cmd tab", "start_node_creation_with_ai_searcher"),
            (Press, "!is_searcher_opened", "enter", "start_node_creation_with_component_browser"),
            (Press, "is_searcher_opened", "enter", "accept_searcher_input"),
            (Press, "debug_mode", "ctrl shift enter", "debug_push_breadcrumb"),
            (Press, "debug_mode", "ctrl shift b", "debug_pop_breadcrumb"),
        ]
        .iter()
        .map(|(a, b, c, d)| Self::self_shortcut_when(*a, *c, *d, *b))
        .collect()
    }
}
