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
use ensogl_component::text;
use ensogl_component::text::selection::Selection;
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
        editing_committed_old_searcher (NodeId, Option<searcher::entry::Id>),
        editing_committed              (NodeId, Option<component_list_panel::grid::GroupEntryId>),
        project_list_shown             (bool),
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

#[derive(Clone, CloneRef, Debug)]
struct Model {
    app:                    Application,
    display_object:         display::object::Instance,
    /// These buttons are present only in a cloud environment.
    window_control_buttons: Immutable<Option<crate::window_control_buttons::View>>,
    graph_editor:           Rc<GraphEditor>,
    searcher:               component_browser::View,
    code_editor:            code_editor::View,
    fullscreen_vis:         Rc<RefCell<Option<visualization::fullscreen::Panel>>>,
    project_list:           Rc<ProjectList>,
    debug_mode_popup:       debug_mode_popup::View,
    popup:                  popup::View,
}

impl Model {
    fn new(app: &Application) -> Self {
        let scene = &app.display.default_scene;
        let display_object = display::object::Instance::new();
        let searcher = app.new_view::<component_browser::View>();
        let graph_editor = app.new_view::<GraphEditor>();
        let code_editor = app.new_view::<code_editor::View>();
        let fullscreen_vis = default();
        let debug_mode_popup = debug_mode_popup::View::new(app);
        let popup = popup::View::new(app);
        let runs_in_web = ARGS.groups.startup.options.platform.value == "web";
        let window_control_buttons = runs_in_web.as_some_from(|| {
            let window_control_buttons = app.new_view::<crate::window_control_buttons::View>();
            display_object.add_child(&window_control_buttons);
            scene.layers.panel.add(&window_control_buttons);
            window_control_buttons
        });
        let window_control_buttons = Immutable(window_control_buttons);
        let project_list = Rc::new(ProjectList::new(app));

        display_object.add_child(&graph_editor);
        display_object.add_child(&code_editor);
        display_object.add_child(&searcher);
        display_object.add_child(&debug_mode_popup);
        display_object.add_child(&popup);
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

        let scene = app.display.default_scene.clone_ref();
        scene.begin_shader_initialization();
        let model = Model::new(app);
        let frp = Frp::new();
        let network = &frp.network;
        let searcher = &model.searcher.frp();
        let graph = &model.graph_editor.frp;
        let code_editor = &model.code_editor;
        let project_list = &model.project_list;
        let searcher_anchor = DEPRECATED_Animation::<Vector2<f32>>::new(network);

        // FIXME[WD]: Think how to refactor it, as it needs to be done before model, as we do not
        //   want shader recompilation. Model uses styles already.
        model.set_style(theme);
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
            init <- source::<()>();
            shape <- all(shape, init)._0();
            eval shape ((shape) model.on_dom_shape_changed(shape));

            eval_ frp.show_graph_editor(model.show_graph_editor());
            eval_ frp.hide_graph_editor(model.hide_graph_editor());


            // === Read-only mode ===

            graph.set_read_only <+ frp.set_read_only;
            code_editor.set_read_only <+ frp.set_read_only;


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

        let grid = &model.searcher.model().list.model().grid;
        frp::extend! { network
            committed_in_browser <- grid.expression_accepted.map2(&last_searcher, |&entry, &s| (s.input, Some(entry)));
            frp.source.editing_committed <+ committed_in_browser;
            frp.source.editing_committed <+ finished_with_searcher.map(|id| (*id,None));
        }

        let anchor = &searcher_anchor.value;
        frp::extend! { network
            committed_in_searcher_event <- searcher.editing_committed.constant(());
            aborted_in_searcher_event <- aborted_in_searcher.constant(());
            graph.stop_editing <+ any(&committed_in_searcher_event, &aborted_in_searcher_event);


            // === Editing ===

            node_edited_by_user <- graph.node_being_edited.gate_not(&frp.adding_new_node);
            existing_node_edited <- graph.node_expression_edited.gate_not(&frp.is_searcher_opened);
            open_searcher <- existing_node_edited.map2(&node_edited_by_user,
                |(id, _, _), edited| edited.map_or(false, |edited| *id == edited)
            ).on_true().debounce();
            cursor_position <- existing_node_edited.map2(
                &node_edited_by_user,
                |(node_id, _, selections), edited| {
                    edited.map_or(None, |edited| {
                        let position = || selections.last().map(|sel| sel.end).unwrap_or_default();
                        (*node_id == edited).then(position)
                    })
                }
            ).filter_map(|pos| *pos);
            edited_node <- node_edited_by_user.filter_map(|node| *node);
            position_and_edited_node <- cursor_position.map2(&edited_node, |pos, id| (*pos, *id));
            prepare_params <- position_and_edited_node.sample(&open_searcher);
            frp.source.searcher <+ prepare_params.map(|(pos, node_id)| {
                Some(SearcherParams::new_for_edited_node(*node_id, *pos))
            });
            searcher_input_change_opt <- graph.node_expression_edited.map2(&frp.searcher,
                |(node_id, expr, selections), searcher| {
                    let input_change = || (*node_id, expr.clone_ref(), selections.clone());
                    (searcher.as_ref()?.input == *node_id).then(input_change)
                }
            );
            searcher_input_change <- searcher_input_change_opt.unwrap();
            input_change_delay.restart <+ searcher_input_change.constant(INPUT_CHANGE_DELAY_MS);
            update_searcher_input_on_commit <- frp.output.editing_committed.constant(());
            input_change_delay.cancel <+ update_searcher_input_on_commit;
            update_searcher_input <- any(&input_change_delay.on_expired, &update_searcher_input_on_commit);
            input_change_and_searcher <- all_with(&searcher_input_change, &frp.searcher,
                |c, s| (c.clone(), *s)
            );
            updated_input <- input_change_and_searcher.sample(&update_searcher_input);
            input_changed <- updated_input.filter_map(|((node_id, expr, selections), searcher)| {
                let input_change = || (expr.clone_ref(), selections.clone());
                (searcher.as_ref()?.input == *node_id).then(input_change)
            });
            frp.source.searcher_input_changed <+ input_changed;

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

            cb_position <- all_with(anchor, &model.searcher.expression_input_position, |anchor, pos| anchor - pos);
            eval cb_position ((pos) model.searcher.set_xy(*pos));

            // === Project Dialog ===

            eval_ frp.show_project_list  (model.show_project_list());
            project_chosen   <- project_list.grid.entry_selected.constant(());
            mouse_down       <- scene.mouse.frp_deprecated.down.constant(());
            clicked_on_bg    <- mouse_down.filter(f_!(scene.mouse.target.get().is_background()));
            should_be_closed <- any(frp.hide_project_list,project_chosen,clicked_on_bg);
            eval_ should_be_closed (model.hide_project_list());
            frp.source.project_list_shown <+ bool(&should_be_closed,&frp.show_project_list);


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

            // === Disabling Navigation ===

            let documentation = &model.searcher.model().documentation;
            searcher_active <- searcher.is_hovered || documentation.frp.is_selected;
            disable_navigation <- searcher_active || frp.project_list_shown;
            graph.set_navigator_disabled <+ disable_navigation;

            // === Disabling Dropping ===

            frp.source.drop_files_enabled <+ init.constant(true);
            frp.source.drop_files_enabled <+ frp.project_list_shown.map(|v| !v);

            // === Debug Mode ===

            frp.source.debug_mode <+ bool(&frp.disable_debug_mode, &frp.enable_debug_mode);
            graph.set_debug_mode <+ frp.source.debug_mode;

            model.debug_mode_popup.enabled <+ frp.enable_debug_mode;
            model.debug_mode_popup.disabled <+ frp.disable_debug_mode;

            // === Error Pop-up ===

            model.popup.set_label <+ model.graph_editor.model.breadcrumbs.project_name_error;
        }

        init.emit(());

        Self { model, frp }
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
            (Press, "", "cmd r", "restore_project_snapshot"),
            (Press, "", "cmd z", "undo"),
            (Press, "", "cmd y", "redo"),
            (Press, "", "cmd shift z", "redo"),
            (Press, "!debug_mode", DEBUG_MODE_SHORTCUT, "enable_debug_mode"),
            (Press, "debug_mode", DEBUG_MODE_SHORTCUT, "disable_debug_mode"),
            (Press, "", "cmd shift t", "execution_context_interrupt"),
            (Press, "", "cmd shift r", "execution_context_restart"),
            // TODO(#6179): Remove this temporary shortcut when Play button is ready.
            (Press, "", "ctrl shift b", "toggle_read_only"),
        ]
        .iter()
        .map(|(a, b, c, d)| Self::self_shortcut_when(*a, *c, *d, *b))
        .collect()
    }
}
