//! The main view of single project opened in IDE.

use crate::prelude::*;

use crate::code_editor;
use crate::graph_editor::component::node;
use crate::graph_editor::component::node::Expression;
use crate::graph_editor::GraphEditor;
use crate::graph_editor::NodeId;
use crate::searcher;

use enso_frp as frp;
use ensogl::application;
use ensogl::application::Application;
use ensogl::application::shortcut;
use ensogl::display;
use ensogl::gui::component::Animation;



// =============
// === Model ===
// =============

#[derive(Clone,CloneRef,Debug)]
struct Model {
    app            : Application,
    logger         : Logger,
    display_object : display::object::Instance,
    graph_editor   : GraphEditor,
    searcher       : searcher::View,
    code_editor    : code_editor::View,
}

impl Model {
    fn new(app:&Application) -> Self {
        let logger         = Logger::new("project::View");
        let display_object = display::object::Instance::new(&logger);
        let searcher       = app.new_view::<searcher::View>();
        let graph_editor   = app.new_view::<GraphEditor>();
        let code_editor    = app.new_view::<code_editor::View>();
        display_object.add_child(&graph_editor);
        display_object.add_child(&code_editor);
        display_object.add_child(&searcher);
        display_object.remove_child(&searcher);
        let app = app.clone_ref();
        Self{app,logger,display_object,graph_editor,searcher,code_editor}
    }

    fn set_style(&self, is_light:bool) {
        if is_light { self.app.themes.set_enabled(&["dark"])  }
        else        { self.app.themes.set_enabled(&["light"]) }
    }

    fn searcher_left_top_position_when_under_node_at(position:Vector2<f32>) -> Vector2<f32> {
        let x = position.x;
        let y = position.y - node::NODE_HEIGHT/2.0;
        Vector2(x,y)
    }

    fn searcher_left_top_position_when_under_node(&self, node_id:NodeId) -> Vector2<f32> {
        if let Some(node) = self.graph_editor.model.nodes.get_cloned_ref(&node_id) {
            Self::searcher_left_top_position_when_under_node_at(node.position().xy())
        } else {
            error!(self.logger, "Trying to show searcher under nonexisting node");
            default()
        }
    }

    /// Update Searcher View - its visibility and position - when edited node changed.
    fn update_searcher_view
    (&self, edited_node:Option<NodeId>, searcher_left_top_position:&Animation<Vector2<f32>>) {
        if let Some(id) = edited_node {
            self.searcher.show();
            let new_position = self.searcher_left_top_position_when_under_node(id);
            searcher_left_top_position.set_target_value(new_position);
        } else {
            self.searcher.hide();
            self.searcher.clear_suggestions();
        }
    }

    fn add_node_and_edit(&self) {
        let graph_editor_inputs = &self.graph_editor.frp.input;
        graph_editor_inputs.add_node_at_cursor.emit(());
        let created_node_id = self.graph_editor.frp.output.node_added.value();
        graph_editor_inputs.set_node_expression.emit(&(created_node_id,Expression::default()));
        graph_editor_inputs.edit_node.emit(&created_node_id);
    }
}

// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Add new node and start editing it's expression.
        add_new_node(),
        /// Abort currently node edit. If it was added node, it will be removed, if the existing node was edited, its old expression will be restored.
        abort_node_editing(),
        /// Simulates a style toggle press event.
        toggle_style(),
        /// Saves the currently opened module to file.
        save_module(),
    }

    Output {
        adding_new_node               (bool),
        node_being_edited             (Option<NodeId>),
        editing_node                  (bool),
        old_expression_of_edited_node (Expression),
        editing_aborted               (NodeId),
        editing_committed             (NodeId),
        code_editor_shown             (bool),
        style_light                   (bool),
    }
}



// ============
// === View ===
// ============

/// The main view of single project opened in IDE.
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug)]
pub struct View {
    model   : Model,
    pub frp : Frp,
}

impl Deref for View {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl View {
    /// Constructor.
    pub fn new(app:&Application) -> Self {

        let model                      = Model::new(app);
        let frp                        = Frp::new_network();
        let searcher                   = &model.searcher.frp;
        let graph                      = &model.graph_editor.frp;
        let network                    = &frp.network;
        let searcher_left_top_position = Animation::<Vector2<f32>>::new(network);

        frp::extend!{ network
            // === Searcher Position and Size ===

            _eval <- all_with(&searcher_left_top_position.value,&searcher.size,f!([model](lt,size) {
                let x = lt.x + size.x / 2.0;
                let y = lt.y - size.y / 2.0;
                model.searcher.set_position_xy(Vector2(x,y));
            }));

            eval searcher.is_visible ([model](is_visible) {
                let is_attached = model.searcher.has_parent();
                if !is_attached && *is_visible {
                    model.display_object.add_child(&model.searcher);
                } else if is_attached && !is_visible {
                    model.display_object.remove_child(&model.searcher);
                }
            });


            // === Editing ===

            // The order of instructions below is important to properly distinguish between
            // committing and aborting node editing.

            // This node is false when received "abort_node_editing" signal, and should get true
            // once processing of "node_being_edited" event from graph is performed.
            editing_aborted <- any(...);
            editing_aborted <+ frp.abort_node_editing.constant(true);
            should_finish_editing_if_any <-
                any(frp.abort_node_editing,searcher.editing_committed,frp.add_new_node);
            should_finish_editing <- should_finish_editing_if_any.gate(&graph.output.node_editing);
            eval should_finish_editing ((()) graph.input.stop_editing.emit(()));
            _eval <- graph.output.node_being_edited.map2(&searcher.is_visible,
                f!([model,searcher_left_top_position](node_id,is_visible) {
                    model.update_searcher_view(*node_id,&searcher_left_top_position);
                    if !is_visible {
                        // Do not animate
                        searcher_left_top_position.skip();
                    }
                })
            );
            _eval <- graph.output.node_position_set.map2(&graph.output.node_being_edited,
                f!([searcher_left_top_position]((node_id,position),edited_node_id) {
                    if edited_node_id.contains(node_id) {
                        let new = Model::searcher_left_top_position_when_under_node_at(*position);
                        searcher_left_top_position.set_target_value(new);
                    }
                })
            );
            editing_not_aborted          <- editing_aborted.map(|b| !b);
            let editing_finished         =  graph.output.node_editing_finished.clone_ref();
            frp.source.editing_committed <+ editing_finished.gate(&editing_not_aborted);
            frp.source.editing_aborted   <+ editing_finished.gate(&editing_aborted);
            editing_aborted              <+ graph.output.node_being_edited.constant(false);

            frp.source.node_being_edited <+ graph.output.node_being_edited;
            frp.source.editing_node      <+ frp.node_being_edited.map(|n| n.is_some());

            // === Adding New Node ===

            frp.source.adding_new_node <+ frp.add_new_node.constant(true);
            eval frp.add_new_node ((()) model.add_node_and_edit());

            adding_committed           <- frp.editing_committed.gate(&frp.adding_new_node);
            adding_aborted             <- frp.editing_aborted.gate(&frp.adding_new_node);
            frp.source.adding_new_node <+ any(&adding_committed,&adding_aborted).constant(false);
            eval adding_aborted ((node) graph.remove_node.emit(node));

            // === Style toggle ===

            let style_toggle_ev     = frp.toggle_style.clone_ref();
            style_pressed          <- style_toggle_ev.toggle() ;
            style_was_pressed      <- style_pressed.previous();
            style_press            <- style_toggle_ev.gate_not(&style_was_pressed);
            style_press_on_off     <- style_press.map2(&frp.style_light, |_,is_light| !is_light);
            frp.source.style_light <+ style_press_on_off;
            eval frp.style_light ((is_light) model.set_style(*is_light));
        }

        Self{model,frp}
    }

    /// Graph Editor View.
    pub fn graph(&self) -> &GraphEditor { &self.model.graph_editor }

    /// Searcher View.
    pub fn searcher(&self) -> &searcher::View { &self.model.searcher }

    /// Code Editor View.
    pub fn code_editor(&self) -> &code_editor::View { &self.model.code_editor }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance { &self.model.display_object }
}

impl application::command::FrpNetworkProvider for View {
    fn network(&self) -> &frp::Network { &self.frp.network }
}

impl application::View for View {
    fn label() -> &'static str { "ProjectView" }

    fn new(app:&Application) -> Self { View::new(app) }

    fn app(&self) -> &Application {
        &self.model.app
    }

    fn default_shortcuts() -> Vec<application::shortcut::Shortcut> {
        use shortcut::ActionType::*;
        (&[ (Press   , "!editing_node" , "tab"          , "add_new_node")
          , (Press   , ""              , "escape"       , "abort_node_editing")
          , (Press   , ""              , "ctrl shift s" , "toggle_style")
          , (Release , ""              , "ctrl shift s" , "toggle_style")
          , (Press   , ""              , "cmd s"        , "save_module")
          ]).iter().map(|(a,b,c,d)|Self::self_shortcut_when(*a,*c,*d,*b)).collect()
    }
}
