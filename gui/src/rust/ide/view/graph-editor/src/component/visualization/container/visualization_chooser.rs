//! UI entity that allows for the selection of a visualisation. Uses the
//! `drop_down_menu::DropDownMenu` but provides convenience functionality to extract map the
//! indices of the `DropDownMenu` to `Path` values.
//!
//! TODO: If similar things are needed elsewhere, refactor this to a
//! Chooser<T:Eq+Display> (or similar) which would represent a `DropDownMenu` for specific owned
//! values.

use crate::prelude::*;

use crate::component::visualization;
use crate::data::enso;

use enso_frp as frp;
use enso_frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl_gui_components::list_view;
use ensogl_gui_components::drop_down_menu;



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        set_icon_size       (Vector2),
        set_icon_padding    (Vector2),
        hide_selection_menu (),
        set_selected        (Option<visualization::Path>),
        set_menu_offset_y   (f32),
    }

    Output {
        menu_visible  (bool),
        menu_closed   (),
        chosen_entry  (Option<visualization::Path>),
        mouse_over    (),
        mouse_out     (),
        entries       (Rc<Vec<visualization::Path>>)
    }
}



// =============
// === Model ===
// =============

#[derive(Clone,CloneRef,Debug)]
struct Model {
    selection_menu : drop_down_menu::DropDownMenu,
    registry       : visualization::Registry,
}

impl Model {
    pub fn new(app:&Application, registry:visualization::Registry) -> Self {
        let selection_menu = drop_down_menu::DropDownMenu::new(&app);
        app.display.scene().layers.below_main.add_exclusive(&selection_menu);
        Self{selection_menu,registry}
    }

    pub fn entries(&self) -> Vec <visualization::Path> {
        // TODO[ao]: The returned visualizations should take the current type on node into account.
        //           See https://github.com/enso-org/ide/issues/837
        let definitions_iter = self.registry.valid_sources(&enso::Type::any()).into_iter();
        definitions_iter.map(|d| d.signature.path).collect_vec()
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        self.selection_menu.display_object()
    }
}



// ============================
// === VisualisationChooser ===
// ============================

/// UI entity that shows a button that opens a list of visualisations that can be selected from.
#[derive(Clone,CloneRef,Debug)]
pub struct VisualizationChooser {
    pub frp : Frp,
    model   : Model,
}

impl VisualizationChooser {
    pub fn new(app:&Application, registry:visualization::Registry) -> Self {
        let frp   = Frp::new();
        let model = Model::new(app,registry);
        Self {frp,model}.init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let model   = &self.model;
        let menu    = &self.model.selection_menu.frp;

        frp::extend! { network

            // === Input Processing ===

            eval  frp.set_icon_size ((size) menu.set_icon_size.emit(size) );
            eval  frp.set_icon_padding ((size) menu.set_icon_padding.emit(size) );
            eval_ frp.hide_selection_menu ( menu.hide_selection_menu.emit(()) );
            eval  frp.set_menu_offset_y ((offset) menu.set_menu_offset_y.emit(offset) );

            set_selected_ix <= frp.input.set_selected.map2(&frp.output.entries,|selected,entries|
                selected.as_ref().map(|s| entries.iter().position(|item| item == s))
            );
            eval set_selected_ix ((ix) menu.set_selected.emit(ix));


            // === Showing Entries ===


            frp.source.entries <+ menu.menu_visible.gate(&menu.menu_visible).map(f_!([model] {
                let entries  = Rc::new(model.entries());
                let provider = list_view::entry::AnyModelProvider::from(entries.clone_ref());
                model.selection_menu.set_entries(provider);
                entries
            }));


            // === Output Processing ===

            frp.source.mouse_over   <+ menu.icon_mouse_over;
            frp.source.mouse_out    <+ menu.icon_mouse_out;
            frp.source.menu_closed  <+ menu.menu_closed;
            frp.source.menu_visible <+ menu.menu_visible;

            selected_path <- model.selection_menu.frp.chosen_entry.map2(&frp.output.entries,
                |entry_id,entries| entry_id.map(|entry_id| entries.get(entry_id).cloned()).flatten()
            );

            frp.source.chosen_entry <+ selected_path;

            eval frp.source.chosen_entry([](entry){
                if let Some(entry) = entry{
                    let event     = "graph_editor::visualization_chooser::vis_selected";
                    let name:&str = entry.name.as_ref();
                    let field     = "visualisation_name";
                    let data      = analytics::AnonymousData(|| name.to_string());
                    analytics::remote_log_value(event,field,data);
                }
            });
        }
        self
    }
}

impl display::Object for VisualizationChooser {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}
