//! UI entity that allows for the selection of a visualisation. Uses the
//! `drop_down_menu::DropDownMenu` but provides convenience functionality to extract map the
//! indices of the `DropDownMenu` to `Path` values.
//!
//! TODO: If similar things are needed elsewhere, refactor this to a
//! Chooser<T:Eq+Display> (or similar) which would represent a `DropDownMenu` for specific owned
//! values.

use crate::prelude::*;

use crate::component::visualization;

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
        set_entries         (Vec<visualization::Path>),
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
    }
}



// =============
// === Model ===
// =============

#[derive(Clone,Debug)]
struct Model {
    selection_menu             : drop_down_menu::DropDownMenu,
    visualization_alternatives : RefCell<Vec<visualization::Path>>,
}

impl Model {
    pub fn new(app:&Application) -> Self {
        let visualization_alternatives = default();
        let selection_menu             = drop_down_menu::DropDownMenu::new(&app);

        Self{visualization_alternatives,selection_menu}
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

/// UI entity that shows a button that opens a list of visualisations that can be sel:ected from.
#[derive(Clone,CloneRef,Debug)]
pub struct VisualizationChooser {
        model : Rc<Model>,
    pub frp   : Frp,
}

impl VisualizationChooser {
    pub fn new(app:&Application) -> Self {
        let frp   = Frp::new_network();
        let model = Rc::new(Model::new(app));
        Self {frp,model}.init()
    }

    fn init(self) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let model   = &self.model;
        let menu    = &self.model.selection_menu.frp;

        frp::extend! { network


            // === Input Processing ===

            eval frp.input.set_entries ([model,menu](alternatives) {
                model.visualization_alternatives.replace(alternatives.clone());
                let alternatives:list_view::entry::AnyModelProvider  = alternatives.clone().into();
                menu.set_entries.emit(alternatives);
            });

            eval frp.set_icon_size ((size) menu.set_icon_size.emit(size) );
            eval frp.set_icon_padding ((size) menu.set_icon_padding.emit(size) );
            eval frp.hide_selection_menu ((size) menu.hide_selection_menu.emit(size) );
            eval frp.set_menu_offset_y ((offset) menu.set_menu_offset_y.emit(offset) );

            eval frp.input.set_selected ([model,menu](selected) {
                if let Some(selected) = selected {
                   let ix = model.visualization_alternatives.borrow().iter().position(|item| item == selected);
                   menu.set_selected.emit(ix);
                }
            });

            // === Output Processing ===

            frp.source.mouse_over   <+ menu.icon_mouse_over;
            frp.source.mouse_out    <+ menu.icon_mouse_out;
            frp.source.menu_closed  <+ menu.menu_closed;
            frp.source.menu_visible <+ menu.menu_visible;

            selected_path <- model.selection_menu.frp.chosen_entry.map(f!([model](entry_id)({
                entry_id.map(|entry_id| {
                    model.visualization_alternatives.borrow().get(entry_id).cloned()
                }).flatten()
            })));

            frp.source.chosen_entry <+ selected_path;
        }
        self
    }
}

impl display::Object for VisualizationChooser {
    fn display_object(&self) -> &display::object::Instance {
        self.model.display_object()
    }
}
