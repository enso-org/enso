//! Drop Down Menu Component.
use ensogl_core::prelude::*;

use crate::list_view;
use crate::list_view::ListView;
use crate::card::Card;
use crate::card;

use enso_frp as frp;
use enso_frp;
use ensogl_core::DEPRECATED_Animation;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::shape::*;
use ensogl_core::display::shape::primitive::StyleWatch;
use ensogl_core::display;
use ensogl_text as text;
use ensogl_theme as theme;


// =================
// === Constants ===
// =================

/// Invisible dummy color to catch hover events.
const HOVER_COLOR : color::Rgba = color::Rgba::new(1.0,0.0,0.0,0.000_001);
/// The width of the visualisation selection menu.
const MENU_WIDTH  : f32         = 180.0;



// ==============
// === Shapes ===
// ==============

/// Arrow icon that indicates the drop down menu.
pub mod arrow {
    use super::*;

    ensogl_core::define_shape_system! {
        below = [chooser_hover_area];
        (style:Style) {
            let width            = Var::<Pixels>::from("input_size.x");
            let height           = Var::<Pixels>::from("input_size.y");
            let triangle         = Triangle(width,height);
            let triangle_down    = triangle.rotate(Var::<f32>::from(std::f32::consts::PI));
            let color_path       = ensogl_theme::graph_editor::visualization::action_bar::icon;
            let icon_color       = style.get_color(color_path);
            let triangle_colored = triangle_down.fill(icon_color);

            triangle_colored.into()
        }
    }
}

/// Invisible rectangular area around the icon
pub mod chooser_hover_area {
    use super::*;

    ensogl_core::define_shape_system! {
        () {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let background           = Rect((&width,&height));
            let background           = background.fill(HOVER_COLOR);
            background.into()
        }
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_entries         (Rc<Vec<String>>),
        set_icon_size       (Vector2),
        set_icon_padding    (Vector2),
        hide_selection_menu (),
        set_selected        (Option<list_view::entry::Id>),
        set_menu_offset_y   (f32),
    }
    Output {
        menu_visible    (bool),
        menu_closed     (),
        chosen_entry    (Option<list_view::entry::Id>),
        icon_mouse_over (),
        icon_mouse_out  (),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone,Debug)]
struct Model {
    logger         : Logger,
    app            : Application,
    display_object : display::object::Instance,

    icon           : arrow::View,
    icon_overlay   : chooser_hover_area::View,

    label          : text::Area,
    selection_menu : ListView,
    background     : Card,

    content        : RefCell<Option<Rc<Vec<String>>>>,
}

impl Model {
    fn new(app:&Application) -> Self {
        let logger         = Logger::new("drop_down_menu");
        let app            = app.clone_ref();
        let display_object = display::object::Instance::new(&logger);
        let icon           = arrow::View::new(&logger);
        let icon_overlay   = chooser_hover_area::View::new(&logger);
        let selection_menu = app.new_view::<ListView>();
        let label          = app.new_view::<text::Area>();
        let background     = Card::new();
        let content        = default();

        selection_menu.focus();

        Self{logger,app,display_object,icon,icon_overlay,label,selection_menu,background,content}
            .init()
    }

    fn init(self) -> Self {
        self.add_child(&self.icon);
        self.add_child(&self.icon_overlay);
        self.add_child(&self.label);
        self.selection_menu.add_child(&self.background);
        self.app.display.scene().layers.add_shapes_order_dependency::<card::View,list_view::selection::View>();

        self.background.set_corner_radius(list_view::CORNER_RADIUS_PX);

        // Clear default parent and hide again.
        self.show_selection_menu();

        self
    }

    fn set_label(&self, label:&str) {
        self.label.set_cursor(&default());
        self.label.select_all();
        self.label.insert(label);
        self.label.remove_all_cursors();
    }

    fn show_selection_menu(&self) {
        self.add_child(&self.selection_menu);
    }

    fn hide_selection_menu(&self) {
        self.selection_menu.unset_parent();
    }

    fn get_item_label(&self, id:Option<list_view::entry::Id>) -> Option<Ref<str>> {
        id.and_then(|id|
            self.content.borrow().is_some().as_some(Ref::map(self.content.borrow(), |content|
                    content.as_ref().unwrap().get(id).unwrap().as_str())))
    }
}

impl display::Object for Model {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ============================
// === VisualisationChooser ===
// ============================

/// UI entity that shows a button that opens a list of visualisations that can be selected from.
#[allow(missing_docs)]
#[derive(Clone,CloneRef,Debug)]
pub struct DropDownMenu {
    model   : Rc<Model>,
    pub frp : Frp,
}

impl Deref for DropDownMenu {
    type Target = Frp;
    fn deref(&self) -> &Self::Target { &self.frp }
}

impl DropDownMenu {
    /// Constructor.
    pub fn new(app:&Application) -> Self {
        let frp   = Frp::new();
        let model = Rc::new(Model::new(app));
        Self {model,frp}.init(app)
    }

    fn init(self, app:&Application) -> Self {
        let network = &self.frp.network;
        let frp     = &self.frp;
        let model   = &self.model;

        let scene   = app.display.scene();
        let mouse   = &scene.mouse.frp;

        frp::extend! { network

            // === Input Processing ===

            eval frp.input.set_entries ([model](entries) {
                model.content.set(Rc::clone(entries));
                let entries:list_view::entry::AnyEntryProvider = Rc::clone(entries).into();
                model.selection_menu.frp.set_entries.emit(entries);
            });


            // === Layouting ===

            let menu_height = DEPRECATED_Animation::<f32>::new(&network);

            eval menu_height.value ([model](height) {
                let size = Vector2::new(MENU_WIDTH,*height);
                model.selection_menu.frp.resize.emit(size);
                model.background.resize(size);
                if *height <= 0.0 {
                    model.hide_selection_menu();
                } else if *height > 0.0 {
                    model.show_selection_menu();
                }
            });

            icon_size <- all(frp.input.set_icon_size,frp.input.set_icon_padding);
            eval icon_size (((size,padding)) {
                model.icon.size.set(size-2.0*padding);
            });

            resize_menu <- all(model.selection_menu.size,frp.input.set_icon_size,frp.input.set_menu_offset_y);
            eval resize_menu (((menu_size,icon_size,menu_offset_y)) {
                // Align the top of the menu to the bottom of the icon.
                model.selection_menu.set_position_y(-menu_size.y/2.0-icon_size.y/2.0-menu_offset_y);
                // Align the right of the menu to the right of the icon.
                let offfset_y = -menu_size.x/2.0+icon_size.x/2.0;
                model.selection_menu.set_position_x(offfset_y);
            });

            label_position <- all(model.label.frp.width,frp.input.set_icon_size);
            eval label_position (((text_width,icon_size)) {
                model.label.set_position_x(-text_width-icon_size.x/2.0);
                // Adjust for text offset, so this appears more centered.
                model.label.set_position_y(0.25 * icon_size.y);
            });

            overlay_size <- all(model.label.frp.width,frp.input.set_icon_size);
            eval overlay_size ([model]((text_width,icon_size)) {
                let size = Vector2::new(text_width + icon_size.x,icon_size.y);
                model.icon_overlay.size.set(size);
                model.icon_overlay.set_position_x(-text_width/2.0);
            });


             // === Menu State ===

            hide_menu <- source::<()>();
            show_menu <- source::<()>();

            eval_ hide_menu (model.selection_menu.deselect_entries.emit(()));

            frp.source.menu_visible <+ hide_menu.constant(false);
            frp.source.menu_visible <+ show_menu.constant(true);
            frp.source.menu_closed  <+ hide_menu;

            target_height <- all_with(&frp.output.menu_visible,&model.selection_menu.frp.set_entries,
                f!([](visible,entries) {
                    if *visible {
                        let item_count  = entries.entry_count();
                        let line_height = list_view::entry::HEIGHT;
                        line_height * item_count as f32 + list_view::PADDING_VERTICAL * 2.0
                    } else {
                        // TODO[mm]: The following line is a workaround for #815.
                        //           If we end at 0.0 the `ListView` will still display the first
                        //           content item. This avoids the slowdown close to 0.0, so we can
                        //           manually remove the `ListView` from the scene at 0.0.
                        //           See #815
                        -20.0
                    }
                })
            );
            eval target_height ((h) menu_height.set_target_value(*h));


            // === Selection ===

            eval_ model.selection_menu.chosen_entry (hide_menu.emit(()));
            frp.source.chosen_entry <+ model.selection_menu.chosen_entry;

            set_selected <- any(frp.input.set_selected,model.selection_menu.chosen_entry);

            eval set_selected([model](entry_id) {
                if let Some(entry_id) = entry_id {
                    if let Some(_content) = model.content.borrow().as_ref() {
                        if let Some(item) = model.get_item_label(Some(*entry_id)) {
                            model.set_label(item.as_ref())
                        }
                    };
                };
            });


            // === Menu Toggle Through Mouse Interaction ===

            icon_hovered <- source::<bool>();
            eval_ model.icon_overlay.events.mouse_over ( icon_hovered.emit(true) );
            eval_ model.icon_overlay.events.mouse_out ( icon_hovered.emit(false) );

            frp.source.icon_mouse_over <+ model.icon_overlay.events.mouse_over;
            frp.source.icon_mouse_out  <+ model.icon_overlay.events.mouse_out;


            let icon_mouse_down = model.icon_overlay.events.mouse_down.clone_ref();
            visibility_on_mouse_down <- frp.source.menu_visible.sample(&icon_mouse_down) ;

            eval visibility_on_mouse_down ([show_menu,hide_menu](is_visible){
                if !is_visible {
                    show_menu.emit(());
                } else {
                    hide_menu.emit(());
                }
            });


           // === Close Menu ===

           mouse_down        <- mouse.down.constant(());
           mouse_down_remote <- mouse_down.gate_not(&icon_hovered);
           dismiss_menu      <- any(&frp.hide_selection_menu,&mouse_down_remote);
           eval_ dismiss_menu ( hide_menu.emit(()) );
        }

        // FIXME : StyleWatch is unsuitable here, as it was designed as an internal tool for
        // shape system (#795)
        let styles     = StyleWatch::new(&app.display.scene().style_sheet);
        let text_color = styles.get_color(theme::widget::list_view::text);
        model.label.set_default_color(text_color);

        self
    }
}

impl display::Object for DropDownMenu {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
