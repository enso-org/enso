//! Drop Down Menu Component.

#![recursion_limit = "512"]
// === Features ===
#![feature(option_result_contains)]
#![feature(trait_alias)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl_core::display::shape::*;
use ensogl_core::prelude::*;

use enso_frp as frp;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_core::display::shape::primitive::StyleWatch;
use ensogl_core::DEPRECATED_Animation;
use ensogl_hardcoded_theme as theme;
use ensogl_list_view as list_view;
use ensogl_list_view::entry::ModelProvider;
use ensogl_text as text;



// =================
// === Constants ===
// =================

/// Invisible dummy color to catch hover events.
const HOVER_COLOR: color::Rgba = color::Rgba::new(1.0, 0.0, 0.0, 0.000_001);
/// The width of the visualisation selection menu.
const MENU_WIDTH: f32 = 180.0;



// ==============
// === Shapes ===
// ==============

/// Arrow icon that indicates the drop down menu.
pub mod arrow {
    use super::*;

    ensogl_core::shape! {
        below = [chooser_hover_area];
        alignment = center;
        (style:Style) {
            let width            = Var::<Pixels>::from("input_size.x");
            let height           = Var::<Pixels>::from("input_size.y");
            let triangle         = Triangle(width,height);
            let triangle_down    = triangle.rotate(Var::<f32>::from(std::f32::consts::PI));
            let color_path       = ensogl_hardcoded_theme::graph_editor::visualization::action_bar::icon;
            let icon_color       = style.get_color(color_path);
            let triangle_colored = triangle_down.fill(icon_color);

            triangle_colored.into()
        }
    }
}

/// Invisible rectangular area around the icon
pub mod chooser_hover_area {
    use super::*;

    ensogl_core::shape! {
        above = [arrow];
        alignment = center;
        (style: Style) {
            let width  : Var<Pixels> = "input_size.x".into();
            let height : Var<Pixels> = "input_size.y".into();
            let background           = Rect((&width,&height));
            let background           = background.fill(HOVER_COLOR);
            background.into()
        }
    }
}


// =================
// === Alignment ===
// =================

/// Indicates the alignment of the selection menu.
#[derive(Clone, Copy, Debug)]
pub enum Alignment {
    /// Align the menu to the left side of the selection menu.
    Left,
    /// Align the menu to the right side of the selection menu.
    Right,
}

impl Default for Alignment {
    fn default() -> Self {
        Self::Left
    }
}


// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_entries         (list_view::entry::AnyModelProvider<Entry>),
        set_icon_size       (Vector2),
        set_icon_padding    (Vector2),
        hide_selection_menu (),
        set_selected        (Option<list_view::entry::Id>),
        set_menu_offset_y   (f32),
        set_menu_alignment  (Alignment),
        set_label_alignment (Alignment),
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

/// A type of Entry used in DropDownMenu's ListView.
pub type Entry = list_view::entry::Label;

#[derive(Clone, Debug)]
struct Model {
    display_object: display::object::Instance,

    icon:          arrow::View,
    click_overlay: chooser_hover_area::View,

    label:          text::Text,
    selection_menu: list_view::ListView<Entry>,

    // `SingleMaskedProvider` allows us to hide the selected element.
    content: RefCell<Option<list_view::entry::SingleMaskedProvider<Entry>>>,
}

impl Model {
    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let icon = arrow::View::new();
        let click_overlay = chooser_hover_area::View::new();
        let selection_menu = list_view::ListView::new(app);
        let label = app.new_view::<text::Text>();
        let content = default();

        Self { display_object, icon, click_overlay, label, selection_menu, content }.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.icon);
        self.add_child(&self.click_overlay);
        self.add_child(&self.label);
        // Clear default parent and hide again.
        self.show_selection_menu();
        self
    }

    fn set_label(&self, label: &str) {
        #[allow(clippy::needless_borrow)] // Removing the borrow breaks type inference.
        self.label.set_cursor(&default());
        self.label.select_all();
        self.label.insert(label);
        self.label.remove_all_cursors();
    }

    fn show_selection_menu(&self) {
        self.add_child(&self.selection_menu);
    }

    fn hide_selection_menu(&self) {
        self.selection_menu.unset_parent()
    }

    fn get_content_item(
        &self,
        id: Option<list_view::entry::Id>,
    ) -> Option<<Entry as list_view::entry::Entry>::Model> {
        self.content.borrow().as_ref()?.get(id?)
    }

    /// Transform index of an element visible in the menu, to the index of the all the objects,
    /// accounting for the removal of the selected item.
    ///
    /// Example:
    /// Widget state: Selected [B], menu content [A, C]
    /// Item list      [A, B,  C]
    /// Unmasked index [0, 1,  2]
    /// Masked indices [0, na, 1]
    fn get_unmasked_index(&self, ix: Option<usize>) -> Option<usize> {
        Some(self.content.borrow().as_ref()?.unmasked_index(ix?))
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
#[derive(Clone, CloneRef, Debug)]
pub struct DropDownMenu {
    model:   Rc<Model>,
    pub frp: Frp,
}

impl Deref for DropDownMenu {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl DropDownMenu {
    /// Constructor.
    pub fn new(app: &Application) -> Self {
        let frp = Frp::new();
        let model = Rc::new(Model::new(app));
        Self { model, frp }.init(app)
    }

    fn init(self, app: &Application) -> Self {
        let network = &self.frp.network;
        let frp = &self.frp;
        let model = &self.model;

        let scene = &app.display.default_scene;
        let mouse = &scene.mouse.frp_deprecated;

        frp::extend! { network

            // === Input Processing ===

            eval frp.input.set_entries ([model](entries) {
                let entries:list_view::entry::SingleMaskedProvider<Entry> = entries.clone_ref().into();
                model.content.set(entries.clone());
                let entries = list_view::entry::AnyModelProvider::<Entry>::new(entries);
                model.selection_menu.frp.set_entries.emit(entries);
            });


            // === Layouting ===

            let menu_height = DEPRECATED_Animation::<f32>::new(network);

            eval menu_height.value ([model](height) {
                model.selection_menu.frp.resize.emit(Vector2::new(MENU_WIDTH,*height));
                if *height <= 0.0 {
                    model.hide_selection_menu();
                } else if *height > 0.0 {
                    model.show_selection_menu();
                }
            });

            icon_size <- all(frp.input.set_icon_size,frp.input.set_icon_padding);
            eval icon_size (((size,padding)) {
                model.icon.set_size(size-2.0*padding);
            });

            resize_menu <- all(model.selection_menu.size,frp.input.set_icon_size,
                frp.input.set_menu_offset_y,frp.input.set_menu_alignment);
            eval resize_menu (((menu_size,icon_size,menu_offset_y,alignment)) {
                // Align the top of the menu to the bottom of the icon.
                model.selection_menu.set_y(-menu_size.y/2.0-icon_size.y/2.0-menu_offset_y);
                // Align the right of the menu to the right of the icon.
                let x_offset = match alignment {
                    Alignment::Left => -menu_size.x/2.0+icon_size.x/2.0-list_view::SHADOW_PX/2.0,
                    Alignment::Right => 0.0,
                };
                model.selection_menu.set_x(x_offset);
            });

            label_position <- all(model.label.frp.width,frp.input.set_icon_size,model.label.frp.height,
                frp.input.set_label_alignment);
            eval label_position ([model]((text_width,icon_size,text_height,alignment)) {
                let base_offset = match alignment {
                    Alignment::Left => -MENU_WIDTH/2.0+icon_size.x/2.0,
                    Alignment::Right => -text_width-icon_size.x/2.0,
                };
                model.label.set_x(base_offset);
                // Adjust for text offset, so this appears more centered.
                model.label.set_y(0.5 * text_height);
            });

            overlay_size <- all(
                model.label.frp.width,
                model.label.frp.height,
                frp.input.set_icon_size,
                frp.input.set_icon_padding);
            eval overlay_size ([model]((text_width,text_height,icon_size,icon_padding)) {
                let height = icon_size.y.max(*text_height);
                let width = text_width + icon_size.x + icon_padding.x;
                let size = Vector2::new(width,height);
                model.click_overlay.set_size(size);
                model.click_overlay.set_x(-width/2.0 + icon_size.x/2.0 - icon_padding.x);
            });


             // === Menu State ===

            hide_menu <- source::<()>();
            show_menu <- source::<()>();

            eval_ hide_menu (model.selection_menu.deselect_entries.emit(()));

            menu_visible            <- bool(&hide_menu,&show_menu).on_change();
            frp.source.menu_visible <+ menu_visible;
            frp.source.menu_closed  <+ menu_visible.on_false();

            target_height <- all_with(&frp.output.menu_visible,&model.selection_menu.frp.set_entries,
                f!([](visible,entries) {
                    if *visible {
                        let item_count  = entries.entry_count();
                        let line_height = list_view::entry::HEIGHT;
                        line_height * item_count as f32
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
            chosen_entry_unmasked <- model.selection_menu.chosen_entry.map(f!((entry_id)
                model.get_unmasked_index(*entry_id))
            );
            frp.source.chosen_entry <+ chosen_entry_unmasked;
            set_selected            <- any(frp.input.set_selected, chosen_entry_unmasked);

            eval set_selected([model](entry_id) {
                if let Some(entry_id) = entry_id {
                    if let Some(content) = model.content.borrow().as_ref() {
                        // We get an external item index, so we operate on all items, thus we
                        // clear the mask.
                        content.clear_mask();
                        if let Some(item) = model.get_content_item(Some(*entry_id)) {
                            model.set_label(&item)
                        };
                        // Remove selected item from menu list
                        content.set_mask(*entry_id);
                        // Update menu content.
                        let entries = list_view::entry::AnyModelProvider::<Entry>::new(content.clone());
                        model.selection_menu.frp.set_entries.emit(entries);
                    };
                };
            });


            // === Menu Toggle Through Mouse Interaction ===

            icon_hovered <- source::<bool>();
            eval_ model.click_overlay.events_deprecated.mouse_over ( icon_hovered.emit(true) );
            eval_ model.click_overlay.events_deprecated.mouse_out ( icon_hovered.emit(false) );

            frp.source.icon_mouse_over <+ model.click_overlay.events_deprecated.mouse_over;
            frp.source.icon_mouse_out  <+ model.click_overlay.events_deprecated.mouse_out;


            let icon_mouse_down = model.click_overlay.events_deprecated.mouse_down_primary.clone_ref();
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
        let styles = StyleWatch::new(&app.display.default_scene.style_sheet);
        let text_color = styles.get_color(theme::widget::list_view::text);
        model.label.set_property_default(text_color);

        self
    }

    /// Set the label of the dropdown menu.
    pub fn set_label_color(&self, color: color::Rgba) {
        self.model.label.set_property_default(color);
    }

    /// Set the layer of all text labels.
    pub fn set_label_layer(&self, layer: &display::scene::Layer) {
        self.model.selection_menu.set_label_layer(layer);
        self.model.label.add_to_scene_layer(layer);
    }

    /// Set the correct order for the shapes. To be sued after moving the component to a different
    /// layer. Workaround for #6241.
    pub fn restore_shape_constraints(&self, app: &Application) {
        let scene = &app.display.default_scene;
        shapes_order_dependencies! {
            scene => {
                 arrow -> chooser_hover_area;
            }
        }
    }
}

impl display::Object for DropDownMenu {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
