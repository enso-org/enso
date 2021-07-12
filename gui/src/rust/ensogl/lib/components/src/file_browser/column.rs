
use crate::prelude::*;

use crate::file_browser::ModelWithFrp;
use crate::file_browser::icons::DynamicIcon;
use crate::file_browser::icons;
use crate::file_browser::model::*;
use crate::file_browser;
use crate::list_view::ListView;
use crate::list_view;
use crate::shadow;

use enso_frp as frp;
use ensogl_core::Animation;
use ensogl_core::application::Application;
use ensogl_core::data::color;
use ensogl_core::display::Scene;
use ensogl_core::display::shape::*;
use ensogl_core::display;
use ensogl_text as text;
use ensogl_theme as theme;
use std::path::PathBuf;


// =================
// === Constants ===
// =================

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
const ENTRY_PADDING_LEFT   : f32 = 6.0;
// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
const ICON_TO_LABEL        : f32 = 5.0;
// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
const LABEL_TO_ARROW       : f32 = 6.0;
// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
const ENTRY_PADDING_RIGHT  : f32 = 2.0;
// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
const STATE_LABEL_PADDING  : f32 = 20.0;
// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
const STATE_LABEL_OFFSET_Y : f32 = 20.0;

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
fn entry_color_normal() -> color::Rgba {
    color::Rgba(0.341,0.341,0.341,1.0)
}

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
fn entry_color_focused() -> color::Rgba {
    color::Rgba::white()
}

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
fn state_label_color() -> color::Rgba {
    color::Rgba(0.5,0.5,0.5,1.0)
}



// ==============
// === Shadow ===
// ==============

mod column_shadow {
    use super::*;

    pub const SHADOW_PX:f32 = 10.0;

    ensogl_core::define_shape_system! {
        (style:Style,opacity:f32) {
            let background = HalfPlane().rotate(90.0_f32.to_radians().radians());
            let color      = style.get_color(theme::application::file_browser::background);
            let background = background.fill(color);
            let shadow     = shadow::from_shape_with_alpha((&background).into(),&opacity,style);

            (shadow + background).into()
        }
    }
}


// =========================
// === ListEntryProvider ===
// =========================

#[derive(Debug)]
struct ListEntry {
    display_object : display::object::Instance,
    label          : text::Area,
    icon           : Box<dyn DynamicIcon>,
    arrow          : Option<super::icons::Arrow>,
}

impl ListEntry {
    fn new(app:&Application, entry:Entry) -> Self {
        let logger         = Logger::new("ListEntry");
        let scene          = app.display.scene();
        let display_object = display::object::Instance::new(&logger);

        let label = text::Area::new(app);
        display_object.add_child(&label);
        label.set_position_x(ENTRY_PADDING_LEFT + icons::ICON_SIZE + ICON_TO_LABEL);
        label.set_position_y(6.0);
        label.set_default_color(entry_color_normal());
        label.set_content(entry.name);
        label.add_to_scene_layer(&scene.layers.panel_text);

        let icon  : Box<dyn DynamicIcon>;
        let arrow : Option<super::icons::Arrow>;
        match entry.type_ {
            EntryType::File => {
                icon = Box::new(icons::File::new());
                Self::declare_order_dependencies_for_icon::<icons::file::View>(scene);
                arrow = None;
            }
            EntryType::Folder {type_,..} => {
                match type_ {
                    FolderType::Standard => {
                        icon = Box::new(icons::Folder::new());
                        Self::declare_order_dependencies_for_icon::<icons::folder::View>(scene);
                    }
                    FolderType::Root => {
                        icon = Box::new(icons::Root::new());
                        Self::declare_order_dependencies_for_icon::<icons::root::View>(scene);
                    }
                    FolderType::Home => {
                        icon = Box::new(icons::Home::new());
                        Self::declare_order_dependencies_for_icon::<icons::home::View>(scene);
                    }
                    FolderType::Project => {
                        icon = Box::new(icons::Project::new());
                        Self::declare_order_dependencies_for_icon::<icons::project::View>(scene);
                    }
                    _ => {
                        icon = Box::new(icons::Root::new());
                        Self::declare_order_dependencies_for_icon::<icons::root::View>(scene);
                    }
                };
                let arrow_icon = super::icons::Arrow::new();
                Self::declare_order_dependencies_for_icon::<icons::arrow::View>(scene);
                display_object.add_child(&arrow_icon);
                scene.layers.panel.add_exclusive(&arrow_icon);
                arrow = Some(arrow_icon);
            }
        }
        display_object.add_child(icon.as_ref());
        scene.layers.panel.add_exclusive(icon.as_ref());
        icon.deref().set_position_x(ENTRY_PADDING_LEFT + icons::ICON_SIZE / 2.0);

        Self { display_object,label,icon,arrow }
    }

    fn width(&self) -> f32 {
        ENTRY_PADDING_LEFT + icons::ICON_SIZE + ICON_TO_LABEL + self.label.width.value()
            + LABEL_TO_ARROW + icons::ICON_SIZE + ENTRY_PADDING_RIGHT
    }

    fn declare_order_dependencies_for_icon<Icon>(scene:&Scene)
    where Icon: HasContent, Content<Icon>: KnownShapeSystemId {
        scene.layers.add_shapes_order_dependency::<list_view::selection::View,Icon>();
        scene.layers.add_shapes_order_dependency::<Icon,list_view::io_rect::View>();
    }
}

impl display::Object for ListEntry {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl list_view::entry::Entry for ListEntry {
    fn set_focused(&self, focused: bool) {
        let text_color = if focused {
            entry_color_focused()
        } else {
            entry_color_normal()
        };
        self.label.set_color_all(text_color);
        self.icon.set_focused(focused);
        if let Some(arrow) = &self.arrow {
            arrow.set_focused(focused);
        }
    }

    fn set_width(&self, width: f32) {
        if let Some(arrow) = &self.arrow {
            arrow.set_position_x(width-ENTRY_PADDING_RIGHT-icons::ICON_SIZE /2.0);
        }
    }
}

// TODO: On construction, the `ListEntryProvider` creates a display object for every single folder
//       entry, such that it can determine their maximum width. This can be expensive and might be
//       improved. (#1704)
// TODO: The `width` of `ListEntryProviders` can be arbitrarily large if one of the provided entries
//       has a long name. Instead, the width should be bounded by some maximum value. But this
//       requires that we truncate labels that do not fit into this width. (#1705)
#[derive(Debug,Clone,CloneRef,Default)]
struct ListEntryProvider(Rc<Vec<Rc<ListEntry>>>);

impl ListEntryProvider {
    fn new<'a>(app:&Application, entries:impl IntoIterator<Item=&'a Entry>) -> Self {
        let entries = entries.into_iter().map(|entry| Rc::new(ListEntry::new(app,entry.clone())));
        ListEntryProvider(Rc::new(entries.collect_vec()))
    }

    fn width(&self) -> f32 {
        let widths = self.0.iter().map(|entry| entry.width());
        widths.max_by(|x,y| x.partial_cmp(y).unwrap()).unwrap_or(0.0)
    }
}

impl list_view::entry::EntryProvider for ListEntryProvider {
    fn entry_count(&self) -> usize {
        self.0.len()
    }

    fn get(&self, _app: &Application, id: usize) -> Option<list_view::entry::AnyEntry> {
        Some(list_view::entry::AnyEntry::from(self.0.get(id)?.clone()))
    }
}



// =============
// === Model ===
// =============

#[derive(Clone,Debug)]
pub struct Model {
    app            : Application,
    pub entries    : RefCell<Option<Rc<Vec<Entry>>>>,
    pub list_view  : ListView,
    // The state label displays when the column is empty or loading or when there was an error.
    state_label    : text::Area,
    shadow         : column_shadow::View,
    display_object : display::object::Instance,
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints! {
    Input {
        set_entries (Rc<Vec<Entry>>),
        set_error   (ImString),
    }
    Output {
        entry_selected (PathBuf),
        entry_chosen   (PathBuf),
        left           (f32),
        right          (f32),
        width          (f32),
    }
}



// ==============
// === Column ===
// ==============

#[derive(Debug,Clone,CloneRef)]
pub struct Column {
    pub model : Rc<Model>,
    frp       : Frp,
}

impl Column {
    pub fn new(browser:Rc<ModelWithFrp>, index:usize) -> Column {
        let weak_browser   = Rc::downgrade(&browser);
        let app            = browser.model.app.clone();
        let layers         = &app.display.scene().layers;
        let display_object = display::object::Instance::new(Logger::new(""));
        let entries        = RefCell::new(None);
        let frp            = Frp::new();
        let network        = &frp.network;


        // === List View ===

        let list_view = app.new_view::<ListView>();
        display_object.add_child(&list_view);
        app.display.scene().layers.panel.add_exclusive(&list_view);
        list_view.set_position_y(-file_browser::CONTENT_HEIGHT/2.0);
        list_view.set_selection_method(list_view::SelectionMethod::Click);


        // === State Label ===

        let state_label = text::Area::new(&app);
        display_object.add_child(&state_label);
        state_label.add_to_scene_layer(&layers.panel_text);
        state_label.set_default_color(state_label_color());
        state_label.set_position_y(-file_browser::CONTENT_HEIGHT/2.0+STATE_LABEL_OFFSET_Y);
        state_label.set_content("Loading.");
        state_label.set_position_x(STATE_LABEL_PADDING);

        frp::extend! { network
            state_label.set_content <+ frp.set_entries.map(|entries|
                if entries.is_empty() {
                    "This folder is empty."
                } else {
                    ""
                }.to_string()
            );
            state_label.set_content <+ frp.set_error.map(ImString::to_string);
        }


        // === Shadow ===

        let shadow = column_shadow::View::new(&Logger::new("file browser column"));
        display_object.add_child(&shadow);
        layers.panel.add_exclusive(&shadow);
        shadow.set_position_y(-file_browser::CONTENT_HEIGHT/2.0);
        shadow.size.set(Vector2(column_shadow::SHADOW_PX*2.0,super::CONTENT_HEIGHT));
        layers.add_shapes_order_dependency::<file_browser::background::View,column_shadow::View>();
        layers.add_shapes_order_dependency::<column_shadow::View,list_view::selection::View>();
        let shadow_opacity = Animation::new(&network);

        frp::extend! { network
            eval shadow_opacity.value((&opacity) shadow.opacity.set(opacity));
        }


        // === Model ===

        let model = Rc::new(Model {app:app.clone(), entries, list_view:list_view.clone(),
            state_label: state_label.clone(), shadow, display_object});


        // === Entries ===

        frp::extend! { network
            eval frp.set_entries((entries) model.entries.set(entries.clone()));
            list_entries <- frp.set_entries.map(f!([app](entries)
                ListEntryProvider::new(&app,entries.as_ref())));

            updating_entries <- source::<bool>();
            eval list_entries([model,updating_entries](entries) {
                updating_entries.emit(true);
                let entry_provider = list_view::entry::AnyEntryProvider::from(entries.clone());
                model.list_view.set_entries(entry_provider);
                model.list_view.move_selection_to_first();
                model.list_view.skip_animations();
                updating_entries.emit(false);
            });
        }


        // === Positioning ===

        frp::extend! { network
            frp.source.right <+ all_with(&frp.left,&frp.width,|&left,&width| left + width);
            eval frp.left((&x) model.display_object.set_position_x(x));
            eval frp.width((&w) model.shadow.set_position_x(w));
            eval frp.width((&w) {
                model.list_view.resize(Vector2(w,file_browser::CONTENT_HEIGHT));
                model.list_view.set_position_x(w/2.0);
            });
            eval_ frp.right(weak_browser.upgrade().unwrap().model.update_content_width());
            frp.source.width <+ all_with(&list_entries,&state_label.width,|entries,&label_width|
                (entries.width()+2.0*list_view::PADDING_HORIZONTAL)
                    .max(label_width+2.0*STATE_LABEL_PADDING));
        }

        if index > 0 {
            let predecessor = browser.model.columns.borrow()[index-1].clone();
            frp::extend! { network
                frp.source.left <+ predecessor.right;
            }
            frp.source.left.emit(predecessor.right.value());
        } else {
            frp.source.left.emit(0.0);
        }


        // === Focus and Scroll-To ===

        frp::extend! { network
            scroll_to_this <- any_mut::<()>();
            eval scroll_to_this([weak_browser](_) {
                let browser = weak_browser.upgrade().unwrap();
                browser.model.scroll_to_column(&browser.model.columns.borrow()[index]);
            });
            scroll_to_this <+ frp.set_entries.constant(());
            scroll_to_this <+ frp.set_error.constant(());

            focus_this_column <- frp.entry_selected.gate_not(&updating_entries).constant(());
            eval_ focus_this_column(weak_browser.upgrade().unwrap().model.focus_column(index));

            list_view.set_focus <+ frp.focused;
        }


        // === Opening of Selected Folders ===

        frp::extend! { network
            focus_enters <- frp.focused.on_true();
            x            <- all(&list_view.selected_entry,&focus_enters)._0();
            open_entry   <- x.gate_not(&updating_entries).on_change();
            eval open_entry([weak_browser,model,shadow_opacity](id) {
                weak_browser.upgrade().unwrap().model.close_columns_from(index + 1);
                if let Some(id) = *id {
                    let selected_entry = model.entries.borrow().as_ref().unwrap()[id].clone();
                    shadow_opacity.target.emit(0.0);
                    if let EntryType::Folder{content,..} = selected_entry.type_ {
                        weak_browser.upgrade().unwrap().push_column(content);
                        shadow_opacity.target.emit(1.0);
                    }
                }
            });
        }


        // === Output Events ===

        frp::extend! { network
            frp.source.entry_chosen <+ model.list_view.chosen_entry.filter_map(
                f!([model](&id) Some(model.entries.borrow().as_ref().unwrap()[id?].path.clone())));
            frp.source.entry_selected <+ model.list_view.selected_entry.filter_map(
                f!([model](&id) Some(model.entries.borrow().as_ref().unwrap()[id?].path.clone())));
            browser.frp.source.entry_chosen   <+ frp.entry_chosen;
            browser.frp.source.entry_selected <+ frp.entry_selected;
        }

        Column {model,frp}
    }
}

impl Deref for Column {
    type Target = Frp;

    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl display::Object for Column {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}
