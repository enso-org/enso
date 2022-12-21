use enso_frp::prelude::*;
use ensogl::prelude::*;
use ensogl_grid_view::prelude::*;

use crate::projects_spinner;

use enso_frp as frp;
use ensogl::display::scene;
use ensogl_core::application;
use ensogl_core::application::frp::API as _;
use ensogl_core::data::color;
use ensogl_core::display;
use ensogl_grid_view as grid_view;
use ensogl_grid_view::entry;
use ensogl_text as text;



// ===================
// === EntryParams ===
// ===================

/// The parameters of [`ProjectsTable`]`s [entries].
///
/// [entries]: crate::projects_entry::Entry
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct EntryParams {
    pub bg_color:            color::Lcha,
    pub bg_margin:           f32,
    pub hover_color:         color::Lcha,
    pub selection_color:     color::Lcha,
    pub font:                ImString,
    pub text_offset:         f32,
    pub text_size:           text::Size,
    pub spinner_size:        f32,
    pub text_color:          color::Lcha,
    pub disabled_text_color: color::Lcha,
}

impl Default for EntryParams {
    fn default() -> Self {
        Self {
            bg_color:            color::Lcha::transparent(),
            bg_margin:           0.0,
            hover_color:         color::Lcha::from(color::Rgba(0.9, 0.9, 0.9, 1.0)),
            selection_color:     color::Lcha::from(color::Rgba(0.8, 0.8, 0.8, 1.0)),
            font:                text::font::DEFAULT_FONT.into(),
            text_offset:         7.0,
            text_size:           text::Size(14.0),
            spinner_size:        projects_spinner::SPINNER_SIZE,
            text_color:          color::Lcha::from(color::Rgba(0.0, 0.0, 0.0, 1.0)),
            disabled_text_color: color::Lcha::from(color::Rgba(0.7, 0.7, 0.7, 1.0)),
        }
    }
}



// ==================
// === EntryModel ===
// ==================

/// The model of [`ProjectsTable`]`s [entries].
///
/// [entries]: crate::projects_entry::Entry
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Default)]
pub struct EntryModel {
    pub spinner_state:  Immutable<Option<projects_spinner::State>>,
    pub text:           ImString,
    pub disabled:       Immutable<bool>,
    pub override_width: Immutable<Option<f32>>,
}



// =============
// === Entry ===
// =============

// === ProjectsTableColumn ===

/// An enum for selecting which type of entry is being created. There is one variant for each column
/// that appears in the [`ProjectsTable`]. This is used to determine what to render to the table.
/// For example, when the [`ProjectsTableColumn::Projects`] column is rendered, we want to display
/// an interactive spinner for the [`Project`] state as well as the [`Project`]'s name.
///
/// [`Project`]: ::enso_cloud_view::project::Project
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Default, PartialEq)]
enum ProjectsTableColumn {
    Projects,
    #[default]
    Other,
}

// === EntryData ===

/// An internal structure of [`Entry`], which may be passed to FRP network.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
struct EntryData {
    display_object: display::object::Instance,
    spinner:        projects_spinner::View,
    label:          text::Text,
    background:     entry::shape::View,
    column:         Rc<Cell<ProjectsTableColumn>>,
}

impl EntryData {
    fn new(app: &application::Application, text_layer: Option<&scene::Layer>) -> Self {
        let display_object = display::object::Instance::new();
        let label = app.new_view::<text::Text>();
        label.set_long_text_truncation_mode(true);
        let background = entry::shape::View::new();
        display_object.add_child(&label);
        display_object.add_child(&background);
        if let Some(layer) = text_layer {
            label.add_to_scene_layer(layer);
        }
        let state = projects_spinner::State::Closed;
        let spinner = projects_spinner::View::new(state, app);
        spinner.set_size(Vector2(projects_spinner::SPINNER_SIZE, projects_spinner::SPINNER_SIZE));
        let column = default();
        Self { display_object, column, spinner, label, background }
    }

    fn update_layout(
        &self,
        contour: entry::Contour,
        text_size: text::Size,
        spinner_size: f32,
        text_offset: f32,
    ) {
        self.background.set_contour(contour);
        let size = contour.size;
        let x = text_offset - size.x / 2.0 + spinner_size / 2.0;
        self.spinner.set_xy(Vector2(x, 0.0));

        let mut x = text_offset - size.x / 2.0;
        if self.column.get() == ProjectsTableColumn::Projects {
            x += spinner_size;
            x += text_offset;
        }
        let y = text_size.value / 2.0;
        self.label.set_xy(Vector2(x, y));
    }

    fn hide_current_visual_representation(&self) {
        self.label.unset_parent();
        match self.column.get() {
            ProjectsTableColumn::Projects => self.spinner.unset_parent(),
            ProjectsTableColumn::Other => {}
        }
    }

    fn set_model(&self, model: &EntryModel) {
        match model {
            // # Safety
            //
            // Spinner state is only ever `None` for invalid entries (e.g. if we're showing an
            // error). In all other cases, the spinner state is always `Some` since we may need to
            // render it. Thus, it is safe to unwrap here, since the spinner state must be
            // initialized if we're trying to render this type of entry.
            EntryModel { spinner_state, .. } if spinner_state.is_some() =>
                self.switch_to_projects(spinner_state.unwrap()),
            EntryModel { .. } => self.switch_to_other(),
        }
    }

    fn switch_to_projects(&self, state: projects_spinner::State) {
        self.spinner.set_state(state);
        if self.column.get() != ProjectsTableColumn::Projects {
            self.hide_current_visual_representation();
            self.display_object.add_child(&self.spinner);
            self.display_object.add_child(&self.label);
            self.column.set(ProjectsTableColumn::Projects);
        }
    }

    fn switch_to_other(&self) {
        if self.column.get() != ProjectsTableColumn::Other {
            self.hide_current_visual_representation();
            self.display_object.add_child(&self.label);
            self.column.set(ProjectsTableColumn::Other);
        }
    }

    fn max_width_px(&self, mut width: f32) -> f32 {
        if self.column.get() == ProjectsTableColumn::Projects {
            width -= projects_spinner::SPINNER_SIZE;
        };
        width
    }
}


// === Entry ===

/// A [`ProjectsTable`] entry - a label with background.
#[derive(Clone, CloneRef, Debug)]
pub struct Entry {
    frp:  entry::EntryFrp<Self>,
    data: Rc<EntryData>,
}


// === Trait `impl`s ===

impl grid_view::Entry for Entry {
    type Model = EntryModel;
    type Params = EntryParams;

    fn new(app: &application::Application, text_layer: Option<&scene::Layer>) -> Self {
        let data = Rc::new(EntryData::new(app, text_layer));
        let frp = entry::EntryFrp::<Self>::new();
        let input = &frp.private().input;
        let out = &frp.private().output;
        let network = frp.network();

        frp::extend! { network
            size <- input.set_size.on_change();
            bg_color <- input.set_params.map(|p| p.bg_color).on_change();
            bg_margin <- input.set_params.map(|p| p.bg_margin).on_change();
            hover_color <- input.set_params.map(|p| p.hover_color).on_change();
            selection_color <- input.set_params.map(|p| p.selection_color).on_change();
            font <- input.set_params.map(|p| p.font.clone_ref()).on_change();
            text_offset <- input.set_params.map(|p| p.text_offset).on_change();
            text_color <- input.set_params.map(|p| p.text_color).on_change();
            text_size <- input.set_params.map(|p| p.text_size).on_change();
            spinner_size <- input.set_params.map(|p| p.spinner_size).on_change();
            dis_text_color <- input.set_params.map(|p| p.disabled_text_color).on_change();

            contour <- all_with(&size, &bg_margin, |size, margin| entry::Contour {
                size: *size - Vector2(*margin, *margin) * 2.0,
                corners_radius: 0.0,
            });
            layout <- all(contour, text_size, spinner_size, text_offset);
            eval layout ((&(c, ts, ss, to)) data.update_layout(c, ts, ss, to));
            eval bg_color ((color) data.background.color.set(color.into()));
            disabled <- input.set_model.map(|m| *m.disabled);
            data.label.set_property_default <+ all_with3(
                &text_color,
                &dis_text_color,
                &disabled,
                |c, dc, d| if *d { *dc } else { *c }
            ).ref_into_some();
            data.label.set_font <+ font;
            data.label.set_property_default <+ text_size.ref_into_some();
            content <- input.set_model.map(|m| m.text.clone_ref());
            max_width_px <- input.set_size.map(f!((size) data.max_width_px(size.x)));
            data.label.set_content <+ content;
            data.label.set_view_width <+ max_width_px.some();
            eval input.set_model ((m) data.set_model(m));

            out.override_column_width <+ input.set_model.filter_map(
                f!([data](model) {
                    data.set_model(model);
                    *model.override_width
                })
            );
            out.contour <+ contour;
            out.highlight_contour <+ contour;
            out.disabled <+ disabled;
            out.hover_highlight_color <+ hover_color;
            out.selection_highlight_color <+ selection_color;
        }
        Self { frp, data }
    }

    fn frp(&self) -> &entry::EntryFrp<Self> {
        &self.frp
    }
}

impl display::Object for Entry {
    fn display_object(&self) -> &display::object::Instance {
        &self.data.display_object
    }
}
