//! Cloud dashboard scene showing a table of projects.

#![recursion_limit = "512"]
// === Features ===
#![allow(incomplete_features)]
#![feature(negative_impls)]
#![feature(associated_type_defaults)]
#![feature(cell_update)]
#![feature(const_type_id)]
#![feature(drain_filter)]
#![feature(entry_insert)]
#![feature(fn_traits)]
#![feature(marker_trait_attr)]
#![feature(specialization)]
#![feature(trait_alias)]
#![feature(type_alias_impl_trait)]
#![feature(unboxed_closures)]
#![feature(trace_macros)]
#![feature(const_trait_impl)]
#![feature(slice_as_chunks)]
#![feature(variant_count)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use ensogl::prelude::*;
use wasm_bindgen::prelude::*;

use ensogl_core::application::Application;
use ensogl_core::display::object::ObjectOps;
use ensogl_hardcoded_theme as theme;



// =====================
// === ProjectsTable ===
// =====================

/// Module containing the [`ProjectsTable`], which is used to display a list of [`Project`]s in the
/// Cloud dashboard.
///
/// The list of [`Project`]s, as rendered, contains information about the state of the [`Project`],
/// the data associated with the [`Project`], the permissions assigned to it, etc. Buttons for
/// interacting with the [`Project`]s (e.g., starting and stopping the [`Project`]s) are also
/// rendered.
///
/// [`Project`]: ::enso_cloud_view::project::Project
pub mod projects_table {
    use ensogl::prelude::*;

    use enso_cloud_view as view;
    use ensogl::application;
    use ensogl::data::color;
    use ensogl_core::application::Application;
    use ensogl_core::display;
    use ensogl_core::frp;
    use ensogl_grid_view::simple::EntryModel;
    use ensogl_grid_view::Col;
    use ensogl_grid_view::Row;



    // =================
    // === Constants ===
    // =================

    /// The width of a single entry in the [`ProjectsTable`], in pixels.
    const ENTRY_WIDTH: f32 = 130.0;
    /// The height of a single entry in the [`ProjectsTable`], in pixels.
    const ENTRY_HEIGHT: f32 = 28.0;
    /// A placeholder value for the initial height of the viewport. This value is a placeholder
    /// because the value must be provided when creating the viewport, but is not used since the
    /// viewport is resized to fit the [`ProjectsTable`] when the graphics context is initialized.
    const VIEWPORT_HEIGHT: f32 = 300.0;
    /// A placeholder value for the initial width of the viewport. This value is a placeholder
    /// because the value must be provided when creating the viewport, but is not used since the
    /// viewport is resized to fit the [`ProjectsTable`] when the graphics context is initialized.
    const VIEWPORT_WIDTH: f32 = 400.0;
    /// The grid is intended to take up 20% of the viewport height, expressed as a fraction;
    const GRID_HEIGHT_RATIO: f32 = 0.2;
    /// In the future, we want to display the last modification time of a [`Project`]. For now, the
    /// API does not provide that information so we use a placeholder value.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    // TODO [NP]: Update the cloud projects API to record and provide last modification time.
    const LAST_MODIFIED: &str = "2022-10-08 13:30";
    /// In the future, we want to display icons for users/groups with access to the [`Project`] and
    /// their corresponding permissions (e.g., read/write/execute).
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    // TODO [NP]: Update the cloud projects API to record and provide user/group access
    // information.
    const SHARED_WITH: &str = "Baron von Münchhausen (Read/Write)";
    /// In the future, we want to display icons for the [`Project`]'s labels. Labels may be
    /// user-defined or system-defined (e.g., labels indicating high resource usage or outdated
    /// version).
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    // TODO [NP]: Update the cloud projects API to provide project labels.
    const LABELS: &str = "(!) outdated version";
    /// In the future, we want to display icons for datasets associated with the [`Project`], as
    /// well as what permissions are set on the dataset, from the user's perspective.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    // TODO [NP]: Update the cloud projects API to provide dataset information.
    const DATA_ACCESS: &str = "./user_data";
    /// In the future, we want to display which usage plan the [`Project`] is configured for (e.g.,
    /// "Interactive" or cron-style, etc.).
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    // TODO [NP]: Update the cloud projects API to provide usage plan information.
    const USAGE_PLAN: &str = "Interactive";

    /// ID of the Amazon API Gateway serving the Cloud API.
    const API_GATEWAY_ID: &str = "7aqkn3tnbc";
    /// The AWS region in which the Amazon API Gateway referenced by [`API_GATEWAY_ID`] is deployed.
    ///
    /// [`API_GATEWAY_ID`]: crate::projects_table::API_GATEWAY_ID
    const AWS_REGION: enso_cloud_http::AwsRegion = enso_cloud_http::AwsRegion::EuWest1;
    /// Access token used to authenticate requests to the Cloud API.
    // TODO [NP]: find a way to get the token from the browser
    const TOKEN: &str = "eyJraWQiOiJiVjd1ZExrWTkxU2lVUWRpWVhDSVByRitoSTRYVHlOYTQ2TXhJRDlmY3EwPSIsImFsZyI6IlJTMjU2In0.eyJzdWIiOiI0YjBhMzExNy1hNmI5LTRkYmYtOGEyMC0wZGZmNWE4NjA2ZGYiLCJjb2duaXRvOmdyb3VwcyI6WyJldS13ZXN0LTFfOUt5Y3UyU2JEX0dvb2dsZSJdLCJpc3MiOiJodHRwczpcL1wvY29nbml0by1pZHAuZXUtd2VzdC0xLmFtYXpvbmF3cy5jb21cL2V1LXdlc3QtMV85S3ljdTJTYkQiLCJ2ZXJzaW9uIjoyLCJjbGllbnRfaWQiOiI0ajliZnM4ZTc0MTVlcmY4MmwxMjl2MHFoZSIsIm9yaWdpbl9qdGkiOiI4MDYxMDkxMS01OGVlLTRjYzctYjU0Ny1lNmZjNzY2OTMwNmYiLCJ0b2tlbl91c2UiOiJhY2Nlc3MiLCJzY29wZSI6Im9wZW5pZCBlbWFpbCIsImF1dGhfdGltZSI6MTY2ODUyNzkwNiwiZXhwIjoxNjY4NTMxNTA2LCJpYXQiOjE2Njg1Mjc5MDYsImp0aSI6ImU2Y2QwZDBkLWZlMTgtNGYwYy1hZDQ5LTNmMGE2ZTk3NDY4MiIsInVzZXJuYW1lIjoiR29vZ2xlXzEwNDA4MDA2MTY5NTg0NDYwMDk2OCJ9.pAH00UChIp1R4Y_0UZg4nHycFuu75unANVYyRW_HtoMQLkRdiwYvBp5MUMU1swElY3tO5hNbfIfUciqAAMxkDyyr9GZzFLotV8bXuOUaXsCUhP2s2ew_HNsfpXjk9HTvn8eqhRQL9ZiA6iySYaAuTpSHT4x6iCgXyUK6kJolvdMFIkamcSeDoD-JfQAm5e1EtAFfoR5WT6o_Y8nV4VVWrOxT6bTkGUp9pu5Be1XdQS_nCzirj_GGWGj3rS9BQo1LaNMF9GSlFwqZ4rEKWD89tR1zxdx6GeYAU3DfMtgPto2ittElgm8ejQ3jaOrfkTDY19KI8Sogw1mS0BP9sGy4tg";



    // ==================================
    // === define_columns_enum! macro ===
    // ==================================

    /// A helper macro for defining the [`Columns`] enum.
    ///
    /// The variants of this enum are defined only at the location where the enum is invoked. This
    /// ensures that all the functions implemented on this enum always cover all the variants of the
    /// enum, and are defined programmatically rather than manually. This helps avoid drift and bugs
    /// due to programmer error.
    macro_rules! define_columns_enum  {
        (
            $($column_name:ident),*
        ) => {
            // ===============
            // === Columns ===
            // ===============

            /// Names of the columns that we want to display in the grid view. The [`Display`]
            /// implementation for this enum represents the user-facing values shown in the grid
            /// headers.
            ///
            /// These columns correspond roughly, but not exactly, to the fields of the [`Project`]
            /// struct. The differences are mainly in the fact that we display some properties of
            /// the [`Project`] as icons (e.g., the [`state`]).
            ///
            /// [`Project`]: ::enso_cloud_view::project::Project
            /// [`state`]: ::enso_cloud_view::project::Project.state
            #[derive(Clone, Copy, Debug, Eq, PartialEq)]
            #[allow(missing_docs)]
            pub enum Columns {
                $($column_name,)+
            }


            // === Main `impl` ===

            impl Columns {
                /// Number of columns that we want to display in the [`ProjectsTable`] view. This
                /// corresponds to the number of variants in this enum, since each variant is a
                /// column of data that we want to display.
                const LEN: usize = mem::variant_count::<Self>();

                /// Returns the [`Columns`] variant corresponding to the given discriminant value.
                ///
                /// If the discriminant is out of bounds, returns `None`.
                ///
                /// Example:
                /// ```rust
                /// # use debug_scene_cloud_dashboard::projects_table::Columns;
                /// assert_eq!(Columns::from_discriminant(0), Some(Columns::Projects));
                /// assert_eq!(Columns::from_discriminant(1), Some(Columns::LastModified));
                /// ```
                pub fn from_discriminant(discriminant: usize) -> Option<Self> {
                    // It is non-trivial to write a macro that can use `match` for this purpose,
                    // because a macro can't return both the pattern and the expression parts of a
                    // `match` arm (i.e., `x => y`). So instead we use an iterator over the values
                    // `0..` combined with `if` statements. This is functionally equivalent and is
                    // optimized by the compiler to the same instructions.
                    let mut i = 0..;
                    $(if discriminant == i.next().unwrap() { Some(Self::$column_name) } else)*
                    { unreachable!() }
                }
            }
        };
    }

    define_columns_enum!(Projects, LastModified, SharedWith, Labels, DataAccess, UsagePlan);


    // === Trait `impl`s ===

    impl Display for Columns {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let debug_str = format!("{:?}", self);
            let display_str = view::separate_pascal_case_str_with_spaces(&debug_str);
            write!(f, "{display_str}")
        }
    }



    // =====================
    // === ProjectsTable ===
    // =====================

    /// A table of [`Project`]s displayed in the Cloud dashboard.
    ///
    /// The table contains information about the [`Project`]s (e.g. their names, their state, etc.)
    /// as well as components for interacting with the [`Project`]s (e.g. buttons to start/stop) the
    /// projects, etc.
    ///
    /// Under the hood, we use a scrollable grid since the user may have more [`Project`]s than can
    /// fit on the screen. We use a grid with headers since the user needs to know which [`Project`]
    /// property each column represents.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    pub type ProjectsTable =
        ensogl_grid_view::simple::SimpleScrollableSelectableGridViewWithHeaders;



    // ================
    // === Position ===
    // ================

    /// The row and column coordinates of a cell in the [`ProjectsTable`].
    #[derive(Clone, Copy, Debug, Default)]
    pub struct Position {
        row:    Row,
        column: Col,
    }


    // === Trait `impl`s ===

    impl From<(Row, Col)> for Position {
        fn from((row, column): (Row, Col)) -> Self {
            Self { row, column }
        }
    }

    impl From<Position> for (Row, Col) {
        fn from(Position { row, column }: Position) -> Self {
            (row, column)
        }
    }



    // ===========
    // === FRP ===
    // ===========

    ensogl_core::define_endpoints_2! {
        Input {
            set_projects(Rc<Vec<view::project::Project>>),
            model_for_entry(Position, EntryModel),
        }
        Output {
            model_for_entry_needed(Position),
        }
    }



    // =============
    // === Model ===
    // =============

    #[derive(Clone, CloneRef, Debug)]
    struct Model {
        display_object: display::object::Instance,
        grid:           ProjectsTable,
        application:    Application,
        projects:       ide_view_graph_editor::SharedVec<view::project::Project>,
    }


    // === Main `impl` ===

    impl Model {
        fn new(app: &Application) -> Self {
            let application = app.clone_ref();
            let display_object = display::object::Instance::new();
            let grid = ProjectsTable::new(app);
            let projects = ide_view_graph_editor::SharedVec::new();
            display_object.add_child(&grid);
            Self { application, display_object, grid, projects }
        }

        fn model_for_entry(&self, position: Position) -> Option<(Position, EntryModel)> {
            let (row, column) = (position.row, position.column);
            // The first row is the header row, which is displayed with a different entry model, so
            // we should ignore it here.
            if row == 0 {
                return None;
            }
            // The rows of the grid are zero-indexed, but the first row is the header row, so we
            // need to subtract 1 to get the index of the project we want to display.
            let idx = row - 1;
            if idx >= self.projects.len() {
                warn!("Attempted to display entry at index {idx}, but we only have data up to index {}.", self.projects.len());
                return None;
            }
            let column = match Columns::from_discriminant(column) {
                Some(column) => column,
                None => {
                    warn!("Attempted to display entry at column {column}, but we the table only has {} columns.", Columns::LEN);
                    return None;
                }
            };

            let project = &self.projects.raw.borrow()[idx];
            // Map the requested column to the corresponding field of the `Project` struct.
            let model = match column {
                Columns::Projects => {
                    // TODO [NP]: Use a proper icon to display the project state.
                    let state = &project.state;
                    let name = &project.name;
                    let state = match state {
                        view::project::StateTag::New => "New".to_string(),
                        view::project::StateTag::Created => "Created".to_string(),
                        view::project::StateTag::OpenInProgress => "OpenInProgress".to_string(),
                        view::project::StateTag::Opened => "Opened".to_string(),
                        view::project::StateTag::Closed => "Closed".to_string(),
                    };
                    format!("({state}) {name}")
                }
                Columns::LastModified => LAST_MODIFIED.to_string(),
                // TODO [NP]: Display icons for users/groups with access to the project and their
                // corresponding permissions (e.g., read/write/execute).
                Columns::SharedWith => SHARED_WITH.to_string(),
                // TODO [NP]: Display icons for the project's labels. Labels may be user-defined
                // or system-defined (e.g., labels indicating high resource usage or outdated
                // version).
                Columns::Labels => LABELS.to_string(),
                // TODO [NP]: Display icons for datasets associated with the project, as well as
                // what permissions are set on the dataset, from the user's perspective.
                Columns::DataAccess => DATA_ACCESS.to_string(),
                // TODO [NP]: Display which usage plan the project is configured for (e.g.,
                // "Interactive" or cron-style, etc.).
                Columns::UsagePlan => USAGE_PLAN.to_string(),
            };

            let entry_model = EntryModel {
                text:           model.into(),
                disabled:       Immutable(false),
                override_width: Immutable(None),
            };
            Some((position, entry_model))
        }

        fn set_projects(&self, projects: Rc<Vec<view::project::Project>>) {
            *self.projects.raw.borrow_mut() = projects.to_vec();

            let row = self.rows();
            let col = Columns::LEN;
            self.grid.resize_grid(row, col);
        }

        fn refit_grid_to_shape(&self, shape: &ensogl::display::scene::Shape) {
            // FIXME [NP]: Make the margins into StyleWatchFrp variables?
            const TOP_MARGIN: f32 = 10f32;
            const BOTTOM_MARGIN: f32 = 10f32;
            const LEFT_MARGIN: f32 = 10f32;
            const RIGHT_MARGIN: f32 = 10f32;
            const HORIZONTAL_MARGIN: f32 = LEFT_MARGIN + RIGHT_MARGIN;
            const VERTICAL_MARGIN: f32 = TOP_MARGIN + BOTTOM_MARGIN;

            // === Resize ===

            let screen_size = Vector2::from(shape);
            let margin = Vector2::new(HORIZONTAL_MARGIN, VERTICAL_MARGIN);
            let size = screen_size - margin;
            self.grid.scroll_frp().resize(size);


            // === Reposition ===

            let screen_size = Vector2::from(shape);
            let screen_size_halved = screen_size / 2.0;
            let viewport_min_y = -screen_size_halved.y + BOTTOM_MARGIN;
            let viewport_min_x = -screen_size_halved.x + LEFT_MARGIN;
            let grid_height = screen_size.y * GRID_HEIGHT_RATIO;
            let grid_min_x = viewport_min_x;
            let grid_max_y = viewport_min_y + grid_height;
            let position = Vector2::new(grid_min_x, grid_max_y);
            self.grid.set_position_xy(position);
        }

        fn refit_entries_to_shape(&self, shape: &ensogl::display::scene::Shape) {
            // === Resize ===

            let width = shape.width / Columns::LEN as f32;
            let size = Vector2(width, ENTRY_HEIGHT);
            self.grid.set_entries_size(size);
        }
    }


    // === Getter `impl`s ===

    impl Model {
        /// Returns the number of [`ProjectsTable`] rows needed to display all the data in this
        /// [`Model`].
        ///
        /// The number of rows is equal to the number of [`Project`]s in the [`Model`] plus one,
        /// because the first row is the header row.
        ///
        /// [`Project`]: ::enso_cloud_view::project::Project
        fn rows(&self) -> usize {
            self.projects.len() + 1
        }
    }



    // ============
    // === View ===
    // ============

    /// The view implementation for the Cloud dashboard.
    ///
    /// This is the combination of the [`ProjectsTable`] controlling the rendering of the table of
    /// [`Project`]s, the [`Model`] containing the data to be displayed, and the [`Frp`] network
    /// used to update the [`Model`] with network-fetched [`Project`]s data and re-render the
    /// [`ProjectsTable`] accordingly.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    #[derive(Clone, Debug)]
    pub struct View {
        frp:   Frp,
        model: Model,
    }


    // === Main `impl` ===

    impl View {
        fn new(app: &Application) -> Self {
            let frp = Frp::new();
            let network = &frp.network;
            let model = Model::new(app);

            let projects_table = &model.grid;
            let root = &model.display_object;
            let input = &frp.public.input;
            let scene = &app.display.default_scene;

            frp::extend! { network


                // === Model I/O ===

                // FIXME [NP]: How do we get rid of this clone?
                eval input.set_projects((projects) model.set_projects(projects.clone()));


                // === Grid Resizing ===

                eval scene.frp.shape ((shape) model.refit_grid_to_shape(shape));
                eval scene.frp.shape ((shape) model.refit_entries_to_shape(shape));


                // === Entries Models ===

                // FIXME [NP]: How do we stop the grid from calling `model_for_entry` on the
                // header row? It should be handled via the later extension to the network.
                projects_table.model_for_entry <+
                    projects_table.model_for_entry_needed.filter_map(f!((input) model.model_for_entry((*input).into()).map(|(position, entry_model)| {
                        let (row, col) = position.into();
                        (row, col, entry_model)
                    })));
            }

            configure_projects_table_grid(projects_table, &model);
            configure_projects_table_header(projects_table, network);
            configure_projects_table_scroll_parameters(projects_table);

            app.display.add_child(root);

            populate_table_with_data(input);


            Self { frp, model }
        }
    }

    fn configure_projects_table_grid(projects_table: &ProjectsTable, model: &Model) {
        let entry_size = Vector2(ENTRY_WIDTH, ENTRY_HEIGHT);
        projects_table.set_entries_size(entry_size);
        let params = ensogl_grid_view::simple::EntryParams {
            // FIXME [NP]: Turn these into StyleWatchFrp values or at the very least constants.
            bg_color: color::Lcha::transparent(),
            bg_margin: 1.0,
            // TODO [NP]: Apply the hover color to the whole row, not just the selected entry.
            hover_color: color::Lcha::from(color::Rgba(
                62f32 / u8::MAX as f32,
                81f32 / u8::MAX as f32,
                95f32 / u8::MAX as f32,
                0.05,
            )),
            selection_color: color::Lcha::from(color::Rgba(1.0, 0.0, 0.0, 1.0)),
            ..default()
        };
        projects_table.set_entries_params(params);
        let row = model.rows();
        let col = Columns::LEN;
        projects_table.reset_entries(row, col);
        projects_table.focus();
    }

    fn configure_projects_table_header(
        projects_table: &ProjectsTable,
        network: &enso_frp::Network,
    ) {
        let header_frp = projects_table.header_frp();
        frp::extend! { network
            requested_section <- header_frp.section_info_needed.map(|&(row, col)| {
                let sections_size = 2 + col;
                let section_start = row - (row % sections_size);
                let section_end = section_start + sections_size;
                let position = (section_start, col).into();
                let model = header_entry_model(position);
                (section_start..section_end, col, model)
            });
            header_frp.section_info <+ requested_section;
        }
    }

    fn configure_projects_table_scroll_parameters(projects_table: &ProjectsTable) {
        // These sizes are arbitrary, because the grid will get resized as soon as the WebGL
        // context is ready (because the shape of the viewport will change as a result).
        let viewport_size = Vector2(VIEWPORT_WIDTH, VIEWPORT_HEIGHT);
        let scroll_frp = projects_table.scroll_frp();
        scroll_frp.resize(viewport_size);
    }

    fn populate_table_with_data(input: &api::public::Input) {
        let api_gateway_id = enso_cloud_http::ApiGatewayId(API_GATEWAY_ID.to_string());
        let token = enso_cloud_http::AccessToken(TOKEN.to_string());
        let client = enso_cloud_http::Client::new(api_gateway_id, AWS_REGION, token);
        get_projects(client, input.clone_ref());
    }

    // FIXME [NP]: Should we be assuming that the `row, col` are always in bounds? If we do so, we
    // can avoid having to return `Option` here, but we run the risk of panicking in the event of an
    // out of bounds `row, col`. Though this should only happen on implementation error.
    fn header_entry_model(position: Position) -> EntryModel {
        let Position { row, column } = position;
        assert!(row == 0, "Header row was {row}, but it is expected to be first row in the table.");

        let column = match Columns::from_discriminant(column) {
            Some(column) => column,
            None => panic!(
                "Attempted to display entry at column {column}, but we the table only has {} columns.",
                Columns::LEN
            ),
        };

        EntryModel {
            // TODO [NP]: Columns should not only render their name, but also their sorting,
            // represented by an arrow icon.
            text:           column.to_string().into(),
            disabled:       Immutable(true),
            override_width: Immutable(None),
        }
    }

    fn get_projects(
        client: enso_cloud_http::Client,
        input: crate::projects_table::api::public::Input,
    ) {
        wasm_bindgen_futures::spawn_local(async move {
            let response = client.list_projects().await.unwrap();
            let projects = response.projects;
            let projects = Rc::new(projects);
            input.set_projects(projects);
        });
    }


    // === Trait `impl`s ===

    impl Deref for View {
        type Target = Frp;
        fn deref(&self) -> &Self::Target {
            &self.frp
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
            "grid::View"
        }

        fn new(app: &Application) -> Self {
            Self::new(app)
        }

        fn app(&self) -> &Application {
            &self.model.application
        }
    }
}



// ===================
// === Entry Point ===
// ===================

/// The example entry point.
#[entry_point]
#[allow(dead_code)]
pub fn main() {
    ensogl_text_msdf::run_once_initialized(|| {
        let app = Application::new("root");
        theme::builtin::light::register(&app);
        theme::builtin::light::enable(&app);

        let scene = &app.display.default_scene;

        let projects_table = app.new_view::<projects_table::View>();
        scene.add_child(&projects_table);
    })
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use crate::projects_table;

    #[test]
    fn test_columns_display() {
        assert_eq!(projects_table::Columns::Projects.to_string(), "Projects");
        assert_eq!(projects_table::Columns::LastModified.to_string(), "Last Modified");
        assert_eq!(projects_table::Columns::SharedWith.to_string(), "Shared With");
        assert_eq!(projects_table::Columns::Labels.to_string(), "Labels");
        assert_eq!(projects_table::Columns::DataAccess.to_string(), "Data Access");
        assert_eq!(projects_table::Columns::UsagePlan.to_string(), "Usage Plan");
    }
}
