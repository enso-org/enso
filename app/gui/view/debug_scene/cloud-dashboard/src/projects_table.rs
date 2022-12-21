//! Module containing the [`ProjectsTable`] implementation, which is used to display a list of
//! [`Project`]s in the Cloud dashboard.
//!
//! The list of [`Project`]s, as rendered, contains information about the state of the [`Project`],
//! the data associated with the [`Project`], the permissions assigned to it, etc. Buttons for
//! interacting with the [`Project`]s (e.g., starting and stopping the [`Project`]s) are also
//! rendered.
//!
//! # Note
//!
//! The [`ProjectsTable`] *type* itself is declared in [`crate::projects_entry`], but all of the
//! implementation is contained in this module. This is done to ensure a one-way dependency between
//! this module and the [`crate::projects_entry`] module. That module also depends on the
//! [`ProjectsTable`] type definition, but aside from that is self-contained.
//!
//! [`Project`]: ::enso_cloud_view::project::Project
//! [`ProjectsTable`]: crate::projects_entry::ProjectsTable

use enso_cloud_view::prelude::*;
use ensogl::prelude::*;

use crate::projects_entry;
use crate::projects_spinner;

use enso_cloud_view as view;
use ensogl::application;
use ensogl::data::color;
use ensogl_core::application::Application;
use ensogl_core::display;
use ensogl_core::frp;
use ensogl_grid_view as grid_view;



// =================
// === Constants ===
// =================

/// The width of a single entry in the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// FIXME [NP]: https://www.pivotaltracker.com/story/show/183909467
//             Move the style constants to StyleWatchFrp variables.
const ENTRY_WIDTH: f32 = 130.0;
/// The height of a single entry in the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// FIXME [NP]: https://www.pivotaltracker.com/story/show/183909467
//             Move the style constants to StyleWatchFrp variables.
const ENTRY_HEIGHT: f32 = 28.0;
/// The grid is intended to take up 20% of the viewport height, expressed as a fraction;
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// FIXME [NP]: https://www.pivotaltracker.com/story/show/183909467
//             Move the style constants to StyleWatchFrp variables.
const GRID_HEIGHT_RATIO: f32 = 0.2;
/// The top margin of the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// FIXME [NP]: https://www.pivotaltracker.com/story/show/183909467
//             Move the style constants to StyleWatchFrp variables.
const TOP_MARGIN: f32 = 10f32;
/// The bottom margin of the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// FIXME [NP]: https://www.pivotaltracker.com/story/show/183909467
//             Move the style constants to StyleWatchFrp variables.
const BOTTOM_MARGIN: f32 = 10f32;
/// The left margin of the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// FIXME [NP]: https://www.pivotaltracker.com/story/show/183909467
//             Move the style constants to StyleWatchFrp variables.
const LEFT_MARGIN: f32 = 10f32;
/// The right margin of the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// FIXME [NP]: https://www.pivotaltracker.com/story/show/183909467
//             Move the style constants to StyleWatchFrp variables.
const RIGHT_MARGIN: f32 = 10f32;
/// The combined horizontal margin of the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
const HORIZONTAL_MARGIN: f32 = LEFT_MARGIN + RIGHT_MARGIN;
/// The combined vertical margin of the [`ProjectsTable`], in pixels.
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
const VERTICAL_MARGIN: f32 = TOP_MARGIN + BOTTOM_MARGIN;
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable

/// In the future, we want to display the last modification time of a [`Project`]. For now, the API
/// does not provide that information so we use a placeholder value.
///
/// [`Project`]: ::enso_cloud_view::project::Project
// TODO [NP]: https://www.pivotaltracker.com/story/show/183909494
//            Remove the unused columns from the `"Projects" Table`.
const LAST_MODIFIED: &str = "2022-10-08 13:30";
/// In the future, we want to display icons for users/groups with access to the [`Project`] and
/// their corresponding permissions (e.g., read/write/execute).
///
/// [`Project`]: ::enso_cloud_view::project::Project
// TODO [NP]: https://www.pivotaltracker.com/story/show/183909494
//            Remove the unused columns from the `"Projects" Table`.
const SHARED_WITH: &str = "Baron von Münchhausen (Read/Write)";
/// In the future, we want to display icons for the [`Project`]'s labels. Labels may be user-defined
/// or system-defined (e.g., labels indicating high resource usage or outdated version).
///
/// [`Project`]: ::enso_cloud_view::project::Project
// TODO [NP]: https://www.pivotaltracker.com/story/show/183909420
//            `"Home Screen" User` can see a `"running" Label` for any currently running `Project`s.
const LABELS: &str = "(!) outdated version";
/// In the future, we want to display icons for datasets associated with the [`Project`], as well as
/// what permissions are set on the dataset, from the user's perspective.
///
/// [`Project`]: ::enso_cloud_view::project::Project
// TODO [NP]: https://www.pivotaltracker.com/story/show/183909494
//            Remove the unused columns from the `"Projects" Table`.
const DATA_ACCESS: &str = "./user_data";
/// In the future, we want to display which usage plan the [`Project`] is configured for (e.g.,
/// "Interactive" or cron-style, etc.).
///
/// [`Project`]: ::enso_cloud_view::project::Project
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
// TODO [NP]: https://www.pivotaltracker.com/story/show/183909494
//            Remove the unused columns from the `"Projects" Table`.
const USAGE_PLAN: &str = "Interactive";

/// ID of the Amazon API Gateway serving the Cloud API.
const API_GATEWAY_ID: &str = "7aqkn3tnbc";
/// The AWS region in which the Amazon API Gateway referenced by [`API_GATEWAY_ID`] is deployed.
///
/// [`API_GATEWAY_ID`]: crate::projects_table::API_GATEWAY_ID
const AWS_REGION: enso_cloud_http::AwsRegion = enso_cloud_http::AwsRegion::EuWest1;
/// Access token used to authenticate requests to the Cloud API.
// TODO [NP]: https://www.pivotaltracker.com/story/show/183909294
//            `"Home Screen" User` is authenticated using the browser-stored `Access Token`.
const TOKEN: &str = "eyJraWQiOiJiVjd1ZExrWTkxU2lVUWRpWVhDSVByRitoSTRYVHlOYTQ2TXhJRDlmY3EwPSIsImFsZyI6IlJTMjU2In0.eyJzdWIiOiJiMWM0MjkwNi1mMmMxLTRkMTEtYTU2Mi01MDUzNTYyYjBhODciLCJjb2duaXRvOmdyb3VwcyI6WyJldS13ZXN0LTFfOUt5Y3UyU2JEX0dpdGh1YiJdLCJpc3MiOiJodHRwczpcL1wvY29nbml0by1pZHAuZXUtd2VzdC0xLmFtYXpvbmF3cy5jb21cL2V1LXdlc3QtMV85S3ljdTJTYkQiLCJ2ZXJzaW9uIjoyLCJjbGllbnRfaWQiOiI0ajliZnM4ZTc0MTVlcmY4MmwxMjl2MHFoZSIsIm9yaWdpbl9qdGkiOiJlODBhN2MyZi1iY2QwLTRlNjUtOTY2My1mMmM0ZWRmZjA5YWUiLCJ0b2tlbl91c2UiOiJhY2Nlc3MiLCJzY29wZSI6Im9wZW5pZCBlbWFpbCIsImF1dGhfdGltZSI6MTY3MTYzMjIwNywiZXhwIjoxNjcxNjM1ODA3LCJpYXQiOjE2NzE2MzIyMDcsImp0aSI6IjJkZTMwZWJiLTA4MWMtNDFiYy1iMjMyLWI2MWM3NmFmN2U4NCIsInVzZXJuYW1lIjoiR2l0aHViXzY0MTU0NyJ9.nDmOR7NO89Ja_f-9goo0IK2MZMFSq3aldF7zaxHJXIY7aP4SnK0_eCkUIgEUB0v0GPBglGo7gp7qs-i6WjH-yJKyCZ9G1F7Qi4jJEMr7MbJbsDb3IYMrXxm3RWH1QyDyWAZ06hyIDVrvKvOhCY_EKssWHLw01I2FNhmEEC1Ke4M4BpwLc2ck5ZQGAxv8Jp5AWZJVBVbFi6PoCpVAO_xQ8tSLvpVTAgk6BdYxuU5kXReKoyDgdOSCBu0YWbmsTdbKUqQktrATQuqBT7mostYGIDPoj53yG-tx2qK9k3AxADGxj_v1DswS2FAFI9WGEmPWUJUXC51-oHxH5BtR9FM7Pg";



// ==================================
// === define_columns_enum! macro ===
// ==================================

/// A helper macro for defining the [`Columns`] enum.
///
/// The variants of this enum are defined only at the location where the enum is invoked. This
/// ensures that all the functions implemented on this enum always cover all the variants of the
/// enum, and are defined programmatically rather than manually. This helps avoid drift and bugs due
/// to programmer error.
macro_rules! define_columns_enum  {
    ($($column_name:ident),*) => {
        // ===============
        // === Columns ===
        // ===============

        /// Names of the columns that we want to display in the grid view. The [`Display`]
        /// implementation for this enum represents the user-facing values shown in the grid
        /// headers.
        ///
        /// These columns correspond roughly, but not exactly, to the fields of the [`Project`]
        /// struct. The differences are mainly in the fact that we display some properties of the
        /// [`Project`] as icons (e.g., the [`state`]).
        ///
        /// [`Project`]: ::enso_cloud_view::project::Project
        /// [`state`]: ::enso_cloud_view::project::Project.state
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #[allow(missing_docs)]
        pub(crate) enum Columns {
            $($column_name,)+
        }


        // === Main `impl` ===

        impl Columns {
            /// Number of columns that we want to display in the [`ProjectsTable`] view. This
            /// corresponds to the number of variants in this enum, since each variant is a column
            /// of data that we want to display.
            ///
            /// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
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
            pub(crate) fn from_discriminant(discriminant: usize) -> Option<Self> {
                // It is non-trivial to write a macro that can use `match` for this purpose, because
                // a macro can't return both the pattern and the expression parts of a `match` arm
                // (i.e., `x => y`). So instead we use an iterator over the values `0..` combined
                // with `if` statements. This is functionally equivalent and is optimized by the
                // compiler to the same instructions.
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



// ================
// === Position ===
// ================

/// The row and column coordinates of a cell in the [`ProjectsTable`].
///
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
#[derive(Clone, Copy, Debug, Default)]
struct Position {
    row:    grid_view::Row,
    column: grid_view::Col,
}


// === Trait `impl`s ===

impl From<(grid_view::Row, grid_view::Col)> for Position {
    fn from((row, column): (grid_view::Row, grid_view::Col)) -> Self {
        Self { row, column }
    }
}

impl From<Position> for (grid_view::Row, grid_view::Col) {
    fn from(Position { row, column }: Position) -> Self {
        (row, column)
    }
}



// ==========================
// === ProjectStateChange ===
// ==========================

/// A request to change the [`State`] of the [`Project`] at the given `index` in the
/// [`ProjectsTable`].
///
/// For example, if the [`Project`] is currently [`Closed`], this will optimistically update the UI
/// to display the [`Project`] as [`OpenInProgress`] and then send a request to the backend to open
/// the [`Project`].
///
/// [`State`]: ::enso_cloud_view::project::StateTag
/// [`Project`]: ::enso_cloud_view::project::Project
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
/// [`Closed`]: ::enso_cloud_view::project::StateTag::Closed
/// [`OpenInProgress`]: ::enso_cloud_view::project::StateTag::OpenInProgress
#[derive(Clone, Copy, Debug)]
struct ProjectStateChange {
    index:     usize,
    new_state: view::project::StateTag,
    command:   projects_spinner::Command,
}


// === Main `impl` ===

impl ProjectStateChange {
    /// Create a new [`ProjectStateChange`] from the *current* state of the [`Project`] and the
    /// `index` that the [`Project`] is at in the [`ProjectsTable`].
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    /// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
    fn new(index: usize, previous_state: view::project::StateTag) -> Option<Self> {
        let (new_state, command) = match previous_state {
            view::project::StateTag::Created =>
                (view::project::StateTag::OpenInProgress, projects_spinner::Command::Open),
            view::project::StateTag::Closed =>
                (view::project::StateTag::OpenInProgress, projects_spinner::Command::Open),
            view::project::StateTag::Opened =>
                (view::project::StateTag::Closed, projects_spinner::Command::Close),
            view::project::StateTag::OpenInProgress =>
                (view::project::StateTag::Closed, projects_spinner::Command::Close),
            // If the project is in one of the following states, it can't transition to another
            // state yet.
            view::project::StateTag::New => None?,
        };
        Some(Self { index, new_state, command })
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        set_projects(Rc<Vec<view::project::Project>>),
    }
}



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
struct Model {
    display_object: display::object::Instance,
    projects_table: projects_entry::ProjectsTable,
    projects:       ide_view_graph_editor::SharedVec<view::project::Project>,
}


// === Internal `impl` ===

impl Model {
    fn new(app: &Application) -> Self {
        let display_object = display::object::Instance::new();
        let projects_table = projects_entry::ProjectsTable::new(app);
        let projects = ide_view_graph_editor::SharedVec::new();
        display_object.add_child(&projects_table);
        Self { display_object, projects_table, projects }
    }

    fn model_for_entry(
        &self,
        position: Position,
    ) -> Option<(Position, projects_entry::EntryModel)> {
        let Position { row, .. } = position;
        // If the row is the first one in the table, we're trying to render a header so we use that
        // model instead.
        if row == 0 {
            let entry_model = self.header_entry_model(position);
            Some((position, entry_model))
        } else {
            let idx = self.project_index_for_entry(position)?;
            let column = column_for_entry(position)?;
            let project = &self.projects.raw.borrow()[idx];
            let entry_model = project_entry_model(project, column);
            Some((position, entry_model))
        }
    }

    /// Returns the index of the [`Project`] in the list of [`Project`]s that corresponds to the
    /// provided [`Position`] of the [`ProjectsTable`].
    ///
    /// Aside from the header row, each row of the [`ProjectsTable`] contains data on a [`Project`]
    /// from the backing [`Model.projects`] list. But the index of the row doesn't correspond to the
    /// index in the table, since the header row doesn't contain data on a [`Project`]. So this
    /// function performs a conversion between the two indices.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    /// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
    fn project_index_for_entry(&self, position: Position) -> Option<usize> {
        let Position { row, .. } = position;
        // The rows of the grid are zero-indexed, but the first row is the header row, so we need to
        // subtract 1 to get the index of the project we want to display.
        let idx = row.checked_sub(1)?;
        if idx >= self.projects.len() {
            warn!(
                "Attempted to display entry at index {idx}, but we only have data up to index {}.",
                self.projects.len()
            );
            return None;
        }
        Some(idx)
    }

    fn resize_grid_to_shape(&self, shape: &ensogl::display::scene::Shape) -> Vector2<f32> {
        let screen_size = Vector2::from(shape);
        let margin = Vector2::new(HORIZONTAL_MARGIN, VERTICAL_MARGIN);
        let size = screen_size - margin;
        size
    }

    fn reposition_grid_to_shape(&self, shape: &ensogl::display::scene::Shape) {
        let screen_size = Vector2::from(shape);
        let screen_size_halved = screen_size / 2.0;
        let viewport_min_y = -screen_size_halved.y + BOTTOM_MARGIN;
        let viewport_min_x = -screen_size_halved.x + LEFT_MARGIN;
        let grid_height = screen_size.y * GRID_HEIGHT_RATIO;
        let grid_min_x = viewport_min_x;
        let grid_max_y = viewport_min_y + grid_height;
        let position = Vector2::new(grid_min_x, grid_max_y);
        self.projects_table.set_xy(position);
    }

    fn refit_entries_to_shape(&self, shape: &ensogl::display::scene::Shape) -> Vector2<f32> {
        let width = shape.width / Columns::LEN as f32;
        let size = Vector2(width, ENTRY_HEIGHT);
        size
    }

    /// Returns the information about what section the requested visible entry is in.
    ///
    /// The grid view wants to know what to display as a header of the top of the viewport in the
    /// current column. Therefore, it asks about information about the section the topmost visible
    /// entry is in.
    ///
    /// `section_info_needed` assumes that there may be more than one section, therefore it needs to
    /// know the span of rows belonging to the returned section (so it knows when to ask about
    /// another one).
    ///
    /// Since we use separate tables to display separate types of data in the dashboard, this table
    /// only ever has one section. So it should always return the range `0..=self.rows()`.
    fn position_to_requested_section(
        &self,
        position: Position,
    ) -> (Range<grid_view::Row>, grid_view::Col, projects_entry::EntryModel) {
        /// The first row in the section the requested visible entry is in.
        ///
        /// This is always `0` because we only have one section, so the first row is the first
        /// visible one in the table.
        const SECTION_START: usize = 0;
        /// The last row in the section the requested visible entry is in is an inclusive value, so
        /// we need to increment `self.rows()` by 1 to get it, since `self.rows()` isn't including
        /// the header row.
        const HEADER_OFFSET: usize = 1;

        let Position { column, .. } = position;
        let position = (SECTION_START, column).into();
        let model = self.header_entry_model(position);
        let section_end = self.projects.len() + HEADER_OFFSET;
        let section_range = SECTION_START..section_end;
        (section_range, column, model)
    }

    fn header_entry_model(&self, position: Position) -> projects_entry::EntryModel {
        let Position { row, .. } = position;
        assert!(row == 0, "Header row was {row}, but it is expected to be first row in the table.");
        let column = column_for_entry(position);
        let entry_model = column.map(|column| projects_entry::EntryModel {
            spinner_state:  Immutable(None),
            text:           column.to_string().into(),
            disabled:       Immutable(true),
            override_width: Immutable(None),
        });
        let entry_model = entry_model.unwrap_or_else(invalid_entry_model);
        entry_model
    }
}


// === Setter `impl` ===

impl Model {
    fn set_projects(
        &self,
        projects: Rc<Vec<view::project::Project>>,
    ) -> (grid_view::Row, grid_view::Col) {
        *self.projects.raw.borrow_mut() = projects.to_vec();

        let rows = self.rows();
        let cols = Columns::LEN;
        (rows, cols)
    }

    /// Sets the [`Project`] at a specified index. Does nothing if `index` is out of bounds.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    fn set_project(&self, project: view::project::Project, index: usize) {
        if let Some(p) = self.projects.raw.borrow_mut().get_mut(index) {
            *p = project;
        }
        self.projects_table.request_model_for_visible_entries();
    }
}

/// Returns an [`EntryModel`] representing a [`Project`] in the [`ProjectsTable`].
///
/// [`EntryModel`]: crate::projects_entry::EntryModel
/// [`Project`]: ::enso_cloud_view::project::Project
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
fn project_entry_model(
    project: &view::project::Project,
    column: Columns,
) -> projects_entry::EntryModel {
    // Map the requested column to the corresponding field of the `Project` struct.
    let text = match column {
        Columns::Projects => project.name.clone(),
        Columns::LastModified => LAST_MODIFIED.to_string(),
        Columns::SharedWith => SHARED_WITH.to_string(),
        // TODO [NP]: https://www.pivotaltracker.com/story/show/183909420
        //            `"Home Screen" User` can see a `"running" Label` for any currently running
        //            `Project`s.
        Columns::Labels => LABELS.to_string(),
        Columns::DataAccess => DATA_ACCESS.to_string(),
        Columns::UsagePlan => USAGE_PLAN.to_string(),
    };

    let spinner_state = match column {
        Columns::Projects => {
            let spinner_state = match project.state {
                view::project::StateTag::New => projects_spinner::State::Closed,
                view::project::StateTag::Created => projects_spinner::State::Closed,
                view::project::StateTag::OpenInProgress => projects_spinner::State::Opening,
                view::project::StateTag::Opened => projects_spinner::State::Opened,
                view::project::StateTag::Closed => projects_spinner::State::Closed,
            };
            Some(spinner_state)
        }
        _ => None,
    };

    projects_entry::EntryModel {
        spinner_state:  Immutable(spinner_state),
        text:           text.into(),
        disabled:       Immutable(false),
        override_width: Immutable(None),
    }
}

/// Returns an [`EntryModel`] representing an "invalid" entry, which is used to fill in the table in
/// the event that we request a [`Position`] that is out of bounds.
///
/// [`EntryModel`]: crate::projects_entry::EntryModel
fn invalid_entry_model() -> projects_entry::EntryModel {
    projects_entry::EntryModel {
        spinner_state:  Immutable(None),
        text:           "Invalid entry".into(),
        disabled:       Immutable(false),
        override_width: Immutable(None),
    }
}


// === Getter `impl`s ===

impl Model {
    /// Returns the number of [`ProjectsTable`] rows needed to display all the data in this
    /// [`Model`].
    ///
    /// The number of rows is equal to the number of [`Project`]s in the [`Model`] plus one, because
    /// the first row is the header row.
    ///
    /// [`Project`]: ::enso_cloud_view::project::Project
    /// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
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
/// [`Project`]s, the [`Model`] containing the data to be displayed, and the [`Frp`] network used to
/// update the [`Model`] with network-fetched [`Project`]s data and re-render the [`ProjectsTable`]
/// accordingly.
///
/// [`Project`]: ::enso_cloud_view::project::Project
/// [`ProjectsTable`]: crate::projects_entry::ProjectsTable
#[derive(Clone, Debug, Deref)]
pub struct View {
    #[deref]
    frp:    Frp,
    model:  Model,
    client: Rc<RefCell<Option<enso_cloud_http::Client>>>,
    app:    Application,
}


// === Internal `impl` ===

impl View {
    fn new(app: Application) -> Self {
        let frp = Frp::new();
        let model = Model::new(&app);
        let client = None;
        let client = Rc::new(RefCell::new(client));
        Self { frp, model, client, app }
    }

    pub fn init(self) -> Result<Self, Error> {
        let frp = &self.frp;
        let model = &self.model;
        let app = &self.app;
        let root = &model.display_object;
        let input = &frp.public.input;

        self.init_projects_table_model_data_loading();
        self.init_projects_table_grid_resizing(app);
        self.init_projects_table_entries_models();
        self.init_projects_table_grid();
        self.init_projects_table_header();
        self.init_entry_selected();
        self.init_event_tracing(app);

        app.display.add_child(root);

        // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909432
        //             Rather than pass these errors up, display the error in this view.
        let api_gateway_id = enso_cloud_http::ApiGatewayId(API_GATEWAY_ID.to_string());
        let token = enso_cloud_http::AccessToken::new(TOKEN)?;
        let base_url = enso_cloud_http::base_url_for_api_gateway(api_gateway_id, AWS_REGION)?;
        let client = enso_cloud_http::Client::new(base_url, token)?;
        *self.client.borrow_mut() = Some(client);

        // # Safety
        //
        // The client is initialized just above, so we know that it is safe to unwrap here. It is
        // also safe to unwrap past this point because the client is never set to `None` again.
        self.init_projects_table_data((*self.client).borrow().clone().unwrap(), input.clone_ref());

        Ok(self)
    }

    fn init_projects_table_model_data_loading(&self) {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let input = &frp.public.input;
        let projects_table = &model.projects_table;

        frp::extend! { network
            grid_size <- input.set_projects.map(f!((projects) model.set_projects(projects.clone_ref())));
            projects_table.resize_grid <+ grid_size;
            projects_table.reset_entries <+ grid_size;
        }
    }

    fn init_projects_table_grid_resizing(&self, app: &Application) {
        let network = &self.frp.network;
        let model = &self.model;
        let scene = &app.display.default_scene;
        let projects_table = &model.projects_table;
        let scroll_frp = projects_table.scroll_frp();

        frp::extend! { network
            scroll_frp.resize <+ scene.frp.shape.map(f!((shape) model.resize_grid_to_shape(shape)));
            eval scene.frp.shape ((shape) model.reposition_grid_to_shape(shape));
            projects_table.set_entries_size <+ scene.frp.shape.map(f!((shape) model.refit_entries_to_shape(shape)));
        }
    }

    fn init_projects_table_entries_models(&self) {
        let network = &self.frp.network;
        let model = &self.model;
        let projects_table = &model.projects_table;

        frp::extend! { network
            // We want to work with our `Position` struct rather than a coordinate pair, so convert.
            needed_entries <- projects_table.model_for_entry_needed.map(|position| Position::from(*position));
            projects_table.model_for_entry <+
                needed_entries.filter_map(f!((position) model.model_for_entry(*position).map(|(position, entry_model)| {
                    let (row, col) = position.into();
                    (row, col, entry_model)
                })));
        }
    }

    fn init_projects_table_grid(&self) {
        let model = &self.model;
        let projects_table = &model.projects_table;

        let entry_size = Vector2(ENTRY_WIDTH, ENTRY_HEIGHT);
        projects_table.set_entries_size(entry_size);
        let params = projects_entry::EntryParams {
            // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909458
            //             Move the EntryParams values to StyleWatchFrp values.
            bg_color: color::Lcha::transparent(),
            // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909458
            //             Move the EntryParams values to StyleWatchFrp values.
            bg_margin: 1.0,
            // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909458
            //             Move the EntryParams values to StyleWatchFrp values.
            // TODO [NP]: https://www.pivotaltracker.com/story/show/183909450
            //            `"Home Screen" User` can see `Project` row is highlighted when hovering
            //            over the row.
            hover_color: color::Lcha::from(color::Rgba(
                62f32 / u8::MAX as f32,
                81f32 / u8::MAX as f32,
                95f32 / u8::MAX as f32,
                0.05,
            )),
            // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909458
            //             Move the EntryParams values to StyleWatchFrp values.
            selection_color: color::Lcha::from(color::Rgba(
                62f32 / u8::MAX as f32,
                81f32 / u8::MAX as f32,
                95f32 / u8::MAX as f32,
                0.10,
            )),
            ..default()
        };
        projects_table.set_entries_params(params);
        let row = model.rows();
        let col = Columns::LEN;
        projects_table.reset_entries(row, col);
        projects_table.focus();
    }

    fn init_projects_table_header(&self) {
        let model = &self.model;
        let network = self.frp.network();
        let header_frp = model.projects_table.header_frp();

        frp::extend! { network
            requested_section <- header_frp.section_info_needed.map(f!((position) model.position_to_requested_section((*position).into())));
            header_frp.section_info <+ requested_section;
        }
    }

    fn init_entry_selected(&self) {
        let frp = &self.frp;
        let network = frp.network();
        let model = &self.model;
        let client = &self.client;
        let table = &model.projects_table;
        let input = &frp.public.input;

        frp::extend! { network
            // When an entry is selected, map that to a project in the projects list.
            selected_position <- table.entry_selected.filter_map(|position| *position);
            selected_position <- selected_position.map(|p| (*p).into());
            selected_index <- selected_position.map(f!((p) model.project_index_for_entry(*p)));
            selected_project <- selected_index.map(f!([model](i) i.and_then(f!((i) model.projects.raw.borrow().get(i).cloned()))));

            // When a project is selected, get the state and ID of that project.
            selected_project_state <- selected_project.map(|p| p.as_ref().map(|p| p.state));
            selected_project_id <- selected_project.map(|p| p.as_ref().map(|p| p.project_id));

            // When a project is selected, determine which state the project is transitioning to
            // (e.g. if we selected an open project, we're trying to close the project).
            state_change <- all_with(&selected_project_state, &selected_index,
                f!([](state, index) state.and_then(|state| index.and_then(|index| ProjectStateChange::new(index, state))))
            );
            // Apply the state change to the project's current state, then save the project's model to update the table.
            new_state <- state_change.map(|state_change| state_change.map(|sc| sc.new_state));
            new_project <- all_with(&selected_project, &new_state,
                move |project, state| {
                    let mut project = project.clone()?;
                    let state = (*state)?;
                    project.state = state;
                    Some(project)
                }
            );
            new_project_with_index <- all(selected_index, new_project);
            eval new_project_with_index (((selected_index, new_project)) model.set_project(new_project.clone().unwrap(), selected_index.unwrap()));

            // # Safety
            //
            // It is always safe to unwrap the client, since it is created during the `init` method
            // and never set to `None` again. It is a lazily-initialized field.
            eval selected_project_id ([client, input](selected_project_id) open_project(client.borrow().clone().unwrap(), input.clone_ref(), selected_project_id.unwrap()));
        }
    }

    fn init_event_tracing(&self, app: &Application) {
        let frp = &self.frp;
        let network = &frp.network;
        let model = &self.model;
        let projects_table = &model.projects_table;
        let input = &frp.public.input;
        let scene = &app.display.default_scene;

        frp::extend! { network
            trace input.set_projects;
            trace projects_table.model_for_entry;
            trace scene.frp.shape;
        }
    }

    fn init_projects_table_data(
        &self,
        client: enso_cloud_http::Client,
        input: crate::projects_table::api::public::Input,
    ) {
        // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909482
        //             Replace `wasm_bindgen_futures` with the futures runtime used throughout the
        //             remainder of the project.
        wasm_bindgen_futures::spawn_local(async move {
            let response = client.list_projects().await.unwrap();
            let projects = response.projects;
            let projects = Rc::new(projects);
            input.set_projects(projects);
        });
    }
}

/// Returns the [`Columns`] variant for the entry at the given [`Position`], letting us select over
/// what field of data from a [`Project`] we want to display.
///
/// [`Project`]: ::enso_cloud_view::project::Project
fn column_for_entry(position: Position) -> Option<Columns> {
    let Position { column, .. } = position;
    let column = match Columns::from_discriminant(column) {
        Some(column) => column,
        None => {
            warn!("Attempted to display entry at column {column}, but we the table only has {} columns.", Columns::LEN);
            return None;
        }
    };
    Some(column)
}


fn open_project(
    client: enso_cloud_http::Client,
    _input: crate::projects_table::api::public::Input,
    project_id: view::id::ProjectId,
) {
    // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909482
    //             Replace `wasm_bindgen_futures` with the futures runtime used throughout the
    //             remainder of the project.
    wasm_bindgen_futures::spawn_local(async move {
        let result = client.open_project(project_id).await;
        match result {
            // TODO [NP]: https://www.pivotaltracker.com/story/show/183909415
            //            Bind the result back to the table.
            Ok(_) => {}
            // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909432
            //             Rather than pass these errors up, display the error in this view.
            Err(e) => error!("Failed to open project: {e}"),
        }
    });
}

fn close_project(
    client: enso_cloud_http::Client,
    _input: crate::projects_table::api::public::Input,
    project_id: view::id::ProjectId,
) {
    // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909482
    //             Replace `wasm_bindgen_futures` with the futures runtime used throughout the
    //             remainder of the project.
    wasm_bindgen_futures::spawn_local(async move {
        let result = client.close_project(project_id).await;
        match result {
            // TODO [NP]: https://www.pivotaltracker.com/story/show/183909415
            //            Bind the result back to the table.
            Ok(_) => {}
            // FIXME [NP]: https://www.pivotaltracker.com/story/show/183909432
            //             Rather than pass these errors up, display the error in this view.
            Err(e) => error!("Failed to open project: {e}"),
        }
    });
}


// === Trait `impl`s ===

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
        let app = app.clone_ref();
        Self::new(app)
    }

    fn app(&self) -> &Application {
        &self.app
    }
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
