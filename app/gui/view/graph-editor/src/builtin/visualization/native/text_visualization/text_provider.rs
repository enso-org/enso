//! Text providers that can be used to back a text grid visualization.
//!
//! Contains a dummy implementation [`StringTextProvider`] that is backed by a `String` that can be
//! used for testing, and the text provider `BackendTextProvider` used for communicating with the
//! backend.

use crate::builtin::visualization::native::text_visualization::*;
use crate::prelude::*;

use crate::builtin::visualization::native::text_visualization::grid_cache::GridCache;
use crate::builtin::visualization::native::text_visualization::CACHE_PADDING;
use crate::component::visualization;
use crate::component::visualization::instance::PreprocessorConfiguration;

use super::GridPosition;
use super::GridSize;
use super::GridWindow;
use enso_prelude::serde_reexports::Deserialize;



/// Width of a divider (e.g., space between columns) in the table in characters.
const DIVIDER_WIDTH_IN_CHARS: usize = 3;

// ===========================
// === Text Provider Trait ===
// ===========================

/// Trait for providing text for the TextGrid.
pub trait TextProvider {
    /// Return a slice of the text.
    ///
    /// The slice is indexed by the line and the "chunk", where a chunk is a sequence of characters
    /// of fixed length, into which the line is divided. For example, for "abcdef" there could
    /// be chunks of size two: ["ab", "cd", "ef"], or of size three ["abc", "def"].
    fn get_slice(&self, line: usize, chunk_index: usize) -> Option<String>;

    /// Return the FRP api for the text provider.
    fn frp(&self) -> &Frp;
}



// ===========================
// === Text Provider FRP ===
// ===========================

type ColumnIndex = usize;
/// Column Width in characters.
type ColumnWidth = usize;
/// Column Width in lines
type RowHeight = usize;
type RowIndex = usize;

/// Specification of a table layout. Contains the column and row widths, as well as the column and
/// row names. Provides methods for updating the specification and for accessing layout properties.
/// We allow for a table specification to be partially specified, i.e., some of the information may
/// be `None`. This is used to allow for lazy initialisation of the layout and incremental updates
/// as the table is updated.
#[derive(Clone, Debug, Deserialize, Serialize, Default)]
pub struct TableSpecification {
    /// The width of each column in characters.
    pub columns:      Vec<Option<ColumnWidth>>,
    /// The height of each row in lines.
    pub rows:         Vec<Option<RowHeight>>,
    /// The name of each column.
    pub column_names: Vec<Option<String>>,
    /// The name of each row.
    pub row_names:    Vec<Option<String>>,
}


#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq, Copy)]
/// A table item and its position. Only used for working on the layout, not for individual cells
/// or accessing content.
pub enum TableItem {
    /// A row or column heading.
    Heading {
        /// The size of the heading in characters.  
        size:   usize,
        /// The offset of this heading from the start of the heading (used in case of a heading
        /// that spans multiple adjacent items, e.g., when divided by chunks).
        offset: usize,
    },
    /// A cell with text in the table.
    Content {
        /// The position of the cell as content row/column index.
        index:  usize,
        /// The size of the cell in characters.
        size:   usize,
        /// The offset of this cell from the start of the cell (used in case of a cell that spans
        /// multiple adjacent items, e.g., when divided by chunks).
        offset: usize,
    },
    /// A divider between rows or columns.
    Divider,
}

impl TableItem {
    /// Return the size of the item in characters.
    pub fn size(&self) -> usize {
        match self {
            TableItem::Content { size, .. } => *size,
            TableItem::Divider => DIVIDER_WIDTH_IN_CHARS,
            TableItem::Heading { size, .. } => *size,
        }
    }
}

/// A table content item. Either a cell with text, a divider, or a row or column heading.
#[derive(Copy, Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub enum TableContentItem {
    /// A cell with text in the table.
    Content {
        /// The position of the cell as content row/column index.
        content_index: GridPosition,
    },
    /// A divider between rows or columns.
    Divider {
        /// Whether the divider connect to the bottom.
        bottom: bool,
        /// Whether the divider connect to the top.
        top:    bool,
        /// Whether the divider connect to the left.
        left:   bool,
        /// Whether the divider connect to the right.
        right:  bool,
    },
    /// A column heading.
    ColumnHeading {
        /// The index of the column the heading belongs to.
        column_index: usize,
        /// The index of the chunk within the heading string.
        chunk_index:  usize,
        /// The index of the line within the heading string.
        line_index:   usize,
    },
    /// A row heading.
    RowHeading {
        /// The index of the row the heading belongs to.
        row_index:   usize,
        /// The index of the chunk within the heading string.
        chunk_index: usize,
        /// The index of the line within the heading string.
        line_index:  usize,
    },
    /// An empty cell in the table.
    Empty,
}

/// Insert the given values ath the corresponding index into the target vector, resizing the target
/// vector if necessary.
fn resize_and_insert<T: Clone>(values: &[(usize, T)], target: &mut Vec<Option<T>>) {
    let max_index = values.iter().map(|(index, _)| *index).max();
    if let Some(max_index) = max_index {
        target.resize(max_index + 1, None);
        for (index, value) in values.iter() {
            target[*index] = Some(value.clone());
        }
    }
}

impl TableSpecification {
    /// Update the specification with the given update.
    fn update(&mut self, update: &TableSpecificationUpdate) {
        resize_and_insert(&update.column_widths, &mut self.columns);
        resize_and_insert(&update.row_heights, &mut self.rows);
        resize_and_insert(&update.column_names, &mut self.column_names);
        resize_and_insert(&update.row_names, &mut self.row_names);
    }

    /// Return whether the given line of the GridView should be a divider.
    /// This assumes a divider at the start and the end of the table, and a divider between each
    /// row. The divider is one line high.
    pub fn is_row_divider(&self, line: usize) -> bool {
        self.iter_row_items_per_line().nth(line) == Some(TableItem::Divider)
    }

    /// Return the width of the content column at the given index in chars.
    fn column_width(&self, index: usize) -> usize {
        let column_width = self.columns.get(index).copied().flatten().unwrap_or_default();
        let heading_width = self
            .column_names
            .get(index)
            .cloned()
            .flatten()
            .map(|x| x.chars().count())
            .unwrap_or_default();
        column_width.max(heading_width)
    }

    /// Return the height of the content row at the given index in lines.
    fn row_height(&self, index: usize) -> usize {
        let row_height = self.rows.get(index).copied().flatten().unwrap_or_default();
        let heading_height = self
            .column_names
            .get(index)
            .cloned()
            .flatten()
            .map(|x| x.lines().count())
            .unwrap_or_default();
        row_height.max(heading_height)
    }

    /// Iterate over the item in the table. Only returns the actual content items. Does not return
    /// the dividers or the headings.
    fn iter_content_columns(&self) -> impl Iterator<Item = TableItem> + '_ {
        self.columns.iter().enumerate().map(|(index, _)| TableItem::Content {
            size: self.column_width(index),
            index,
            offset: 0,
        })
    }

    /// Iterate over the item in the table. Only returns the actual content items. Does not return
    /// the dividers or the headings.
    fn iter_content_rows(&self) -> impl Iterator<Item = TableItem> + '_ {
        self.rows.iter().enumerate().map(|(index, _)| TableItem::Content {
            size: self.row_height(index),
            index,
            offset: 0,
        })
    }

    /// Return the number of content rows. This does noit count the row headings or the row
    /// dividers.
    pub fn content_row_count(&self) -> usize {
        self.rows.len()
    }

    /// Return the number of content columns. This does not count the column headings or the column
    /// dividers.
    pub fn content_column_count(&self) -> usize {
        self.columns.len()
    }

    /// Return the width of the table in characters.
    pub fn width_in_chars(&self) -> usize {
        let res = self.iter_column_items().map(|x| x.size()).sum();
        res
    }

    /// Convert the given cell / chunk position from the backend data into a chunk position for the
    /// text cache.
    pub fn backend_table_position_to_grid_content_position(
        &self,
        table_position: TablePosition,
    ) -> GridPosition {
        let TablePosition { cell, chunk } = table_position;
        let (col, row) = (cell.x as usize, cell.y as usize);
        let chunk_x = self
            .columns
            .iter()
            .take(col)
            .fold(0, |acc, x| acc + x.unwrap_or(0).div_ceil(CHARS_PER_CHUNK))
            + chunk.x as usize;
        let chunk_y =
            self.rows.iter().take(row).fold(0, |acc, x| acc + x.unwrap_or(0)) + chunk.y as usize;
        GridPosition::new(chunk_x as i32, chunk_y as i32)
    }

    /// Convert the given character chunk / line position into a cell / chunk / line position.
    /// Ignoring column/row names. This does not consider the dividers, as neither the
    /// text cache nor the backend data consider them.
    pub fn grid_content_position_to_table_position(
        &self,
        grid_position: GridPosition,
    ) -> TablePosition {
        let (chunk_x, chunk_y) = (grid_position.x as usize, grid_position.y as usize);
        let mut column_ix = 0;
        let mut row_ix = 0;
        let mut all_column_width = 0;
        let mut all_row_height = 0;
        for (i, width) in self.columns.iter().enumerate() {
            if let Some(width) = width {
                all_column_width += width;
                if chunk_x < all_column_width {
                    column_ix = i;
                    // We want to remember the width before the current column.
                    all_column_width -= width;
                    break;
                }
            }
        }
        for (i, height) in self.rows.iter().enumerate() {
            if let Some(height) = height {
                all_row_height += height;
                if chunk_y < all_row_height {
                    row_ix = i;
                    // We want to remember the height before the current row.
                    all_row_height -= height;
                    break;
                }
            }
        }
        let chunk_x = chunk_x - all_column_width;
        let chunk_y = chunk_y - all_row_height;
        let cell = GridPosition::new(column_ix as i32, row_ix as i32);
        let chunk = GridPosition::new(chunk_x as i32, chunk_y as i32);

        TablePosition { cell, chunk }
    }

    /// Return the size of the grid in chunks/lines. This includes the dividers, but not the
    /// column/row names.
    pub fn grid_size_in_chunks(&self) -> (usize, usize) {
        let column_content_width: usize = self
            .columns
            .iter()
            .map(|width| width.map(|value| value.div_ceil(CHARS_PER_CHUNK)).unwrap_or(1))
            .sum();
        let column_content_height: usize = self.rows.iter().map(|height| height.unwrap_or(1)).sum();

        // We need to account for dividers.
        let height = column_content_height + self.content_row_count() - 1;
        let width = column_content_width + self.content_column_count() + 1;
        (width, height)
    }

    /// Iterate over the column layout items.
    pub fn iter_column_items(&self) -> impl Iterator<Item = TableItem> + '_ {
        let content_columns = self.iter_content_columns();
        [TableItem::Heading { size: self.row_heading_width(), offset: 0 }, TableItem::Divider]
            .into_iter()
            .chain(Iterator::intersperse(content_columns, TableItem::Divider))
            .chain([TableItem::Divider])
    }

    /// Iterate over the row layout items.
    fn iter_row_items(&self) -> impl Iterator<Item = TableItem> + '_ {
        let content_rows = self.iter_content_rows();
        [TableItem::Heading { size: self.column_heading_height(), offset: 0 }, TableItem::Divider]
            .into_iter()
            .chain(content_rows)
            .chain([TableItem::Divider])
    }

    /// Iterate over the items in the table, but each item is split into multiple items, one for
    /// each line in the item.
    fn iter_row_items_per_line(&self) -> impl Iterator<Item = TableItem> + '_ {
        self.iter_row_items().flat_map(|item| match item {
            TableItem::Content { size, index, .. } =>
                (0..size).map(|offset| TableItem::Content { size: 1, index, offset }).collect_vec(),
            TableItem::Divider => iter::once(TableItem::Divider).collect_vec(),
            TableItem::Heading { size, .. } =>
                (0..size).map(|offset| TableItem::Heading { size: 1, offset }).collect_vec(),
        })
    }

    /// Iterate over the items in the table, but each item is split into multiple items, one for
    /// each chunk in the item.
    fn iter_column_items_per_chunk(&self) -> impl Iterator<Item = TableItem> + '_ {
        self.iter_column_items().flat_map(|item| match item {
            TableItem::Content { size, index, .. } => (0..size.div_ceil(CHARS_PER_CHUNK))
                .map(|offset| TableItem::Content { size: 1, index, offset })
                .collect_vec(),
            TableItem::Divider => iter::once(TableItem::Divider).collect_vec(),
            TableItem::Heading { size, .. } => (0..size.div_ceil(CHARS_PER_CHUNK))
                .map(|offset| TableItem::Heading { size: 1, offset })
                .collect_vec(),
        })
    }

    /// Return the index of the content row at the given chunk line index.
    pub fn get_content_row_index(&self, row: usize) -> Option<usize> {
        let row_item = self.iter_row_items_per_line().nth(row)?;
        match row_item {
            TableItem::Content { index, .. } => Some(index),
            _ => None,
        }
    }

    /// Return the table item at the given position in the grid. The given grid coordinates address
    /// the grid in chunks/lines.
    pub fn grid_view_position_to_table_item(
        &self,
        position: ChunkCoordinate,
    ) -> Option<TableContentItem> {
        let row_item = self.iter_row_items_per_line().nth(position.y as usize)?;
        let column_item = self.iter_column_items_per_chunk().nth(position.x as usize)?;

        let last_row_index = self.iter_row_items_per_line().count() - 1;
        let last_column_index = self.iter_column_items_per_chunk().count() - 1;
        let row = position.y as usize;
        let column = position.x as usize;

        use TableItem::*;
        match (column_item, row_item) {
            (
                Content { index: column_index, offset: column_offset, .. },
                Content { index: row_index, offset: row_offset, .. },
            ) => {
                // If a heading is larger than the content, we can get overhang items that do not
                // have a corresponding content item and need to be empty.
                let is_overhang_column = column_offset * CHARS_PER_CHUNK
                    >= self.columns.get(column_index).copied().flatten().unwrap_or(0);
                let is_overhang_row =
                    row_offset >= self.rows.get(row_index).copied().flatten().unwrap_or(0);
                if is_overhang_column || is_overhang_row {
                    Some(TableContentItem::Empty)
                } else {
                    let cell = GridPosition::new(column_index as i32, row_index as i32);
                    let chunk = GridPosition::new(column_offset as i32, row_offset as i32);
                    let table_index = TablePosition::new(cell, chunk);
                    let content_index =
                        self.backend_table_position_to_grid_content_position(table_index);
                    Some(TableContentItem::Content { content_index })
                }
            }
            (
                Heading { offset: column_offset, .. },
                Content { index: row_index, offset: row_offset, .. },
            ) => Some(TableContentItem::RowHeading {
                row_index,
                chunk_index: column_offset,
                line_index: row_offset,
            }),
            (
                Content { index: column_index, offset: column_offset, .. },
                Heading { offset: row_offset, .. },
            ) => Some(TableContentItem::ColumnHeading {
                column_index,
                chunk_index: column_offset,
                line_index: row_offset,
            }),
            (Heading { .. }, Heading { .. }) => {
                // This is the top left corner of the table.
                Some(TableContentItem::Empty)
            }
            (Divider, Divider) => Some(TableContentItem::Divider {
                bottom: row != last_row_index,
                top:    true,
                left:   column != 0,
                right:  column != last_column_index,
            }),
            (Divider, Content { .. }) => Some(TableContentItem::Divider {
                bottom: row != last_row_index,
                top:    row != 0,
                left:   false,
                right:  false,
            }),
            (Content { .. }, Divider) => Some(TableContentItem::Divider {
                bottom: false,
                top:    false,
                left:   column != 0,
                right:  column != last_column_index,
            }),
            (Heading { .. }, Divider) => Some(TableContentItem::Divider {
                bottom: false,
                top:    false,
                left:   true,
                right:  true,
            }),
            (Divider, Heading { .. }) => Some(TableContentItem::Divider {
                bottom: true,
                top:    true,
                left:   false,
                right:  false,
            }),
        }
    }

    /// Return height of the column names in lines.
    fn column_heading_height(&self) -> usize {
        self.column_names
            .iter()
            .filter_map(|name| name.as_ref())
            .map(|name| name.lines().count())
            .max()
            .unwrap_or(1)
    }

    /// Return width of the row names in characters.
    fn row_heading_width(&self) -> usize {
        self.row_names
            .iter()
            .filter_map(|name| name.as_ref())
            .map(|name| name.chars().count())
            .max()
            .unwrap_or(1)
    }

    /// Return the name of the column at the given index.
    pub fn column_name(&self, column_ix: usize) -> Option<&str> {
        self.column_names.get(column_ix).and_then(|name| name.as_ref()).map(|x| &**x)
    }

    /// Return the name of the row at the given index.
    pub fn row_name(&self, row_ix: usize) -> Option<&str> {
        self.row_names.get(row_ix).and_then(|name| name.as_ref()).map(|x| &**x)
    }
}



ensogl::define_endpoints_2! {
    Input {}
    Output {
        line_count(u32),
        chars_in_longest_line(u32),
        table_specification(Option<TableSpecification>),
        data_refresh(),
    }
}



// =============================
// === String Text Provider ====
// =============================

/// A text provider that is backed by a string.
#[derive(Debug)]
pub struct StringTextProvider {
    text:       String,
    frp:        Frp,
    chunk_size: usize,
}

impl StringTextProvider {
    /// Create a new [`StringTextProvider`].
    pub fn new(text: String, chunk_size: usize) -> Self {
        let frp = Frp::new();

        let line_count = text.lines().count() as u32;
        frp.private().output.line_count.emit(line_count);
        let longest_line_frp = &frp.private().output.chars_in_longest_line;
        let longest_line = text.lines().map(|line| line.chars().count()).max().unwrap_or(0) as u32;
        longest_line_frp.emit(longest_line);

        Self { text, frp, chunk_size }
    }
}

impl TextProvider for StringTextProvider {
    fn get_slice(&self, line: usize, chunk_index: usize) -> Option<String> {
        self.text.lines().nth(line).and_then(|line| {
            line.chars()
                .chunks(self.chunk_size)
                .into_iter()
                .nth(chunk_index)
                .map(|chunk| chunk.collect::<String>())
        })
    }

    fn frp(&self) -> &Frp {
        &self.frp
    }
}


// ====================================
// === Infinite Grid Text Provider ====
// ====================================

/// A table data provider that provides an arbitrary number of rows and columns.
#[derive(Debug)]
pub struct DebugGridTextProvider {
    frp: Frp,
}

impl DebugGridTextProvider {
    /// Create a new [`DebugGridTextProvider`].
    pub fn new(column_count: u32, line_count: u32) -> Self {
        let frp = Frp::new();

        frp.private().output.line_count.emit(line_count);
        frp.private().output.chars_in_longest_line.emit(column_count * CHARS_PER_CHUNK as u32);

        let spec = TableSpecification {
            columns:      Self::column_widths(column_count, line_count),
            rows:         iter::repeat(Some(1)).take(line_count as usize).collect(),
            column_names: vec![],
            row_names:    vec![],
        };
        frp.private().output.table_specification.emit(Some(spec));

        Self { frp }
    }

    fn column_widths(column_count: u32, row_count: u32) -> Vec<Option<usize>> {
        let mut widths = Vec::with_capacity(column_count as usize);
        let max_row_index_text_width = row_count.to_string().chars().count();
        let divider_width = 1;

        for column_ix in 0..column_count {
            let column_index_text_width = column_ix.to_string().chars().count();
            let width = max_row_index_text_width + divider_width + column_index_text_width;
            widths.push(Some(width));
        }

        widths
    }
}

impl TextProvider for DebugGridTextProvider {
    fn get_slice(&self, line: usize, chunk_index: usize) -> Option<String> {
        let line = line as u32;
        let text = format!("{chunk_index}/{line}");
        Some(text)
    }

    fn frp(&self) -> &Frp {
        &self.frp
    }
}



// =============================
// === Backend Text Provider ===
// =============================

const DEFAULT_GRID_SIZE: u32 = 20;

/// A cache for a grid of Strings.
///
/// The cache is backed by the engine and will communicate via the FRP endpoints passed in the
/// constructor.
#[derive(Debug, Clone)]
pub struct BackendTextProvider {
    frp:             Frp,
    text_cache:      Rc<RefCell<GridCache<String>>>,
    register_access: frp::Any,
}

impl BackendTextProvider {
    /// Create a new `BackendTextProvider`.
    pub fn new(
        receive_data: frp::Source<visualization::Data>,
        preprocessor_update: frp::Any<PreprocessorConfiguration>,
    ) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let output = frp.private().output.clone_ref();

        let longest_observed_line: Rc<Cell<u32>> = default();
        let max_observed_lines: Rc<Cell<u32>> = default();

        frp::extend! { network
            grid_window <- any_mut::<GridWindow>();
        }

        let grid_cache_update = f!((new_window) grid_window.emit(new_window));

        let text_cache = Rc::new(RefCell::new(GridCache::<String>::new(
            GridPosition::default(),
            GridSize::new(DEFAULT_GRID_SIZE, DEFAULT_GRID_SIZE),
            CACHE_PADDING,
            Box::new(grid_cache_update),
        )));

        frp::extend! { network
            register_access <- any_mut();
            update_preprocessor <- all(register_access, grid_window);
            update_preprocessor <- update_preprocessor._1().on_change();
            preprocessor_update <+ update_preprocessor.map(|grid_window| {
                let grid_posititon = grid_window.position.map(|value| value.max(0));
                let grid_size = grid_window.size;
                if grid_size == GridSize::default() {
                    None
                } else {
                    Some(text_preprocessor(grid_posititon, grid_size))
                }
            }).unwrap();
            grid_data_update <- receive_data.map(|data| {
                LazyGridDataUpdate::try_from(data.clone()).ok()
            });
            grid_data_update <- grid_data_update.map(|data| data.clone()).unwrap();
            needs_refresh <- grid_data_update.map(f!([text_cache,longest_observed_line,max_observed_lines](update) {
                let needs_refresh = update.update_type == UpdateType::FullUpdate;
                if needs_refresh {
                    text_cache.borrow_mut().clear();
                    longest_observed_line.set(1);
                    max_observed_lines.set(1);
                }

                for (pos, text) in &update.data.chunks {
                    if let Some(text) = text {
                        text_cache.borrow_mut().add_item(*pos, text.clone());
                    } else {
                        text_cache.borrow_mut().add_item(*pos, "".to_string());
                    }
                }
                needs_refresh
            }));
            output.data_refresh <+ needs_refresh.on_true().constant(());

            line_count <- grid_data_update.map(|grid_data| grid_data.data.line_count);
            longest_line <- grid_data_update.map(|grid_data| grid_data.data.longest_line);

            output.chars_in_longest_line <+ longest_line.map(f!([longest_observed_line](longest_line) {
                    let observed_value = longest_observed_line.get();
                    let longest_line = observed_value.max(*longest_line);
                    longest_observed_line.set(longest_line);
                    longest_line
            })).on_change();

            output.line_count <+ line_count.map(f!([max_observed_lines](line_count) {
                let observed_value = max_observed_lines.get();
                let max_lines = observed_value.max(*line_count);
                max_observed_lines.set(max_lines);
                max_lines
            })).on_change();
        }

        Self { frp, text_cache, register_access }
    }
}

impl TextProvider for BackendTextProvider {
    fn get_slice(&self, line: usize, chunk_index: usize) -> Option<String> {
        let y = line as i32;
        let x = chunk_index as i32;
        let result = self.text_cache.borrow_mut().get_item(Vector2::new(x, y));
        self.register_access.emit(());
        result
    }

    fn frp(&self) -> &Frp {
        &self.frp
    }
}

/// Crate a preprocessor configuration for the lazy text preprocessor.
fn text_preprocessor(
    grid_position: GridPosition,
    grids_size: GridSize,
) -> PreprocessorConfiguration {
    PreprocessorConfiguration::new(
        "Standard.Visualization.Preprocessor",
        "lazy_preprocessor",
        vec![
            serde_json::to_string(&grid_position)
                .expect("Could not serialise [`GridPosition`] as JSON string."),
            serde_json::to_string(&grids_size)
                .expect("Could not serialise [`GridSize`] as JSON string."),
            serde_json::to_string(&(CHARS_PER_CHUNK as u32))
                .expect("Could not serialise [`u32`] as JSON string."),
        ],
    )
}


/// Crate a preprocessor configuration for the lazy text preprocessor.
fn lazy_table_preprocessor(
    window_position: TablePosition,
    grids_size: GridSize,
) -> PreprocessorConfiguration {
    PreprocessorConfiguration::new(
        "Standard.Visualization.Preprocessor",
        "lazy_preprocessor",
        vec![
            serde_json::to_string(&window_position.cell.map(|cell| cell.max(0)))
                .expect("Could not serialise [`TablePosition`] as JSON string."),
            serde_json::to_string(&window_position.chunk.map(|chunk| chunk.max(0)))
                .expect("Could not serialise [`TablePosition`] as JSON string."),
            serde_json::to_string(&grids_size.map(|size| size.max(0)))
                .expect("Could not serialise [`GridSize`] as JSON string."),
            serde_json::to_string(&(CHARS_PER_CHUNK as u32))
                .expect("Could not serialise [`u32`] as JSON string."),
        ],
    )
}


// ==============================
// === Backend Table Provider ===
// ==============================

/// A cache for a grid of Strings.
/// The cache is backed by the engine and will communicate via the FRP endpoints passed in the
/// constructor.
#[derive(Debug, Clone)]
pub struct BackendTableProvider {
    frp:             Frp,
    text_cache:      Rc<RefCell<GridCache<String>>>,
    register_access: frp::Any,
}

impl BackendTableProvider {
    /// Create a new `BackendTableProvider`.
    pub fn new(
        receive_data: frp::Source<visualization::Data>,
        preprocessor_update: frp::Any<PreprocessorConfiguration>,
    ) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let output = frp.private().output.clone_ref();

        frp::extend! { network
            table_window <- any_mut::<TableWindow>();
        }

        let longest_observed_line: Rc<Cell<u32>> = default();
        let max_observed_lines: Rc<Cell<u32>> = default();

        let table_specification = Rc::new(RefCell::new(default()));
        let grid_cache_update = f!([table_specification,table_window](new_grid_window:GridWindow){
            let new_window = new_grid_window.to_table_window(&table_specification.borrow());
            table_window.emit(new_window);
        });

        let text_cache = Rc::new(RefCell::new(GridCache::<String>::new(
            GridPosition::default(),
            GridSize::new(DEFAULT_GRID_SIZE, DEFAULT_GRID_SIZE),
            CACHE_PADDING,
            Box::new(grid_cache_update),
        )));

        frp::extend! { network
            register_access <- any_mut();
            update_preprocessor <- all(register_access, table_window);
            update_preprocessor <- update_preprocessor._1();
            preprocessor_update <+ update_preprocessor.map(|table_window| {
                let grid_posititon = table_window.position;
                let grid_size = table_window.size;
                let preprocessor = lazy_table_preprocessor(grid_posititon, grid_size);
                preprocessor
            });
            grid_data_update <- receive_data.map(|data| {
                TableDataUpdate::try_from(data.clone()).ok()
            });
            grid_data_update <- grid_data_update.map(|data| data.clone()).unwrap();
            chunks_update <- grid_data_update.map(|grid_data| grid_data.chunks.clone());
            table_spec_update <- grid_data_update.map(|grid_data| grid_data.table_specification_update.clone());

            eval table_spec_update([table_specification](table_spec_update) {
                table_specification.borrow_mut().update(table_spec_update);
            });
            output.table_specification <+ table_spec_update.map(f!([table_specification](_) {
                Some(table_specification.borrow().clone())
            }));

            eval chunks_update([text_cache,table_specification](chunks) {
                for (pos, text) in chunks {
                    let grid_position = table_specification.borrow().backend_table_position_to_grid_content_position(*pos);
                    text_cache.borrow_mut().add_item(grid_position, text.clone());
                }
            });

            grid_size <- table_spec_update.map(f!([table_specification](_) {
                table_specification.borrow().grid_size_in_chunks()
            }));

            chars_in_longest_line <- table_spec_update.map(f_!([table_specification] table_specification.borrow().width_in_chars() as u32)).on_change();
            line_count <- grid_size._1().map(|line_count| *line_count as u32).on_change();

            output.chars_in_longest_line <+ chars_in_longest_line.map(f!([longest_observed_line](longest_line) {
                    let observed_value = longest_observed_line.get();
                    let longest_line = observed_value.max(*longest_line);
                    longest_observed_line.set(longest_line);
                    longest_line
            })).on_change();

            output.line_count <+ line_count.map(f!([max_observed_lines](line_count) {
                let observed_value = max_observed_lines.get();
                let max_lines = observed_value.max(*line_count);
                max_observed_lines.set(max_lines);
                max_lines
            })).on_change();

        }
        Self { frp, text_cache, register_access }
    }
}

impl TextProvider for BackendTableProvider {
    fn get_slice(&self, line: usize, chunk_index: usize) -> Option<String> {
        let y = line as i32;
        let x = chunk_index as i32;
        let result = self.text_cache.borrow_mut().get_item(Vector2::new(x, y));
        self.register_access.emit(());
        result
    }

    fn frp(&self) -> &Frp {
        &self.frp
    }
}



// =============================
// === Lazy Grid Data Update ===
// =============================

type Chunk = (GridPosition, Option<String>);

/// Struct for deserialising the data sent from the engine.
#[derive(Clone, Debug, Deserialize, Serialize, Default)]
struct LazyGridData {
    pub chunks:       Vec<Chunk>,
    pub line_count:   u32,
    pub longest_line: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum UpdateType {
    FullUpdate,
    PartialUpdate,
}

impl Default for UpdateType {
    fn default() -> Self {
        Self::FullUpdate
    }
}

#[derive(Clone, Debug, Default)]
struct LazyGridDataUpdate {
    pub data:    LazyGridData,
    update_type: UpdateType,
}

impl TryFrom<visualization::Data> for LazyGridDataUpdate {
    type Error = visualization::DataError;

    fn try_from(data: visualization::Data) -> Result<Self, Self::Error> {
        if let visualization::Data::Json { content } = data {
            let grid_data = serde_json::from_value(content.deref().clone());
            let (data, update_type) = match grid_data {
                Ok(data) => (data, UpdateType::PartialUpdate),
                Err(_) => {
                    let data_str = if content.is_string() {
                        // We need to access the content `as_str` to preserve newlines. Just using
                        // `content.to_string()` would turn them into the characters `\n` in the
                        // output. The unwrap can never fail, as we just
                        // checked that the content is a string.
                        Ok(content.as_str().map(|s| s.to_owned()).unwrap_or_default())
                    } else {
                        serde_json::to_string_pretty(&*content)
                    };
                    let data_str =
                        data_str.unwrap_or_else(|e| format!("<Cannot render data: {e}.>"));
                    (data_str.into(), UpdateType::FullUpdate)
                }
            };
            Ok(LazyGridDataUpdate { data, update_type })
        } else {
            Err(visualization::DataError::BinaryNotSupported)
        }
    }
}

impl From<String> for LazyGridData {
    fn from(content: String) -> Self {
        let lines = content.lines();
        let numbered_lines = lines.enumerate();
        let chunks = numbered_lines
            .flat_map(|(line_ix, line)| {
                let chunks = line.chars().chunks(CHARS_PER_CHUNK);
                let numbered_chunks = chunks.into_iter().enumerate();
                let chunks_with_position = numbered_chunks.map(move |(chunk_ix, chunk)| {
                    let chunk = chunk.collect::<String>();
                    let pos = GridPosition::new(chunk_ix as i32, line_ix as i32);
                    (pos, Some(chunk))
                });
                chunks_with_position.collect_vec()
            })
            .collect();
        let line_count = content.lines().count() as u32;
        let longest_line = content.lines().map(|l| l.len()).max().unwrap_or_default() as u32;
        let chunks = fill_emtpy_chunks(chunks);
        LazyGridData { chunks, line_count, longest_line }
    }
}

/// Take a vector of chunks and fill the empty spaces of the bounding grid with `None`. The bounding
/// grid is determined by the maximum x and y values of the chunks.
fn fill_emtpy_chunks(chunks: Vec<Chunk>) -> Vec<Chunk> {
    let grid_width = chunks.iter().map(|(pos, _)| pos.x).max().unwrap_or_default() + 1;
    let grid_height = chunks.iter().map(|(pos, _)| pos.y).max().unwrap_or_default() + 1;
    let chunk_map: HashMap<GridPosition, Option<String>> = chunks.into_iter().collect();
    let full_grid_coordinates = (0..grid_width).cartesian_product(0..grid_height);
    full_grid_coordinates
        .map(|(x, y)| {
            let pos = GridPosition::new(x, y);
            let chunk = chunk_map.get(&pos).cloned().flatten();
            (pos, chunk)
        })
        .collect()
}


// =========================
// === Table Data Update ===
// =========================

type TableChunk = (TablePosition, String);

/// Struct for deserialising the data sent from the engine.
#[derive(Clone, Debug, Deserialize, Serialize, Default)]
struct TableData {
    chunks:              Vec<TableChunk>,
    table_specification: TableSpecification,
}


#[derive(Clone, Debug, Deserialize, Serialize, Default)]
struct TableSpecificationUpdate {
    row_heights:   Vec<(RowIndex, RowHeight)>,
    column_widths: Vec<(ColumnIndex, ColumnWidth)>,
    column_names:  Vec<(ColumnIndex, String)>,
    row_names:     Vec<(RowIndex, String)>,
}

/// Struct for deserialising the data sent from the engine.
#[derive(Clone, Debug, Deserialize, Serialize, Default)]
struct TableDataUpdate {
    chunks:                     Vec<TableChunk>,
    table_specification_update: TableSpecificationUpdate,
}

impl TryFrom<serde_json::Value> for TableDataUpdate {
    type Error = visualization::DataError;

    fn try_from(data: serde_json::Value) -> Result<Self, Self::Error> {
        serde_json::from_value(data).map_err(|_| Self::Error::InvalidDataType)
    }
}

impl TryFrom<visualization::Data> for TableDataUpdate {
    type Error = visualization::DataError;

    fn try_from(data: visualization::Data) -> Result<Self, Self::Error> {
        if let visualization::Data::Json { content } = data {
            serde_json::from_value(content.deref().clone())
                .map_err(|_| Self::Error::InvalidDataType)
        } else {
            Err(visualization::DataError::BinaryNotSupported)
        }
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backend_message_deserialization() {
        let sample_message =
            r#"{"chunks":[[[0, 0], "ABCDE"], [[0, 1], "12345"]],"line_count":2,"longest_line":19}"#;
        let json =
            serde_json::from_str(sample_message).expect("Text example contains invalid JSON.");
        let result: Result<LazyGridData, _> = serde_json::from_value(json);
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
    }

    #[test]
    fn test_backend_message_with_null_deserialization() {
        let sample_message =
            r#"{"chunks":[[[0, 0], "ABCDE"], [[0, 1], null]],"line_count":2,"longest_line":19}"#;
        let result: Result<LazyGridData, _> = sample_message.to_string().try_into();
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
    }

    #[test]
    fn test_backend_message_multiline_string_deserialization() {
        let sample_message = r#""Just a simple string
            with two lines.""#;
        let result: Result<LazyGridData, _> = sample_message.to_string().try_into();
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
    }

    #[test]
    fn test_backend_message_simple_string_deserialization() {
        let sample_message = "10";
        let result: Result<LazyGridData, _> = sample_message.to_string().try_into();
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
        let lazy_grid = result.unwrap();
        assert_eq!(lazy_grid.line_count, 1);
    }

    #[test]
    fn text_backend_message_for_table_parsing() {
        let sample_message = r#"{"chunks":[[[[0,0],[0,0]],"A0"],[[[1,0],[0,0]],"B0"],[[[0,1],[0,0]],"A1"],[[[1,1],[0,0]],"B1"]],"table_specification_update":{"row_heights":{"0":1,"1":1},"column_widths":{"0":2,"1":2}}}"#;
        let json =
            serde_json::from_str(sample_message).expect("Text example contains invalid JSON.");
        let result: Result<TableDataUpdate, _> = serde_json::from_value(json);
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
    }

    #[test]
    fn text_backend_message_for_table_parsing_2() {
        let sample_message = "{\"chunks\":[[[[0,0],[0,0]],\"123456789\"],\"table_specification_update\":{\"column_names\":[[0,\"foo\"],[1,\"bar\"],[2,\"Baz\"],[3,\"foo_1\"],[4,\"foo_2\"],[5,\"ab.+123\"],[6,\"abcd123\"]],\"column_widths\":[[0,9],[1,1],[2,1],[3,2],[4,2],[5,2],[6,23]],\"constructor\":\"Value\",\"row_heights\":[[0,1],[1,1],[2,1]],\"row_names\":[],\"type\":\"TableSpecificationUpdate\"}}";
        let json =
            serde_json::from_str(sample_message).expect("Text example contains invalid JSON.");
        let result: Result<TableDataUpdate, _> = serde_json::from_value(json);
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
        let table_data = result.unwrap();
        assert_eq!(table_data.chunks.len(), 4);
    }

    #[test]
    fn text_backend_message_for_table_processing() {
        let sample_message = r#"{"chunks":[[[[0,0],[0,0]],"A0"],[[[1,0],[0,0]],"B0"],[[[0,1],[0,0]],"A1"],[[[1,1],[0,0]],"B1"]],"table_specification_update":{"row_heights":{"0":1,"1":1},"column_widths":{"0":2,"1":2}}}"#;
        let json =
            serde_json::from_str(sample_message).expect("Text example contains invalid JSON.");
        let result: Result<TableDataUpdate, _> = serde_json::from_value(json);
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
    }

    #[test]
    fn test_table_specification_updates() {
        let mut specification = TableSpecification::default();
        let update1 = TableSpecificationUpdate {
            row_heights: vec![(0, 1), (1, 1)],
            column_widths: vec![(0, 5), (1, 15), (2, 10)],
            column_names: vec![(0, "a".to_string()), (1, "b".to_string()), (2, "c".to_string())],
            ..default()
        };

        specification.update(&update1);

        assert_eq!(specification.rows.len(), 2);
        assert_eq!(specification.rows[0], Some(1));
        assert_eq!(specification.rows[1], Some(1));
        assert_eq!(specification.columns.len(), 3);
        assert_eq!(specification.columns[0], Some(5));
        assert_eq!(specification.columns[1], Some(15));
        assert_eq!(specification.columns[2], Some(10));
        assert_eq!(specification.column_names.len(), 3);
        assert_eq!(specification.column_names[0], Some("a".to_string()));
        assert_eq!(specification.column_names[1], Some("b".to_string()));
        assert_eq!(specification.column_names[2], Some("c".to_string()));

        let update2 = TableSpecificationUpdate { column_widths: vec![(5, 1)], ..default() };
        specification.update(&update2);
        assert_eq!(specification.columns.len(), 6);
        assert_eq!(specification.columns[5], Some(1));
        assert_eq!(specification.columns[4], None);
    }

    #[test]
    fn convert_grid_position_to_table_position() {
        let specification = TableSpecification {
            rows: vec![Some(2), Some(1)],
            columns: vec![Some(2), Some(2)],
            ..default()
        };
        let table_position =
            specification.grid_content_position_to_table_position(GridPosition::new(0, 0));
        assert_eq!(table_position.cell, GridPosition::new(0, 0));
        assert_eq!(table_position.chunk, GridPosition::new(0, 0));

        let table_position =
            specification.grid_content_position_to_table_position(GridPosition::new(1, 0));
        assert_eq!(table_position.cell, GridPosition::new(0, 0));
        assert_eq!(table_position.chunk, GridPosition::new(1, 0));

        let table_position =
            specification.grid_content_position_to_table_position(GridPosition::new(0, 1));
        assert_eq!(table_position.cell, GridPosition::new(0, 0));
        assert_eq!(table_position.chunk, GridPosition::new(0, 1));

        let table_position =
            specification.grid_content_position_to_table_position(GridPosition::new(0, 2));
        assert_eq!(table_position.cell, GridPosition::new(0, 1));
        assert_eq!(table_position.chunk, GridPosition::new(0, 0));
    }

    #[test]
    fn test_table_position_to_grid_position() {
        let specification = TableSpecification {
            rows: vec![Some(2), Some(1)],
            columns: vec![Some(2), Some(2)],
            ..default()
        };

        let grid_position =
            specification.backend_table_position_to_grid_content_position(TablePosition {
                cell:  GridPosition::new(0, 0),
                chunk: GridPosition::new(0, 0),
            });
        assert_eq!(grid_position, GridPosition::new(0, 0));

        let grid_position =
            specification.backend_table_position_to_grid_content_position(TablePosition {
                cell:  GridPosition::new(0, 0),
                chunk: GridPosition::new(1, 0),
            });
        assert_eq!(grid_position, GridPosition::new(1, 0));

        let grid_position =
            specification.backend_table_position_to_grid_content_position(TablePosition {
                cell:  GridPosition::new(0, 0),
                chunk: GridPosition::new(0, 1),
            });
        assert_eq!(grid_position, GridPosition::new(0, 1));

        let grid_position =
            specification.backend_table_position_to_grid_content_position(TablePosition {
                cell:  GridPosition::new(0, 1),
                chunk: GridPosition::new(0, 0),
            });
        assert_eq!(grid_position, GridPosition::new(0, 2));
    }

    #[test]
    fn test_table_specification_update() {
        let mut specification = TableSpecification::default();

        let simple_row_height_update =
            TableSpecificationUpdate { row_heights: vec![(0, 2), (1, 1)], ..default() };
        specification.update(&simple_row_height_update);
        assert_eq!(specification.rows.len(), 2);
        assert_eq!(specification.rows[0], Some(2));
        assert_eq!(specification.rows[1], Some(1));
        assert!(specification.columns.is_empty());

        let simple_column_height_update =
            TableSpecificationUpdate { column_widths: vec![(0, 2), (1, 1)], ..default() };
        specification.update(&simple_column_height_update);
        assert_eq!(specification.columns.len(), 2);
        assert_eq!(specification.columns[0], Some(2));
        assert_eq!(specification.columns[1], Some(1));
        assert_eq!(specification.rows.len(), 2);

        let column_name_update = TableSpecificationUpdate {
            column_names: vec![(0, "a".to_string()), (1, "b".to_string())],
            ..default()
        };
        specification.update(&column_name_update);
        assert_eq!(specification.column_names.len(), 2);
        assert_eq!(specification.column_names[0], Some("a".to_string()));
        assert_eq!(specification.column_names[1], Some("b".to_string()));

        let column_update_with_gap =
            TableSpecificationUpdate { column_widths: vec![(99, 1), (200, 3)], ..default() };
        specification.update(&column_update_with_gap);
        assert_eq!(specification.columns.len(), 201);
        assert_eq!(specification.columns[98], None);
        assert_eq!(specification.columns[99], Some(1));
        assert_eq!(specification.columns[100], None);
        assert_eq!(specification.columns[200], Some(3));

        let row_update_with_gap =
            TableSpecificationUpdate { row_heights: vec![(99, 4), (300, 5)], ..default() };
        specification.update(&row_update_with_gap);
        assert_eq!(specification.rows.len(), 301);
        assert_eq!(specification.rows[98], None);
        assert_eq!(specification.rows[99], Some(4));
        assert_eq!(specification.rows[100], None);
        assert_eq!(specification.rows[200], None);
        assert_eq!(specification.rows[300], Some(5));
    }

    #[test]
    pub fn test_divider_locations() {
        let specification = TableSpecification {
            rows: vec![Some(2), Some(1)],
            columns: vec![Some(2 * CHARS_PER_CHUNK), Some(3 * CHARS_PER_CHUNK)],
            ..default()
        };

        assert!(!specification.is_row_divider(0));
        assert!(specification.is_row_divider(1));
        assert!(!specification.is_row_divider(2));
        assert!(!specification.is_row_divider(3));
        assert!(specification.is_row_divider(4));
        assert!(!specification.is_row_divider(5));
        assert!(specification.is_row_divider(6));
    }

    #[test]
    fn test_table_items_access() {
        //   | A__ | B |
        //  -|-----+---+
        //  1| ___ | _ |
        //   | ___ | _ |
        //  -|-----+---+
        //  2| ___ | _ |
        //  -|-----+---+
        let spec = TableSpecification {
            rows:         vec![Some(2), Some(1)],
            columns:      vec![Some(CHARS_PER_CHUNK * 5 / 2), Some(CHARS_PER_CHUNK)],
            column_names: vec![Some("A".to_string()), Some("B".to_string())],
            row_names:    vec![Some("1".to_string()), Some("2".to_string())],
        };

        let assert_item = |column: usize, row: usize, item: TableContentItem| {
            assert_eq!(
                spec.grid_view_position_to_table_item(
                    GridPosition::new(column as i32, row as i32,)
                ),
                Some(item),
                "Column: {column}, Row: {row} did not match."
            );
        };

        use TableContentItem::*;
        // Top left corner of the table
        assert_item(0, 0, Empty);
        // Column Headings
        assert_item(1, 0, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(2, 0, ColumnHeading { column_index: 0, chunk_index: 0, line_index: 0 });
        assert_item(3, 0, ColumnHeading { column_index: 0, chunk_index: 1, line_index: 0 });
        assert_item(4, 0, ColumnHeading { column_index: 0, chunk_index: 2, line_index: 0 });
        assert_item(5, 0, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(6, 0, ColumnHeading { column_index: 1, chunk_index: 0, line_index: 0 });
        assert_item(7, 0, Divider { bottom: true, top: true, left: false, right: false });

        // First Row (divider under the column headings)
        assert_item(0, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(1, 1, Divider { bottom: true, top: true, left: true, right: true });
        assert_item(2, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(3, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(4, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(5, 1, Divider { bottom: true, top: true, left: true, right: true });
        assert_item(6, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(7, 1, Divider { bottom: true, top: true, left: true, right: false });

        // Second Row (First line of first content row)
        assert_item(0, 2, RowHeading { row_index: 0, chunk_index: 0, line_index: 0 });
        assert_item(1, 2, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(2, 2, Content { content_index: GridPosition::new(0, 0) });
        assert_item(3, 2, Content { content_index: GridPosition::new(1, 0) });
        assert_item(4, 2, Content { content_index: GridPosition::new(2, 0) });
        assert_item(5, 2, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(6, 2, Content { content_index: GridPosition::new(3, 0) });
        assert_item(7, 2, Divider { bottom: true, top: true, left: false, right: false });

        // Third Row (Second line of first content row)
        assert_item(0, 3, RowHeading { row_index: 0, chunk_index: 0, line_index: 1 });
        assert_item(1, 3, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(2, 3, Content { content_index: GridPosition::new(0, 1) });
        assert_item(3, 3, Content { content_index: GridPosition::new(1, 1) });
        assert_item(4, 3, Content { content_index: GridPosition::new(2, 1) });
        assert_item(5, 3, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(6, 3, Content { content_index: GridPosition::new(3, 1) });
        assert_item(7, 3, Divider { bottom: true, top: true, left: false, right: false });

        // Fourth Row (divider)
        assert_item(0, 4, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(1, 4, Divider { bottom: true, top: true, left: true, right: true });
        assert_item(2, 4, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(3, 4, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(4, 4, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(5, 4, Divider { bottom: true, top: true, left: true, right: true });
        assert_item(6, 4, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(7, 4, Divider { bottom: true, top: true, left: true, right: false });

        // Fifth Row (First line of second content row)
        assert_item(0, 5, RowHeading { row_index: 1, chunk_index: 0, line_index: 0 });
        assert_item(1, 5, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(2, 5, Content { content_index: GridPosition::new(0, 2) });
        assert_item(3, 5, Content { content_index: GridPosition::new(1, 2) });
        assert_item(4, 5, Content { content_index: GridPosition::new(2, 2) });
        assert_item(5, 5, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(6, 5, Content { content_index: GridPosition::new(3, 2) });
        assert_item(7, 5, Divider { bottom: true, top: true, left: false, right: false });
    }

    #[test]
    fn test_overlong_column_name() {
        //   | A_ | B |
        //  -|----+---+
        //  1| _  | _ |
        //  -|----+---+
        let spec = TableSpecification {
            rows:         vec![Some(1)],
            columns:      vec![Some(CHARS_PER_CHUNK), Some(CHARS_PER_CHUNK)],
            column_names: vec![Some("A".repeat(CHARS_PER_CHUNK * 2)), Some("B".to_string())],
            row_names:    vec![Some("1".to_string())],
        };

        let assert_item = |column: usize, row: usize, item: TableContentItem| {
            assert_eq!(
                spec.grid_view_position_to_table_item(
                    GridPosition::new(column as i32, row as i32,)
                ),
                Some(item),
                "Column: {column}, Row: {row} did not match.",
            );
        };

        use TableContentItem::*;
        // First Row (divider under the column headings)
        assert_item(0, 0, Empty);
        assert_item(1, 0, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(2, 0, ColumnHeading { column_index: 0, chunk_index: 0, line_index: 0 });
        assert_item(3, 0, ColumnHeading { column_index: 0, chunk_index: 1, line_index: 0 });
        assert_item(4, 0, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(5, 0, ColumnHeading { column_index: 1, chunk_index: 0, line_index: 0 });
        assert_item(6, 0, Divider { bottom: true, top: true, left: false, right: false });

        // Second Row (divider under the row headings)
        assert_item(0, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(1, 1, Divider { bottom: true, top: true, left: true, right: true });
        assert_item(2, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(3, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(4, 1, Divider { bottom: true, top: true, left: true, right: true });
        assert_item(5, 1, Divider { bottom: false, top: false, left: true, right: true });
        assert_item(6, 1, Divider { bottom: true, top: true, left: true, right: false });

        // Third Row (first line of first content row)
        assert_item(0, 2, RowHeading { row_index: 0, chunk_index: 0, line_index: 0 });
        assert_item(1, 2, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(2, 2, Content { content_index: GridPosition::new(0, 0) });
        assert_item(3, 2, Empty);
        assert_item(4, 2, Divider { bottom: true, top: true, left: false, right: false });
        assert_item(5, 2, Content { content_index: GridPosition::new(1, 0) });
        assert_item(6, 2, Divider { bottom: true, top: true, left: false, right: false });
    }
}
