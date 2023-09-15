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

ensogl::define_endpoints_2! {
    Input {}
    Output {
        line_count(u32),
        longest_line(u32),
        data_refresh(),
    }
}



// =============================
// === String Text Provider ====
// =============================

/// A text provider that is backed by a string.
#[derive(Debug)]
pub struct StringTextProvider {
    text: String,
    frp:  Frp,
}

impl StringTextProvider {
    /// Create a new [`StringTextProvider`].
    pub fn new(text: String) -> Self {
        let frp = Frp::new();

        let line_count = text.lines().count() as u32;
        frp.private().output.line_count.emit(line_count);
        let longest_line_frp = &frp.private().output.longest_line;
        let longest_line = text.lines().map(|line| line.chars().count()).max().unwrap_or(0) as u32;
        longest_line_frp.emit(longest_line);

        Self { text, frp }
    }
}

impl TextProvider for StringTextProvider {
    fn get_slice(&self, line: usize, chunk_index: usize) -> Option<String> {
        self.text.lines().nth(line).and_then(|line| {
            line.chars()
                .chunks(CHARS_PER_CHUNK)
                .into_iter()
                .nth(chunk_index)
                .map(|chunk| chunk.collect::<String>())
        })
    }

    fn frp(&self) -> &Frp {
        &self.frp
    }
}



// =============================
// === Backend Text Provider ===
// =============================

const DEFAULT_GRID_SIZE: i32 = 20;

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
            GridSize::new(DEFAULT_GRID_SIZE, CHARS_PER_CHUNK as i32 * DEFAULT_GRID_SIZE),
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

            output.longest_line <+ longest_line.map(f!([longest_observed_line](longest_line) {
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

    #[profile(Debug)]
    fn try_from(data: visualization::Data) -> Result<Self, Self::Error> {
        let content = data.as_json()?;
        let grid_data: Result<LazyGridData, _> = content.deserialize();
        let (data, update_type) = match grid_data {
            Ok(data) => (data, UpdateType::PartialUpdate),
            Err(_) => {
                let data_str = content.to_string();
                let data_str = data_str.unwrap_or_else(|e| format!("<Cannot render data: {e}.>"));
                (data_str.into(), UpdateType::FullUpdate)
            }
        };
        Ok(LazyGridDataUpdate { data, update_type })
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
        let lazy_grid = result.unwrap();
        assert_eq!(lazy_grid.line_count, 2);
    }

    #[test]
    fn test_backend_message_simple_string_deserialization() {
        let sample_message = "10";
        let result: Result<LazyGridData, _> = sample_message.to_string().try_into();
        assert!(result.is_ok(), "Deserialization failed with error: {:?}.", result.err());
        let lazy_grid = result.unwrap();
        assert_eq!(lazy_grid.line_count, 1);
    }
}
