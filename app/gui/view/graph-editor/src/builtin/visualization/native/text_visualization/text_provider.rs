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
        let longest_line_frp = &frp.private().output.longest_line;
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
            update_preprocessor <- update_preprocessor._1();
            preprocessor_update <+ update_preprocessor.map(|grid_window| {
                let grid_posititon = grid_window.position.map(|value| value.max(0));
                let grid_size = grid_window.size;
                if grid_size == GridSize::default() {
                    None
                } else {
                    Some(lazy_text_preprocessor(grid_posititon, grid_size))
                }
            }).unwrap();
            grid_data_update <- receive_data.map(|data| {
                warn!("{:?}", data);

                LazyGridData::try_from(data.clone()).ok()
            });
            grid_data_update <- grid_data_update.map(|data| data.clone()).unwrap();
            chunks_update <- grid_data_update.map(|grid_data| grid_data.chunks.clone());
            eval chunks_update([text_cache](chunks) {
                warn!("{:?}", chunks.len());

                for (pos, text) in chunks {
                    if let Some(text) = text {
                        text_cache.borrow_mut().add_item(*pos, text.clone());
                    }
                }
            });
            output.line_count <+ grid_data_update.map(|grid_data| grid_data.line_count);
            output.longest_line <+ grid_data_update.map(|grid_data| grid_data.longest_line);
        }

        Self { frp, text_cache, register_access }
    }
}

impl TextProvider for BackendTextProvider {
    fn get_slice(&self, line: usize, chunk_index: usize) -> Option<String> {
        let y = line as i32;
        let x = chunk_index as i32;
        let result = self.text_cache.borrow_mut().get_item(Vector2::new(x, y));
        // self.register_access.emit(());
        result
    }

    fn frp(&self) -> &Frp {
        &self.frp
    }
}

/// Crate a preprocessor configuration for the lazy text preprocessor.
fn lazy_text_preprocessor(
    grid_position: GridPosition,
    grids_size: GridSize,
) -> PreprocessorConfiguration {
    let p = PreprocessorConfiguration::new(
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
    );
    warn!("{:?}", p);
    p
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

impl TryFrom<visualization::Data> for LazyGridData {
    type Error = visualization::DataError;

    fn try_from(data: visualization::Data) -> Result<Self, Self::Error> {
        if let visualization::Data::Json { content } = data {
            Ok(serde_json::from_value(content.deref().clone()).unwrap_or_else(|_| {
                let data_str = if content.is_string() {
                    // We need to access the content `as_str` to preserve newlines. Just using
                    // `content.to_string()` would turn them into the characters `\n` in the output.
                    // The unwrap can never fail, as we just checked that the content is a string.
                    Ok(content.as_str().map(|s| s.to_owned()).unwrap_or_default())
                } else {
                    serde_json::to_string_pretty(&*content)
                };
                let data_str = data_str.unwrap_or_else(|e| format!("<Cannot render data: {}.>", e));
                data_str.into()
            }))
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
        LazyGridData { chunks, line_count, longest_line }
    }
}


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
