//! Definition of data understandable by visualizations.

use crate::prelude::*;



// ============
// === Json ===
// ============

/// Json representation with a fast clone operation. Used for transmitting visualization data via
/// FRP networks.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Json {
    rc: Rc<serde_json::Value>,
}

impl Deref for Json {
    type Target = serde_json::Value;
    fn deref(&self) -> &Self::Target {
        &self.rc
    }
}

impl From<serde_json::Value> for Json {
    fn from(t: serde_json::Value) -> Self {
        let rc = Rc::new(t);
        Self { rc }
    }
}

// ===================
// === Data Format ===
// ====================

/// Data formats that can be used in a visualisation.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[allow(missing_docs)]
pub enum Format {
    Json,
    Binary,
}

/// Error that can occur when parsing a `Format` from a string.
#[derive(Clone, Debug, Display)]
pub enum ParseError {
    /// The given string does not represent a valid Format.
    NotAValidFormat(String),
}

impl FromStr for Format {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "json" => Ok(Format::Json),
            "binary" => Ok(Format::Binary),
            _ => Err(ParseError::NotAValidFormat(s.to_string())),
        }
    }
}

impl Default for Format {
    fn default() -> Self {
        Format::Json
    }
}



// ============
// === Data ===
// ============

/// Wrapper for data that can be consumed by a visualization.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum Data {
    Json { content: Json },
    Binary, // TODO replace with actual binary data stream.
}

impl Default for Data {
    fn default() -> Self {
        let content = default();
        Self::Json { content }
    }
}

impl From<serde_json::Value> for Data {
    fn from(t: serde_json::Value) -> Self {
        let content = t.into();
        Self::Json { content }
    }
}



// ==============
// === Errors ===
// ==============

/// Indicates a problem with the provided data. That is, the data has the wrong format, or maybe
/// violates some other assumption of the visualization.
#[derive(Copy, Clone, Debug)]
pub enum DataError {
    /// Visualization received a binary data package, which is currently not supported.
    BinaryNotSupported,
    /// Indicates that that the provided data type does not match the expected data format.
    InvalidDataType,
    /// Received text data is not valid JSON while it is required.
    InvalidJsonText,
    /// The data caused an error in the computation of the visualization.
    InternalComputationError,
}



// =============================
// === Sample Data Generator ===
// =============================

/// The `MockDataGenerator3D` creates sample data in the format of `Vec<Vector3<f32>>`. The data
/// is changing incrementally on every call. The data is meant to be interpreted as a number of
/// circles defined through x-coordinate, y-coordinate and radius which respectively correspond to
/// the `Vectors3`s x/y/z values.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct MockDataGenerator3D {
    counter: Rc<Cell<f32>>,
}

impl MockDataGenerator3D {
    /// Generate new data set.
    pub fn generate_data(&self) -> Vec<Vector3<f32>> {
        let current_value = self.counter.get();
        self.counter.set(current_value + 0.1);

        let delta1 = current_value.sin() * 10.0;
        let delta2 = current_value.cos() * 10.0;

        vec![
            Vector3::new(25.0, 75.0, 25.0 + delta1),
            Vector3::new(25.0, 25.0, 25.0 + delta2),
            Vector3::new(75.0 - 12.5, 75.0 + delta1, 5.0),
            Vector3::new(75.0 + 12.5, 75.0 + delta2, 15.0),
            Vector3::new(75.0 - 12.5 + delta1, 25.0 + delta2, 5.0),
            Vector3::new(75.0 + 12.5 + delta2, 25.0 + delta1, 15.0),
        ]
    }
}


/// The `MockDocGenerator` creates sample documentation string in the format of `String`.
#[derive(Clone, CloneRef, Copy, Debug, Default)]
pub struct MockDocGenerator;

impl MockDocGenerator {
    /// Generate new data set.
    pub fn generate_data(self) -> String {
        let input = r#"Optional values.

Type `Option` represents an optional value: every `Option` is either `Some`
and contains a value, or `None`, and does not.

? Information
 `Option`s are commonly paired with pattern matching to query the presence of
 a value and take action, always accounting for the None case.
"#;
        input.to_string()
    }
}
