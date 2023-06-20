//! Definition of data understandable by visualizations.

use crate::prelude::*;



// ============
// === Json ===
// ============

/// Json representation with a fast clone operation. Used for transmitting visualization data via
/// FRP networks.
#[derive(Clone, CloneRef, Debug)]
pub struct Json {
    rc: Rc<serde_json::value::RawValue>,
}

impl Default for Json {
    fn default() -> Self {
        let null = serde_json::Value::default();
        let raw_null = serde_json::value::to_raw_value(&null).unwrap();
        let rc = raw_null.into();
        Self { rc }
    }
}

impl Json {
    /// Deserialize the JSON data into an arbitrary type.
    #[profile(Debug)]
    pub fn deserialize<T: serde::de::DeserializeOwned>(&self) -> Result<T, DataError> {
        serde_json::from_str(self.rc.get()).map_err(|_| DataError::InvalidJsonText)
    }

    /// Produce a string from the value. If it's a JSON string, return it; otherwise, pretty-print
    /// the JSON data.
    #[profile(Debug)]
    pub fn to_string(&self) -> serde_json::Result<String> {
        let data_str: Result<String, _> = serde_json::from_str(self.rc.get());
        data_str.or_else(|_| serde_json::to_string_pretty(&self.rc))
    }

    /// Return the raw data.
    pub fn raw(&self) -> &str {
        self.rc.get()
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

impl Data {
    /// Deserialize JSON visualization data from the given bytes.
    #[profile(Debug)]
    pub fn json(data: &[u8]) -> FallibleResult<Self> {
        let rc: Rc<serde_json::value::RawValue> = serde_json::from_slice(data)?;
        let content = Json { rc };
        Ok(Self::Json { content })
    }

    /// Get a reference to the contained JSON data. Fails if this is non-JSON (binary) data.
    pub fn as_json(&self) -> Result<&Json, DataError> {
        match self {
            Data::Json { content } => Ok(content),
            Data::Binary => Err(DataError::BinaryNotSupported),
        }
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
