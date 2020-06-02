//! This module defines the `Data` struct and related functionality.

use crate::prelude::*;

use crate::component::visualization::EnsoType;

use serde::Deserialize;



// ======================================
// === Wrapper for Visualization Data ===
// =======================================

/// Type indicator
pub type DataType = EnsoType;

/// Wrapper for data that can be consumed by a visualization.
/// TODO[mm] consider static versus dynamic typing for visualizations and data!
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub enum Data {
    JSON   { content : Rc<serde_json::Value> },
    // TODO replace with actual binary data stream.
    Binary { content : Rc<dyn Any>           },
}

impl Data {
    /// Wraps the given JSON value into a visualization Data.
    pub fn new_json(content:serde_json::Value) -> Data {
        let content = Rc::new(content);
        Data::JSON {content}
    }

    /// Returns the data as as JSON. If the data cannot be returned as JSON, it will return a
    /// `DataError` instead.
    pub fn as_json(&self) -> Result<Rc<serde_json::Value>, DataError> {
        match &self {
            Data::JSON { content } => Ok(Rc::clone(content)),
            _ => { Err(DataError::InvalidDataType{})  },
        }
    }

    /// Returns the wrapped data in Rust format. If the data cannot be returned as rust datatype, a
    /// `DataError` is returned instead.
    pub fn as_binary<T>(&self) -> Result<Rc<T>, DataError>
        where for<'de> T:Deserialize<'de> + 'static {
        match &self {
            Data::JSON { content } => {
                // We try to deserialize here. Just in case it works.
                // This is useful for simple data types where we don't want to care to much about
                // representation, e.g., a list of numbers.
                let value : serde_json::Value = content.as_ref().clone();
                if let Ok(result) = serde_json::from_value(value) {
                    Ok(Rc::new(result))
                } else {
                    Err(DataError::InvalidDataType)
                }
            },
            Data::Binary { content } => { Rc::clone(content).downcast()
                .or(Err(DataError::InvalidDataType))},
        }
    }
}



// ==============
// === Errors ===
// ==============

/// Indicates a problem with the provided data. That is, the data has the wrong format, or maybe
/// violates some other assumption of the visualization.
// TODO[mm] add more information to errors once typing is defined.
#[derive(Copy,Clone,Debug)]
pub enum DataError {
    /// Indicates that that the provided data type does not match the expected data type.
    InvalidDataType,
    /// The data caused an error in the computation of the visualization.
    InternalComputationError,
}



// =============================
// === Sample Data Generator ===
// =============================
// TODO this will go away once we have real data

#[derive(Clone,CloneRef,Debug,Default)]
pub(crate) struct MockDataGenerator3D {
    counter: Rc<Cell<f32>>
}

impl MockDataGenerator3D {

    pub fn generate_data(&self) -> Vec<Vector3<f32>> {

        let current_value = self.counter.get();
        self.counter.set(current_value + 0.1);

        let delta1 = current_value.sin() * 10.0;
        let delta2 = current_value.cos() * 10.0;

        vec![
            Vector3::new(25.0,                 75.0,          25.0 + delta1),
            Vector3::new(25.0,                 25.0,          25.0 + delta2),
            Vector3::new(75.0 - 12.5,          75.0 + delta1, 5.0          ),
            Vector3::new(75.0 + 12.5,          75.0 + delta2, 15.0         ),
            Vector3::new(75.0 - 12.5 + delta1, 25.0 + delta2, 5.0          ),
            Vector3::new(75.0 + 12.5 + delta2, 25.0 + delta1, 15.0         ),
        ]
    }
}
