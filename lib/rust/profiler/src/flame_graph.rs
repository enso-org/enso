//! The `flame_graph` module contains functionality that allows the profiling framework to
//! generate the data required to render a flame graph. This means creating data for each block
//! that is supposed to be rendered, with start time, end time and labels.
use crate::Measurement;
use crate::ProfilerId;
use crate::Timestamp;
use std::collections::HashMap;



// ====================
// === Measurements ===
// ====================

/// A collections of related measurements. Enables, for example, easy access to `start_time` data,
/// which is not stored in each measurement but has to be looked up from the parent profiler at
/// times.
struct Measurements {
    items:            Vec<Measurement>,
    profiler_mapping: HashMap<ProfilerId, usize>,
}

impl From<Vec<Measurement>> for Measurements {
    fn from(items: Vec<Measurement>) -> Self {
        Measurements::new(items)
    }
}

impl Measurements {
    fn new(items: Vec<Measurement>) -> Self {
        let mut profiler_mapping = HashMap::with_capacity(items.len());
        items.iter().enumerate().for_each(|(ix, item)| {
            profiler_mapping.insert(item.profiler, ix);
        });

        Self { items, profiler_mapping }
    }

    fn start_time(&self, profiler_id: ProfilerId) -> Timestamp {
        let profiler_ix = *self.profiler_mapping.get(&profiler_id).unwrap();
        let profiler = &self.items[profiler_ix];
        if let Some(start_time) = profiler.start {
            start_time
        } else {
            self.start_time(profiler.parent)
        }
    }

    fn items(&self) -> &[Measurement] {
        &self.items
    }

    /// Returns how many parents the given profiler has.
    fn depth(&self, profiler_id: ProfilerId) -> u32 {
        let profiler_ix = *self.profiler_mapping.get(&profiler_id).unwrap();
        let profiler = &self.items[profiler_ix];
        if profiler.parent == ProfilerId::root() {
            0
        } else {
            1 + self.depth(profiler.parent)
        }
    }
}



// ==================
// === Block Data ===
// ==================

/// A `Block` contains the data required to render a single block of a frame graph.
#[derive(Clone, Debug)]
pub struct Block {
    /// Start x coordinate of the block.
    pub start: f64,
    /// End x coordinate of the block.
    pub end:   f64,
    /// Row that the block should be placed in.
    pub row:   u32,
    /// The label to be displayed with the block.
    pub label: String,
}

impl Block {
    /// Width of the block.
    pub fn width(&self) -> f64 {
        self.end - self.start
    }
}



// ========================
// === Flame Graph Data ===
// ========================

/// A `FlameGraph`, contains the information required to render a flame graph, i.e., the data for
/// all blocks that make up the flame graph.
#[derive(Debug)]
pub struct FlameGraph {
    /// Collection of all blocks making up the flame graph.
    pub blocks: Vec<Block>,
}

impl From<Measurements> for FlameGraph {
    fn from(measurements: Measurements) -> Self {
        let mut blocks = Vec::with_capacity(measurements.items().len());

        for measurement in measurements.items() {
            let start = measurements.start_time(measurement.profiler).into_ms();
            let end = measurement.end.into_ms();
            let label = measurement.label.to_string();
            let row = measurements.depth(measurement.profiler);
            blocks.push(Block { start, end, label, row });
        }
        Self { blocks }
    }
}

impl FlameGraph {
    /// Gather and remove all logged measurements and return them as a `FlameGraph`.
    pub fn take_from_log() -> Self {
        let measurements: Measurements = crate::take_log().into();
        measurements.into()
    }
}
