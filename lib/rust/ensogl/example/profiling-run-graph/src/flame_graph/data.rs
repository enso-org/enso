use crate::measurements::Measurements;

#[derive(Clone, Debug)]
pub struct Block {
    pub start: f64,
    pub end:   f64,
    pub row:   u32,
    pub label: String,
}

impl Block {
    pub fn width(&self) -> f64 {
        self.end - self.start
    }
}


pub struct FlameGraph {
    pub blocks: Vec<Block>,
}

impl From<Measurements> for FlameGraph {
    fn from(measurements: Measurements) -> Self {
        let mut blocks = Vec::with_capacity(measurements.items().len());

        for measurement in measurements.items() {
            let start = measurements.start_time(measurement.profiler).into_ms();
            let end = measurement.end.into_ms();
            let label = measurement.label.to_string();
            let row = measurements.depth(measurement.profiler); // TODO needs to be calculated in a different way, if parental information is not
                                                                // present
            blocks.push(Block { start, end, label, row });
        }
        Self { blocks }
    }
}
