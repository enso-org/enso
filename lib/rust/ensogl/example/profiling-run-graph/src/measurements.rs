use crate::profiler;
use enso_profiler::Measurement;
use enso_profiler::Timestamp;
use std::collections::HashMap;

pub struct Measurements {
    items:            Vec<profiler::Measurement>,
    profiler_mapping: HashMap<profiler::ProfilerId, usize>,
}

impl From<Vec<Measurement>> for Measurements {
    fn from(items: Vec<Measurement>) -> Self {
        Measurements::new(items)
    }
}

impl Measurements {
    pub fn new(items: Vec<Measurement>) -> Self {
        let mut profiler_mapping = HashMap::with_capacity(items.len());
        items.iter().enumerate().for_each(|(ix, item)| {
            profiler_mapping.insert(item.profiler, ix);
        });

        Self { items, profiler_mapping }
    }

    pub fn start_time(&self, profiler_id: profiler::ProfilerId) -> Timestamp {
        let profiler_ix = *self.profiler_mapping.get(&profiler_id).unwrap();
        let profiler = &self.items[profiler_ix];
        if let Some(start_time) = profiler.start {
            start_time
        } else {
            // TODO infinite recursion?
            // assert_ne!(profiler_id, profiler.parent);
            self.start_time(profiler.parent)
        }
    }

    pub fn items(&self) -> &[profiler::Measurement] {
        &self.items
    }

    pub fn depth(&self, profiler_id: profiler::ProfilerId) -> u32 {
        let profiler_ix = *self.profiler_mapping.get(&profiler_id).unwrap();
        let profiler = &self.items[profiler_ix];
        if profiler.parent == profiler::ProfilerId::root() {
            0
        } else {
            1 + self.depth(profiler.parent)
        }
    }
}
