#[derive(Clone)]
pub struct CircularVec<T> {
    offset:   usize,
    capacity: usize,
    vec:      Vec<T>,
}

impl<T> CircularVec<T> {
    pub fn new(capacity: usize) -> Self {
        let offset = 0;
        let vec = Vec::with_capacity(capacity);
        Self { offset, capacity, vec }
    }
}
