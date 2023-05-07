use std::collections::VecDeque;

#[derive(Clone, Debug)]
pub struct CircularVecDeque<T> {
    offset:   usize,
    capacity: usize,
    vec:      VecDeque<T>,
}

impl<T> CircularVecDeque<T> {
    pub fn new(capacity: usize) -> Self {
        let offset = 0;
        let vec = VecDeque::with_capacity(capacity);
        Self { offset, capacity, vec }
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_full(&self) -> bool {
        self.len() == self.capacity
    }

    pub fn push_front(&mut self, value: T) {
        if self.is_full() {
            self.vec.pop_back();
        }
        self.vec.push_front(value);
    }

    pub fn push_back(&mut self, value: T) {
        if self.is_full() {
            self.vec.pop_front();
        }
        self.vec.push_back(value);
    }

    pub fn pop_front(&mut self) -> Option<T> {
        self.vec.pop_front()
    }

    pub fn pop_back(&mut self) -> Option<T> {
        self.vec.pop_back()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.vec.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.vec.get_mut(index)
    }

    pub fn last(&self) -> Option<&T> {
        self.vec.back()
    }

    pub fn with_last_n_elems(&mut self, n: usize, mut f: impl FnMut(&mut T)) {
        let len = self.len();
        let start = len.saturating_sub(n);
        for i in start..len {
            f(self.vec.get_mut(i).unwrap());
        }
    }

    pub fn with_last_nth_elem(&mut self, n: usize, f: impl FnOnce(&mut T)) {
        let len = self.len();
        if len > n {
            f(self.vec.get_mut(len - n - 1).unwrap());
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn synchronized() {
        let mut vec = CircularVecDeque::<usize>::new(3);
        assert!(1 == 2);
    }
}
