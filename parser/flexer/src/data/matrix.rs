//! Efficient representation of 2D matrix.

use std::ops::Index;
use std::ops::IndexMut;

// ============
// == Matrix ==
// ============

/// Efficient 2D matrix implemented on top of vector.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Matrix<T> {
    /// The number of rows in matrix.
    rows:usize,
    /// The number of columns in matrix.
    columns:usize,
    /// Matrix implemented with vector.
    matrix:Vec<T>,
}

impl<T> Index<(usize, usize)> for Matrix<T> {
    type Output = T;
    fn index(&self, index:(usize, usize)) -> &T {
        &self.matrix[index.0 * self.columns + index.1]
    }
}

impl<T> IndexMut<(usize, usize)> for Matrix<T> {
    fn index_mut(&mut self, index:(usize, usize)) -> &mut T {
        &mut self.matrix[index.0 * self.columns + index.1]
    }
}

impl<T:Default> Matrix<T> {
    /// Constructs a new matrix for given number of rows and columns.
    pub fn new(rows:usize, columns:usize) -> Self {
        let mut matrix = Vec::with_capacity(rows * columns);
        for _ in 0..matrix.capacity() {
            matrix.push(Default::default())
        }
        Self {
            rows,
            columns,
            matrix,
        }
    }

    /// Adds a new row to matrix, filled with default values.
    pub fn new_row(&mut self) {
        for _ in 0..self.columns {
            self.matrix.push(Default::default());
        }
        self.rows += 1;
    }
}
