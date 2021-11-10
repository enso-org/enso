//! An efficient representation of a 2D matrix.

use crate::prelude::*;

use std::ops::Index;
use std::ops::IndexMut;



// ============
// == Matrix ==
// ============

/// An efficient 2D matrix implemented on top of [`std::vec::Vec`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Matrix<T> {
    pub rows:    usize,
    pub columns: usize,
    pub matrix:  Vec<T>,
}

impl<T: Copy> Matrix<T> {
    /// Get the number of rows in the matrix.
    pub fn rows(&self) -> usize {
        self.rows
    }

    /// Get the number of columns in the matrix.
    pub fn columns(&self) -> usize {
        self.columns
    }

    /// Obtain the indices for the rows in this matrix.
    pub fn row_indices(&self) -> Range<usize> {
        0..self.rows()
    }

    /// Indexing with bounds checking.
    pub fn safe_index(&self, row: usize, column: usize) -> Option<T> {
        (row < self.rows && column < self.columns)
            .as_some_from(|| self.matrix[row * self.columns + column])
    }
}

impl<T: Default> Matrix<T> {
    /// Construct a matrix with the dimensions given by `rows` and `columns`.
    pub fn new(rows: usize, columns: usize) -> Self {
        let mut matrix = Vec::with_capacity(rows * columns);
        for _ in 0..matrix.capacity() {
            matrix.push(default())
        }
        Self { rows, columns, matrix }
    }

    /// Add a new row to the matrix `self`, filled with default values.
    pub fn new_row(&mut self) {
        for _ in 0..self.columns {
            self.matrix.push(default());
        }
        self.rows += 1;
    }

    /// Add a new column to the matrix `self`, filled with default values.
    ///
    /// Note that this is an _expensive_ operation that requires moving potentially very large
    /// allocations around.
    pub fn new_column(&mut self) {
        for n in 0..self.rows {
            let index = self.columns * n + self.columns + n;
            self.matrix.insert(index, default());
        }
        self.columns += 1;
    }
}

// FIXME: Wrong indexing order!
// === Trait Impls ===

impl<T> Index<(usize, usize)> for Matrix<T> {
    type Output = T;
    fn index(&self, index: (usize, usize)) -> &T {
        &self.matrix[index.0 * self.columns + index.1]
    }
}

impl<T> IndexMut<(usize, usize)> for Matrix<T> {
    fn index_mut(&mut self, index: (usize, usize)) -> &mut T {
        &mut self.matrix[index.0 * self.columns + index.1]
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_size() {
        let default = Matrix::<usize>::default();
        assert_eq!(default.rows, 0);
        assert_eq!(default.columns, 0);
    }

    #[test]
    fn construct_with_dimensions() {
        let matrix = Matrix::<usize>::new(5, 10);
        assert_eq!(matrix.rows, 5);
        assert_eq!(matrix.columns, 10);
    }

    #[test]
    fn add_row() {
        let mut matrix = Matrix::<usize>::new(2, 2);
        matrix[(0, 0)] = 1;
        matrix[(0, 1)] = 2;
        matrix[(1, 0)] = 3;
        matrix[(1, 1)] = 4;
        assert_eq!(matrix.rows, 2);
        assert_eq!(matrix.columns, 2);
        matrix.new_row();
        assert_eq!(matrix.rows, 3);
        assert_eq!(matrix.columns, 2);
        assert_eq!(matrix[(0, 0)], 1);
        assert_eq!(matrix[(0, 1)], 2);
        assert_eq!(matrix[(1, 0)], 3);
        assert_eq!(matrix[(1, 1)], 4);
        assert_eq!(matrix[(2, 0)], 0);
        assert_eq!(matrix[(2, 1)], 0);
    }

    #[test]
    fn add_column() {
        let mut matrix = Matrix::<usize>::new(2, 2);
        matrix[(0, 0)] = 1;
        matrix[(0, 1)] = 2;
        matrix[(1, 0)] = 3;
        matrix[(1, 1)] = 4;
        assert_eq!(matrix.rows, 2);
        assert_eq!(matrix.columns, 2);
        matrix.new_column();
        assert_eq!(matrix.rows, 2);
        assert_eq!(matrix.columns, 3);
        assert_eq!(matrix[(0, 0)], 1);
        assert_eq!(matrix[(0, 1)], 2);
        assert_eq!(matrix[(1, 0)], 3);
        assert_eq!(matrix[(1, 1)], 4);
        assert_eq!(matrix[(0, 2)], 0);
        assert_eq!(matrix[(1, 2)], 0);
    }

    #[test]
    fn row_column_indexing() {
        let mut matrix = Matrix::<usize>::new(2, 2);
        matrix[(0, 0)] = 1;
        matrix[(0, 1)] = 2;
        matrix[(1, 0)] = 3;
        matrix[(1, 1)] = 4;
        let mut output = Vec::default();
        for row in 0..2 {
            for col in 0..2 {
                output.push(matrix[(row, col)]);
            }
        }
        assert_eq!(output, vec![1, 2, 3, 4]);
    }

    #[test]
    fn safe_indexing() {
        let matrix = Matrix::<usize>::new(2, 2);
        let exists = matrix.safe_index(0, 0);
        let does_not_exist = matrix.safe_index(3, 0);
        assert_eq!(exists, Some(0));
        assert_eq!(does_not_exist, None);
    }
}
