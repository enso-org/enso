//! An efficient representation of a 2D matrix.

use crate::prelude::*;

use std::ops::Index;
use std::ops::IndexMut;



// ============
// == Matrix ==
// ============

/// An efficient 2D matrix implemented on top of [`std::vec::Vec`].
#[derive(Clone,Debug,Default,PartialEq,Eq)]
#[allow(missing_docs)]
pub struct Matrix<T> {
    pub rows    : usize,
    pub columns : usize,
    pub matrix  : Vec<T>,
}

impl<T:Copy> Matrix<T> {
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
    pub fn safe_index(&self, row:usize, column:usize) -> Option<T> {
        (row < self.rows && column < self.columns).as_some_from(|| {
            self.matrix[row*self.columns+column]
        })
    }
}

impl<T:Default> Matrix<T> {
    /// Constructs a matrix with the dimensions given by `rows` and `columns`.
    pub fn new(rows:usize, columns:usize) -> Self {
        let mut matrix = Vec::with_capacity(rows*columns);
        for _ in 0..matrix.capacity() {
            matrix.push(default())
        }
        Self{rows,columns,matrix}
    }

    /// Adds a new row to the matrix `self`, filled with default values.
    pub fn new_row(&mut self) {
        for _ in 0..self.columns {
            self.matrix.push(default());
        }
        self.rows += 1;
    }
}

// FIXME: Wrong indexing order!
// === Trait Impls ===

impl<T> Index<(usize,usize)> for Matrix<T> {
    type Output = T;
    fn index(&self, index:(usize,usize)) -> &T {
        &self.matrix[index.0*self.columns+index.1]
    }
}

impl<T> IndexMut<(usize,usize)> for Matrix<T> {
    fn index_mut(&mut self, index:(usize,usize)) -> &mut T {
        &mut self.matrix[index.0*self.columns+index.1]
    }
}
