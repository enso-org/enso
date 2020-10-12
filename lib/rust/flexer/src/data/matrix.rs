//! An efficient representation of a 2D matrix.

use enso_prelude::default;

use std::ops::Index;
use std::ops::IndexMut;



// ============
// == Matrix ==
// ============

/// An efficient 2D matrix implemented on top of [`std::vec::Vec`].
#[derive(Clone,Debug,Default,PartialEq,Eq)]
pub struct Matrix<T> {
    /// The number of rows in the matrix.
    rows: usize,
    /// The number of columns in the matrix.
    columns: usize,
    /// The matrix.
    matrix: Vec<T>,
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
