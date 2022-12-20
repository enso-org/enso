//! Universally known environment variables.

use crate::env::accessor::PathLike;



/// PATH environment variable.
///
/// It is a special variable that contains a list of paths, that define the search path for
/// executable files.
pub const PATH: PathLike = PathLike("PATH");
