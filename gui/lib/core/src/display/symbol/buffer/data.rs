use crate::data::seq::observable::Observable;

/// Vector with attached callbacks listening for changes.
pub type Data<T,OnSet,OnResize> = Observable<Vec<T>,OnSet,OnResize>;
