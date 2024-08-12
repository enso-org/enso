// //! Extension methods for `Iterator` and `Iterator`-like types.

// /// Extension methods for `Iterator` and `Iterator`-like types.s
// pub trait TryIteratorExt: Iterator {
//     /// The result of successful iteration.
//     type Ok;

//     /// Collects the results of the iterator into a `Result<Vec<_>>`.
//     fn try_collect_vec(self) -> Result<Vec<Self::Ok>>;
// }

// impl<T, U, E> TryIteratorExt for T
// where
//     T: Iterator<Item = std::result::Result<U, E>>,
//     E: Into<anyhow::Error>,
// {
//     type Ok = U;
//     fn try_collect_vec(self) -> Result<Vec<U>> {
//         self.collect::<Result<Vec<_>>>()
//     }
// }
