//! Helper code meant to be used by the code generated through usage of macros
//! from `enso-shapely-macros` crate.

pub use enso_shapely_macros::*;

use std::ops::Generator;
use std::ops::GeneratorState;
use std::pin::Pin;



// ==========================
// === GeneratingIterator ===
// ==========================

/// Iterates over values yielded from the wrapped `Generator`.
#[derive(Debug)]
pub struct GeneratingIterator<G: Generator>(pub G);

impl<G> Iterator for GeneratingIterator<G>
where G: Generator<Return = ()> + Unpin {
    type Item = G::Yield;
    fn next(&mut self) -> Option<Self::Item> {
        match Pin::new(&mut self.0).resume(()) {
            GeneratorState::Yielded(element) => Some(element),
            _                                => None,
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generating_iterator_works() {
        let generator = || {
            yield 0;
            yield 1;
            yield 2;
        };
        let expected_numbers         = vec!(0, 1, 2);
        let generator_iter           = GeneratingIterator(generator);
        let collected_result: Vec<_> = generator_iter.collect();
        assert_eq!(collected_result, expected_numbers);
    }
}
