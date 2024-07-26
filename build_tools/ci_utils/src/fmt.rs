use crate::prelude::*;



/// Formats list itself like a `Debug` but uses `ToString` to format elements.
pub fn display_list(
    sequence: impl IntoIterator<Item: ToString>,
    f: &mut Formatter,
) -> std::fmt::Result {
    f.debug_list().entries(sequence.into_iter().map(|item| item.to_string())).finish()
}
