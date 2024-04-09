/** @file The type of operation that should be triggered on paste. */

// =================
// === PasteType ===
// =================

/** The type of operation that should be triggered on paste. */
enum PasteType {
  copy = 'copy',
  move = 'move',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default PasteType
