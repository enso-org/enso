/** @file Types and functions for representing optimistic state.
 * Optimistic UI is when UI updates are applied immediately with the expected result, instead of
 * waiting for a server response, based on the assumption that virtually all requests succeed.
 * In our case it is MANDATORY because immediate user feedback is important. */

// ==================
// === Visibility ===
// ==================

/** The state of an item being synced to a remote server. */
enum Visibility {
  /** The item is present. */
  visible = 'visible',
  /** The item will be inserted, but the backend request has not yet finished,
   * or the item has been cut and will potentially be moved in the future. */
  faded = 'faded',
  /** The item will be deleted, but the backend request has not yet finished. */
  hidden = 'hidden',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default Visibility

// =================
// === Constants ===
// =================

/** The corresponding CSS classes for table rows, for each {@link Visibility}. */
export const CLASS_NAME: Record<Visibility, string> = {
  [Visibility.visible]: '',
  // Note that in some cases (e.g. table rows with alternating colors), the element should be
  // completely removed from the DOM.
  [Visibility.hidden]: 'hidden',
  [Visibility.faded]: 'opacity-50 pointer-events-none-recursive',
} as const
