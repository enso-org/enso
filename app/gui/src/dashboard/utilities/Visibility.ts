/** @file The state of an item that needs to be synced with a remote server. */

// ==================
// === Visibility ===
// ==================

/** The state of an item that that needs to be synced with a remote server. */
enum Visibility {
  /** The item is present. */
  visible = 'visibility-visible',
  /**
   * The item will be inserted, but the backend request has not yet finished,
   * or the item has been cut and will potentially be moved in the future.
   */
  faded = 'visibility-faded',
  /** The item will be deleted, but the backend request has not yet finished. */
  hidden = 'visibility-hidden',
}

export default Visibility
