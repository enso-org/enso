/** @file Types and functions for representing optimistic state.
 * Optimistic UI is when UI updates are applied immediately with the expected result, instead of
 * waiting for a server response, based on the assumption that virtually all requests succeed.
 * In our case it is MANDATORY because immediate user feedback is important. */

// ================
// === Presence ===
// ================

/** The state of an item being synced to a remote server. */
export enum Presence {
    /** The item is present. */
    present = 'present',
    /** The item will be inserted, but the backend request has not yet finished. */
    inserting = 'inserting',
    /** The item will be deleted, but the backend request has not yet finished. */
    deleting = 'deleting',
}

// =================
// === Constants ===
// =================

/** The corresponding CSS classes for table rows, for each {@link Presence}. */
export const CLASS_NAME: Record<Presence, string> = {
    [Presence.present]: '',
    // Note that in some cases (e.g. table rows with alternating colors), the element should be
    // completely removed from the DOM.
    [Presence.deleting]: 'hidden',
    [Presence.inserting]: 'opacity-50 pointer-events-none-recursive',
} as const
