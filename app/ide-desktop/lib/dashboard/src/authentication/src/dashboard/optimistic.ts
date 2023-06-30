/** @file Types and functions for representing optimistic state.
 * Optimistic UI is when UI updates are applied immediately with the expected result, instead of
 * waiting for a server response, based on the assumption that virtually all requests succeed.
 * In our case it is MANDATORY because immediate user feedback is important. */

// ========================
// === OptimisticStatus ===
// ========================

/** The state of an item being synced to a remote server. */
export enum OptimisticStatus {
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

export const CLASS_NAME: Record<OptimisticStatus, string> = {
    [OptimisticStatus.present]: '',
    [OptimisticStatus.deleting]: 'hidden',
    [OptimisticStatus.inserting]: 'opacity-50',
} as const
