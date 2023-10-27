/** @file Utility functions related to event handling. */

import type * as React from 'react'

// =============================
// === Mouse event utilities ===
// =============================

/** Returns `true` if and only if the event is a single click event. */
export function isSingleClick(event: React.MouseEvent) {
    return event.detail === 1
}

/** Returns `true` if and only if the event is a double click event. */
export function isDoubleClick(event: React.MouseEvent) {
    return event.detail === 2
}
