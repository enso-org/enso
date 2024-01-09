/** @file Utility functions related to event handling. */
import type * as React from 'react'

import * as detect from 'enso-common/src/detect'

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

/** Returns `true` if and only if the event has the modifier key set
 * (`Ctrl` on Windows/Linux; `Cmd` on macOS). */
export function isModKey(event: React.KeyboardEvent | React.MouseEvent) {
    return detect.isOnMacOS() ? event.metaKey : event.ctrlKey
}
