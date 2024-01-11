/** @file A hook to trigger React re-renders. */
import * as React from 'react'

// ==================
// === useRefresh ===
// ==================

// This must not be a `symbol` as it cannot be sent to Playright.
/** The type of the state returned by {@link useRefresh}. */
// eslint-disable-next-line no-restricted-syntax
export interface RefreshState {}

/** A hook that contains no state. It is used to trigger React re-renders. */
export function useRefresh() {
    // Uses an empty object literal because every distinct literal
    // is a new reference and therefore is not equal to any other object literal.
    return React.useReducer((): RefreshState => ({}), {})
}
