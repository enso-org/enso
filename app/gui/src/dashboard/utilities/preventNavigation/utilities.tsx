/** @file Utilities for `usePreventNavigation`. */

// This variable must be mutable because it is set by the hook below.
// eslint-disable-next-line no-restricted-syntax
export let shouldClose = false

/** Set `shouldClose`. */
export function setShouldClose(newShouldClose: boolean) {
  shouldClose = newShouldClose
}
