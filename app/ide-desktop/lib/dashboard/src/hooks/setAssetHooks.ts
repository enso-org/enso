/** @file A hook that turns a `set` function for an {@link backend.AnyAsset} to a `set` function
 * for a specific variant. */
import type * as React from 'react'

import type * as backend from '#/services/Backend'

// ===================
// === useSetAsset ===
// ===================

/** Converts a React set state action for an {@link backend.AnyAsset} to a set state action for any
 * of its variants. This is unsafe when `T` does not match the type of the item,
 * so this MUST be guarded by checking that the item is of the correct type.
 * A value of type `T` must be provided as the first parameter to ensure that this has been done.
 */
export function useSetAsset<T extends backend.AnyAsset>(
  _value: T,
  setNode: React.Dispatch<React.SetStateAction<backend.AnyAsset>>
) {
  // eslint-disable-next-line no-restricted-syntax
  return setNode as React.Dispatch<React.SetStateAction<T>>
}
