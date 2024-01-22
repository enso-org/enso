/** @file A hook that turns a `set` function for an {@link AssetTreeNode} to a `set` function
 * on its item, for a specific type of item. */
import * as React from 'react'

import type * as backend from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'

// ===================
// === useSetAsset ===
// ===================

/** Converts a React set state action for an {@link AssetTreeNode} to a set state action for any
 * subset of {@link backend.AnyAsset}. This is unsafe when `T` does not match the type of the
 * item contained in the `AssetTreeNode`, so this MUST be guarded by checking that the item is of
 * the correct type. A value of type `T` must be provided as the first parameter to ensure that this
 * has been done. */
export function useSetAsset<T extends backend.AnyAsset>(
  _value: T,
  setNode: React.Dispatch<React.SetStateAction<AssetTreeNode>>
) {
  return React.useCallback(
    (valueOrUpdater: React.SetStateAction<T>) => {
      setNode(oldNode => {
        const item =
          typeof valueOrUpdater === 'function'
            ? // This is SAFE, because it is a mistake for an item to change type.
              // eslint-disable-next-line no-restricted-syntax
              valueOrUpdater(oldNode.item as T)
            : valueOrUpdater
        return oldNode.with({ item })
      })
    },
    [/* should never change */ setNode]
  )
}
