/**
 * @file A hook that turns a `set` function for an {@link Asset} to a `set` function
 * on its item, for a specific type of item.
 */
import type { AnyAsset } from '#/services/Backend'
import { useCallback, type Dispatch, type SetStateAction } from 'react'

/**
 * Converts a React set state action for an {@link Asset} to a set state action for any
 * subset of {@link AnyAsset}. This is unsafe when `T` does not match the type of the
 * item contained in the `Asset`, so this MUST be guarded by checking that the item is of
 * the correct type. A value of type `T` must be provided as the first parameter to ensure that this
 * has been done.
 */
export function useSetAsset<T extends AnyAsset>(
  _value: T,
  setNode: Dispatch<SetStateAction<AnyAsset>>,
) {
  return useCallback(
    (valueOrUpdater: SetStateAction<T>) => {
      setNode((oldNode) => {
        const item =
          typeof valueOrUpdater === 'function' ?
            // This is SAFE, because it is a mistake for an item to change type.
            // eslint-disable-next-line no-restricted-syntax
            valueOrUpdater(oldNode as T)
          : valueOrUpdater
        return item
      })
    },
    [setNode],
  )
}
