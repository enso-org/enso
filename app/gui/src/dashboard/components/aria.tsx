/** @file Barrel re-export of `react-aria` and `react-aria-components`. */
import type { Mutable } from 'enso-common/src/utilities/data/object'
import * as aria from 'react-aria'

export * from '@react-aria/interactions'
export { ClearPressResponder } from '@react-aria/interactions'
export type * from '@react-types/shared'
export * from 'react-aria'
// @ts-expect-error The conflicting exports are props types ONLY.
export * from 'react-aria-components'
export { useTooltipTriggerState, type OverlayTriggerState } from 'react-stately'

// ==================
// === mergeProps ===
// ==================

/**
 * Merges multiple props objects together.
 * Event handlers are chained, classNames are combined, and ids are deduplicated -
 * different ids will trigger a side-effect and re-render components hooked up with `useId`.
 * For all other props, the last prop object overrides all previous ones.
 *
 * The constraint is defaulted to `never` to make an explicit constraint mandatory.
 */
export function mergeProps<Constraint extends object = never>() {
  return <const T extends readonly (Partial<Constraint> | null | undefined)[]>(
    ...args: T & { [K in keyof T]: Pick<T[K], keyof Constraint & keyof T[K]> }
    // This is SAFE, as `args` is an intersection of `T` and another type.
    // eslint-disable-next-line no-restricted-syntax
  ) => aria.mergeProps<Mutable<T>>(...(args as T))
}
