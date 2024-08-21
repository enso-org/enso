/** @file Barrel re-export of `react-aria` and `react-aria-components`. */
import * as aria from 'react-aria'

export type * from '@react-types/shared'
// @ts-expect-error The conflicting exports are props types ONLY.
export * from 'react-aria'
// @ts-expect-error The conflicting exports are props types ONLY.
export * from 'react-aria-components'
export { useTooltipTriggerState, type OverlayTriggerState } from 'react-stately'

// ==================
// === mergeProps ===
// ==================

/** Merges multiple props objects together.
 * Event handlers are chained, classNames are combined, and ids are deduplicated -
 * different ids will trigger a side-effect and re-render components hooked up with `useId`.
 * For all other props, the last prop object overrides all previous ones.
 *
 * The constraint is defaulted to `never` to make an explicit constraint mandatory. */
export function mergeProps<Constraint extends object = never>() {
  return <T extends (Partial<Constraint> | null | undefined)[]>(...args: T) =>
    aria.mergeProps(...args)
}
