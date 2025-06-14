/** @file Barrel file for `react-aria-components`. */
export * from '@react-aria/interactions'
export { ClearPressResponder } from '@react-aria/interactions'
export type * from '@react-types/shared'
export * from 'react-aria'
// @ts-expect-error The conflicting exports are props types ONLY
export * from 'react-aria-components'
// Resolve ambigouous star exports (`react-aria` and `react-aria-components`)
export { I18nProvider, RouterProvider } from 'react-aria-components'
export {
  useTooltipTriggerState,
  type OverlayTriggerState,
  type TooltipTriggerState,
} from 'react-stately'

export { mergeProps } from './aria'
export { TabPanel } from './TabPanel'
