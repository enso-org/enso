/** @file Barrel file for `react-aria-components`. */
export * from '@react-aria/interactions'
export { ClearPressResponder } from '@react-aria/interactions'
// @ts-expect-error All conflicting exports are types
export type * from '@react-types/shared'
// @ts-expect-error All conflicting exports are types
export * from 'react-aria'
// @ts-expect-error All conflicting exports are props types
export * from 'react-aria-components'
// Resolve ambigouous star exports (`react-aria` and `react-aria-components`)
// eslint-disable-next-line camelcase
export { UNSAFE_PortalProvider, useUNSAFE_PortalContext } from '@react-aria/overlays'
export { I18nProvider, RouterProvider } from 'react-aria-components'
export {
  useTooltipTriggerState,
  type OverlayTriggerState,
  type TooltipTriggerState,
} from 'react-stately'

export { mergeProps } from './aria'
export { TabPanel } from './TabPanel'
