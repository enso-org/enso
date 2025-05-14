/** @file Utility functions for `Icon`. */
import type {
  AvailableIconReturn,
  IconProp,
  LegacyAvailableIconReturn,
} from '#/components/AriaComponents/types'
import type { ICON_STYLES } from '#/components/Icon/variants'
import type { VariantProps } from '#/utilities/tailwindVariants'

export const ICON_COLORS = [
  'custom',
  'primary',
  'danger',
  'success',
  'accent',
  'muted',
  'disabled',
  'invert',
  'inherit',
  'current',
] as const satisfies readonly VariantProps<typeof ICON_STYLES>['color'][]

/** Utility function to render an icon based on the icon type and render props. */
export function renderIcon<Icon extends string, Render>(
  icon: IconProp<Icon, Render>,
  renderProps: Render,
): AvailableIconReturn | LegacyAvailableIconReturn<Icon> {
  return typeof icon === 'function' ? icon(renderProps) : icon
}
