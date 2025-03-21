/** @file Utility functions for `Icon`. */
import type {
  AvailableIconReturn,
  IconProp,
  LegacyAvailableIconReturn,
} from '#/components/AriaComponents/types'

/** Utility function to render an icon based on the icon type and render props. */
export function renderIcon<Icon extends string, Render>(
  icon: IconProp<Icon, Render>,
  renderProps: Render,
): AvailableIconReturn | LegacyAvailableIconReturn<Icon> {
  return typeof icon === 'function' ? icon(renderProps) : icon
}
