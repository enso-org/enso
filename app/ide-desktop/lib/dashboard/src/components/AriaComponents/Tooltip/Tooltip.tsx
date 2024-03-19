/**
 * @file
 *
 * A tooltip displays a description of an element on hover or focus.
 */
import * as reactAriaComponents from 'react-aria-components'
import * as tailwindMerge from 'tailwind-merge'

import * as portal from '#/components/Portal'

/**
 *
 */
export interface TooltipProps
  extends Omit<reactAriaComponents.TooltipProps, 'offset' | 'UNSTABLE_portalContainer'> {}

const DEFAULT_CLASSES = 'z-1 flex bg-neutral-800 text-white p-2 rounded-md shadow-lg text-xs'

const DEFAULT_CONTAINER_PADDING = 4
const DEFAULT_OFFSET = 4

/**
 * A tooltip displays a description of an element on hover or focus.
 */
export function Tooltip(props: TooltipProps) {
  const { className, containerPadding = DEFAULT_CONTAINER_PADDING, ...ariaTooltipProps } = props

  const root = portal.useStrictPortalContext()

  const classes = tailwindMerge.twJoin(DEFAULT_CLASSES)

  return (
    <reactAriaComponents.Tooltip
      offset={DEFAULT_OFFSET}
      containerPadding={containerPadding}
      UNSTABLE_portalContainer={root.current}
      className={values =>
        tailwindMerge.twMerge(
          classes,
          typeof className === 'function' ? className(values) : className
        )
      }
      {...ariaTooltipProps}
    />
  )
}

export { TooltipTrigger } from 'react-aria-components'
