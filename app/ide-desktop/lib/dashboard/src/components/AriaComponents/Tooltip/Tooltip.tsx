import {
  Tooltip as AriaTooltip,
  type TooltipProps as AriaTooltipProps,
} from 'react-aria-components'
import { twJoin, twMerge } from 'tailwind-merge'

import { useStrictPortalContext } from '#/components/Portal'

/**
 *
 */
export interface TooltipProps
  extends Omit<AriaTooltipProps, 'offset' | 'UNSTABLE_portalContainer'> {}

const DEFAULT_CLASSES = 'flex bg-neutral-800 text-white p-2 rounded-md shadow-lg text-xs'

const DEFAULT_CONTAINER_PADDING = 4
const DEFAULT_OFFSET = 4

/**
 * A tooltip displays a description of an element on hover or focus.
 */
export function Tooltip(props: TooltipProps) {
  const { className, containerPadding = DEFAULT_CONTAINER_PADDING, ...ariaTooltipProps } = props

  const root = useStrictPortalContext()

  const classes = twJoin(DEFAULT_CLASSES)

  return (
    <AriaTooltip
      offset={DEFAULT_OFFSET}
      containerPadding={containerPadding}
      UNSTABLE_portalContainer={root.current}
      className={values =>
        twMerge(classes, typeof className === 'function' ? className(values) : className)
      }
      {...ariaTooltipProps}
    />
  )
}

export { TooltipTrigger } from 'react-aria-components'
