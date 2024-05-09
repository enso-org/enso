/** @file Displays the description of an element on hover or focus. */
import * as tailwindMerge from 'tailwind-merge'

import * as aria from '#/components/aria'
import * as portal from '#/components/Portal'

/** Props for a {@link Tooltip}. */
export interface TooltipProps
  extends Omit<Readonly<aria.TooltipProps>, 'offset' | 'UNSTABLE_portalContainer'> {}

const DEFAULT_CLASSES =
  'flex bg-frame backdrop-blur-default text-primary px-2 leading-cozy min-h-6 rounded-default shadow-soft text-xs'

const DEFAULT_CONTAINER_PADDING = 4
const DEFAULT_OFFSET = 4

/** Displays the description of an element on hover or focus. */
export function Tooltip(props: TooltipProps) {
  const { className, containerPadding = DEFAULT_CONTAINER_PADDING, ...ariaTooltipProps } = props

  const root = portal.useStrictPortalContext()

  const classes = tailwindMerge.twJoin(DEFAULT_CLASSES)

  return (
    <aria.Tooltip
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

// Re-export the TooltipTrigger component from `react-aria-components`
// eslint-disable-next-line no-restricted-syntax
export const TooltipTrigger = aria.TooltipTrigger
