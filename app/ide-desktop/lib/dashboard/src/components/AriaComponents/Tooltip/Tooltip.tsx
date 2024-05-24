/** @file Displays the description of an element on hover or focus. */
import * as twv from 'tailwind-variants'

import * as aria from '#/components/aria'
import * as portal from '#/components/Portal'

// =================
// === Constants ===
// =================

export const TOOLTIP_STYLES = twv.tv({
  base: 'group flex bg-frame justify-center outline outline-1 outline-primary/15 items-center backdrop-blur-default text-primary px-2 py-1.5 leading-cozy text-center text-balance min-h-6 rounded-lg shadow-lg text-xs max-w-xs',
  variants: {
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-0.5 placement-top:slide-in-from-bottom-0.5 placement-left:slide-in-from-right-0.5 placement-right:slide-in-from-left-0.5 ease-out duration-150',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-0.5 placement-top:slide-out-to-bottom-0.5 placement-left:slide-out-to-right-0.5 placement-right:slide-out-to-left-0.5 ease-in duration-150',
    },
  },
})

const DEFAULT_CONTAINER_PADDING = 6
const DEFAULT_OFFSET = 9

// ===============
// === Tooltip ===
// ===============

/** Props for a {@link Tooltip}. */
export interface TooltipProps
  extends Omit<Readonly<aria.TooltipProps>, 'offset' | 'UNSTABLE_portalContainer'> {}

/** Displays the description of an element on hover or focus. */
export function Tooltip(props: TooltipProps) {
  const { className, containerPadding = DEFAULT_CONTAINER_PADDING, ...ariaTooltipProps } = props
  const root = portal.useStrictPortalContext()

  return (
    <aria.Tooltip
      offset={DEFAULT_OFFSET}
      containerPadding={containerPadding}
      UNSTABLE_portalContainer={root.current}
      className={aria.composeRenderProps(className, (classNames, values) =>
        TOOLTIP_STYLES({ className: classNames, ...values })
      )}
      {...ariaTooltipProps}
    />
  )
}

// Re-export the TooltipTrigger component from `react-aria-components`
// eslint-disable-next-line no-restricted-syntax
export const TooltipTrigger = aria.TooltipTrigger
