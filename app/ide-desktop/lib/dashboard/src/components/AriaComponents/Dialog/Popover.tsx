/** @file An overlay element positioned relative to a trigger, used to display additional content
 * or actions. */
import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as aria from '#/components/aria'
import * as portal from '#/components/Portal'

// =================
// === Constants ===
// =================

export const POPOVER_STYLES = twv.tv({
  base: 'bg-white shadow-lg',
  variants: {
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-1 placement-top:slide-in-from-bottom-1 placement-left:slide-in-from-right-1 placement-right:slide-in-from-left-1 ease-out duration-200',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-1 placement-top:slide-out-to-bottom-1 placement-left:slide-out-to-right-1 placement-right:slide-out-to-left-1 ease-in duration-150',
    },
    size: {
      xsmall: 'max-w-xs',
      small: 'max-w-sm',
      medium: 'max-w-md',
      large: 'max-w-lg',
      hero: 'max-w-xl',
    },
    roundings: {
      none: '',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      full: 'rounded-full',
    },
  },
})

// ===============
// === Popover ===
// ===============

/** Props for a {@link Popover}. */
export interface PopoverProps extends aria.PopoverProps, twv.VariantProps<typeof POPOVER_STYLES> {}

/** An overlay element positioned relative to a trigger, used to display additional content
 * or actions. */
export function Popover(props: PopoverProps) {
  const { children, className, size = 'small', roundings = 'large', ...ariaPopoverProps } = props
  const root = portal.useStrictPortalContext()

  return (
    <aria.Popover
      className={values =>
        POPOVER_STYLES({
          ...values,
          size,
          roundings,
          className: typeof className === 'function' ? className(values) : className,
        })
      }
      UNSTABLE_portalContainer={root}
      {...ariaPopoverProps}
    >
      {opts => (
        <aria.Dialog>
          <div className="relative flex-auto overflow-y-auto p-3.5">
            {typeof children === 'function' ? children(opts) : children}
          </div>
        </aria.Dialog>
      )}
    </aria.Popover>
  )
}
