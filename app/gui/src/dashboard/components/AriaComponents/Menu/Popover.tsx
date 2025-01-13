/**
 * @file
 * A specialized popover component for menus. For internal use only.
 */
import * as React from 'react'

import { useStrictPortalContext } from '#/components/Portal'
import * as aria from 'react-aria-components'

import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { DIALOG_BACKGROUND } from '../Dialog'

const MENU_POPOVER_STYLES = tv({
  base: 'shadow-xl overflow-clip min-w-[150px]',
  variants: {
    variant: {
      light: {
        base: DIALOG_BACKGROUND({ variant: 'light' }),
      },
      dark: {
        base: DIALOG_BACKGROUND({ variant: 'dark' }),
      },
    },
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-1 placement-top:slide-in-from-bottom-1 placement-left:slide-in-from-right-1 placement-right:slide-in-from-left-1 ease-out duration-200',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-1 placement-top:slide-out-to-bottom-1 placement-left:slide-out-to-right-1 placement-right:slide-out-to-left-1 ease-in duration-150',
    },
  },
  defaultVariants: {
    variant: 'light',
  },
})

/** Props for {@link MenuPopover} */
export interface MenuPopoverProps
  extends Omit<aria.PopoverProps, 'children'>,
    Omit<VariantProps<typeof MENU_POPOVER_STYLES>, 'isEntering' | 'isExiting'> {
  readonly children: React.ReactNode
}

/**
 * A specialized popover component for menus.
 * Provides animations and styling specific to menu use cases.
 */
export function MenuPopover(props: MenuPopoverProps) {
  const { children, className, variant, ...popoverProps } = props

  const root = useStrictPortalContext()

  return (
    <aria.Popover
      className={(values) =>
        MENU_POPOVER_STYLES({
          isEntering: values.isEntering,
          isExiting: values.isExiting,
          className: typeof className === 'function' ? className(values) : className,
          variant,
        })
      }
      UNSTABLE_portalContainer={root}
      {...popoverProps}
    >
      {children}
    </aria.Popover>
  )
}
