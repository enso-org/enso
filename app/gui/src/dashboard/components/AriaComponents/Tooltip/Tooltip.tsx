/** @file Displays the description of an element on hover or focus. */
import * as aria from '#/components/aria'
import { useStrictPortalContext } from '#/components/Portal'

import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { ResetButtonGroupContext } from '../Button'

import { DIALOG_BACKGROUND } from '../Dialog'
import { TEXT_STYLE } from '../Text'

// =================
// === Constants ===
// =================

export const TOOLTIP_STYLES = tv({
  base: 'group flex justify-center items-center text-center [overflow-wrap:anywhere]',
  variants: {
    variant: {
      custom: '',
      primary: DIALOG_BACKGROUND({ variant: 'dark', className: 'text-invert' }),
      inverted: DIALOG_BACKGROUND({ variant: 'light', className: 'text-primary' }),
    },
    size: {
      custom: '',
      medium: TEXT_STYLE({ className: 'px-2 py-1', color: 'custom', balance: true }),
    },
    rounded: {
      custom: '',
      full: 'rounded-full',
      xxxlarge: 'rounded-3xl',
      xxlarge: 'rounded-2xl',
      xlarge: 'rounded-xl',
      large: 'rounded-lg',
      medium: 'rounded-md',
      small: 'rounded-sm',
      none: 'rounded-none',
    },
    maxWidth: {
      custom: '',
      xsmall: 'max-w-xs',
      small: 'max-w-sm',
      medium: 'max-w-md',
      large: 'max-w-lg',
      xlarge: 'max-w-xl',
    },
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-0.5 placement-top:slide-in-from-bottom-0.5 placement-left:slide-in-from-right-0.5 placement-right:slide-in-from-left-0.5 ease-out duration-150',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-0.5 placement-top:slide-out-to-bottom-0.5 placement-left:slide-out-to-right-0.5 placement-right:slide-out-to-left-0.5 ease-in duration-150',
    },
  },
  defaultVariants: {
    variant: 'primary',
    size: 'medium',
    maxWidth: 'xsmall',
    rounded: 'xxxlarge',
  },
})

const DEFAULT_CONTAINER_PADDING = 6
const DEFAULT_OFFSET = 9

// ===============
// === Tooltip ===
// ===============

/** Props for a {@link Tooltip}. */
export interface TooltipProps
  extends Omit<Readonly<aria.TooltipProps>, 'offset' | 'UNSTABLE_portalContainer'>,
    Omit<VariantProps<typeof TOOLTIP_STYLES>, 'isEntering' | 'isExiting'> {}

/** Displays the description of an element on hover or focus. */
export function Tooltip(props: TooltipProps) {
  const {
    className,
    containerPadding = DEFAULT_CONTAINER_PADDING,
    variant,
    size,
    rounded,
    ...ariaTooltipProps
  } = props

  const root = useStrictPortalContext()

  return (
    <ResetButtonGroupContext>
      <aria.Tooltip
        offset={DEFAULT_OFFSET}
        containerPadding={containerPadding}
        UNSTABLE_portalContainer={root}
        className={aria.composeRenderProps(className, (classNames, values) =>
          TOOLTIP_STYLES({ className: classNames, variant, size, rounded, ...values }),
        )}
        data-ignore-click-outside
        {...ariaTooltipProps}
      />
    </ResetButtonGroupContext>
  )
}

// Re-export the TooltipTrigger component from `react-aria-components`
// eslint-disable-next-line no-restricted-syntax
export const TooltipTrigger = aria.TooltipTrigger

Tooltip.Trigger = TooltipTrigger
