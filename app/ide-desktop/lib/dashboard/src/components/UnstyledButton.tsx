/** @file An unstyled button with a focus ring and focus movement behavior. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import type * as focusRing from '#/components/styled/FocusRing'
import FocusRing from '#/components/styled/FocusRing'

// =================
// === Constants ===
// =================

export const VARIANT_CLASSES: Readonly<Record<UnstyledButtonVariant, string>> = {
  // Note that the `*:` variant is generally discouraged, but `relative` is a class that users
  // will want to add to all children of elements with `before:absolute` anyway, otherwise the children
  // will render under the backdrop because the backdrop is higher in the stacking order.
  accent:
    'relative flex h-row items-center rounded-full px-3 text-white before:absolute before:inset before:rounded-full before:bg-accent before:transition-all hover:before:brightness-90 *:relative text whitespace-nowrap font-bold',
  regular:
    'flex h-row items-center rounded-full border-0.5 border-primary/20 px-[9.5px] transition-colors hover:bg-primary/10 text whitespace-nowrap font-bold',
}

// =============================
// === UnstyledButtonVariant ===
// =============================

/** Variants of an {@link UnstyledButton}. */
export type UnstyledButtonVariant = 'accent' | 'regular'

// ======================
// === UnstyledButton ===
// ======================

/** Props for a {@link UnstyledButton}. */
export interface UnstyledButtonProps extends Readonly<React.PropsWithChildren> {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'aria-label'?: string
  readonly variant?: UnstyledButtonVariant
  /** Falls back to `aria-label`. Pass `false` to explicitly disable the tooltip. */
  readonly tooltip?: React.ReactNode
  readonly focusRingPlacement?: focusRing.FocusRingPlacement
  readonly autoFocus?: boolean
  /** When `true`, the button is not clickable. */
  readonly isDisabled?: boolean
  readonly className?: string
  readonly style?: React.CSSProperties
  readonly onPress: (event: aria.PressEvent) => void
}

/** An unstyled button with a focus ring and focus movement behavior. */
function UnstyledButton(props: UnstyledButtonProps, ref: React.ForwardedRef<HTMLButtonElement>) {
  const { tooltip, focusRingPlacement, variant, children, className = '', ...buttonProps } = props
  const focusChildProps = focusHooks.useFocusChild()

  const tooltipElement = tooltip === false ? null : tooltip ?? buttonProps['aria-label']

  const button = (
    <FocusRing {...(focusRingPlacement == null ? {} : { placement: focusRingPlacement })}>
      <aria.Button
        {...aria.mergeProps<aria.ButtonProps & React.RefAttributes<HTMLButtonElement>>()(
          buttonProps,
          focusChildProps,
          { className: tailwindMerge.twMerge(variant == null ? '' : VARIANT_CLASSES[variant], className) },
          { ref }
        )}
      >
        {children}
      </aria.Button>
    </FocusRing>
  )

  return tooltipElement == null ? (
    button
  ) : (
    <ariaComponents.TooltipTrigger>
      {button}
      <ariaComponents.Tooltip>{tooltipElement}</ariaComponents.Tooltip>
    </ariaComponents.TooltipTrigger>
  )
}

export default React.forwardRef(UnstyledButton)
