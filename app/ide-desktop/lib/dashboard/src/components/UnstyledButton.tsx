/** @file An unstyled button with a focus ring and focus movement behavior. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import type * as focusRing from '#/components/styled/FocusRing'
import FocusRing from '#/components/styled/FocusRing'

// =================
// === Constants ===
// =================

export const VARIANT_CLASSES: Readonly<Record<UnstyledButtonVariant, string>> = {
  bar: 'flex h-row items-center rounded-full border-0.5 border-primary/20 px-new-project-button-x transition-colors hover:bg-primary/10',
}

// =============================
// === UnstyledButtonVariant ===
// =============================

/** Variants of an {@link UnstyledButton}. */
// This alias is fine as it is a union that happens to only have one element.
// eslint-disable-next-line no-restricted-syntax
export type UnstyledButtonVariant = 'bar'

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
  const { tooltip, focusRingPlacement, variant, children, ...buttonProps } = props
  const focusChildProps = focusHooks.useFocusChild()

  const tooltipElement = tooltip === false ? null : tooltip ?? buttonProps['aria-label']

  const button = (
    <FocusRing {...(focusRingPlacement == null ? {} : { placement: focusRingPlacement })}>
      <aria.Button
        {...aria.mergeProps<aria.ButtonProps & React.RefAttributes<HTMLButtonElement>>()(
          buttonProps,
          focusChildProps,
          { className: variant == null ? '' : VARIANT_CLASSES[variant] },
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
