/** @file An unstyled button with a focus ring and focus movement behavior. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import * as aria from '#/components/aria'
import type * as focusRing from '#/components/styled/FocusRing'
import FocusRing from '#/components/styled/FocusRing'

// ======================
// === UnstyledButton ===
// ======================

/** Props for a {@link UnstyledButton}. */
export interface UnstyledButtonProps extends Readonly<React.PropsWithChildren> {
  readonly focusRingPlacement?: focusRing.FocusRingPlacement
  readonly autoFocus?: boolean
  /** When `true`, the button is not clickable. */
  readonly isDisabled?: boolean
  readonly className?: string
  readonly onPress: (event: aria.PressEvent) => void
}

/** An unstyled button with a focus ring and focus movement behavior. */
export default function UnstyledButton(props: UnstyledButtonProps) {
  const { focusRingPlacement, autoFocus = false, isDisabled = false, className, onPress } = props
  const { children } = props
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  return (
    <FocusRing {...(focusRingPlacement == null ? {} : { placement: focusRingPlacement })}>
      <aria.Button
        autoFocus={autoFocus}
        isDisabled={isDisabled}
        className={className ?? ''}
        onPress={onPress}
        onKeyDown={handleFocusMove}
      >
        {children}
      </aria.Button>
    </FocusRing>
  )
}
