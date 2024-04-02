/** @file A styled submit button. */
import * as React from 'react'

import type * as aria from '#/components/aria'
import UnstyledButton from '#/components/UnstyledButton'
import SvgMask from '#/components/SvgMask'

// ====================
// === SubmitButton ===
// ====================

/** Props for a {@link SubmitButton}. */
export interface SubmitButtonProps {
  readonly isDisabled?: boolean
  readonly text: string
  readonly icon: string
  readonly onPress: (event: aria.PressEvent) => void
}

/** A styled submit button. */
export default function SubmitButton(props: SubmitButtonProps) {
  const { isDisabled = false, text, icon, onPress } = props

  return (
    <UnstyledButton
      isDisabled={isDisabled}
      className={`flex items-center justify-center gap-icon-with-text rounded-full bg-blue-600 py-auth-input-y text-white transition-all duration-auth selectable enabled:active hover:bg-blue-700 focus:bg-blue-700`}
      onPress={onPress}
    >
      {text}
      <SvgMask src={icon} />
    </UnstyledButton>
  )
}
