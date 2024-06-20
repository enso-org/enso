/** @file A styled submit button. */
import * as React from 'react'

import type * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
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
    <ariaComponents.Button
      variant="submit"
      isDisabled={isDisabled}
      isActive={!isDisabled}
      type="submit"
      className="py-1"
      onPress={onPress}
    >
      {text}
      <SvgMask src={icon} />
    </ariaComponents.Button>
  )
}
