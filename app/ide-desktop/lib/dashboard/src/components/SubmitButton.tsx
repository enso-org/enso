/** @file A styled submit button. */
import * as React from 'react'

import type * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

// ====================
// === SubmitButton ===
// ====================

/** Props for a {@link SubmitButton}. */
export interface SubmitButtonProps {
  readonly isLoading?: boolean
  readonly isDisabled?: boolean
  readonly text: string
  readonly icon: string
  readonly onPress: (event: aria.PressEvent) => void
}

/** A styled submit button. */
export default function SubmitButton(props: SubmitButtonProps) {
  const { isDisabled = false, text, icon, onPress, isLoading } = props

  return (
    <ariaComponents.Button
      size="large"
      fullWidth
      variant="submit"
      isDisabled={isDisabled}
      loading={isLoading}
      isActive={!isDisabled}
      type="submit"
      icon={icon}
      iconPosition="end"
      rounded="full"
      onPress={onPress}
    >
      {text}
    </ariaComponents.Button>
  )
}
