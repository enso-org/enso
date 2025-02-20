/** @file A styled submit button. */
import * as React from 'react'

import { Button } from '#/components/AriaComponents'
import { submitForm } from '#/utilities/event'

// ====================
// === SubmitButton ===
// ====================

/** Props for a {@link SubmitButton}. */
export interface SubmitButtonProps {
  readonly isLoading?: boolean
  readonly isDisabled?: boolean
  readonly text: string
  readonly icon: string
}

/** A styled submit button. */
export default function SubmitButton(props: SubmitButtonProps) {
  const { isDisabled = false, text, icon, isLoading } = props

  return (
    <Button
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
      onPress={submitForm}
    >
      {text}
    </Button>
  )
}
