/** @file A styled submit button. */
import * as React from 'react'

import SvgMask from '#/components/SvgMask'

// ====================
// === SubmitButton ===
// ====================

/** Props for a {@link SubmitButton}. */
export interface SubmitButtonProps {
  readonly disabled?: boolean
  readonly text: string
  readonly icon: string
}

/** A styled submit button. */
export default function SubmitButton(props: SubmitButtonProps) {
  const { disabled = false, text, icon } = props
  return (
    <button
      disabled={disabled}
      type="submit"
      className={`flex items-center justify-center gap-icon-with-text rounded-full bg-blue-600 py-auth-input-y text-white transition-all duration-auth selectable hover:bg-blue-700 focus:bg-blue-700 focus:outline-none enabled:active`}
    >
      {text}
      <SvgMask src={icon} />
    </button>
  )
}
