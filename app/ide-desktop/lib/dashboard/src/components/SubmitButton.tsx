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
      className="flex gap-icon-with-text items-center justify-center focus:outline-none text-white bg-blue-600 hover:bg-blue-700 focus:bg-blue-700 rounded-full py-auth-input-y w-full transition-all duration-auth ease-in disabled:opacity-disabled"
    >
      {text}
      <SvgMask src={icon} />
    </button>
  )
}
