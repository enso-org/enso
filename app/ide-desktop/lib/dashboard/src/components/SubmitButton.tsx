/** @file A styled submit button. */
import * as React from 'react'

import SvgMask from '#/components/SvgMask'

// ====================
// === SubmitButton ===
// ====================

/** Props for a {@link SubmitButton}. */
export interface SubmitButtonProps {
  disabled?: boolean
  text: string
  icon: string
}

/** A styled submit button. */
export default function SubmitButton(props: SubmitButtonProps) {
  const { disabled = false, text, icon } = props
  return (
    <button
      disabled={disabled}
      type="submit"
      className="flex gap-2 items-center justify-center focus:outline-none text-white bg-blue-600 hover:bg-blue-700 focus:bg-blue-700 rounded-full py-2 w-full transition-all duration-300 ease-in disabled:opacity-50"
    >
      {text}
      <SvgMask src={icon} />
    </button>
  )
}
