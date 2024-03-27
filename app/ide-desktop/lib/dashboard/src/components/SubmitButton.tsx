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
  const { disabled: disabled = false, text, icon } = props
  return (
    // This MUST be a button as `aria.Button` does not support `type="submit"`.
    // eslint-disable-next-line no-restricted-syntax
    <button
      disabled={disabled}
      type="submit"
      className={`focus-child flex items-center justify-center gap-icon-with-text rounded-full bg-blue-600 py-auth-input-y text-white transition-all duration-auth selectable enabled:active hover:bg-blue-700 focus:bg-blue-700 focus:outline-none`}
    >
      {text}
      <SvgMask src={icon} />
    </button>
  )
}
