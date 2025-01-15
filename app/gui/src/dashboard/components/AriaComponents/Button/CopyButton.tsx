/** @file A button that copies text to the clipboard. */
import * as React from 'react'

import Error from '#/assets/cross.svg'
import CopyIcon from '#/assets/duplicate.svg'
import Done from '#/assets/tick.svg'

import * as copyHook from '#/hooks/copyHooks'

import * as textProvider from '#/providers/TextProvider'

import * as button from './Button'

// ==================
// === CopyButton ===
// ==================

/** Props for a {@link CopyButton}. */
export interface CopyButtonProps extends Omit<button.ButtonProps, 'icon' | 'loading' | 'onPress'> {
  /** The text to copy to the clipboard. */
  readonly copyText: string
  /**
   * Custom icon
   * If `false` is provided, no icon will be shown.
   */
  readonly copyIcon?: string | false
  readonly errorIcon?: string
  readonly successIcon?: string
  readonly onCopy?: () => void
  /**
   * Show a toast message when the copy is successful.
   * If a string is provided, it will be used as the toast message.
   * If `true` is provided, a default toast message will be shown with the text "Copied to clipboard".
   * If `false` is provided, no toast message will be shown.
   */
  readonly successToastMessage?: boolean | string
}

/** A button that copies text to the clipboard. */
export function CopyButton(props: CopyButtonProps) {
  const {
    variant = 'icon',
    copyIcon = CopyIcon,
    successIcon = Done,
    errorIcon = Error,
    ...buttonProps
  } = props
  const { getText } = textProvider.useText()
  const copyQuery = copyHook.useCopy(props)
  const successfullyCopied = copyQuery.isSuccess
  const isError = copyQuery.isError
  const showIcon = copyIcon !== false
  const icon =
    showIcon ?
      isError ? errorIcon
      : successfullyCopied ? successIcon
      : copyIcon
    : null

  return (
    <button.Button
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
      variant={variant}
      aria-label={props['aria-label'] ?? getText('copyShortcut')}
      onPress={() => copyQuery.mutateAsync()}
      icon={icon}
    />
  )
}
