/** @file A button for closing a modal. */

import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import Dismiss from 'enso-assets/dismiss.svg'

import * as textProvider from '#/providers/TextProvider'

import * as button from '#/components/AriaComponents/Button'

// ===================
// === CloseButton ===
// ===================

/** Props for a {@link CloseButton}. */
export type CloseButtonProps = Omit<
  button.ButtonProps,
  'children' | 'rounding' | 'size' | 'variant'
>

/** A styled button with a close icon that appears on hover. */
export function CloseButton(props: CloseButtonProps) {
  const { getText } = textProvider.useText()
  const {
    className,
    icon = Dismiss,
    tooltip = false,
    'aria-label': ariaLabel = getText('closeModalShortcut'),
    ...buttonProps
  } = props

  return (
    <button.Button
      variant="icon"
      className={values =>
        tailwindMerge.twMerge(
          'h-3 w-3 bg-primary/30 hover:bg-red-500/80 focus-visible:bg-red-500/80 focus-visible:outline-offset-1',
          // @ts-expect-error className can be a function or a string
          typeof className === 'function' ? className(values) : className
        )
      }
      tooltip={tooltip}
      showIconOnHover
      size="custom"
      rounded="full"
      icon={icon}
      aria-label={ariaLabel}
      {...buttonProps}
    />
  )
}
