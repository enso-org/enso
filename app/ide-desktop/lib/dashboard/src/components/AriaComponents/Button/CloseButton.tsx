/** @file A button for closing a modal. */

import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import Dismiss from '#/assets/dismiss.svg'

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
      // @ts-expect-error ts fails to infer the type of the className prop
      className={values =>
        tailwindMerge.twMerge(
          'h-3 w-3 bg-primary/30 hover:bg-red-500/80 focus-visible:bg-red-500/80 focus-visible:outline-offset-1',
          // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
          typeof className === 'function' ? className(values) : className
        )
      }
      tooltip={tooltip}
      showIconOnHover
      size="custom"
      rounded="full"
      icon={icon}
      aria-label={ariaLabel}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    />
  )
}
