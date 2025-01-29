/** @file A button for closing a modal. */
import DismissIcon from '#/assets/dismiss.svg'
import { useText } from '#/providers/TextProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { isOnMacOS } from 'enso-common/src/detect'
import { memo } from 'react'
import { Button } from './Button'
import type { ButtonProps } from './types'

/** Props for a {@link CloseButton}. */
export type CloseButtonProps = Omit<ButtonProps, 'children' | 'rounding' | 'size' | 'variant'>

/** A styled button with a close icon that appears on hover. */
export const CloseButton = memo(function CloseButton(props: CloseButtonProps) {
  const { getText } = useText()

  const {
    className,
    icon = DismissIcon,
    tooltip = false,
    'aria-label': ariaLabel = getText('closeModalShortcut'),
    testId = 'close-button',
    ...buttonProps
  } = props

  return (
    <Button
      variant="icon"
      className={(values) =>
        twMerge(
          'hover:bg-red-500/80 focus-visible:bg-red-500/80 focus-visible:outline-offset-1',
          isOnMacOS() ? 'bg-primary/30' : (
            'text-primary/90 hover:text-primary focus-visible:text-primary'
          ),
          // @ts-expect-error TypeScript fails to infer the type of the `className` prop
          // But it's safe because we are passing all values transparently
          // and they are typed outside.
          typeof className === 'function' ? className(values) : className,
        )
      }
      tooltip={tooltip}
      showIconOnHover={isOnMacOS()}
      size="xsmall"
      rounded="full"
      extraClickZone="medium"
      icon={icon}
      aria-label={ariaLabel}
      testId={testId}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    />
  )
})
