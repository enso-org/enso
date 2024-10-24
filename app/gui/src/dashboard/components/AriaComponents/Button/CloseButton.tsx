/** @file A button for closing a modal. */
import DismissIcon from '#/assets/dismiss.svg'
import { Button, type ButtonProps } from '#/components/AriaComponents/Button'
import { useText } from '#/providers/TextProvider'
import { twMerge } from '#/utilities/tailwindMerge'
import { isOnMacOS } from 'enso-common/src/detect'

// ===================
// === CloseButton ===
// ===================

/** Props for a {@link CloseButton}. */
export type CloseButtonProps = Omit<ButtonProps, 'children' | 'rounding' | 'size' | 'variant'>

/** A styled button with a close icon that appears on hover. */
export function CloseButton(props: CloseButtonProps) {
  const { getText } = useText()
  const {
    className,
    icon = DismissIcon,
    tooltip = false,
    'aria-label': ariaLabel = getText('closeModalShortcut'),
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
          // @ts-expect-error ts fails to infer the type of the className prop
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
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    />
  )
}
