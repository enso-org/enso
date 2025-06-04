/** @file A button that copies text to the clipboard. */
import Error from '#/assets/cross.svg'
import Done from '#/assets/tick.svg'
import type { SvgUseIcon } from '#/components/types'
import { useCopy } from '#/hooks/copyHooks'
import { useText } from '$/providers/react'
import { Button } from './Button'
import type { ButtonProps } from './types'

/** Props for a {@link CopyButton}. */
export interface CopyButtonProps<IconType extends string>
  extends Omit<ButtonProps<IconType>, 'icon' | 'loading' | 'onPress'> {
  /** The text to copy to the clipboard. */
  readonly copyText: string
  /**
   * Custom icon
   * If `false` is provided, no icon will be shown.
   */
  readonly copyIcon?: SvgUseIcon | false | (string & {})
  readonly errorIcon?: SvgUseIcon | (string & {})
  readonly successIcon?: SvgUseIcon | (string & {})
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
export function CopyButton<IconType extends string>(props: CopyButtonProps<IconType>) {
  const {
    variant = 'icon',
    copyIcon = 'duplicate',
    successIcon = Done,
    errorIcon = Error,
    copyText,
    onCopy,
    ...buttonProps
  } = props
  const { getText } = useText()
  const copyQuery = useCopy({ onCopy })
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
    <Button
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
      variant={variant}
      aria-label={props['aria-label'] ?? getText('copyShortcut')}
      onPress={() => copyQuery.mutateAsync(copyText)}
      icon={icon}
    />
  )
}
