/** @file A button to close a dialog without submitting it. */
import type { JSX } from 'react'

import { useText } from '#/providers/TextProvider'
import { Button, type ButtonProps } from '../Button'
import { useDialogContext } from './DialogProvider'

/** Additional props for the Cancel component. */
interface DialogDismissBaseProps<IconType extends string> {
  readonly variant?: ButtonProps<IconType>['variant']
}

/** Props for a {@link DialogDismiss}. */
export type DialogDismissProps<IconType extends string> = DialogDismissBaseProps<IconType> &
  Omit<ButtonProps<IconType>, 'formnovalidate' | 'href' | 'variant'>

/**
 * Dismiss button for dialogs.
 * @deprecated Use {@link Close} instead.
 */
export function DialogDismiss<IconType extends string>(
  props: DialogDismissProps<IconType>,
): JSX.Element {
  const { getText } = useText()

  const { size = 'medium', ...buttonProps } = props

  const dialogContext = useDialogContext()

  return (
    <Button
      testId="form-cancel-button"
      formnovalidate
      type="button"
      variant="outline"
      size={size}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
      onPress={async (event) => {
        dialogContext?.close()
        await buttonProps.onPress?.(event)
      }}
    >
      {getText('cancel')}
    </Button>
  )
}
