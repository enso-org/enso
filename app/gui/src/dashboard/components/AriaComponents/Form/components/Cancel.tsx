/**
 * @file Cancel button for forms.
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
import type { JSX } from 'react'

import { Form, useDialogContext, type ButtonProps } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type { FormInstance } from './types'

/**
 * Additional props for the Cancel component.
 */
interface CancelButtonBaseProps {
  readonly variant?: ButtonProps['variant']
  /**
   * Connects the submit button to a form.
   * If not provided, the button will use the nearest form context.
   *
   * This field is helpful when you need to use the submit button outside of the form.
   */
  // We do not need to know the form fields.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: FormInstance<any>
}

/**
 * Props for the Cancel component.
 */
export type CancelProps = CancelButtonBaseProps &
  Omit<ButtonProps, 'formnovalidate' | 'href' | 'variant'>

/**
 * Cancel button for forms.
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
export function Cancel(props: CancelProps): JSX.Element {
  const { getText } = useText()

  const { size = 'medium', ...buttonProps } = props

  const dialogContext = useDialogContext()

  return (
    <Form.Submit
      testId="form-cancel-button"
      formnovalidate
      type="button"
      variant="outline"
      size={size}
      onPress={() => {
        dialogContext?.close()
      }}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
    >
      {getText('cancel')}
    </Form.Submit>
  )
}
