/**
 * @file
 *
 * Submit button for forms.
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
import type { JSX } from 'react'

import { Button, useDialogContext, type ButtonProps } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { useFormContext } from './FormProvider'
import type { FormInstance } from './types'

/** Additional props for the Submit component. */
interface SubmitButtonBaseProps {
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
  /** Defaults to `submit`. */
  readonly action?: 'cancel' | 'submit' | 'update'
}

/** Props for the Submit component. */
export type SubmitProps = Omit<ButtonProps, 'formnovalidate' | 'href' | 'variant'> &
  SubmitButtonBaseProps

/**
 * Submit button for forms.
 *
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
export function Submit(props: SubmitProps): JSX.Element {
  const { getText } = useText()

  const {
    size = 'medium',
    action = 'submit',
    loading = false,
    children = action === 'cancel' ? getText('cancel')
    : action === 'update' ? getText('update')
    : getText('submit'),
    variant = action === 'cancel' ? 'outline' : 'submit',
    testId = action === 'cancel' ? 'form-cancel-button' : 'form-submit-button',
    ...buttonProps
  } = props

  const dialogContext = useDialogContext()
  const form = useFormContext(props.form)
  const { formState } = form

  const isLoading = action === 'cancel' ? false : loading || formState.isSubmitting
  const type = action === 'cancel' || isLoading ? 'button' : 'submit'

  return (
    <Button
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
      type={type}
      variant={variant}
      size={size}
      loading={isLoading}
      testId={testId}
      onPress={() => {
        if (action === 'cancel') {
          dialogContext?.close()
        }
      }}
    >
      {children}
    </Button>
  )
}
