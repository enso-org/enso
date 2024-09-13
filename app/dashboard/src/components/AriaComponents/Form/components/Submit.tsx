/**
 * @file
 *
 * Submit button for forms.
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import * as formContext from './FormProvider'
import type * as types from './types'

/**
 * Additional props for the Submit component.
 */
interface SubmitButtonBaseProps {
  readonly variant?: ariaComponents.ButtonProps['variant']
  /**
   * Connects the submit button to a form.
   * If not provided, the button will use the nearest form context.
   *
   * This field is helpful when you need to use the submit button outside of the form.
   */
  // We do not need to know the form fields.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: types.FormInstance<any>
  /**
   * Prop that allows to close the parent dialog without submitting the form.
   *
   * This looks tricky, but it's recommended by MDN as a receipt for closing the dialog without submitting the form.
   * @see https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dialog#closing_a_dialog_with_a_required_form_input
   */
  readonly formnovalidate?: boolean
}

/**
 * Props for the Submit component.
 */
export type SubmitProps = Omit<ariaComponents.ButtonProps, 'href' | 'variant'> &
  SubmitButtonBaseProps

/**
 * Submit button for forms.
 *
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
export function Submit(props: SubmitProps): React.JSX.Element {
  const { getText } = textProvider.useText()

  const {
    size = 'medium',
    formnovalidate = false,
    loading = false,
    children = formnovalidate ? getText('cancel') : getText('submit'),
    variant = formnovalidate ? 'ghost-fading' : 'submit',
    testId = formnovalidate ? 'form-cancel-button' : 'form-submit-button',
    ...buttonProps
  } = props

  const dialogContext = ariaComponents.useDialogContext()
  const form = formContext.useFormContext(props.form)
  const { formState } = form

  const isLoading = formnovalidate ? false : loading || formState.isSubmitting
  const type = formnovalidate || isLoading ? 'button' : 'submit'

  return (
    <ariaComponents.Button
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
      type={type}
      variant={variant}
      size={size}
      loading={isLoading}
      testId={testId}
      onPress={() => {
        if (formnovalidate) {
          dialogContext?.close()
        }
      }}
    >
      {children}
    </ariaComponents.Button>
  )
}
