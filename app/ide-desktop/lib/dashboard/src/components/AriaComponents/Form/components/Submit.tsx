/**
 * @file
 *
 * Submit button for forms.
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
import * as React from 'react'

import * as reactHookForm from 'react-hook-form'

import * as ariaComponents from '#/components/AriaComponents'

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
  // For this component, we don't need to know the form fields
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: reactHookForm.UseFormReturn<any>
}

/**
 * Props for the Submit component.
 */
export type SubmitProps = Omit<ariaComponents.ButtonProps, 'loading' | 'variant'> &
  SubmitButtonBaseProps

/**
 * Submit button for forms.
 *
 * Manages the form state and displays a loading spinner when the form is submitting.
 */
export function Submit(props: SubmitProps): React.JSX.Element {
  const { form = reactHookForm.useFormContext(), variant = 'submit', size = 'medium' } = props
  const { formState } = form

  return (
    <ariaComponents.Button
      {...props}
      type="submit"
      variant={variant}
      size={size}
      loading={formState.isSubmitting}
    />
  )
}
