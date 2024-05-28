/** @file Submit button for a form.
 * Manages the form state and displays a loading spinner when the form is submitting. */
import * as React from 'react'

import * as reactHookForm from 'react-hook-form'

import * as ariaComponents from '#/components/AriaComponents'

// ==============
// === Submit ===
// ==============

/** Props for a {@link Submit}. */
export interface SubmitProps extends Omit<ariaComponents.ButtonProps, 'loading' | 'variant'> {
  readonly variant?: ariaComponents.ButtonProps['variant']
  /** Connects the submit button to a form.
   * If not provided, the button will use the nearest form context.
   *
   * This field is helpful when you need to use the submit button outside of the form. */
  readonly form?: reactHookForm.UseFormReturn<reactHookForm.FieldValues>
}

/** Submit button for a form.
 *
 * Manages the form state and displays a loading spinner when the form is submitting. */
export function Submit(props: SubmitProps): React.JSX.Element {
  const {
    form = reactHookForm.useFormContext(),
    variant = 'submit',
    size = 'medium',
    testId = 'form-submit-button',
  } = props
  const { formState } = form

  return (
    <ariaComponents.Button
      {...props}
      type="submit"
      variant={variant}
      size={size}
      loading={formState.isSubmitting}
      testId={testId}
    />
  )
}
