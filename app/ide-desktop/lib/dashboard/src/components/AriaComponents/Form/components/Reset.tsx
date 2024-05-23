/**
 * @file
 *
 * Reset button for forms.
 */
import * as React from 'react'

import * as reactHookForm from 'react-hook-form'

import * as ariaComponents from '#/components/AriaComponents'

/**
 * Props for the Reset component.
 */
export interface ResetProps extends Omit<ariaComponents.ButtonProps, 'loading'> {
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
 * Reset button for forms.
 */
export function Reset(props: ResetProps): React.JSX.Element {
  const { form = reactHookForm.useFormContext(), variant = 'cancel', size = 'medium' } = props
  const { formState } = form

  return (
    <ariaComponents.Button
      {...props}
      variant={variant}
      size={size}
      isDisabled={formState.isSubmitting}
      onPress={form.reset}
    />
  )
}
