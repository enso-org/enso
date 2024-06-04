/**
 * @file
 *
 * Reset button for forms.
 */
import * as React from 'react'

import * as ariaComponents from '#/components/AriaComponents'

import type * as types from './types'
import * as formContext from './useFormContext'

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
  readonly form?: types.FormInstance<any, any>
}

/**
 * Reset button for forms.
 */
export function Reset(props: ResetProps): React.JSX.Element {
  const {
    form = formContext.useFormContext(),
    variant = 'cancel',
    size = 'medium',
    testId = 'form-reset-button',
  } = props
  const { formState } = form

  return (
    <ariaComponents.Button
      {...props}
      type="reset"
      variant={variant}
      size={size}
      isDisabled={formState.isSubmitting || !formState.isDirty}
      onPress={form.reset}
      testId={testId}
    />
  )
}
