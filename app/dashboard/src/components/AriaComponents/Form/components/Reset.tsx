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
   * Connects the reset button to a form.
   * If not provided, the button will use the nearest form context.
   *
   * This field is helpful when you need to use the reset button outside of a form.
   */
  // We do not need to know the form fields.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: types.FormInstance<any>
}

/**
 * Reset button for forms.
 */
export function Reset(props: ResetProps): React.JSX.Element {
  const {
    form = formContext.useFormContext(),
    variant = 'outline',
    size = 'medium',
    testId = 'form-reset-button',
    ...buttonProps
  } = props
  const { formState } = form

  return (
    <ariaComponents.Button
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
      type="reset"
      variant={variant}
      size={size}
      isDisabled={formState.isSubmitting || !formState.isDirty}
      testId={testId}
    />
  )
}
