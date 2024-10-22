/**
 * @file
 *
 * Reset button for forms.
 */
import * as React from 'react'

import * as ariaComponents from '#/components/AriaComponents'

import { useText } from '#/providers/TextProvider'
import * as formContext from './FormProvider'
import type * as types from './types'

/** Props for the Reset component. */
export interface ResetProps extends Omit<ariaComponents.ButtonProps, 'loading'> {
  /**
   * Connects the submit button to a form.
   * If not provided, the button will use the nearest form context.
   *
   * This field is helpful when you need to use the submit button outside of the form.
   */
  // We do not need to know the form fields.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: types.FormInstance<any>
  /** Defaults to `reset`. */
  readonly action?: 'cancel' | 'reset'
}

/** Reset button for forms. */
export function Reset(props: ResetProps): React.JSX.Element {
  const { getText } = useText()
  const {
    variant = 'outline',
    size = 'medium',
    testId = 'form-reset-button',
    action = 'reset',
    children = action === 'cancel' ? getText('cancel') : getText('reset'),
    ...buttonProps
  } = props

  const form = formContext.useFormContext(props.form)
  const { formState } = form

  return (
    <ariaComponents.Button
      onPress={() => {
        form.reset()
      }}
      /* This is safe because we are passing all props to the button */
      /* eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax */
      {...(buttonProps as any)}
      variant={variant}
      size={size}
      isDisabled={formState.isSubmitting || !formState.isDirty}
      testId={testId}
      children={children}
    />
  )
}
