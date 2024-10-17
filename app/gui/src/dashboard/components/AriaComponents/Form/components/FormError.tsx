/**
 * @file
 *
 * Form error component.
 */

import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as reactAriaComponents from '#/components/AriaComponents'

import * as formContext from './FormProvider'
import type * as types from './types'

/** Props for the FormError component. */
export interface FormErrorProps extends Omit<reactAriaComponents.AlertProps, 'children'> {
  // We do not need to know the form fields.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: types.FormInstance<any>
}

/** Form error component. */
export function FormError(props: FormErrorProps) {
  const { size = 'large', variant = 'error', rounded = 'large', ...alertProps } = props

  const form = formContext.useFormContext(props.form)
  const { formState } = form
  const { errors } = formState
  const { getText } = textProvider.useText()

  /** Get the error message. */
  const getSubmitError = (): string | null => {
    const formErrors = errors.root

    if (formErrors) {
      const submitError = formErrors.submit

      if (submitError) {
        return (
          submitError.message ??
          getText('arbitraryErrorTitle') + '. ' + getText('arbitraryErrorSubtitle')
        )
      } else {
        return null
      }
    } else {
      return null
    }
  }

  const offlineMessage = errors.root?.offline?.message ?? null
  const errorMessage = getSubmitError()

  const submitErrorAlert =
    errorMessage != null ?
      <reactAriaComponents.Alert size={size} variant={variant} rounded={rounded} {...alertProps}>
        <reactAriaComponents.Text
          data-testid="form-submit-error"
          variant="body"
          truncate="3"
          color="primary"
        >
          {errorMessage}
        </reactAriaComponents.Text>
      </reactAriaComponents.Alert>
    : null

  const offlineErrorAlert =
    offlineMessage != null ?
      <reactAriaComponents.Alert size={size} variant="outline" rounded={rounded} {...alertProps}>
        <reactAriaComponents.Text variant="body" truncate="3" color="primary">
          {offlineMessage}
        </reactAriaComponents.Text>
      </reactAriaComponents.Alert>
    : null

  const hasSomethingToShow = submitErrorAlert || offlineErrorAlert

  return hasSomethingToShow ?
      <div className="flex w-full flex-col gap-4">
        {submitErrorAlert} {offlineErrorAlert}
      </div>
    : null
}
