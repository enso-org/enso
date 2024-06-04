/**
 * @file
 *
 * Form error component.
 */

import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as reactAriaComponents from '#/components/AriaComponents'

import type * as types from './types'
import * as formContext from './useFormContext'

/**
 * Props for the FormError component.
 */
export interface FormErrorProps extends Omit<reactAriaComponents.AlertProps, 'children'> {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: types.FormInstance<any, any>
}

/**
 * Form error component.
 */
export function FormError(props: FormErrorProps) {
  const {
    form = formContext.useFormContext(),
    size = 'medium',
    variant = 'error',
    ...alertProps
  } = props

  const { formState } = form
  const { errors } = formState
  const { getText } = textProvider.useText()

  /**
   * Get the error message.
   */
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

  const errorMessage = getSubmitError()

  return errorMessage != null ? (
    <reactAriaComponents.Alert size={size} variant={variant} {...alertProps}>
      {errorMessage}
    </reactAriaComponents.Alert>
  ) : null
}
