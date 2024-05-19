/**
 * @file
 *
 * Form error component.
 */

import * as React from 'react'

import * as reactHookForm from 'react-hook-form'

import * as textProvider from '#/providers/TextProvider'

import * as reactAriaComponents from '#/components/AriaComponents'

import type * as types from '../types'

/**
 * Props for the FormError component.
 */
export interface FormErrorProps<
  TFieldValues extends types.FieldValues,
  TTransformedFieldValues extends types.FieldValues,
> extends Omit<reactAriaComponents.AlertProps, 'children'> {
  readonly form?: reactHookForm.UseFormReturn<TFieldValues, unknown, TTransformedFieldValues>
}

/**
 * Form error component.
 */
export function FormError<
  TFieldValues extends types.FieldValues,
  TTransformedFieldValues extends types.FieldValues,
>(props: FormErrorProps<TFieldValues, TTransformedFieldValues>) {
  const {
    form = reactHookForm.useFormContext(),
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
  const getErrorMessage = (): string | null => {
    const formErrors = errors.root

    if (formErrors) {
      const submitError = formErrors.submitError

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

  const errorMessage = getErrorMessage()

  return errorMessage != null ? (
    <reactAriaComponents.Alert size={size} variant={variant} {...alertProps} />
  ) : null
}
