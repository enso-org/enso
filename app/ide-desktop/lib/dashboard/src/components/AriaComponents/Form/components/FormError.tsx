/** @file Form error component. */
import * as React from 'react'

import * as reactHookForm from 'react-hook-form'

import * as textProvider from '#/providers/TextProvider'

import * as reactAriaComponents from '#/components/AriaComponents'
import type * as types from '#/components/AriaComponents/Form/types'

// =================
// === FormError ===
// =================

/** Props for a {@link FormError}. */
export interface FormErrorProps<
  TFieldValues extends types.FieldValues,
  TTransformedFieldValues extends types.FieldValues,
> extends Omit<reactAriaComponents.AlertProps, 'children'> {
  readonly form?: reactHookForm.UseFormReturn<TFieldValues, unknown, TTransformedFieldValues>
}

/** Form error component. */
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
  const errorMessage = errors.root?.submit
    ? errors.root.submit.message ??
      getText('arbitraryErrorTitle') + '. ' + getText('arbitraryErrorSubtitle')
    : null

  return errorMessage == null ? null : (
    <reactAriaComponents.Alert size={size} variant={variant} {...alertProps}>
      {errorMessage}
    </reactAriaComponents.Alert>
  )
}
