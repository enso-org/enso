/**
 * @file
 *
 * A hook for creating a field and field state for a form.
 */
import * as reactHookForm from 'react-hook-form'

import * as formContext from './FormProvider'
import type * as types from './types'

/** Options for {@link useField} hook. */
export interface UseFieldOptions<
  BaseValueType,
  Schema extends types.TSchema,
  TFieldName extends types.FieldPath<Schema>,
> extends types.FormWithValueValidation<BaseValueType, Schema, TFieldName> {
  readonly name: TFieldName
  readonly isDisabled?: boolean | undefined
  readonly defaultValue?: types.FieldValues<Schema>[TFieldName] | undefined
}

/** A hook that connects a field to a form state. */
export function useField<
  BaseValueType,
  Schema extends types.TSchema,
  TFieldName extends types.FieldPath<Schema>,
>(options: UseFieldOptions<BaseValueType, Schema, TFieldName>) {
  const { name, defaultValue, isDisabled = false } = options

  const formInstance = formContext.useFormContext(options.form)

  const { field, fieldState, formState } = reactHookForm.useController({
    name,
    disabled: isDisabled,
    control: formInstance.control,
    ...(defaultValue != null ? { defaultValue } : {}),
  })

  return { field, fieldState, formState, formInstance } as const
}
