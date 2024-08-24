/**
 * @file
 *
 * Hook to get the state of a field.
 */
import { useFormState } from 'react-hook-form'
import { useFormContext } from './FormProvider'
import type { FieldPath, FieldValues, FormWithValueValidation, TSchema } from './types'

/**
 * Options for the `useFieldState` hook.
 */
export interface UseFieldStateOptions<
  BaseValueType,
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends FormWithValueValidation<
    BaseValueType,
    Schema,
    TFieldValues,
    TFieldName,
    TTransformedValues
  > {
  readonly name: TFieldName
}

/**
 * Hook to get the state of a field.
 */
export function useFieldState<
  BaseValueType,
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  options: UseFieldStateOptions<
    BaseValueType,
    Schema,
    TFieldValues,
    TFieldName,
    TTransformedValues
  >,
) {
  // eslint-disable-next-line react-hooks/rules-of-hooks
  const { name } = options

  const form = useFormContext(options.form)

  const { errors, dirtyFields, isValidating, touchedFields } = useFormState({
    control: form.control,
    name,
  })

  const isDirty = name in dirtyFields
  const isTouched = name in touchedFields
  const error = errors[name]?.message?.toString()

  return {
    error,
    isDirty,
    isTouched,
    isValidating,
    hasError: error != null,
  } as const
}
