/**
 * @file
 *
 * Hook to get the state of a field.
 */
import { useFormState } from 'react-hook-form'
import { useFormContext } from './FormProvider'
import type { FieldPath, FormInstanceValidated, TSchema } from './types'

/** Options for the `useFieldState` hook. */
export interface UseFieldStateOptions<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
> {
  readonly name: TFieldName
  readonly form?: FormInstanceValidated<Schema> | undefined
}

/** Hook to get the state of a field. */
export function useFieldState<Schema extends TSchema, TFieldName extends FieldPath<Schema>>(
  options: UseFieldStateOptions<Schema, TFieldName>,
) {
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
