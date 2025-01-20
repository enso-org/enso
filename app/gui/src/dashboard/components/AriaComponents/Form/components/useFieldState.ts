/**
 * @file
 *
 * Hook to get the state of a field.
 */
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
  const { error, isDirty, isTouched, isValidating } = form.getFieldState(name)

  return {
    error: error?.message?.toString(),
    isDirty,
    isTouched,
    isValidating,
    hasError: error != null,
  } as const
}
