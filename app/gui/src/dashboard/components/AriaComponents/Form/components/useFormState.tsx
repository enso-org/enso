/**
 * @file
 *
 * A hook for subscribing to the state of a form.
 */
import * as reactHookForm from 'react-hook-form'
import { useFormContext } from './FormProvider'
import type { FieldPath, FormWithValueValidation, TSchema } from './types'

/** Options for {@link useFormState} hook. */
export interface UseFormStateOptions<
  BaseValueType,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> extends FormWithValueValidation<BaseValueType, Schema, TFieldName, Constraint> {
  /** Whether to subscribe to the state of the form. */
  readonly isDisabled?: boolean | undefined
}

/** A hook that subscribes to the state of a form. */
export function useFormState<
  BaseValueType,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
>(options: UseFormStateOptions<BaseValueType, Schema, TFieldName, Constraint>) {
  const { isDisabled = false } = options
  const form = useFormContext(options.form)

  return reactHookForm.useFormState({
    control: form.control,
    disabled: isDisabled,
  })
}
