/**
 * @file
 *
 * A hook for creating a field and field state for a form.
 */
import { useController } from 'react-hook-form'

import { useFormContext } from './FormProvider'
import type { FieldPath, FieldValues, FormWithValueValidation, TSchema } from './types'

/**
 * Options for {@link useField} hook.
 */
export interface UseFieldOptions<
  BaseValueType,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
> extends FormWithValueValidation<BaseValueType, Schema, TFieldName, Constraint> {
  readonly name: TFieldName
  readonly isDisabled?: boolean | undefined
  readonly defaultValue?: FieldValues<Schema>[TFieldName] | undefined
}

/**
 * A hook that connects a field to a form state.
 */
export function useField<
  BaseValueType,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, Constraint>,
  Constraint,
>(options: UseFieldOptions<BaseValueType, Schema, TFieldName, Constraint>) {
  const { name, defaultValue, isDisabled = false } = options

  const formInstance = useFormContext(options.form)

  const { field, fieldState, formState } = useController({
    name,
    disabled: isDisabled,
    control: formInstance.control,
    ...(defaultValue != null ? { defaultValue } : {}),
  })

  return { field, fieldState, formState, formInstance } as const
}

/**
 * A hook that connects a field to a form state.
 */
export function makeUseField<Constraint>() {
  return function useFieldWithConstraint<
    BaseValueType,
    Schema extends TSchema,
    TFieldName extends FieldPath<Schema, Constraint>,
  >(options: UseFieldOptions<BaseValueType, Schema, TFieldName, Constraint>) {
    return useField(options)
  }
}
