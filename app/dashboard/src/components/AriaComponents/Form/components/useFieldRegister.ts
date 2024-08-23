/**
 * @file
 *
 * Form field registration hook.
 * Use this hook to register a field in the form.
 */
import { useFormContext } from './FormProvider'
import type { FieldPath, FieldStateProps, FieldValues, FormInstance, TSchema } from './types'
import { useField } from './useField'

/**
 * Options for the useFieldRegister hook.
 */
export type UseFieldRegisterOptions<
  BaseValueType extends { value?: unknown },
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> = FieldStateProps<BaseValueType, Schema, TFieldValues, TFieldName, TTransformedValues> & {
  name: TFieldName
  defaultValue?: TFieldValues[TFieldName] | undefined
  min?: number | string | undefined
  max?: number | string | undefined
  minLength?: number | undefined
  maxLength?: number | undefined
  setValueAs?: (value: unknown) => unknown
}

/**
 * Registers a field in the form.
 */
export function useFieldRegister<
  BaseValueType extends { value?: unknown },
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  options: UseFieldRegisterOptions<
    BaseValueType,
    Schema,
    TFieldValues,
    TFieldName,
    TTransformedValues
  >,
) {
  const { name, min, max, minLength, maxLength, isRequired, isDisabled, form, setValueAs } = options

  // eslint-disable-next-line no-restricted-syntax
  const formInstance = (form ??
    // eslint-disable-next-line react-hooks/rules-of-hooks
    useFormContext<Schema, TFieldValues, TTransformedValues>()) as FormInstance<
    Schema,
    TFieldValues,
    TTransformedValues
  >

  const extractedValidationDetails = unsafe__extractValidationDetailsFromSchema<
    Schema,
    TFieldValues,
    TFieldName
  >(formInstance.schema, name)

  const fieldProps = formInstance.register(name, {
    disabled: isDisabled ?? false,
    required: isRequired ?? extractedValidationDetails?.required ?? false,
    ...(setValueAs != null ? { setValueAs } : {}),
    ...(extractedValidationDetails?.min != null ? { min: extractedValidationDetails.min } : {}),
    ...(extractedValidationDetails?.max != null ? { min: extractedValidationDetails.max } : {}),
    ...(min != null ? { min } : {}),
    ...(max != null ? { max } : {}),
    ...(minLength != null ? { minLength } : {}),
    ...(maxLength != null ? { maxLength } : {}),
  })

  return { fieldProps, formInstance } as const
}
/**
 * Tried to extract validation details from the schema.
 */
// This name is intentional to highlight that this function is unsafe and should be used with caution.
// eslint-disable-next-line @typescript-eslint/naming-convention
function unsafe__extractValidationDetailsFromSchema<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
>(schema: Schema, name: TFieldName) {
  console.log('schema', schema)
  try {
    if ('shape' in schema) {
      if (name in schema.shape) {
        // THIS is 100% unsafe, so we need to be very careful here
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-assignment
        const fieldShape = schema.shape[name]
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access,@typescript-eslint/no-unsafe-assignment
        const min: number | null = fieldShape.minLength
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access,@typescript-eslint/no-unsafe-assignment
        const max: number | null = fieldShape.maxLength
        const required = min != null && min > 0

        // eslint-disable-next-line no-restricted-syntax
        return { required, min, max } as const
      }

      // eslint-disable-next-line no-restricted-syntax
      return null
    }
  } catch {
    // eslint-disable-next-line no-restricted-syntax
    return null
  }
}
