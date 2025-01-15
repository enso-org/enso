/**
 * @file
 *
 * Form field registration hook.
 * Use this hook to register a field in the form.
 */
import { useFormContext } from './FormProvider'
import type {
  FieldPath,
  FieldValues,
  FormFieldProps,
  FormInstanceValidated,
  TSchema,
} from './types'

/** Options for the useFieldRegister hook. */
export type UseFieldRegisterOptions<
  BaseValueType extends { value?: unknown },
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
> = Omit<FormFieldProps<BaseValueType, Schema, TFieldName>, 'form'> & {
  name: TFieldName
  form?: FormInstanceValidated<Schema> | undefined
  defaultValue?: FieldValues<Schema>[TFieldName] | undefined
  min?: number | string | undefined
  max?: number | string | undefined
  minLength?: number | undefined
  maxLength?: number | undefined
  setValueAs?: ((value: unknown) => unknown) | undefined
}

/** Registers a field in the form. */
export function useFieldRegister<
  BaseValueType extends { value?: unknown },
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(options: UseFieldRegisterOptions<BaseValueType, Schema, TFieldName>) {
  const { name, min, max, minLength, maxLength, isRequired, isDisabled, form, setValueAs } = options

  const formInstance = useFormContext(form)

  const extractedValidationDetails = unsafe__extractValidationDetailsFromSchema<Schema, TFieldName>(
    formInstance.schema,
    name,
  )

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
/** Tried to extract validation details from the schema. */
// This name is intentional to highlight that this function is unsafe and should be used with caution.
// eslint-disable-next-line camelcase, @typescript-eslint/naming-convention
function unsafe__extractValidationDetailsFromSchema<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(schema: Schema, name: TFieldName) {
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

        return { required, min, max } as const
      }

      return null
    }
    return null
  } catch {
    return null
  }
}
