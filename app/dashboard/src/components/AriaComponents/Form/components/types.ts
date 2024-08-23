/**
 * @file
 * Types for the Form component.
 */
import type * as React from 'react'

import type * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

import type { FormEvent } from 'react'
import type * as schemaModule from './schema'

/**
 * Field values type.
 */
// eslint-disable-next-line no-restricted-syntax
export type FieldValues<Schema extends TSchema | undefined> =
  Schema extends TSchema ? z.infer<Schema> : reactHookForm.FieldValues

/**
 * Field path type.
 * @alias reactHookForm.FieldPath
 */
export type FieldPath<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
> = reactHookForm.FieldPath<TFieldValues>

/**
 * Schema type
 */
export type TSchema = z.AnyZodObject | z.ZodEffects<z.AnyZodObject>

/**
 *
 */
export interface OnSubmitCallbacks<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
  SubmitResult = void,
> {
  readonly onSubmit?:
    | ((
        values: TFieldValues,
        form: UseFormReturn<Schema, TFieldValues, TTransformedValues>,
      ) => Promise<SubmitResult> | SubmitResult)
    | undefined

  readonly onSubmitFailed?:
    | ((
        error: unknown,
        values: TFieldValues,
        form: UseFormReturn<Schema, TFieldValues, TTransformedValues>,
      ) => Promise<void> | void)
    | undefined
  readonly onSubmitSuccess?:
    | ((
        data: SubmitResult,
        values: TFieldValues,
        form: UseFormReturn<Schema, TFieldValues, TTransformedValues>,
      ) => Promise<void> | void)
    | undefined
  readonly onSubmitted?:
    | ((
        data: SubmitResult,
        error: unknown,
        values: TFieldValues,
        form: UseFormReturn<Schema, TFieldValues, TTransformedValues>,
      ) => Promise<void> | void)
    | undefined
}

/**
 * Props for the useForm hook.
 */
export interface UseFormProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
  SubmitResult = void,
> extends Omit<
      reactHookForm.UseFormProps<TFieldValues>,
      'handleSubmit' | 'resetOptions' | 'resolver'
    >,
    OnSubmitCallbacks<Schema, TFieldValues, TTransformedValues, SubmitResult> {
  readonly schema: Schema | ((schema: typeof schemaModule.schema) => Schema)
  /**
   * Whether the form can submit offline.
   * @default false
   */
  readonly canSubmitOffline?: boolean

  /**
   * Debug name for the form. Use it to identify the form in the tanstack query devtools.
   */
  readonly debugName?: string
}

/**
 * Register function for a form field.
 */
export type UseFormRegister<Schema extends TSchema, TFieldValues extends FieldValues<Schema>> = <
  TFieldName extends FieldPath<Schema, TFieldValues> = FieldPath<Schema, TFieldValues>,
>(
  name: TFieldName,
  options?: reactHookForm.RegisterOptions<TFieldValues, TFieldName>,
  // eslint-disable-next-line no-restricted-syntax
) => UseFormRegisterReturn<Schema, TFieldValues, TFieldName>

/**
 * UseFormRegister return type.
 */
export interface UseFormRegisterReturn<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues> = FieldPath<Schema, TFieldValues>,
> extends Omit<reactHookForm.UseFormRegisterReturn<TFieldName>, 'onBlur' | 'onChange'> {
  // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
  readonly onChange: <Value>(value: Value) => Promise<boolean | void>
  // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
  readonly onBlur: <Value>(value: Value) => Promise<boolean | void>
  readonly isDisabled: boolean
  readonly isRequired: boolean
  readonly isInvalid: boolean
  readonly disabled: boolean
  readonly required: boolean
  readonly invalid: boolean
}

/**
 * Return type of the useForm hook.
 * @alias reactHookForm.UseFormReturn
 */
export interface UseFormReturn<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends Omit<
    reactHookForm.UseFormReturn<TFieldValues, unknown, TTransformedValues>,
    'handleSubmit' | 'resetOptions' | 'resolver'
  > {
  readonly register: UseFormRegister<Schema, TFieldValues>
  readonly submit: (event?: FormEvent<HTMLFormElement> | null | undefined) => Promise<void>
  readonly schema: Schema
  readonly setFormError: (error: string) => void
}

/**
 * Form state type.
 * @alias reactHookForm.FormState
 */
export type FormState<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
> = reactHookForm.FormState<TFieldValues>

/**
 * Form instance type
 * @alias UseFormReturn
 */
export type FormInstance<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema> = FieldValues<Schema>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> = UseFormReturn<Schema, TFieldValues, TTransformedValues>

/**
 * Form type interface that check if FieldValues type is compatible with the value type from component
 */
export interface FormWithValueValidation<
  BaseValueType,
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
  ErrorType = [
    'Type mismatch: Expected',
    TFieldValues[TFieldName],
    'got',
    BaseValueType,
    'instead.',
  ],
> {
  readonly form?:
    | (BaseValueType extends TFieldValues[TFieldName] ?
        FormInstance<Schema, TFieldValues, TTransformedValues>
      : ErrorType)
    | undefined
}

/**
 * Props for the Field component.
 */
// Readonly omitted here to avoid type mismatch with native HTML attributes
// eslint-disable-next-line no-restricted-syntax
export interface FieldProps {
  readonly isRequired?: boolean | undefined
  readonly label?: React.ReactNode | undefined
  readonly description?: React.ReactNode | undefined
  readonly error?: React.ReactNode | undefined

  /**
   * Defines a string value that labels the current element.
   */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-label'?: string | undefined

  /**
   * Identifies the element (or elements) that labels the current element.
   */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-labelledby'?: string | undefined

  /**
   * Identifies the element (or elements) that describes the object.
   */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-describedby'?: string | undefined

  /**
   * Identifies the element (or elements) that provide a detailed, extended description for the object.
   */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-details'?: string | undefined
}
/**
 * Base Props for a Form Field.
 * @private
 */
interface FormFieldProps<
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
  readonly value?: BaseValueType extends TFieldValues[TFieldName] ? TFieldValues[TFieldName] : never
  readonly defaultValue?: TFieldValues[TFieldName] | undefined
  readonly isDisabled?: boolean | undefined
  readonly isRequired?: boolean | undefined
  readonly isInvalid?: boolean | undefined
}

/**
 * Field State Props
 */
export type FieldStateProps<
  // eslint-disable-next-line no-restricted-syntax
  BaseProps extends { value?: unknown },
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> = FormFieldProps<BaseProps['value'], Schema, TFieldValues, TFieldName, TTransformedValues> & {
  // to avoid conflicts with the FormFieldProps we need to omit the FormFieldProps from the BaseProps
  [K in keyof Omit<
    BaseProps,
    keyof FormFieldProps<BaseProps['value'], Schema, TFieldValues, TFieldName, TTransformedValues>
  >]: BaseProps[K]
}
