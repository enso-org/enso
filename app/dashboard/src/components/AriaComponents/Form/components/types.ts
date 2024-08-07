/**
 * @file
 * Types for the Form component.
 */
import type * as React from 'react'

import type * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

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
 * Props for the useForm hook.
 */
export interface UseFormProps<Schema extends TSchema, TFieldValues extends FieldValues<Schema>>
  extends Omit<
    reactHookForm.UseFormProps<TFieldValues>,
    'handleSubmit' | 'resetOptions' | 'resolver'
  > {
  readonly schema: Schema | ((schema: typeof schemaModule.schema) => Schema)
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
  readonly isDisabled?: boolean
  readonly isRequired?: boolean
  readonly isInvalid?: boolean
}

/**
 * Return type of the useForm hook.
 * @alias reactHookForm.UseFormReturn
 */
export interface UseFormReturn<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TTransformedValues extends Record<string, unknown> | undefined = undefined,
> extends reactHookForm.UseFormReturn<TFieldValues, unknown, TTransformedValues> {
  readonly register: UseFormRegister<Schema, TFieldValues>
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
  TTransformedValues extends Record<string, unknown> | undefined = undefined,
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
