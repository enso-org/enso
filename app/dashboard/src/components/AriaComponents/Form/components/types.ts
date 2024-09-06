/**
 * @file
 * Types for the Form component.
 */
import type * as React from 'react'

import type * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

import type * as schemaModule from './schema'

/** The type of the inputs to the form, used for UI inputs. */
export type FieldValues<Schema extends TSchema | undefined> =
  Schema extends TSchema ? z.input<Schema> : reactHookForm.FieldValues

/** The type of the outputs of the form, used for the callback. */
export type TransformedValues<Schema extends TSchema | undefined> =
  Schema extends TSchema ? z.output<Schema> : reactHookForm.FieldValues

/**
 * Field path type.
 * @alias reactHookForm.FieldPath
 */
export type FieldPath<Schema extends TSchema> = reactHookForm.FieldPath<FieldValues<Schema>>

/**
 * Schema type
 */
export type TSchema =
  | z.AnyZodObject
  | z.ZodEffects<z.AnyZodObject>
  | z.ZodEffects<z.ZodEffects<z.AnyZodObject>>

/**
 * Props for the useForm hook.
 */
export interface UseFormProps<Schema extends TSchema>
  extends Omit<
    reactHookForm.UseFormProps<FieldValues<Schema>>,
    'handleSubmit' | 'resetOptions' | 'resolver'
  > {
  readonly schema: Schema | ((schema: typeof schemaModule.schema) => Schema)
}

/**
 * Register function for a form field.
 */
export type UseFormRegister<Schema extends TSchema> = <
  TFieldName extends FieldPath<Schema> = FieldPath<Schema>,
>(
  name: TFieldName,
  options?: reactHookForm.RegisterOptions<FieldValues<Schema>, TFieldName>,
  // eslint-disable-next-line no-restricted-syntax
) => UseFormRegisterReturn<Schema, TFieldName>

/**
 * UseFormRegister return type.
 */
export interface UseFormRegisterReturn<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema> = FieldPath<Schema>,
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
export interface UseFormReturn<Schema extends TSchema>
  extends reactHookForm.UseFormReturn<FieldValues<Schema>, unknown, TransformedValues<Schema>> {
  readonly register: UseFormRegister<Schema>
}

/**
 * Form state type.
 * @alias reactHookForm.FormState
 */
export type FormState<Schema extends TSchema> = reactHookForm.FormState<FieldValues<Schema>>

/**
 * Form instance type
 * @alias UseFormReturn
 */
export type FormInstance<Schema extends TSchema> = UseFormReturn<Schema>

/**
 * Form type interface that check if FieldValues type is compatible with the value type from component
 */
export interface FormWithValueValidation<
  BaseValueType,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
  // It is not ideal to have this as a parameter as it can be edited, but this is the simplest way
  // to avoid distributive conditional types to affect the error message. We want distributivity
  // to happen, just not for the error message itself.
  ErrorType = [
    'Type mismatch: Expected',
    FieldValues<Schema>[TFieldName],
    'got',
    BaseValueType,
    'instead.',
  ],
> {
  readonly form?:
    | (BaseValueType extends FieldValues<Schema>[TFieldName] ? FormInstance<Schema> : ErrorType)
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
