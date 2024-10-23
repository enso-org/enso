/**
 * @file
 * Types for the Form component.
 */
import type * as React from 'react'

import type * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

import type { FormEvent } from 'react'
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

/** Schema type */
export type TSchema =
  | z.AnyZodObject
  | z.ZodEffects<z.AnyZodObject>
  | z.ZodEffects<z.ZodEffects<z.AnyZodObject>>

/** OnSubmitCallbacks type. */
export interface OnSubmitCallbacks<Schema extends TSchema, SubmitResult = void> {
  readonly onSubmit?:
    | ((
        values: FieldValues<Schema>,
        form: UseFormReturn<Schema>,
      ) => Promise<SubmitResult> | SubmitResult)
    | undefined

  readonly onSubmitFailed?:
    | ((
        error: unknown,
        values: FieldValues<Schema>,
        form: UseFormReturn<Schema>,
      ) => Promise<void> | void)
    | undefined
  readonly onSubmitSuccess?:
    | ((
        data: SubmitResult,
        values: FieldValues<Schema>,
        form: UseFormReturn<Schema>,
      ) => Promise<void> | void)
    | undefined
  readonly onSubmitted?:
    | ((
        data: SubmitResult | undefined,
        error: unknown,
        values: FieldValues<Schema>,
        form: UseFormReturn<Schema>,
      ) => Promise<void> | void)
    | undefined
}

/** Props for the useForm hook. */
export interface UseFormProps<Schema extends TSchema, SubmitResult = void>
  extends Omit<
      reactHookForm.UseFormProps<FieldValues<Schema>>,
      'handleSubmit' | 'resetOptions' | 'resolver'
    >,
    OnSubmitCallbacks<Schema, SubmitResult> {
  readonly schema: Schema | ((schema: typeof schemaModule.schema) => Schema)
  /**
   * Whether the form can submit offline.
   * @default false
   */
  readonly canSubmitOffline?: boolean

  /** Debug name for the form. Use it to identify the form in the tanstack query devtools. */
  readonly debugName?: string
  readonly method?: 'dialog' | (string & {}) | undefined
}

/** Register function for a form field. */
export type UseFormRegister<Schema extends TSchema> = <
  TFieldName extends FieldPath<Schema> = FieldPath<Schema>,
>(
  name: TFieldName,
  options?: reactHookForm.RegisterOptions<FieldValues<Schema>, TFieldName>,
) => UseFormRegisterReturn<Schema, TFieldName>

/** UseFormRegister return type. */
export interface UseFormRegisterReturn<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema> = FieldPath<Schema>,
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
export interface UseFormReturn<Schema extends TSchema>
  extends Omit<
    reactHookForm.UseFormReturn<FieldValues<Schema>, unknown, TransformedValues<Schema>>,
    'onSubmit' | 'resetOptions' | 'resolver'
  > {
  readonly register: UseFormRegister<Schema>
  readonly submit: (event?: FormEvent<HTMLFormElement> | null) => Promise<void>
  readonly schema: Schema
  readonly setFormError: (error: string) => void
  readonly closeRef: React.MutableRefObject<() => void>
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

/** Form type interface that check if FieldValues type is compatible with the value type from component */
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
 * Form instance type that has been validated.
 * Cast validatable form type to FormInstance
 */
export type FormInstanceValidated<
  Schema extends TSchema,
  // We use any here because we want to bypass the type check for Error type as it won't be a case here
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
> = FormInstance<Schema> | (any[] & NonNullable<unknown>)

/** Props for the Field component. */
// Readonly omitted here to avoid type mismatch with native HTML attributes
// eslint-disable-next-line no-restricted-syntax
export interface FieldProps {
  readonly isRequired?: boolean | undefined
  readonly label?: React.ReactNode | undefined
  readonly description?: React.ReactNode | undefined
  readonly error?: React.ReactNode | undefined

  /** Defines a string value that labels the current element. */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-label'?: string | undefined

  /** Identifies the element (or elements) that labels the current element. */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-labelledby'?: string | undefined

  /** Identifies the element (or elements) that describes the object. */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-describedby'?: string | undefined

  /** Identifies the element (or elements) that provide a detailed, extended description for the object. */
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'aria-details'?: string | undefined
}
/**
 * Base Props for a Form Field.
 * @internal
 */
export interface FormFieldProps<
  BaseValueType,
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
> extends FormWithValueValidation<BaseValueType, Schema, TFieldName> {
  readonly name: TFieldName
  readonly value?: BaseValueType extends FieldValues<Schema> ? FieldValues<Schema>[TFieldName]
  : never
  readonly defaultValue?: FieldValues<Schema>[TFieldName] | undefined
  readonly isDisabled?: boolean | undefined
  readonly isRequired?: boolean | undefined
  readonly isInvalid?: boolean | undefined
}

/** Field State Props */
export type FieldStateProps<
  BaseProps extends { value?: unknown },
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
> = FormFieldProps<BaseProps['value'], Schema, TFieldName> & {
  // to avoid conflicts with the FormFieldProps we need to omit the FormFieldProps from the BaseProps
  [K in keyof Omit<
    BaseProps,
    keyof FormFieldProps<BaseProps['value'], Schema, TFieldName>
  >]: BaseProps[K]
}
