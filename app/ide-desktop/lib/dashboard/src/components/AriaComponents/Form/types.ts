/**
 * @file
 * Types for the Form component.
 */

import type * as React from 'react'

import type * as reactHookForm from 'react-hook-form'

import type * as components from './components'
import type * as styles from './styles'

export type * from './components'

/**
 * Props for the Form component
 */
export type FormProps<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
> = BaseFormProps<Schema, TFieldValues, TTransformedValues> &
  (
    | FormPropsWithOptions<Schema, TFieldValues>
    | FormPropsWithParentForm<Schema, TFieldValues, TTransformedValues>
  )

/**
 * Base props for the Form component.
 */
interface BaseFormProps<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
> extends Omit<
      React.HTMLProps<HTMLFormElement>,
      'children' | 'className' | 'form' | 'onSubmit' | 'onSubmitCapture' | 'style'
    >,
    styles.FormStyleProps {
  /**
   * The default values for the form fields
   *
   * __Note:__ Even though this is optional,
   * it is recommended to provide default values and specify all fields defined in the schema.
   * Otherwise Typescript fails to infer the correct type for the form values.
   * This is a known limitation and we are working on a solution.
   */
  readonly defaultValues?: components.UseFormProps<Schema, TFieldValues>['defaultValues']
  readonly onSubmit: (
    values: TFieldValues,
    form: components.UseFormReturn<Schema, TFieldValues, TTransformedValues>
  ) => unknown
  readonly style?:
    | React.CSSProperties
    | ((
        props: FormStateRenderProps<Schema, TFieldValues, TTransformedValues>
      ) => React.CSSProperties)
  readonly children:
    | React.ReactNode
    | ((props: FormStateRenderProps<Schema, TFieldValues, TTransformedValues>) => React.ReactNode)
  readonly formRef?: React.MutableRefObject<
    components.UseFormReturn<Schema, TFieldValues, TTransformedValues>
  >

  readonly className?:
    | string
    | ((props: FormStateRenderProps<Schema, TFieldValues, TTransformedValues>) => string)

  readonly onSubmitFailed?: (error: unknown) => Promise<void> | void
  readonly onSubmitSuccess?: () => Promise<void> | void
  readonly onSubmitted?: () => Promise<void> | void

  readonly testId?: string
  /**
   * When set to `dialog`, form submission will close the parent dialog on successfull submission.
   */
  // eslint-disable-next-line @typescript-eslint/ban-types,no-restricted-syntax
  readonly method?: 'dialog' | (string & {})
}

/**
 * Props for the Form component with parent form
 * or if form is passed as a prop.
 */
interface FormPropsWithParentForm<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
> {
  readonly form: components.UseFormReturn<Schema, TFieldValues, TTransformedValues>
  readonly schema?: never
  readonly formOptions?: never
}

/**
 * Props for the Form component with schema and form options.
 * Creates a new form instance. This is the default way to use the form.
 */
interface FormPropsWithOptions<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
> {
  readonly schema: Schema
  readonly form?: never
  readonly formOptions?: Omit<components.UseFormProps<Schema, TFieldValues>, 'resolver' | 'schema'>
}

/**
 *
 */
export type UseFormRegister<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
> = <
  TFieldName extends components.FieldPath<Schema, TFieldValues> = components.FieldPath<
    Schema,
    TFieldValues
  >,
>(
  name: TFieldName,
  options?: reactHookForm.RegisterOptions<TFieldValues, TFieldName>
  // eslint-disable-next-line no-restricted-syntax
) => UseFormRegisterReturn<Schema, TFieldValues, TFieldName>

/**
 * UseFormRegister return type.
 */
export interface UseFormRegisterReturn<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  TFieldName extends components.FieldPath<Schema, TFieldValues> = components.FieldPath<
    Schema,
    TFieldValues
  >,
> extends Omit<reactHookForm.UseFormRegisterReturn<TFieldName>, 'onChange'> {
  // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
  readonly onChange: <Value>(value: Value) => Promise<boolean | void> | void
  readonly isDisabled?: boolean
  readonly isRequired?: boolean
  readonly isInvalid?: boolean
}

/**
 * Form Render Props.
 */
export interface FormStateRenderProps<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
> {
  /**
   * The form state. Contains the current values of the form fields.
   */
  readonly formState: components.FormState<Schema, TFieldValues>
  /**
   * The form register function.
   * Adds a field to the form state.
   */
  readonly register: UseFormRegister<Schema, TFieldValues>
  /**
   * The form unregister function.
   * Removes a field from the form state.
   */
  readonly unregister: components.FormInstance<
    Schema,
    TFieldValues,
    TTransformedValues
  >['unregister']
  readonly setValue: components.FormInstance<Schema, TFieldValues, TTransformedValues>['setValue']
  readonly getValues: components.FormInstance<Schema, TFieldValues, TTransformedValues>['getValues']
  readonly setError: components.FormInstance<Schema, TFieldValues, TTransformedValues>['setError']
  readonly clearErrors: components.FormInstance<
    Schema,
    TFieldValues,
    TTransformedValues
  >['clearErrors']
  readonly setFocus: components.FormInstance<Schema, TFieldValues, TTransformedValues>['setFocus']
  readonly reset: components.FormInstance<Schema, TFieldValues, TTransformedValues>['reset']
  readonly control: components.FormInstance<Schema, TFieldValues, TTransformedValues>['control']
  readonly form: components.FormInstance<Schema, TFieldValues, TTransformedValues>
}

/**
 * Base Props for a Form Field.
 * @private
 */
interface FormFieldProps<
  BaseValueType,
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  TFieldName extends components.FieldPath<Schema, TFieldValues>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
> extends components.FormWithValueValidation<
    BaseValueType,
    Schema,
    TFieldValues,
    TFieldName,
    TTransformedValues
  > {
  readonly name: TFieldName
  readonly value?: BaseValueType extends TFieldValues[TFieldName] ? TFieldValues[TFieldName] : never
  readonly defaultValue?: TFieldValues[TFieldName]
  readonly isDisabled?: boolean
  readonly isRequired?: boolean
  readonly isInvalid?: boolean
}

/**
 * Field State Props
 */
export type FieldStateProps<
  // eslint-disable-next-line no-restricted-syntax
  BaseProps extends { value?: unknown },
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  TFieldName extends components.FieldPath<Schema, TFieldValues>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
> = FormFieldProps<BaseProps['value'], Schema, TFieldValues, TFieldName, TTransformedValues> & {
  // to avoid conflicts with the FormFieldProps we need to omit the FormFieldProps from the BaseProps
  [K in keyof Omit<
    BaseProps,
    keyof FormFieldProps<BaseProps['value'], Schema, TFieldValues, TFieldName, TTransformedValues>
  >]: BaseProps[K]
}
