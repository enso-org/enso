/**
 * @file
 * Types for the Form component.
 */

import type * as React from 'react'

import type * as reactHookForm from 'react-hook-form'

import type { DeepPartialSkipArrayKey } from 'react-hook-form'
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
  SubmitResult extends unknown = void,
> = BaseFormProps<Schema, TFieldValues, TTransformedValues, SubmitResult> &
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
  SubmitResult extends unknown = void,
> extends Omit<
      React.HTMLProps<HTMLFormElement>,
      'children' | 'className' | 'form' | 'onSubmit' | 'onSubmitCapture' | 'style'
    >,
    styles.FormStyleProps,
    components.OnSubmitCallbacks<Schema, TFieldValues, TTransformedValues, SubmitResult> {
  /**
   * The default values for the form fields
   *
   * __Note:__ Even though this is optional,
   * it is recommended to provide default values and specify all fields defined in the schema.
   * Otherwise Typescript fails to infer the correct type for the form values.
   * This is a known limitation and we are working on a solution.
   */
  readonly defaultValues?: components.UseFormProps<Schema, TFieldValues>['defaultValues']
  readonly style?:
    | React.CSSProperties
    | ((
        props: components.UseFormReturn<Schema, TFieldValues, TTransformedValues>,
      ) => React.CSSProperties)
  readonly children:
    | React.ReactNode
    | ((
        props: components.UseFormReturn<Schema, TFieldValues, TTransformedValues> & {
          readonly form: components.UseFormReturn<Schema, TFieldValues, TTransformedValues>
          readonly values: DeepPartialSkipArrayKey<TFieldValues>
        },
      ) => React.ReactNode)
  readonly formRef?: React.MutableRefObject<
    components.UseFormReturn<Schema, TFieldValues, TTransformedValues>
  >

  readonly className?:
    | string
    | ((props: components.UseFormReturn<Schema, TFieldValues, TTransformedValues>) => string)

  readonly testId?: string
  /**
   * When set to `dialog`, form submission will close the parent dialog on successful submission.
   */
  // eslint-disable-next-line @typescript-eslint/ban-types,no-restricted-syntax
  readonly method?: 'dialog' | (string & {})

  readonly canSubmitOffline?: boolean
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
  readonly onSubmit?: never
  readonly onSubmitFailed?: never
  readonly onSubmitSuccess?: never
  readonly onSubmitted?: never
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
  readonly schema: Schema | ((schema: typeof components.schema) => Schema)
  readonly form?: never
  readonly formOptions?: Omit<
    components.UseFormProps<Schema, TFieldValues>,
    'resolver' | 'schema' | 'handleSubmit'
  >
}

/**
 * Register function for a form field.
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
  options?: reactHookForm.RegisterOptions<TFieldValues, TFieldName>,
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
> extends Omit<reactHookForm.UseFormRegisterReturn<TFieldName>, 'onBlur' | 'onChange'> {
  // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
  readonly onChange: <Value>(value: Value) => Promise<boolean | void> | void
  // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
  readonly onBlur: <Value>(value: Value) => Promise<boolean | void> | void
  readonly isDisabled?: boolean
  readonly isRequired?: boolean
  readonly isInvalid?: boolean
}

/**
 * Form Render Props.
 */
export type FormStateRenderProps<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
> = Pick<
  components.FormInstance<Schema, TFieldValues, TTransformedValues>,
  | 'clearErrors'
  | 'control'
  | 'formState'
  | 'getValues'
  | 'reset'
  | 'setError'
  | 'setFocus'
  | 'setValue'
  | 'unregister'
  // eslint-disable-next-line no-restricted-syntax
> & {
  /**
   * The form register function.
   * Adds a field to the form state.
   */
  readonly register: UseFormRegister<Schema, TFieldValues>
  /**
   * Form Instance
   */
  readonly form: components.FormInstance<Schema, TFieldValues, TTransformedValues>
}
