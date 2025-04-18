/** @file Types for `Form`. */
import type * as React from 'react'
import type { TestIdProps } from '../types'
import type * as components from './components'
import type * as styles from './variants'
export type * from './components/types'

/** Props for the Form component */
export type FormProps<Schema extends components.TSchema, SubmitResult = void> =
  | FormPropsWithOptions<Schema, SubmitResult>
  | FormPropsWithParentForm<Schema>

/** Base props for the Form component. */
interface BaseFormProps<Schema extends components.TSchema>
  extends Omit<
      React.HTMLProps<HTMLFormElement>,
      'children' | 'className' | 'form' | 'onSubmit' | 'onSubmitCapture' | 'style'
    >,
    Omit<styles.FormStyleProps, 'class' | 'className'>,
    TestIdProps {
  readonly style?:
    | React.CSSProperties
    | ((props: components.UseFormReturn<Schema>) => React.CSSProperties)
  readonly children:
    | React.ReactNode
    | ((
        props: components.UseFormReturn<Schema> & {
          readonly form: components.UseFormReturn<Schema>
        },
      ) => React.ReactNode)
  readonly formRef?: React.MutableRefObject<components.UseFormReturn<Schema>>

  readonly className?: string | ((props: components.UseFormReturn<Schema>) => string)

  /** When set to `dialog`, form submission will close the parent dialog on successful submission. */
  readonly method?: 'dialog' | (NonNullable<unknown> & string)

  readonly canSubmitOffline?: boolean
}

/**
 * Props for the Form component with parent form
 * or if form is passed as a prop.
 */
export interface FormPropsWithParentForm<Schema extends components.TSchema>
  extends BaseFormProps<Schema> {
  readonly form: components.UseFormReturn<Schema>
  readonly schema?: never
  readonly formOptions?: never
  readonly defaultValues?: never
  readonly onSubmit?: never
  readonly onSubmitSuccess?: never
  readonly onSubmitFailed?: never
  readonly onSubmitted?: never
}

/**
 * Props for the Form component with schema and form options.
 * Creates a new form instance. This is the default way to use the form.
 */
export interface FormPropsWithOptions<Schema extends components.TSchema, SubmitResult = void>
  extends BaseFormProps<Schema>,
    components.OnSubmitCallbacks<Schema, SubmitResult> {
  readonly schema: Schema | ((schema: typeof components.schema) => Schema)
  readonly formOptions?: Omit<
    components.UseFormOptions<Schema, SubmitResult>,
    'defaultValues' | 'onSubmit' | 'onSubmitFailed' | 'onSubmitSuccess' | 'onSubmitted' | 'schema'
  >
  /**
   * The default values for the form fields
   *
   * __Note:__ Even though this is optional,
   * it is recommended to provide default values and specify all fields defined in the schema.
   * Otherwise Typescript fails to infer the correct type for the form values.
   */
  readonly defaultValues?: components.UseFormOptions<Schema>['defaultValues']
  readonly form?: never
}

/** Form Render Props. */
export type FormStateRenderProps<Schema extends components.TSchema> = Pick<
  components.FormInstance<Schema>,
  | 'clearErrors'
  | 'control'
  | 'formState'
  | 'getValues'
  | 'reset'
  | 'setError'
  | 'setFocus'
  | 'setValue'
  | 'unregister'
> & {
  /** The form register function. Adds a field to the form state. */
  readonly register: components.UseFormRegister<Schema>
  /** The form instance. */
  readonly form: components.FormInstance<Schema>
}
