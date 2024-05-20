/**
 * @file
 * Types for the Form component.
 */

import type * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

import type * as components from './components'

export type * from './components'

/**
 * Props for the Form component
 */
export type FormProps<
  TFieldValues extends components.FieldValues,
  // This type is defined on library level and we can't change it
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues | undefined = undefined,
> = BaseFormProps<TFieldValues, TTransformedValues> &
  (FormPropsWithOptions<TFieldValues> | FormPropsWithParentForm<TFieldValues, TTransformedValues>)

/**
 * Base props for the Form component.
 */
interface BaseFormProps<
  TFieldValues extends components.FieldValues,
  // This type is defined on library level and we can't change it
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues | undefined = undefined,
> extends Omit<
    React.HTMLProps<HTMLFormElement>,
    'children' | 'className' | 'form' | 'onSubmit' | 'onSubmitCapture' | 'style'
  > {
  readonly className?: string | ((props: FormStateRenderProps<TFieldValues>) => string)
  readonly onSubmit: (
    values: TFieldValues,
    form: components.UseFormReturn<TFieldValues, TTransformedValues>
  ) => unknown
  readonly style?:
    | React.CSSProperties
    | ((props: FormStateRenderProps<TFieldValues>) => React.CSSProperties)
  readonly children:
    | React.ReactNode
    | ((props: FormStateRenderProps<TFieldValues>) => React.ReactNode)
  readonly formRef?: React.MutableRefObject<
    components.UseFormReturn<TFieldValues, TTransformedValues>
  >

  readonly onSubmitFailed?: (error: unknown) => Promise<void> | void
  readonly onSubmitSuccess?: () => Promise<void> | void
  readonly onSubmitted?: () => Promise<void> | void
}

/**
 * Props for the Form component with parent form
 * or if form is passed as a prop.
 */
interface FormPropsWithParentForm<
  TFieldValues extends components.FieldValues,
  // This type is defined on library level and we can't change it
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues | undefined = undefined,
> {
  readonly form: components.UseFormReturn<TFieldValues, TTransformedValues>
  readonly schema?: never
  readonly formOptions?: never
}

/**
 * Props for the Form component with schema and form options.
 * Creates a new form instance. This is the default way to use the form.
 */
interface FormPropsWithOptions<TFieldValues extends components.FieldValues> {
  readonly form?: never
  readonly schema?: z.ZodObject<TFieldValues>
  readonly formOptions: Omit<components.UseFormProps<TFieldValues>, 'resolver'>
}

/**
 * Form Render Props.
 */
export interface FormStateRenderProps<TFieldValues extends components.FieldValues> {
  /**
   * The form state. Contains the current values of the form fields.
   */
  readonly formState: components.FormState<TFieldValues>
  /**
   * The form register function.
   * Adds a field to the form state.
   */
  readonly register: reactHookForm.UseFormRegister<TFieldValues>
  /**
   * The form unregister function.
   * Removes a field from the form state.
   */
  readonly unregister: reactHookForm.UseFormUnregister<TFieldValues>
}
