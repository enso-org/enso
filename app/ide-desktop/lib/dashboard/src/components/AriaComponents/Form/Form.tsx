/**
 * @file
 *
 * Form component
 */
import * as React from 'react'

import * as reactHookForm from 'react-hook-form'

import * as components from './components'
import type * as types from './types'

/**
 * Props for the Form component
 */
export interface FormProps<
  TFieldValues extends components.FieldValues,
  // This type
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues | undefined = undefined,
> extends Omit<
    React.HTMLProps<HTMLFormElement>,
    'children' | 'className' | 'form' | 'onSubmit' | 'style'
  > {
  readonly form?: components.UseFormReturn<TFieldValues, TTransformedValues>
  readonly className?: string | ((props: types.FormStateRenderProps<TFieldValues>) => string)
  readonly onSubmit: (values: TFieldValues) => Promise<void> | void
  readonly style?:
    | React.CSSProperties
    | ((props: types.FormStateRenderProps<TFieldValues>) => React.CSSProperties)
  readonly children:
    | React.ReactNode
    | ((props: types.FormStateRenderProps<TFieldValues>) => React.ReactNode)
  readonly formRef?: React.MutableRefObject<
    components.UseFormReturn<TFieldValues, TTransformedValues>
  >
  readonly formOptions?: components.UseFormProps<TFieldValues>
}

/**
 * Form component
 */
// There is no way to avoid type casting here
// eslint-disable-next-line no-restricted-syntax
export const Form = React.forwardRef(function Form<
  TFieldValues extends reactHookForm.FieldValues,
  // This type
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends reactHookForm.FieldValues | undefined = undefined,
>(props: FormProps<TFieldValues, TTransformedValues>, ref: React.Ref<HTMLFormElement>) {
  const {
    children,
    onSubmit,
    formRef,
    form,
    formOptions = {},
    className,
    style,
    ...formProps
  } = props
  const innerForm = components.useForm(form ?? formOptions)

  React.useImperativeHandle(formRef, () => innerForm, [innerForm])

  // There is no way to avoid type casting here
  // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, no-restricted-syntax
  const formOnSubmit = innerForm.handleSubmit((async () => {
    try {
      await onSubmit(innerForm.getValues())
    } catch (error) {
      const defaultErrorMessage = 'An error occurred while submitting the form.'

      if (error instanceof Error) {
        innerForm.setError('root.submit', { message: error.message })
      } else {
        innerForm.setError('root.submit', { message: defaultErrorMessage })
      }

      // TODO: Should we throw the error here?
      // Or should we just log it?
      // What's about sentry?
      // eslint-disable-next-line no-restricted-syntax
      throw error
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
  }) as any)

  const formStateRenderProps = {
    formState: innerForm.formState,
    register: innerForm.register,
    unregister: innerForm.unregister,
  }

  return (
    <form
      ref={ref}
      onSubmit={formOnSubmit}
      className={typeof className === 'function' ? className(formStateRenderProps) : className}
      style={typeof style === 'function' ? style(formStateRenderProps) : style}
      {...formProps}
    >
      <reactHookForm.FormProvider {...innerForm}>
        {typeof children === 'function' ? children(formStateRenderProps) : children}
      </reactHookForm.FormProvider>
    </form>
  )
}) as unknown as (<
  TFieldValues extends reactHookForm.FieldValues,
  // The type is defined on library level and we can't change it
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends reactHookForm.FieldValues | undefined = undefined,
>(
  props: FormProps<TFieldValues, TTransformedValues>
  // eslint-disable-next-line no-restricted-syntax
) => React.JSX.Element) & {
  useForm: typeof components.useForm
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Submit: typeof components.Submit
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Reset: typeof components.Reset
  // eslint-disable-next-line @typescript-eslint/naming-convention
  FormError: typeof components.FormError
}

Form.Submit = components.Submit
Form.useForm = components.useForm
Form.Reset = components.Reset
Form.FormError = components.FormError
