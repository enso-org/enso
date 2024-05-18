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
  TFieldValues extends reactHookForm.FieldValues,
  // This type
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends reactHookForm.FieldValues | undefined = undefined,
> extends Omit<
    React.HTMLProps<HTMLFormElement>,
    'children' | 'className' | 'form' | 'onSubmit' | 'style'
  > {
  readonly form?: reactHookForm.UseFormReturn<TFieldValues, TTransformedValues>
  readonly className?: string | ((props: types.FormStateRenderProps<TFieldValues>) => string)
  readonly onSubmit: (values: TFieldValues) => Promise<void> | void
  readonly style?:
    | React.CSSProperties
    | ((props: types.FormStateRenderProps<TFieldValues>) => React.CSSProperties)
  readonly children:
    | React.ReactNode
    | ((props: types.FormStateRenderProps<TFieldValues>) => React.ReactNode)
  readonly formRef?: React.MutableRefObject<reactHookForm.UseFormReturn<TFieldValues>>
  readonly formOptions?: reactHookForm.UseFormProps<TFieldValues, TTransformedValues>
}

/**
 *
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
  const innerForm = components.useForm<TFieldValues>(form ?? formOptions)

  React.useImperativeHandle(formRef, () => innerForm, [innerForm])

  return (
    <form
      ref={ref}
      className={
        typeof className === 'function' ? className({ formState: innerForm.formState }) : className
      }
      style={typeof style === 'function' ? style({ formState: innerForm.formState }) : style}
      onSubmit={innerForm.handleSubmit(async (data, event) => {
        event?.preventDefault()
        return onSubmit(data)
      })}
      {...formProps}
    >
      <reactHookForm.FormProvider {...innerForm}>
        {typeof children === 'function' ? children({ formState: innerForm.formState }) : children}
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
}

Form.Submit = components.Submit
Form.useForm = components.useForm
Form.Reset = components.Reset
