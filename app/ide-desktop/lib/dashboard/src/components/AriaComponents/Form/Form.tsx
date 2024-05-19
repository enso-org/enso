/**
 * @file
 *
 * Form component
 */
import * as React from 'react'

import * as zodResolver from '@hookform/resolvers/zod'
import * as reactQuery from '@tanstack/react-query'
import * as reactHookForm from 'react-hook-form'
import type * as z from 'zod'

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
    'children' | 'className' | 'form' | 'onSubmit' | 'onSubmitCapture' | 'style'
  > {
  readonly schema?: z.ZodObject<TFieldValues>
  readonly form?: components.UseFormReturn<TFieldValues, TTransformedValues>
  readonly className?: string | ((props: types.FormStateRenderProps<TFieldValues>) => string)
  readonly onSubmit: (values: TFieldValues) => unknown
  readonly style?:
    | React.CSSProperties
    | ((props: types.FormStateRenderProps<TFieldValues>) => React.CSSProperties)
  readonly children:
    | React.ReactNode
    | ((props: types.FormStateRenderProps<TFieldValues>) => React.ReactNode)
  readonly formRef?: React.MutableRefObject<
    components.UseFormReturn<TFieldValues, TTransformedValues>
  >
  readonly formOptions?: Omit<components.UseFormProps<TFieldValues>, 'resolver'>
  readonly onSubmitFailed?: (error: unknown) => Promise<void> | void
  readonly onSubmitSuccess?: () => Promise<void> | void
  readonly onSubmitted?: () => Promise<void> | void
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
  const formId = React.useId()

  const {
    children,
    onSubmit,
    formRef,
    form,
    formOptions = {},
    className,
    style,
    onSubmitted = () => {},
    onSubmitSuccess = () => {},
    onSubmitFailed = () => {},
    id = formId,
    schema,
    ...formProps
  } = props

  const innerForm = components.useForm<TFieldValues, TTransformedValues>(
    form ?? {
      ...formOptions,
      ...(schema && { resolver: zodResolver.zodResolver(schema) }),
    }
  )

  React.useImperativeHandle(formRef, () => innerForm, [innerForm])

  const formMutation = reactQuery.useMutation({
    mutationKey: ['FormSubmit', id],
    mutationFn: async () => {
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
    },
    onError: onSubmitFailed,
    onSuccess: onSubmitSuccess,
    onMutate: onSubmitted,
    onSettled: onSubmitted,
  })

  // There is no way to avoid type casting here
  // eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax,@typescript-eslint/no-unsafe-argument
  const formOnSubmit = innerForm.handleSubmit(formMutation.mutateAsync as any)

  const formStateRenderProps = {
    formState: innerForm.formState,
    register: innerForm.register,
    unregister: innerForm.unregister,
  }

  return (
    <form
      id={id}
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
  // eslint-disable-next-line @typescript-eslint/naming-convention
  useFormSchema: typeof components.useFormSchema
  schema: typeof components.schema
}

Form.Submit = components.Submit
Form.useForm = components.useForm
Form.Reset = components.Reset
Form.FormError = components.FormError
Form.useFormSchema = components.useFormSchema
Form.schema = components.schema
