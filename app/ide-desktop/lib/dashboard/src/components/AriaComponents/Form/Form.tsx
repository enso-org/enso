/**
 * @file
 *
 * Form component
 */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as reactHookForm from 'react-hook-form'

import * as textProvider from '#/providers/TextProvider'

import * as components from './components'
import type * as types from './types'

/**
 * Form component. It wraps the form and provides the form context.
 * It also handles the form submission.
 * Provides better error handling and form state management.
 * And serves a better UX out of the box.
 *
 * ## Component is in BETA and will be improved in the future.
 */
// There is no way to avoid type casting here
// eslint-disable-next-line no-restricted-syntax
export const Form = React.forwardRef(function Form<
  TFieldValues extends reactHookForm.FieldValues,
  // This type is defined on library level and we can't change it
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends reactHookForm.FieldValues | undefined = undefined,
>(props: types.FormProps<TFieldValues, TTransformedValues>, ref: React.Ref<HTMLFormElement>) {
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

  const { getText } = textProvider.useText()

  const innerForm = components.useForm<TFieldValues, TTransformedValues>(
    form ?? {
      ...formOptions,
      ...(schema ? { schema } : {}),
    }
  )

  React.useImperativeHandle(formRef, () => innerForm, [innerForm])

  const formMutation = reactQuery.useMutation({
    mutationKey: ['FormSubmit', id],
    mutationFn: async (fieldValues: TFieldValues) => {
      try {
        await onSubmit(fieldValues, innerForm)
      } catch (error) {
        innerForm.setError('root.submit', {
          message: error instanceof Error ? error.message : getText('arbitraryFormErrorMessage'),
        })
        // TODO: Should we throw the error here?
        // Or should we just log it?
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
      noValidate
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
  props: React.RefAttributes<HTMLFormElement> & types.FormProps<TFieldValues, TTransformedValues>
  // eslint-disable-next-line no-restricted-syntax
) => React.JSX.Element) & {
  /* eslint-disable @typescript-eslint/naming-convention */
  schema: typeof components.schema
  useForm: typeof components.useForm
  Submit: typeof components.Submit
  Reset: typeof components.Reset
  FormError: typeof components.FormError
  useFormSchema: typeof components.useFormSchema
  /* eslint-enable @typescript-eslint/naming-convention */
}

Form.schema = components.schema
Form.useForm = components.useForm
Form.Submit = components.Submit
Form.Reset = components.Reset
Form.FormError = components.FormError
Form.useFormSchema = components.useFormSchema
