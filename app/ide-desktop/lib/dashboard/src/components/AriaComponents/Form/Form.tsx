/** @file Form component. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import * as reactHookForm from 'react-hook-form'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'

import * as components from './components'
import * as styles from './styles'
import type * as types from './types'

/** Form component. It wraps a `form` and provides form context.
 * It also handles form submission.
 * Provides better error handling and form state management and better UX out of the box.
 *
 * ## Component is in BETA and will be improved in the future. */
// There is no way to avoid type casting here
// eslint-disable-next-line no-restricted-syntax
export const Form = React.forwardRef(function Form<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
>(
  props: types.FormProps<Schema, TFieldValues, TTransformedValues>,
  ref: React.Ref<HTMLFormElement>
) {
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
    testId,
    schema,
    defaultValues,
    gap,
    ...formProps
  } = props

  const { getText } = textProvider.useText()

  if (defaultValues) {
    formOptions.defaultValues = defaultValues
  }

  const innerForm = components.useForm(
    form ?? {
      shouldFocusError: true,
      ...formOptions,
      schema,
    }
  )

  React.useImperativeHandle(formRef, () => innerForm, [innerForm])

  const formMutation = reactQuery.useMutation({
    mutationKey: ['FormSubmit', testId, id],
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
  const {
    formState,
    clearErrors,
    getValues,
    setValue,
    setError,
    register,
    unregister,
    setFocus,
    reset,
  } = innerForm

  const formStateRenderProps: types.FormStateRenderProps<Schema, TFieldValues> = {
    formState,
    register: (name, options) => {
      const registered = register(name, options)

      const onChange: types.UseFormRegisterReturn<Schema, TFieldValues>['onChange'] = value => {
        if (typeof value === 'object' && value != null && 'target' in value && 'type' in value) {
          return registered.onChange(value)
        } else {
          return registered.onChange({ target: { event: value } })
        }
      }

      const result: types.UseFormRegisterReturn<Schema, TFieldValues, typeof name> = {
        ...registered,
        ...(registered.disabled != null ? { isDisabled: registered.disabled } : {}),
        ...(registered.required != null ? { isRequired: registered.required } : {}),
        isInvalid: !!formState.errors[name],
        onChange,
      }

      return result
    },
    unregister,
    setError,
    clearErrors,
    getValues,
    setValue,
    setFocus,
    reset,
  }

  const base = styles.FORM_STYLES({
    className: typeof className === 'function' ? className(formStateRenderProps) : className,
    gap,
  })

  // eslint-disable-next-line no-restricted-syntax
  const errors = Object.fromEntries(
    Object.entries(formState.errors).map(([key, error]) => {
      const message = error?.message ?? getText('arbitraryFormErrorMessage')
      return [key, message]
    })
  ) as Record<keyof TFieldValues, string>

  return (
    <form
      id={id}
      ref={ref}
      onSubmit={formOnSubmit}
      className={base}
      style={typeof style === 'function' ? style(formStateRenderProps) : style}
      noValidate
      data-testid={testId}
      {...formProps}
    >
      <aria.FormValidationContext.Provider value={errors}>
        <reactHookForm.FormProvider {...innerForm}>
          {typeof children === 'function' ? children(formStateRenderProps) : children}
        </reactHookForm.FormProvider>
      </aria.FormValidationContext.Provider>
    </form>
  )
}) as unknown as (<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
>(
  props: React.RefAttributes<HTMLFormElement> &
    types.FormProps<Schema, TFieldValues, TTransformedValues>
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
