/** @file Form component. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'

import { useEventCallback } from '../../../hooks/eventCallbackHooks'
import * as dialog from '../Dialog'
import * as components from './components'
import * as styles from './styles'
import type * as types from './types'

/** Form component. It wraps a `form` and provides form context.
 * It also handles form submission.
 * Provides better error handling and form state management and better UX out of the box. */
// There is no way to avoid type casting here
// eslint-disable-next-line no-restricted-syntax
export const Form = React.forwardRef(function Form<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
  SubmitResult = void,
>(
  props: types.FormProps<Schema, TFieldValues, TTransformedValues, SubmitResult>,
  ref: React.Ref<HTMLFormElement>,
) {
  const formId = React.useId()

  const {
    children,
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
    method,
    canSubmitOffline = false,
    ...formProps
  } = props

  const { getText } = textProvider.useText()

  if (defaultValues) {
    formOptions.defaultValues = defaultValues
  }

  const dialogContext = dialog.useDialogContext()

  const onSubmit = useEventCallback(
    async (
      fieldValues: TFieldValues,
      formInstance: types.UseFormReturn<Schema, TFieldValues, TTransformedValues>,
    ) => {
      const result = await props.onSubmit?.(fieldValues, formInstance)

      if (method === 'dialog') {
        dialogContext?.close()
      }

      return result
    },
  )

  const innerForm = components.useForm<Schema, TFieldValues, TTransformedValues, SubmitResult>(
    form ??
      // This is unsafe, but it is necessary to avoid a type error and make typescript happy here
      // eslint-disable-next-line no-restricted-syntax
      ({
        schema,
        canSubmitOffline,
        onSubmit,
        onSubmitFailed,
        onSubmitSuccess,
        onSubmitted,
        shouldFocusError: true,
        debugName: `Form ${testId} id: ${id}`,
        ...formOptions,
      } as components.UseFormProps<Schema, TFieldValues, TTransformedValues, SubmitResult>),
  )

  React.useImperativeHandle(formRef, () => innerForm, [innerForm])

  const base = styles.FORM_STYLES({
    className: typeof className === 'function' ? className(innerForm) : className,
    gap,
  })

  const { formState } = innerForm

  // eslint-disable-next-line no-restricted-syntax
  const errors = Object.fromEntries(
    Object.entries(formState.errors).map(([key, error]) => {
      const message = error?.message ?? getText('arbitraryFormErrorMessage')
      return [key, message]
    }),
  ) as Record<keyof TFieldValues, string>

  const values = components.useWatch({ control: innerForm.control })

  return (
    <form
      {...formProps}
      id={id}
      ref={ref}
      className={base}
      style={typeof style === 'function' ? style(innerForm) : style}
      noValidate
      data-testid={testId}
      onSubmit={innerForm.submit}
    >
      <aria.FormValidationContext.Provider value={errors}>
        <components.FormProvider form={innerForm}>
          {typeof children === 'function' ?
            children({ ...innerForm, form: innerForm, values })
          : children}
        </components.FormProvider>
      </aria.FormValidationContext.Provider>
    </form>
  )
}) as unknown as (<
  Schema extends components.TSchema,
  TFieldValues extends components.FieldValues<Schema>,
  TTransformedValues extends components.FieldValues<Schema> | undefined = undefined,
  SubmitResult = void,
>(
  props: React.RefAttributes<HTMLFormElement> &
    types.FormProps<Schema, TFieldValues, TTransformedValues, SubmitResult>,
  // eslint-disable-next-line no-restricted-syntax
) => React.JSX.Element) & {
  /* eslint-disable @typescript-eslint/naming-convention */
  schema: typeof components.schema
  useForm: typeof components.useForm
  useField: typeof components.useField
  Submit: typeof components.Submit
  Reset: typeof components.Reset
  Field: typeof components.Field
  FormError: typeof components.FormError
  useFormSchema: typeof components.useFormSchema
  Controller: typeof components.Controller
  FIELD_STYLES: typeof components.FIELD_STYLES
  useFormContext: typeof components.useFormContext
  useOptionalFormContext: typeof components.useOptionalFormContext
  useWatch: typeof components.useWatch
  useFieldRegister: typeof components.useFieldRegister
  useFieldState: typeof components.useFieldState
  /* eslint-enable @typescript-eslint/naming-convention */
}

Form.schema = components.schema
Form.useForm = components.useForm
Form.useField = components.useField
Form.useFormSchema = components.useFormSchema
Form.Submit = components.Submit
Form.Reset = components.Reset
Form.FormError = components.FormError
Form.useFormContext = components.useFormContext
Form.useOptionalFormContext = components.useOptionalFormContext
Form.Field = components.Field
Form.Controller = components.Controller
Form.useWatch = components.useWatch
Form.FIELD_STYLES = components.FIELD_STYLES
Form.useFieldRegister = components.useFieldRegister
Form.useFieldState = components.useFieldState
