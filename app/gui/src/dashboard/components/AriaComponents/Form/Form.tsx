/** @file Form component. */
import * as React from 'react'
import * as dialog from '../Dialog'
import * as components from './components'
import * as styles from './styles'
import type * as types from './types'

/**
 * Form component. It wraps a `form` and provides form context.
 * It also handles form submission.
 * Provides better error handling and form state management and better UX out of the box.
 */
export const Form = Object.assign(
  React.forwardRef(function Form<Schema extends components.TSchema, SubmitResult = void>(
    props: types.FormProps<Schema, SubmitResult>,
    ref: React.Ref<HTMLFormElement>,
  ) {
    const formId = React.useId()

    const {
      children,
      formRef,
      form,
      formOptions,
      className,
      style,
      onChange,
      onSubmit,
      onSubmitted = () => {},
      onSubmitSuccess = () => {},
      onSubmitFailed = () => {},
      id = formId,
      schema,
      defaultValues,
      gap,
      method,
      canSubmitOffline = false,
      testId = props['data-testid'],
      ...formProps
    } = props

    const dialogContext = dialog.useDialogContext()

    const innerForm = components.useForm<Schema, SubmitResult>(
      form ?? {
        ...formOptions,
        ...(defaultValues ? { defaultValues } : {}),
        ...(onChange ? { onChange } : {}),
        method,
        schema,
        canSubmitOffline,
        onSubmit,
        onSubmitFailed,
        onSubmitSuccess: async (...args) => {
          if (method === 'dialog') {
            dialogContext?.close()
          }
          await onSubmitSuccess(...args)
        },
        onSubmitted,
        shouldFocusError: true,
        debugName: `Form ${testId} id: ${id}`,
      },
    )

    React.useImperativeHandle(formRef, () => innerForm, [innerForm])
    React.useImperativeHandle(form?.closeRef, () => dialogContext?.close ?? (() => {}), [
      dialogContext?.close,
    ])

    const base = styles.FORM_STYLES({
      className: typeof className === 'function' ? className(innerForm) : className,
      gap,
    })

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
        <components.FormProvider form={innerForm}>
          {typeof children === 'function' ? children({ ...innerForm, form: innerForm }) : children}
        </components.FormProvider>
      </form>
    )
  }),
  {
    /* eslint-disable @typescript-eslint/naming-convention */
    schema: components.schema,
    useForm: components.useForm,
    useField: components.useField,
    makeUseField: components.makeUseField,
    useFormSchema: components.useFormSchema,
    Submit: components.Submit,
    Reset: components.Reset,
    FormError: components.FormError,
    FieldValue: components.FieldValue,
    FieldError: components.FieldError,
    useFormContext: components.useFormContext,
    useOptionalFormContext: components.useOptionalFormContext,
    Field: components.Field,
    Controller: components.Controller,
    Provider: components.FormProvider,
    useWatch: components.useWatch,
    FIELD_STYLES: components.FIELD_STYLES,
    useFieldRegister: components.useFieldRegister,
    useFieldState: components.useFieldState,
    useFormError: components.useFormError,
    useFormState: components.useFormState,
    /* eslint-enable @typescript-eslint/naming-convention */
  },
)
