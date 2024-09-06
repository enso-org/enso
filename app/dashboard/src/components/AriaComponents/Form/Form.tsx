/** @file Form component. */
import * as React from 'react'

import * as sentry from '@sentry/react'
import * as reactQuery from '@tanstack/react-query'
import * as reactHookForm from 'react-hook-form'

import * as offlineHooks from '#/hooks/offlineHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'

import * as errorUtils from '#/utilities/error'

import { forwardRef } from '#/utilities/react'
import * as dialog from '../Dialog'
import * as components from './components'
import * as styles from './styles'
import type * as types from './types'

/** Form component. It wraps a `form` and provides form context.
 * It also handles form submission.
 * Provides better error handling and form state management and better UX out of the box. */
// There is no way to avoid type casting here
// eslint-disable-next-line no-restricted-syntax
export const Form = forwardRef(function Form<Schema extends components.TSchema>(
  props: types.FormProps<Schema>,
  ref: React.Ref<HTMLFormElement>,
) {
  /** Input values for this form. */
  type FieldValues = components.FieldValues<Schema>
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
    method,
    canSubmitOffline = false,
    ...formProps
  } = props

  const { getText } = textProvider.useText()

  if (defaultValues) {
    formOptions.defaultValues = defaultValues
  }

  const innerForm = components.useForm(form ?? { shouldFocusError: true, schema, ...formOptions })

  React.useImperativeHandle(formRef, () => innerForm, [innerForm])

  const dialogContext = dialog.useDialogContext()

  const formMutation = reactQuery.useMutation({
    // We use template literals to make the mutation key more readable in the devtools
    // This mutation exists only for debug purposes - React Query dev tools record the mutation,
    // the result, and the variables(form fields).
    // In general, prefer using object literals for the mutation key.
    mutationKey: ['Form submission', `testId: ${testId}`, `id: ${id}`],
    mutationFn: async (fieldValues: FieldValues) => {
      try {
        await onSubmit?.(fieldValues, innerForm)

        if (method === 'dialog') {
          dialogContext?.close()
        }
      } catch (error) {
        const isJSError = errorUtils.isJSError(error)

        if (isJSError) {
          sentry.captureException(error, {
            contexts: { form: { values: fieldValues } },
          })
        }

        const message =
          isJSError ?
            getText('arbitraryFormErrorMessage')
          : errorUtils.tryGetMessage(error, getText('arbitraryFormErrorMessage'))

        innerForm.setError('root.submit', { message })

        // We need to throw the error to make the mutation fail
        // eslint-disable-next-line no-restricted-syntax
        throw error
      }
    },
    onError: onSubmitFailed,
    onSuccess: onSubmitSuccess,
    onSettled: onSubmitted,
  })

  // There is no way to avoid type casting here
  // eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax,@typescript-eslint/no-unsafe-argument
  const formOnSubmit = innerForm.handleSubmit(formMutation.mutateAsync as any)

  const { isOffline } = offlineHooks.useOffline()

  offlineHooks.useOfflineChange(
    (offline) => {
      if (offline) {
        innerForm.setError('root.offline', { message: getText('unavailableOffline') })
      } else {
        innerForm.clearErrors('root.offline')
      }
    },
    { isDisabled: canSubmitOffline },
  )

  const base = styles.FORM_STYLES({
    className: typeof className === 'function' ? className(innerForm) : className,
    gap,
  })

  const { formState, setError } = innerForm

  // eslint-disable-next-line no-restricted-syntax
  const errors = Object.fromEntries(
    Object.entries(formState.errors).map(([key, error]) => {
      const message = error?.message ?? getText('arbitraryFormErrorMessage')
      return [key, message]
    }),
  ) as Record<keyof FieldValues, string>

  return (
    <form
      id={id}
      ref={ref}
      onSubmit={(event) => {
        event.preventDefault()
        event.stopPropagation()

        if (isOffline && !canSubmitOffline) {
          setError('root.offline', { message: getText('unavailableOffline') })
        } else {
          void formOnSubmit(event)
        }
      }}
      onReset={(event) => {
        event.preventDefault()
        form?.reset(defaultValues, { keepDirty: false })
      }}
      className={base}
      style={typeof style === 'function' ? style(innerForm) : style}
      noValidate
      data-testid={testId}
      {...formProps}
    >
      <aria.FormValidationContext.Provider value={errors}>
        <reactHookForm.FormProvider {...innerForm}>
          {typeof children === 'function' ? children({ ...innerForm, form: innerForm }) : children}
        </reactHookForm.FormProvider>
      </aria.FormValidationContext.Provider>
    </form>
  )
}) as unknown as (<Schema extends components.TSchema>(
  props: React.RefAttributes<HTMLFormElement> & types.FormProps<Schema>,
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
  /* eslint-enable @typescript-eslint/naming-convention */
}

Form.schema = components.schema
Form.useForm = components.useForm
Form.useField = components.useField
Form.useFormSchema = components.useFormSchema
Form.Submit = components.Submit
Form.Reset = components.Reset
Form.FormError = components.FormError
Form.Field = components.Field
Form.Controller = components.Controller
Form.FIELD_STYLES = components.FIELD_STYLES
