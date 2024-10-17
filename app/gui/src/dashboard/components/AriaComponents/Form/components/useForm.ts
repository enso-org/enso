/**
 * @file
 *
 * A hook that returns a form instance.
 */
import * as sentry from '@sentry/react'
import * as React from 'react'

import * as zodResolver from '@hookform/resolvers/zod'
import * as reactHookForm from 'react-hook-form'
import invariant from 'tiny-invariant'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOffline, useOfflineChange } from '#/hooks/offlineHooks'
import { useText } from '#/providers/TextProvider'
import * as errorUtils from '#/utilities/error'
import { useMutation } from '@tanstack/react-query'
import * as schemaModule from './schema'
import type * as types from './types'

/** Maps the value to the event object. */
function mapValueOnEvent(value: unknown) {
  if (typeof value === 'object' && value != null && 'target' in value && 'type' in value) {
    return value
  } else {
    return { target: { value } }
  }
}

/**
 * A hook that returns a form instance.
 * @param optionsOrFormInstance - Either form options or a form instance
 *
 * If form instance is passed, it will be returned as is.
 *
 * If form options are passed, a form instance will be created and returned.
 *
 * ***Note:*** This hook accepts either a form instance(If form is created outside)
 * or form options(and creates a form instance).
 * This is useful when you want to create a form instance outside the component
 * and pass it to the component.
 * But be careful, You should not switch between the two types of arguments.
 * Otherwise you'll be fired
 */
export function useForm<Schema extends types.TSchema, SubmitResult = void>(
  optionsOrFormInstance: types.UseFormProps<Schema, SubmitResult> | types.UseFormReturn<Schema>,
): types.UseFormReturn<Schema> {
  const { getText } = useText()
  const [initialTypePassed] = React.useState(() => getArgsType(optionsOrFormInstance))
  const closeRef = React.useRef(() => {})

  const argsType = getArgsType(optionsOrFormInstance)

  invariant(
    initialTypePassed === argsType,
    `
    Found a switch between form options and form instance. This is not allowed. Please use either form options or form instance and stick to it.\n\n
    Initially passed: ${initialTypePassed}, Currently passed: ${argsType}.
    `,
  )

  if ('formState' in optionsOrFormInstance) {
    return optionsOrFormInstance
  } else {
    const {
      schema,
      onSubmit,
      canSubmitOffline = false,
      onSubmitFailed,
      onSubmitted,
      onSubmitSuccess,
      debugName,
      method,
      ...options
    } = optionsOrFormInstance

    const computedSchema = typeof schema === 'function' ? schema(schemaModule.schema) : schema

    const formInstance = reactHookForm.useForm({
      ...options,
      resolver: zodResolver.zodResolver(
        computedSchema,
        {
          async: true,
          errorMap: (issue) => {
            switch (issue.code) {
              case 'too_small':
                if (issue.minimum === 0) {
                  return {
                    message: getText('arbitraryFieldRequired'),
                  }
                } else {
                  return {
                    message: getText('arbitraryFieldTooSmall', issue.minimum.toString()),
                  }
                }
              case 'too_big':
                return {
                  message: getText('arbitraryFieldTooLarge', issue.maximum.toString()),
                }
              case 'invalid_type':
                return {
                  message: getText('arbitraryFieldInvalid'),
                }
              default:
                return {
                  message: getText('arbitraryFieldInvalid'),
                }
            }
          },
        },
        { mode: 'async' },
      ),
    })

    const register: types.UseFormRegister<Schema> = (name, opts) => {
      const registered = formInstance.register(name, opts)

      const onChange: types.UseFormRegisterReturn<Schema>['onChange'] = (value) =>
        registered.onChange(mapValueOnEvent(value))

      const onBlur: types.UseFormRegisterReturn<Schema>['onBlur'] = (value) =>
        registered.onBlur(mapValueOnEvent(value))

      const result: types.UseFormRegisterReturn<Schema, typeof name> = {
        ...registered,
        disabled: registered.disabled ?? false,
        isDisabled: registered.disabled ?? false,
        invalid: !!formInstance.formState.errors[name],
        isInvalid: !!formInstance.formState.errors[name],
        required: registered.required ?? false,
        isRequired: registered.required ?? false,
        onChange,
        onBlur,
      }

      return result
    }

    // eslint-disable-next-line react-hooks/rules-of-hooks
    const formMutation = useMutation({
      // We use template literals to make the mutation key more readable in the devtools
      // This mutation exists only for debug purposes - React Query dev tools record the mutation,
      // the result, and the variables(form fields).
      // In general, prefer using object literals for the mutation key.
      mutationKey: ['Form submission', `debugName: ${debugName}`],
      mutationFn: async (fieldValues: types.FieldValues<Schema>) => {
        try {
          // This is safe, because we transparently passing the result of the onSubmit function,
          // and the type of the result is the same as the type of the SubmitResult.
          // eslint-disable-next-line no-restricted-syntax
          const result = (await onSubmit?.(fieldValues, form)) as SubmitResult

          if (method === 'dialog') {
            closeRef.current()
          }

          return result
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

          setFormError(message)
          // We need to throw the error to make the mutation fail
          // eslint-disable-next-line no-restricted-syntax
          throw error
        }
      },
      onError: (error, values) => onSubmitFailed?.(error, values, form),
      onSuccess: (data, values) => onSubmitSuccess?.(data, values, form),
      onSettled: (data, error, values) => onSubmitted?.(data, error, values, form),
    })

    // There is no way to avoid type casting here
    // eslint-disable-next-line @typescript-eslint/no-explicit-any,no-restricted-syntax,@typescript-eslint/no-unsafe-argument
    const formOnSubmit = formInstance.handleSubmit(formMutation.mutateAsync as any)

    // eslint-disable-next-line react-hooks/rules-of-hooks
    const { isOffline } = useOffline()

    // eslint-disable-next-line react-hooks/rules-of-hooks
    useOfflineChange(
      (offline) => {
        if (offline) {
          formInstance.setError('root.offline', { message: getText('unavailableOffline') })
        } else {
          formInstance.clearErrors('root.offline')
        }
      },
      { isDisabled: canSubmitOffline },
    )

    // eslint-disable-next-line react-hooks/rules-of-hooks
    const submit = useEventCallback(
      (event: React.FormEvent<HTMLFormElement> | null | undefined) => {
        event?.preventDefault()
        event?.stopPropagation()

        if (isOffline && !canSubmitOffline) {
          formInstance.setError('root.offline', { message: getText('unavailableOffline') })
          return Promise.resolve()
        } else {
          if (event) {
            return formOnSubmit(event)
          } else {
            return formOnSubmit()
          }
        }
      },
    )

    // eslint-disable-next-line react-hooks/rules-of-hooks
    const setFormError = useEventCallback((error: string) => {
      formInstance.setError('root.submit', { message: error })
    })

    const form: types.UseFormReturn<Schema> = {
      ...formInstance,
      submit,
      control: { ...formInstance.control, register },
      register,
      schema: computedSchema,
      setFormError,
      handleSubmit: formInstance.handleSubmit,
      closeRef,
    }

    return form
  }
}

/** Get the type of arguments passed to the useForm hook */
function getArgsType<Schema extends types.TSchema, SubmitResult = void>(
  args: types.UseFormProps<Schema, SubmitResult>,
) {
  return 'formState' in args ? ('formInstance' as const) : ('formOptions' as const)
}
