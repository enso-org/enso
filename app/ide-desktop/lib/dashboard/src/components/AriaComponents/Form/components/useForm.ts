/**
 * @file
 *
 * A hook that returns a form instance.
 */
import * as React from 'react'

import * as zodResolver from '@hookform/resolvers/zod'
import * as reactHookForm from 'react-hook-form'
import invariant from 'tiny-invariant'

import type * as types from './types'

/**
 * A hook that returns a form instance.
 * @param optionsOrFormInstance - Either form options or a form instance
 *
 * If form instance is passed, it will be returned as is
 * If form options are passed, a form instance will be created and returned
 *
 * ***Note:*** This hook accepts either a form instance(If form is created outside) or form options(and creates a form instance).
 * This is useful when you want to create a form instance outside the component and pass it to the component.
 * But be careful, You should not switch between the two types of arguments.
 * Otherwise you'll be fired
 */
export function useForm<
  T extends types.FieldValues,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends types.FieldValues | undefined = undefined,
>(
  optionsOrFormInstance: types.UseFormProps<T> | types.UseFormReturn<T, TTransformedValues>
): types.UseFormReturn<T, TTransformedValues> {
  const initialTypePassed = React.useRef(getArgsType(optionsOrFormInstance))

  const argsType = getArgsType(optionsOrFormInstance)

  invariant(
    initialTypePassed.current === argsType,
    `
    Found a switch between form options and form instance. This is not allowed. Please use either form options or form instance and stick to it.\n\n
    Initially passed: ${initialTypePassed.current}, Currently passed: ${argsType}.
    `
  )

  if ('formState' in optionsOrFormInstance) {
    return optionsOrFormInstance
  } else {
    const { schema, ...options } = optionsOrFormInstance

    return reactHookForm.useForm({
      ...options,
      ...(schema ? { resolver: zodResolver.zodResolver(schema) } : {}),
    })
  }
}

/**
 * Get the type of arguments passed to the useForm hook
 */
function getArgsType<
  T extends Record<string, unknown>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends Record<string, unknown> | undefined = undefined,
>(args: types.UseFormProps<T> | types.UseFormReturn<T, TTransformedValues>) {
  return 'formState' in args ? 'formInstance' : 'formOptions'
}
