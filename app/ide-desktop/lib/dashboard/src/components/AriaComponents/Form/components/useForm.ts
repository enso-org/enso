/**
 * @file
 *
 * A hook that returns a form instance.
 */
import * as React from 'react'

import * as reactHookForm from 'react-hook-form'
import invariant from 'tiny-invariant'

/**
 * Props for the useForm hook.
 */
export type UseFormProps<T extends Record<string, unknown>> = reactHookForm.UseFormProps<T>

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
export function useForm<T extends Record<string, unknown>>(
  optionsOrFormInstance: reactHookForm.UseFormProps<T> | reactHookForm.UseFormReturn<T>
): reactHookForm.UseFormReturn<T> {
  const initialTypePassed = React.useRef(getArgsType(optionsOrFormInstance))

  const argsType = getArgsType(optionsOrFormInstance)

  invariant(
    initialTypePassed.current === argsType,
    `
    Found a switch between form options and form instance. This is not allowed. Please use either form options or form instance. And stick to it.
    Initially passed: ${initialTypePassed.current}, Currently passed: ${argsType} \n\n
    `
  )

  if ('formState' in optionsOrFormInstance) {
    return optionsOrFormInstance
  } else {
    return reactHookForm.useForm<T>(optionsOrFormInstance)
  }
}

/**
 * Get the type of arguments passed to the useForm hook
 */
function getArgsType<T extends Record<string, unknown>>(
  args: reactHookForm.UseFormReturn<T> | UseFormProps<T>
) {
  return 'formState' in args ? 'formInstance' : 'formOptions'
}
