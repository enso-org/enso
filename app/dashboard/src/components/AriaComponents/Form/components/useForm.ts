/**
 * @file
 *
 * A hook that returns a form instance.
 */
import * as React from 'react'

import * as zodResolver from '@hookform/resolvers/zod'
import * as reactHookForm from 'react-hook-form'
import invariant from 'tiny-invariant'

import { useText } from '#/providers/TextProvider'
import * as schemaModule from './schema'
import type * as types from './types'

/**
 * A hook that returns a form instance.
 * @param optionsOrFormInstance - Either form options or a form instance
 *
 * If form instance is passed, it will be returned as is
 * If form options are passed, a form instance will be created and returned
 *
 * ***Note:*** This hook accepts either a form instance(If form is created outside)
 * or form options(and creates a form instance).
 * This is useful when you want to create a form instance outside the component
 * and pass it to the component.
 * But be careful, You should not switch between the two types of arguments.
 * Otherwise you'll be fired
 */
export function useForm<Schema extends types.TSchema>(
  optionsOrFormInstance: types.UseFormProps<Schema> | types.UseFormReturn<Schema>,
): types.UseFormReturn<Schema> {
  const { getText } = useText()
  const initialTypePassed = React.useRef(getArgsType(optionsOrFormInstance))

  const argsType = getArgsType(optionsOrFormInstance)

  invariant(
    initialTypePassed.current === argsType,
    `
    Found a switch between form options and form instance. This is not allowed. Please use either form options or form instance and stick to it.\n\n
    Initially passed: ${initialTypePassed.current}, Currently passed: ${argsType}.
    `,
  )

  const form =
    'formState' in optionsOrFormInstance ? optionsOrFormInstance : (
      (() => {
        const { schema, ...options } = optionsOrFormInstance

        const computedSchema = typeof schema === 'function' ? schema(schemaModule.schema) : schema

        return reactHookForm.useForm<
          types.FieldValues<Schema>,
          unknown,
          types.TransformedValues<Schema>
        >({
          ...options,
          resolver: zodResolver.zodResolver(computedSchema, {
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
      }),
        })
      })()
    )

  return form
}

/**
 * Get the type of arguments passed to the useForm hook
 */
function getArgsType<Schema extends types.TSchema>(
  args: types.UseFormProps<Schema> | types.UseFormReturn<Schema>,
) {
  return 'formState' in args ? 'formInstance' : 'formOptions'
}
