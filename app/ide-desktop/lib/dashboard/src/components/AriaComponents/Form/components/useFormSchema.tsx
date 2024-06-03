/**
 * @file This file contains the useFormSchema hook for creating form schemas.
 */

import * as React from 'react'

import * as callbackEventHooks from '#/hooks/eventCallbackHooks'

import * as schemaComponent from './schema'
import type * as types from './types'

/**
 * Hook to create a form schema.
 */
export function useFormSchema<Schema extends types.TSchema, T extends types.FieldValues<Schema>>(
  callback: (schema: typeof schemaComponent.schema) => schemaComponent.schema.ZodObject<T>
) {
  const callbackEvent = callbackEventHooks.useEventCallback(callback)
  const res = React.useMemo(() => callbackEvent(schemaComponent.schema), [callbackEvent])

  // We assume that all the fields are optional by default
  // This is because we don't want to force the user to provide a value for every field
  // But if the user wants to make a field required, they can do so by providing a default value for the field
  return res.partial()
}
