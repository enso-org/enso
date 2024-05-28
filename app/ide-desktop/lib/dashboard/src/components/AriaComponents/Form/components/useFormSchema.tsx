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

  return res.partial()
}
