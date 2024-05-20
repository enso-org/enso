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
export function useFormSchema<T extends types.FieldValues>(
  callback: (schema: typeof schemaComponent.schema) => schemaComponent.schema.ZodObject<T>
): schemaComponent.schema.ZodObject<T> {
  const callbackEvent = callbackEventHooks.useEventCallback(callback)
  return React.useMemo(() => callbackEvent(schemaComponent.schema), [callbackEvent])
}
