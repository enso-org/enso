/** @file A hook to create a form schema. */
import * as React from 'react'

import * as callbackEventHooks from '#/hooks/eventCallbackHooks'

import * as schemaComponent from './schema'
import type * as types from './types'

// =====================
// === useFormSchema ===
// =====================

/** A hook to create a form schema. */
export function useFormSchema<Schema extends types.TSchema, T extends types.FieldValues<Schema>>(
  callback: (schema: typeof schemaComponent.schema) => schemaComponent.schema.ZodObject<T>,
) {
  const callbackEvent = callbackEventHooks.useEventCallback(callback)

  return React.useMemo(() => callbackEvent(schemaComponent.schema), [callbackEvent])
}
