/** @file A hook to create a form schema. */

import * as callbackEventHooks from '#/hooks/eventCallbackHooks'

import * as schemaComponent from './schema'
import type * as types from './types'

/** A hook to create a form schema. */
export function useFormSchema<Schema extends types.TSchema, T extends types.FieldValues<Schema>>(
  callback: (schema: typeof schemaComponent.schema) => schemaComponent.schema.ZodObject<T>,
) {
  const callbackEvent = callbackEventHooks.useEventCallback(callback)

  return callbackEvent(schemaComponent.schema)
}
