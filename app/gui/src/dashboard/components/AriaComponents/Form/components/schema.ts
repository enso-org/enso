/** @file Create a schema for a form */
import { z } from 'zod'
import type { SchemaCallback, TSchema } from './types'

/** Factory function to create a schema. */
export function createSchema<Schema extends TSchema>(callback: SchemaCallback<Schema>) {
  return callback(z)
}

export { z as schema } from 'zod'
