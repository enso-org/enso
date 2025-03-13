/** @file Constants for `Form`. */
import { createContext } from 'react'
import type { TSchema, UseFormReturn } from './types'

/** Context type for the form provider. */
export interface FormContextType<Schema extends TSchema> {
  readonly form: UseFormReturn<Schema>
}

// At this moment, we don't know the type of the form context.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const FormContext = createContext<FormContextType<any> | null>(null)
