/**
 * @file
 *
 * Context that injects form instance into the component tree.
 */
import type { PropsWithChildren } from 'react'
import { createContext, useContext } from 'react'
import invariant from 'tiny-invariant'
import type * as types from './types'

/**
 * Context type for the form provider.
 */
interface FormContextType<
  Schema extends types.TSchema,
  TFieldValues extends types.FieldValues<Schema>,
  TTransformedValues extends types.FieldValues<Schema> | undefined = undefined,
> {
  readonly form: types.UseFormReturn<Schema, TFieldValues, TTransformedValues>
}

// at this moment, we don't know the type of the form context
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const FormContext = createContext<FormContextType<any, any, any> | null>(null)

/**
 * Provides the form instance to the component tree.
 */
export function FormProvider<
  Schema extends types.TSchema,
  TFieldValues extends types.FieldValues<Schema>,
  TTransformedValues extends types.FieldValues<Schema> | undefined = undefined,
>(props: FormContextType<Schema, TFieldValues, TTransformedValues> & PropsWithChildren) {
  const { children, form } = props
  return <FormContext.Provider value={{ form }}>{children}</FormContext.Provider>
}

/**
 * Returns the form instance from the context.
 */
export function useFormContext<
  Schema extends types.TSchema,
  TFieldValues extends types.FieldValues<Schema>,
  TTransformedValues extends types.FieldValues<Schema> | undefined = undefined,
>() {
  const ctx = useContext(FormContext)

  invariant(ctx, 'FormContext not found')

  const { form } = ctx

  // This is safe, as it's we pass the value transparently and it's typed outside
  // eslint-disable-next-line no-restricted-syntax
  return form as unknown as types.UseFormReturn<Schema, TFieldValues, TTransformedValues>
}

/**
 * Returns the form instance from the context, or null if the context is not available.
 */
export function useOptionalFormContext<
  Schema extends types.TSchema,
  TFieldValues extends types.FieldValues<Schema>,
  TTransformedValues extends types.FieldValues<Schema> | undefined = undefined,
>() {
  try {
    return useFormContext<Schema, TFieldValues, TTransformedValues>()
  } catch {
    return null
  }
}
