/**
 * @file
 *
 * Context that injects form instance into the component tree.
 */
import { createContext, use } from 'react'
import invariant from 'tiny-invariant'
import type * as types from './types'
import type { FormInstance, FormInstanceValidated } from './types'

/** Context type for the form provider. */
interface FormContextType<Schema extends types.TSchema> {
  readonly form: types.UseFormReturn<Schema>
}

// at this moment, we don't know the type of the form context
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const FormContext = createContext<FormContextType<any> | null>(null)

/** Provides the form instance to the component tree. */
export function FormProvider<Schema extends types.TSchema>(
  props: FormContextType<Schema> & { children: React.ReactNode },
) {
  const { children, form } = props

  return (
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any
    <FormContext.Provider value={{ form: form as types.UseFormReturn<any> }}>
      {children}
    </FormContext.Provider>
  )
}

/** Returns the form instance from the context. */
// eslint-disable-next-line react-refresh/only-export-components
export function useFormContext<Schema extends types.TSchema>(
  form?: FormInstanceValidated<Schema>,
): FormInstance<Schema> {
  const formInstance = useOptionalFormContext<Schema, FormInstanceValidated<Schema>>(form)

  invariant(formInstance, 'FormContext not found')

  return formInstance
}

/** Returns the form instance from the context, or null if the context is not available. */
// eslint-disable-next-line react-refresh/only-export-components
export function useOptionalFormContext<
  Schema extends types.TSchema,
  Form extends FormInstanceValidated<Schema> | undefined,
>(form?: Form) {
  // eslint-disable-next-line no-restricted-syntax
  return (form ?? use(FormContext)?.form ?? null) as FormInstance<Schema> | null
}
