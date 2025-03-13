/** @file Context that injects the form instance into the component tree. */
import { FormContext, type FormContextType } from './constants'
import type { TSchema, UseFormReturn } from './types'

/** Provides the form instance to the component tree. */
export function FormProvider<Schema extends TSchema>(
  props: FormContextType<Schema> & { children: React.ReactNode },
) {
  const { children, form } = props

  return (
    // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any
    <FormContext.Provider value={{ form: form as UseFormReturn<any> }}>
      {children}
    </FormContext.Provider>
  )
}
