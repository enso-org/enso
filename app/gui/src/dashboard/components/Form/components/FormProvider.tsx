/**
 * @file
 *
 * Context that injects form instance into the component tree.
 */
import { createContext, useContext } from 'react'
import { FormProvider as ReactHookFormProvider } from 'react-hook-form'
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
  props: FormContextType<Schema> & {
    children: React.ReactNode | ((form: types.UseFormReturn<Schema>) => React.ReactNode)
  },
) {
  const { children, form } = props

  // eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any
  const narrowedForm = form as types.UseFormReturn<any>

  return (
    <ReactHookFormProvider {...narrowedForm}>
      <FormContext.Provider value={{ form: narrowedForm }}>
        {typeof children === 'function' ? children(form) : children}
      </FormContext.Provider>
    </ReactHookFormProvider>
  )
}

/** Returns the form instance from the context. */
// eslint-disable-next-line react-refresh/only-export-components
export function useFormContext<Schema extends types.TSchema>(
  form?: FormInstanceValidated<Schema>,
): FormInstance<Schema> {
  if (form != null && 'control' in form) {
    return form
  } else {
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const ctx = useContext(FormContext)

    invariant(ctx, 'FormContext not found')

    // This is safe, as we pass the value transparently and it is typed outside
    // eslint-disable-next-line no-restricted-syntax
    return ctx.form as unknown as types.UseFormReturn<Schema>
  }
}

/** Returns the form instance from the context, or null if the context is not available. */
// eslint-disable-next-line react-refresh/only-export-components
export function useOptionalFormContext<
  Form extends FormInstanceValidated<Schema> | undefined,
  Schema extends types.TSchema,
>(form?: Form): Form extends undefined ? FormInstance<Schema> | null : FormInstance<Schema> {
  try {
    return useFormContext<Schema>(form)
  } catch {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    return null!
  }
}
