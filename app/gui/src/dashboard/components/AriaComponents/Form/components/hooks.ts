/** @file Hooks for `Form`. */
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { FormContext } from './constants'
import type { FormInstance, FormInstanceValidated, TSchema, UseFormReturn } from './types'

/** Returns the form instance from the context. */
export function useFormContext<Schema extends TSchema>(
  form?: FormInstanceValidated<Schema>,
): FormInstance<Schema> {
  if (form != null && 'control' in form) {
    return form
  } else {
    // eslint-disable-next-line react-compiler/react-compiler
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const ctx = useContext(FormContext)

    invariant(ctx, 'FormContext not found')

    // This is safe, as we pass the value transparently and it is typed outside
    // eslint-disable-next-line no-restricted-syntax
    return ctx.form as unknown as UseFormReturn<Schema>
  }
}

/** Returns the form instance from the context, or null if the context is not available. */
export function useOptionalFormContext<
  Form extends FormInstanceValidated<Schema> | undefined,
  Schema extends TSchema,
>(form?: Form): Form extends undefined ? FormInstance<Schema> | null : FormInstance<Schema> {
  try {
    // eslint-disable-next-line react-compiler/react-compiler
    return useFormContext<Schema>(form)
  } catch {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    return null!
  }
}
