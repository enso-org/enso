/** @file Hooks for credentials dialogs. */
import type { TSchema, UseFormReturn } from '#/components/AriaComponents'
import { useEffect } from 'react'

/** Keep the form's value in sync with the actual state. */
export function useSynchronizeCredentialsValue<Schema extends TSchema>(
  form: UseFormReturn<Schema>,
  value: unknown,
) {
  useEffect(() => {
    const result = form.schema.safeParse(value)
    if (result.success) {
      // This is SAFE, as the shape of the data is validated by `form.schema.safeParse` above.
      // This would not be a type error in non-generic code.
      // eslint-disable-next-line no-restricted-syntax
      form.reset(result.data as never)
    }
  }, [form, value])
}
