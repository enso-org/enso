/**
 * @file
 *
 * This file is a wrapper around the react-hook-form useFormContext hook.
 */
import * as reactHookForm from 'react-hook-form'

/**
 * Returns the form instance from the context.
 */
export function useFormContext() {
  return reactHookForm.useFormContext()
}
