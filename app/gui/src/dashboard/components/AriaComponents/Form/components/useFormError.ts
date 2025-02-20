/**
 * @file
 *
 * Hook to get the error message from the form.
 */
import { useText } from '#/providers/TextProvider'
import { useFormContext } from './FormProvider'
import type { FormInstance } from './types'

/**
 * Props for {@link useFormError}.
 */
export interface UseFormErrorProps {
  // We do not need to know the form fields.
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  readonly form?: FormInstance<any>
}

/**
 * Error type.
 */
interface Error {
  /** The type of the error, either caused by a form field or by an offline error. */
  readonly type: 'error' | 'offline'
  /** The error message. */
  readonly message: string
}

/**
 * Hook to get the error message from the form.
 */
export function useFormError(props: UseFormErrorProps) {
  const form = useFormContext(props.form)

  const { formState } = form
  const { errors } = formState
  const { getText } = useText()

  /** Get the error message. */
  const getSubmitError = (): string | null => {
    const formErrors = errors.root

    if (formErrors) {
      const submitError = formErrors.submit

      if (submitError) {
        return (
          submitError.message ??
          getText('arbitraryErrorTitle') + '. ' + getText('arbitraryErrorSubtitle')
        )
      } else {
        return null
      }
    } else {
      return null
    }
  }

  const offlineMessage = errors.root?.offline?.message ?? null
  const errorMessage = getSubmitError()

  const result: Error[] = []

  if (offlineMessage != null) {
    result.push({
      type: 'offline',
      message: offlineMessage,
    })
  }

  if (errorMessage != null) {
    result.push({
      type: 'error',
      message: errorMessage,
    })
  }

  return result
}
