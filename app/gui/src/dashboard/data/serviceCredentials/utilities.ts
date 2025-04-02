/** @file Hooks for credentials dialogs. */
import type { TSchema, UseFormReturn } from '#/components/AriaComponents'
import { useEffect } from 'react'
import type * as backend from '#/services/Backend'
import invariant from 'tiny-invariant'
import { getOauthCallbackPath } from '#/services/remoteBackendPaths'

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

/**
 * Returns the redirect URI for the given service.
 */
export function getOauthRedirectUri(service: backend.CredentialInput['type']): string {
  const apiUrl = $config.API_URL
  invariant(apiUrl !== undefined, "The API_URL must be defined")

  const path = getOauthCallbackPath(service)
  const separator = apiUrl.endsWith("/") ? "" : "/"
  return apiUrl + separator + path
}
