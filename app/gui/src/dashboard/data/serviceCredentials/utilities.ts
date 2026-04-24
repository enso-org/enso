/** @file Hooks for credentials dialogs. */
import type { Opt } from '@/util/data/opt'
import type * as backend from 'enso-common/src/services/Backend'
import { getOauthCallbackPath } from 'enso-common/src/services/Backend/remoteBackendPaths'
import invariant from 'tiny-invariant'

/**
 * Returns the redirect URI for the given service.
 */
export function getOauthRedirectUri(
  apiUrl: Opt<string>,
  service: backend.CredentialInput['type'],
): string {
  invariant(apiUrl != null, 'The API_URL must be defined')

  const path = getOauthCallbackPath(service)
  const separator = apiUrl.endsWith('/') ? '' : '/'
  return apiUrl + separator + path
}
