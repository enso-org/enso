/** @file Hooks for credentials dialogs. */
import { API_URL } from '#/appUtils'
import type * as backend from '#/services/Backend'
import { getOauthCallbackPath } from '#/services/remoteBackendPaths'
import invariant from 'tiny-invariant'

/**
 * Returns the redirect URI for the given service.
 */
export function getOauthRedirectUri(service: backend.CredentialInput['type']): string {
  invariant(API_URL !== undefined, 'The API_URL must be defined')

  const path = getOauthCallbackPath(service)
  const separator = API_URL.endsWith('/') ? '' : '/'

  return API_URL + separator + path
}
