/**
 * @file Definitions for the Google credentials integration.
 */
import invariant from 'tiny-invariant'

import type { CredentialMetadata, GoogleCredentialInput, SecretId } from "#/services/Backend"
import { getOauthCallbackPath } from "#/services/remoteBackendPaths"
import { z, type ZodType } from 'zod'

export const SCHEMA = z.object({
  type: z.literal('Google'),
  scopes: z.array(z.string())
}) satisfies ZodType<GoogleCredentialInput>

/**
 * TODO
 * @throws foo
 */
export function makeAuthUrl(secretId: SecretId, metadata: CredentialMetadata): string {
  if (metadata.input.type !== 'Google') {
    // TODO can we check statically
    throw new Error("Invalid credential type")
  }

  invariant($config.GOOGLE_OAUTH_CLIENT_ID != null, 'Google OAuth client id is missing')
  const nonce = metadata.nonce
  const state = btoa(JSON.stringify({ secretId, nonce }))
  const scope = metadata.input.scopes.join(' ')
  const query = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention, camelcase */
      response_type: 'code',
      access_type: 'offline',
      prompt: 'consent',
      redirect_uri: getOauthCallbackPath('Google'),
      client_id: $config.GOOGLE_OAUTH_CLIENT_ID,
      state,
      scope
      /* eslint-enable @typescript-eslint/naming-convention, camelcase */
  })
  return `https://accounts.google.com/o/oauth2/v2/auth?${query.toString()}`
}
