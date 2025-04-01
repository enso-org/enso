/**
 * @file Definitions for the Snowflake credentials integration.
 */
import type { CredentialMetadata, SecretId } from "#/services/Backend"
import { getOauthCallbackPath } from "#/services/remoteBackendPaths"

/**
 * TODO
 * @throws foo
 */
export function makeAuthUrl(secretId: SecretId, metadata: CredentialMetadata): string {
  if (metadata.input.type !== 'Snowflake') {
    // TODO can we check statically
    throw new Error("Invalid credential type")
  }
  const account = metadata.input.account
  const nonce = metadata.nonce
  const state = btoa(JSON.stringify({ secretId, nonce }))
  const role = metadata.input.role
  const scope = "refresh_token" + (role == null ? "" : " session:role:" + role)
  const query = new URLSearchParams({
    /* eslint-disable @typescript-eslint/naming-convention, camelcase */
    client_id: metadata.input.client_id,
    response_type: 'code',
    redirect_uri: getOauthCallbackPath('Snowflake'),
    state,
    scope
    /* eslint-enable @typescript-eslint/naming-convention, camelcase */
  })
  const url = `https://${encodeURIComponent(account)}.snowflakecomputing.com/oauth/authorize?${query.toString()}`
  return url
}
