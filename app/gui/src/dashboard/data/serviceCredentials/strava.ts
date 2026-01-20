/**
 * @file Definitions for the Strava credentials integration.
 */
import invariant from 'tiny-invariant'

import type { SecretId, StravaCredentialInput } from 'enso-common/src/services/Backend'
import * as i18n from 'enso-common/src/text'
import { z } from 'zod'
import type { CredentialRecipe } from './types'
import { getOauthRedirectUri } from './utilities'

export const FORM_SCHEMA = z.object({
  name: z.string().min(1),
  scopes: z.array(z.string()).refine((scopes) => scopes.length > 0, {
    message: i18n.getText(i18n.resolveDictionary(), 'stravaCredentialScopesEmptyError'),
  }),
})

/**
 * The logic for submitting the Strava credential form.
 */
export function submitForm(
  createCredentials: (recipe: CredentialRecipe) => Promise<void>,
  values: z.infer<typeof FORM_SCHEMA>,
): Promise<void> {
  invariant($config.STRAVA_OAUTH_CLIENT_ID != null, 'Strava OAuth client id is missing')
  const stravaOauthClientId = $config.STRAVA_OAUTH_CLIENT_ID

  const oauthScopes: string[] = values.scopes
  const input: StravaCredentialInput = {
    type: 'Strava',
    scopes: oauthScopes,
  }
  return createCredentials({
    name: values.name,
    input,
    makeAuthUrl: (secretId: SecretId, nonce: string) => {
      const state = btoa(JSON.stringify({ secretId, nonce }))
      const scope = oauthScopes.join(',')
      const query = new URLSearchParams({
        /* eslint-disable @typescript-eslint/naming-convention, camelcase */
        client_id: stravaOauthClientId,
        redirect_uri: getOauthRedirectUri('Strava'),
        response_type: 'code',
        approval_prompt: 'auto',
        state,
        scope,
        /* eslint-enable @typescript-eslint/naming-convention, camelcase */
      })
      return `https://www.strava.com/oauth/authorize?${query.toString()}`
    },
  })
}
