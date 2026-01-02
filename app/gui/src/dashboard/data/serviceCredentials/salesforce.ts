/** @file Definitions for the Salesforce credentials integration. */
import type { SalesforceCredentialInput, SecretId } from 'enso-common/src/services/Backend'
import * as i18n from 'enso-common/src/text'
import invariant from 'tiny-invariant'
import { z } from 'zod'
import type { CredentialRecipe } from './types'
import { getOauthRedirectUri } from './utilities'

// Salesforce OAuth scopes required by the integration.
const EXTRA_SCOPES = ['api', 'refresh_token', 'offline_access', 'openid']
const SALESFORCE_OAUTH_AUTHORIZE_URL = 'https://login.salesforce.com/services/oauth2/authorize'

export const FORM_SCHEMA = z.object({
  name: z.string().min(1),
  scopes: z.array(z.string()).refine((scopes) => scopes.length > 0, {
    message: i18n.getText(i18n.resolveDictionary(), 'salesforceCredentialScopesEmptyError'),
  }),
})

export const DEFAULT_FORM_VALUES: z.infer<typeof FORM_SCHEMA> = {
  name: 'Salesforce',
  scopes: EXTRA_SCOPES,
}

/**
 * The logic for submitting the Salesforce credential form.
 */
export function submitForm(
  createCredentials: (recipe: CredentialRecipe) => Promise<void>,
  values: z.infer<typeof FORM_SCHEMA>,
): Promise<void> {
  invariant($config.SALESFORCE_OAUTH_CLIENT_ID != null, 'Salesforce OAuth client id is missing')
  const salesforceOauthClientId = $config.SALESFORCE_OAUTH_CLIENT_ID

  const valuesWithDefaults = { ...DEFAULT_FORM_VALUES, ...values }

  const oauthScopes: string[] = [...new Set([...EXTRA_SCOPES, ...valuesWithDefaults.scopes])]
  const input: SalesforceCredentialInput = {
    type: 'Salesforce',
    scopes: oauthScopes,
  }
  return createCredentials({
    name: valuesWithDefaults.name,
    input,
    makeAuthUrl: (secretId: SecretId, nonce: string) => {
      const state = btoa(JSON.stringify({ secretId, nonce }))
      const scope = oauthScopes.join(' ')
      const query = new URLSearchParams({
        /* eslint-disable @typescript-eslint/naming-convention, camelcase */
        client_id: salesforceOauthClientId,
        redirect_uri: getOauthRedirectUri('Salesforce'),
        response_type: 'code',
        response_mode: 'query',
        state,
        scope,
        /* eslint-enable @typescript-eslint/naming-convention, camelcase */
      })
      return `${SALESFORCE_OAUTH_AUTHORIZE_URL}?${query.toString()}`
    },
  })
}
