/** @file Definitions for the Salesforce credentials integration. */
import type { SalesforceCredentialInput, SecretId } from 'enso-common/src/services/Backend'
import * as i18n from 'enso-common/src/text'
import invariant from 'tiny-invariant'
import { z } from 'zod'
import type { CredentialRecipe } from './types'
import { getOauthRedirectUri } from './utilities'

const EXTRA_SCOPES = ['openid', 'profile', 'offline_access']
const SALESFORCE_OAUTH_AUTHORIZE_URL = 'https://login.salesforce.com/services/oauth2/authorize'

export const FORM_SCHEMA = z.object({
  name: z.string().min(1),
  scopes: z.array(z.string()).refine((scopes) => scopes.length > 0, {
    message: i18n.getText(i18n.resolveDictionary(), 'salesforceCredentialScopesEmptyError'),
  }),
  filesPermission: z.enum([
    'Files.ReadWrite.All',
    'Files.Read.All',
    'Files.ReadWrite',
    'Files.Read',
    'NoAccess',
  ]),
  sitesPermission: z.enum([
    'Sites.Read.All',
    'Sites.ReadWrite.All',
    'Sites.Manage.All',
    'NoAccess',
  ]),
})

export type SalesforceFormValues = z.infer<typeof FORM_SCHEMA>

export const DEFAULT_FORM_VALUES: SalesforceFormValues = {
  name: 'Salesforce',
  scopes: ['User.Read'],
  filesPermission: 'Files.ReadWrite.All',
  sitesPermission: 'NoAccess',
}

/**
 * The logic for submitting the Salesforce credential form.
 */
export function submitForm(
  createCredentials: (recipe: CredentialRecipe) => Promise<void>,
  values: SalesforceFormValues,
): Promise<void> {
  invariant($config.SALESFORCE_OAUTH_CLIENT_ID != null, 'Salesforce OAuth client id is missing')
  const salesforceOauthClientId = $config.SALESFORCE_OAUTH_CLIENT_ID

  const valuesWithDefaults = { ...DEFAULT_FORM_VALUES, ...values }

  const permissions = [valuesWithDefaults.filesPermission, valuesWithDefaults.sitesPermission].filter(
    (permission) => permission !== 'NoAccess',
  )
  const oauthScopes: string[] = [...EXTRA_SCOPES, ...valuesWithDefaults.scopes, ...permissions]
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
