/**
 * @file Definitions for the Google credentials integration.
 */
import invariant from 'tiny-invariant'

import type { GoogleCredentialInput, SecretId } from "#/services/Backend"
import { getOauthCallbackPath } from "#/services/remoteBackendPaths"
import * as i18n from 'enso-common/src/text'
import { z } from 'zod'
import type { CredentialRecipe } from './types'


export const FORM_SCHEMA = z.object({
  name: z.string().min(1),
  scopes: z.array(z.string()).refine((scopes) => scopes.length > 0, {message: i18n.getText(i18n.resolveDictionary(), 'googleCredentialScopesEmptyError')}),
})

/**
 * Maps scopes in the form to related sets of actual OAuth scopes.
 * 
 * It is used for simplifying the user-facing form - a single user-facing feature, like "Sheets" may actually require multiple scopes to be enabled to work correctly.
 */
export const SCOPE_MAPPING = {
  "sheets": ["https://www.googleapis.com/auth/spreadsheets", "https://www.googleapis.com/auth/drive"],
  "analytics": ["https://www.googleapis.com/auth/analytics"]
}

/**
 * TODO
 */
function isValidScope(name: string): name is keyof typeof SCOPE_MAPPING {
  return Object.keys(SCOPE_MAPPING).includes(name)
}

/**
 * TODO
 */
export function submitForm(createCredentials: (recipe: CredentialRecipe) => Promise<void>, values: z.infer<typeof FORM_SCHEMA>): Promise<void> {
  invariant($config.GOOGLE_OAUTH_CLIENT_ID != null, 'Google OAuth client id is missing')
  const googleOauthClientId = $config.GOOGLE_OAUTH_CLIENT_ID
  
  const oauthScopesSet = new Set<string>()
  values.scopes.forEach((scope) => {
    invariant(isValidScope(scope), "Scopes used in the form must match ones in SCOPE_MAPPING")
    const translatedScopes: string[] = SCOPE_MAPPING[scope]
    translatedScopes.forEach((s) => oauthScopesSet.add(s))
  })
  const oauthScopes: string[] = Array.from(oauthScopesSet)
  const input: GoogleCredentialInput = {
    type: 'Google',
    scopes: oauthScopes
  }
  return createCredentials({
    name: values.name,
    input,
    makeAuthUrl: (secretId: SecretId, nonce: string) => {
      const state = btoa(JSON.stringify({ secretId, nonce }))
      const scope = oauthScopes.join(' ')
      const query = new URLSearchParams({
          /* eslint-disable @typescript-eslint/naming-convention, camelcase */
          response_type: 'code',
          access_type: 'offline',
          prompt: 'consent',
          redirect_uri: getOauthCallbackPath('Google'),
          client_id: googleOauthClientId,
          state,
          scope
          /* eslint-enable @typescript-eslint/naming-convention, camelcase */
      })
      return `https://accounts.google.com/o/oauth2/v2/auth?${query.toString()}`
    }
  })
}

