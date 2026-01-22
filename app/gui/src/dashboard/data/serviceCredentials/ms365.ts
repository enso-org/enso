/** @file Definitions for the MS365 credentials integration. */
import type { MS365CredentialInput, SecretId } from 'enso-common/src/services/Backend'
import * as i18n from 'enso-common/src/text'
import invariant from 'tiny-invariant'
import { z } from 'zod'
import type { CredentialRecipe } from './types'
import { getOauthRedirectUri } from './utilities'

const EXTRA_SCOPES = ['openid', 'profile', 'offline_access']

export const FORM_SCHEMA = z.object({
  name: z.string().min(1),
  scopes: z.array(z.string()).refine((scopes) => scopes.length > 0, {
    message: i18n.getText(i18n.resolveDictionary(), 'ms365CredentialScopesEmptyError'),
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

/**
 * The logic for submitting the MS365 credential form.
 */
export function submitForm(
  createCredentials: (recipe: CredentialRecipe) => Promise<void>,
  values: z.infer<typeof FORM_SCHEMA>,
): Promise<void> {
  invariant($config.MS365_OAUTH_CLIENT_ID != null, 'MS365 OAuth client id is missing')
  const ms365OauthClientId = $config.MS365_OAUTH_CLIENT_ID

  const permissions = [values.filesPermission, values.sitesPermission].filter(
    (permission) => permission !== 'NoAccess',
  )
  const oauthScopes: string[] = [...EXTRA_SCOPES, ...values.scopes, ...permissions]
  const input: MS365CredentialInput = {
    type: 'MS365',
    scopes: oauthScopes,
  }
  return createCredentials({
    name: values.name,
    input,
    makeAuthUrl: (secretId: SecretId, nonce: string) => {
      const state = btoa(JSON.stringify({ secretId, nonce }))
      const scope = oauthScopes.join(' ')
      const query = new URLSearchParams({
        /* eslint-disable @typescript-eslint/naming-convention, camelcase */
        client_id: ms365OauthClientId,
        redirect_uri: getOauthRedirectUri('MS365'),
        response_type: 'code',
        response_mode: 'query',
        state,
        scope,
        /* eslint-enable @typescript-eslint/naming-convention, camelcase */
      })
      return `https://login.microsoftonline.com/common/oauth2/v2.0/authorize?${query.toString()}`
    },
  })
}
