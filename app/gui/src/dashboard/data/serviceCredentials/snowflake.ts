/**
 * @file Definitions for the Snowflake credentials integration.
 */
import type { SecretId, SnowflakeCredentialInput } from "#/services/Backend";
import { z } from "zod";
import type { CredentialRecipe } from "./types";
import { getOauthRedirectUri } from './utilities'

export const FORM_SCHEMA = z.object({
  name: z.string().min(1),
  account: z.string().min(1),
  clientId: z.string().min(1),
  clientSecret: z.string().min(1),
  role: z.string().nullable().transform((s) => s == null || s.length === 0 ? null : s)
})

/**
 * TODO
 */
export function submitForm(createCredentials: (recipe: CredentialRecipe) => Promise<void>, values: z.infer<typeof FORM_SCHEMA>): Promise<void> {
    const account = values.account
    const role = values.role
    const input: SnowflakeCredentialInput = {
      type: 'Snowflake',
      account,
      clientId: values.clientId,
      clientSecret: values.clientSecret,
      role,
    }
    return createCredentials({
      name: values.name,
      input,
      makeAuthUrl: (secretId: SecretId, nonce: string) => {
        const state = btoa(JSON.stringify({ secretId, nonce }))
        const scope = "refresh_token" + (role == null ? "" : " session:role:" + role)
        const query = new URLSearchParams({
          /* eslint-disable @typescript-eslint/naming-convention, camelcase */
          client_id: values.clientId,
          response_type: 'code',
          redirect_uri: getOauthRedirectUri('Snowflake'),
          state,
          scope
          /* eslint-enable @typescript-eslint/naming-convention, camelcase */
        })
        return `https://${encodeURIComponent(account)}.snowflakecomputing.com/oauth/authorize?${query.toString()}`
      }
    })
  }
