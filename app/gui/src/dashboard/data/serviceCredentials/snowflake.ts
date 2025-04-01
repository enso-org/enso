/**
 * @file Definitions for the Snowflake credentials integration.
 */
import type { SecretId, SnowflakeCredentialInput } from "#/services/Backend";
import { z } from "zod";
import type { CredentialRecipe } from "./types";
import { getOauthCallbackPath } from "#/services/remoteBackendPaths";

export const FORM_SCHEMA = z.object({
  title: z.string().min(1),
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
      /* eslint-disable @typescript-eslint/naming-convention, camelcase */
      type: 'Snowflake',
      account,
      client_id: values.clientId,
      client_secret: values.clientSecret,
      role,
      /* eslint-enable @typescript-eslint/naming-convention, camelcase */
    }
    return createCredentials({
      title: values.title,
      input,
      makeAuthUrl: (secretId: SecretId, nonce: string) => {
        const state = btoa(JSON.stringify({ secretId, nonce }))
        const scope = "refresh_token" + (role == null ? "" : " session:role:" + role)
        const query = new URLSearchParams({
          /* eslint-disable @typescript-eslint/naming-convention, camelcase */
          client_id: values.clientId,
          response_type: 'code',
          redirect_uri: getOauthCallbackPath('Snowflake'),
          state,
          scope
          /* eslint-enable @typescript-eslint/naming-convention, camelcase */
        })
        return `https://${encodeURIComponent(account)}.snowflakecomputing.com/oauth/authorize?${query.toString()}`
      }
    })
  }
