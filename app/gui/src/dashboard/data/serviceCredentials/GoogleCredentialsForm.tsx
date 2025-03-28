/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Form as FormComponent } from '#/components/AriaComponents'
import { CredentialsFormButtons } from '#/data/serviceCredentials/CredentialsFormButtons'
import { useSynchronizeCredentialsValue } from '#/data/serviceCredentials/utilities'
import { useText } from '#/providers/TextProvider'
import { getOauthCallbackPath } from '#/services/remoteBackendPaths'
import { uuidv4 } from 'lib0/random.js'
import invariant from 'tiny-invariant'
import type { CredentialsFormProps } from './types'

/** Dialog for a Google credential. */
export function GoogleCredentialsDialog(props: CredentialsFormProps) {
  const { value, upsertCredential, ...buttonsProps } = props

  const { getText } = useText()
  const { form, Form, CheckboxGroup, Checkbox } = FormComponent.useFormWithComponents({
    mode: 'onChange',
    schema: (z) =>
      z.object({
        scopes: z.string().array(),
      }),
    onSubmit: async (formValue) => {
      const nonce = uuidv4()
      await upsertCredential({ input: { type: 'Google', ...formValue }, nonce }, (id) => {
        invariant($config.GOOGLE_OAUTH_CLIENT_ID != null, 'Google OAuth client id is missing')
        const state = btoa(JSON.stringify({ secretId: id, nonce }))
        const scope = formValue.scopes.join(' ')
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
      })
    },
  })
  useSynchronizeCredentialsValue(form, value)

  return (
    <Form className="w-full">
      {/* `name` field is pre-filtered to only fields with a matching type! */}
      <CheckboxGroup name="scopes">
        <Checkbox value="https://www.googleapis.com/auth/spreadsheets">
          {getText('googleCredentialSheetsScope')}
        </Checkbox>
        <Checkbox value="https://www.googleapis.com/auth/analytics">
          {getText('googleCredentialAnalyticsScope')}
        </Checkbox>
      </CheckboxGroup>
      <CredentialsFormButtons {...buttonsProps} />
    </Form>
  )
}
