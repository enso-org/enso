/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this file is re-exported by `./index.ts`.
 */

import { Form as FormComponent } from '#/components/AriaComponents'
import { CredentialsFormButtons } from '#/data/serviceCredentials/CredentialsFormButtons'
import { useSynchronizeCredentialsValue } from '#/data/serviceCredentials/utilities'
import { useText } from '#/providers/TextProvider'
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
      await upsertCredential(formValue, (id) => {
        const query = new URLSearchParams({
          /* eslint-disable @typescript-eslint/naming-convention, camelcase */
          response_type: 'code',
          access_type: 'offline',
          prompt: 'consent',
          redirect_uri: $config.CLOUD_EXTERNAL_SERVICE_OAUTH_CALLBACK,
          client_id: $config.ENSO_GOOGLE_OAUTH_CLIENT_ID,
          state: id,
          /* eslint-enable @typescript-eslint/naming-convention, camelcase */
        })
        for (const scope of formValue.scopes) {
          query.append('scope', scope)
        }
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
