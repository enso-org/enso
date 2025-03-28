/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Form as FormComponent } from '#/components/AriaComponents'
import { CredentialsFormButtons } from '#/data/serviceCredentials/CredentialsFormButtons'
import { useSynchronizeCredentialsValue } from '#/data/serviceCredentials/utilities'
import { useText } from '#/providers/TextProvider'
import { getOauthCallbackPath } from '#/services/remoteBackendPaths'
import { uuidv4 } from 'lib0/random.js'
import type { CredentialsFormProps } from './types'

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsForm(props: CredentialsFormProps) {
  const { value, upsertCredential, ...buttonsProps } = props

  const { getText } = useText()
  const { form, Form, Input } = FormComponent.useFormWithComponents({
    mode: 'onChange',
    schema: (z) =>
      z
        .object({
          account: z.string(),
          // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
          client_id: z.string(),
          // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
          client_secret: z.string(),
          role: z.string().nullable(),
        })
        .refine((obj) => {
          const { role, ...rest } = obj
          return { ...rest, ...(role !== '' ? { role } : { role: null }) }
        }),
    onSubmit: async (formValue) => {
      const nonce = uuidv4()
      await upsertCredential({ input: { type: 'Snowflake', ...formValue }, nonce }, (id) => {
        const account = formValue.account
        const state = btoa(JSON.stringify({ secretId: id, nonce }))
        const scope = "refresh_token" + (formValue.role == null ? "" : " session:role:" + formValue.role)
        const query = new URLSearchParams({
          /* eslint-disable @typescript-eslint/naming-convention, camelcase */
          client_id: formValue.client_id,
          response_type: 'code',
          redirect_uri: getOauthCallbackPath('Snowflake'),
          state,
          scope
          /* eslint-enable @typescript-eslint/naming-convention, camelcase */
        })
        const url = `https://${encodeURIComponent(account)}.snowflakecomputing.com/oauth/authorize?${query.toString()}`
        return url
      })
    },
  })
  useSynchronizeCredentialsValue(form, value)

  return (
    <Form className="w-full">
      {/* `name` field is pre-filtered to only fields with a matching type! */}
      <Input name="account" label={getText('account')} />
      <Input name="client_id" label={getText('clientId')} />
      <Input name="client_secret" label={getText('clientSecret')} />
      <Input name="role" label={getText('role')} />
      <CredentialsFormButtons {...buttonsProps} />
    </Form>
  )
}
