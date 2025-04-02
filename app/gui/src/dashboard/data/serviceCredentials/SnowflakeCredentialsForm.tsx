/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Form, Input } from '#/components/AriaComponents'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '#/providers/TextProvider'
import { CredentialsFormButtons } from './CredentialsFormButtons'
import * as snowflake from './snowflake'
import type { CredentialFormProps } from './types'

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  const form = Form.useForm({
    method: 'dialog',
    schema: snowflake.FORM_SCHEMA,
    onSubmit: async (values) => {
      try {
        await snowflake.submitForm(createCredentials, values)
      } catch (error) {
        toastAndLog(null, error)
      }
    },
  })

  return (
    <Form form={form} className="w-full">
      <Input form={form} name="name" label={getText('name')} />
      <Input form={form} name="account" label={getText('snowflakeCredentialAccount')} />
      <Input
        form={form}
        name="clientId"
        label={getText('snowflakeCredentialClientId')}
        autoComplete="off"
      />
      <Input
        form={form}
        name="clientSecret"
        label={getText('snowflakeCredentialClientSecret')}
        type="password"
        autoComplete="new-password"
      />
      <Input form={form} name="role" label={getText('snowflakeCredentialRole')} />
      <CredentialsFormButtons isCreating={true} canCancel={false} canReset={false} />
    </Form>
  )
}
