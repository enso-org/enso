/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { CredentialsFormButtons } from './CredentialsFormButtons'
import type { CredentialFormProps } from './types'
import { FORM_SCHEMA, submitForm } from './snowflake'
import { useToastAndLog } from '#/hooks/toastAndLogHooks';

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  const form = Form.useForm({
    method: 'dialog',
    schema: FORM_SCHEMA,
    onSubmit: async (values) => {
      try {
        await submitForm(createCredentials, values)
      } catch (error) {
        toastAndLog(null, error)
      }
    },
  })

  return (
    <Form form={form} className="w-full">
      <Input form={form} name="name" label={getText('name')} />
      <Input form={form} name="account" label={getText('account')} />
      <Input form={form} name="clientId" label={getText('clientId')} />
      <Input form={form} name="clientSecret" label={getText('clientSecret')} />
      <Input form={form} name="role" label={getText('role')} />
      <CredentialsFormButtons isCreating={true} canCancel={false} canReset={false} />
    </Form>
  )
}
