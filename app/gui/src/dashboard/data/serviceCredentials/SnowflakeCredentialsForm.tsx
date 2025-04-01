/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { z } from 'zod'
import { CredentialsFormButtons } from './CredentialsFormButtons'

const FORM_SCHEMA = z.object({
  title: z.string().min(1),
  account: z.string().min(1),
  clientId: z.string().min(1),
  clientSecret: z.string().min(1),
  role: z.string().nullable().transform((s) => s == null || s.length === 0 ? null : s)
})

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsForm() {
  const { getText } = useText()

  const form = Form.useForm({
    method: 'dialog',
    schema: FORM_SCHEMA,
    onSubmit: (values) => {
      console.log(values)
    },
  })

  return (
    <Form form={form} className="w-full">
      <Input form={form} name="title" label={getText('name')} />
      <Input form={form} name="account" label={getText('account')} />
      <Input form={form} name="clientId" label={getText('clientId')} />
      <Input form={form} name="clientSecret" label={getText('clientSecret')} />
      <Input form={form} name="role" label={getText('role')} />
      <CredentialsFormButtons isCreating={true} canCancel={false} canReset={false} />
    </Form>
  )
}
