/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this file is re-exported by `./index.ts`.
 */

import { Form as FormComponent } from '#/components/AriaComponents'
import { CredentialsFormButtons } from '#/data/serviceCredentials/CredentialsFormButtons'
import { useSynchronizeCredentialsValue } from '#/data/serviceCredentials/hooks'
import { useText } from '#/providers/TextProvider'
import type { CredentialsDialogProps } from './types'

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsDialog(props: CredentialsDialogProps) {
  const { value, upsertCredential, ...buttonsProps } = props

  const { getText } = useText()
  const { form, Form, Input } = FormComponent.useFormWithComponents({
    mode: 'onChange',
    schema: (z) =>
      z
        .object({
          account: z.string(),
          clientId: z.string(),
          clientSecret: z.string(),
          role: z.string(),
        })
        .refine((obj) => {
          const { role, ...rest } = obj
          return { ...rest, ...(role !== '' ? { role } : {}) }
        }),
    onSubmit: upsertCredential,
  })
  useSynchronizeCredentialsValue(form, value)

  return (
    <Form className="w-full">
      {/* `name` field is pre-filtered to only fields with a matching type! */}
      <Input name="account" label={getText('account')} />
      <Input name="clientId" label={getText('clientId')} />
      <Input name="clientSecret" label={getText('clientSecret')} />
      <Input name="role" label={getText('role')} />
      <CredentialsFormButtons {...buttonsProps} />
    </Form>
  )
}
