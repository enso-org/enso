/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this file is re-exported by `./index.ts`.
 */

import { ButtonGroup, DialogDismiss, Form } from '#/components/AriaComponents'
import { useSynchronizeCredentialsValue } from '#/data/serviceCredentials/hooks'
import { useText } from '#/providers/TextProvider'
import type { CredentialsDialogProps } from './types'

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsDialog(props: CredentialsDialogProps) {
  const { value, isCreating, canCancel, canReset, upsertCredential } = props

  const { getText } = useText()
  const form = Form.useFormWithComponents({
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
    onSubmit: async (values) => {
      await upsertCredential(values)
    },
  })
  useSynchronizeCredentialsValue(form.form, value)

  return (
    <form.Form>
      {/* `name` field is pre-filtered to only fields with a matching type! */}
      <form.Input name="account" label={getText('account')} />
      <form.Input name="clientId" label={getText('clientId')} />
      <form.Input name="clientSecret" label={getText('clientSecret')} />
      <form.Input name="role" label={getText('role')} />
      <ButtonGroup className="mt-2">
        <Form.Submit>{isCreating ? getText('create') : getText('update')}</Form.Submit>
        {canCancel && <DialogDismiss />}
        {canReset && <Form.Reset>{getText('cancel')}</Form.Reset>}
      </ButtonGroup>
    </form.Form>
  )
}
