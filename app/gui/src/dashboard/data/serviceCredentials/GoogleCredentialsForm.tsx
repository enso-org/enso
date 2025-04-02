/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox, Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { CredentialsFormButtons } from './CredentialsFormButtons'
import * as google from './google'
import type { CredentialFormProps } from './types'
import { useToastAndLog } from '#/hooks/toastAndLogHooks';

/** Dialog for a Google credential. */
export function GoogleCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  
  const form = Form.useForm({
    method: 'dialog',
    schema: google.FORM_SCHEMA,
    onSubmit: async (values) => {
      try {
        await google.submitForm(createCredentials, values)
      } catch (error) {
        toastAndLog(null, error)
      }
    },
  })

  return (
    <Form form={form} className="w-full">
      <Input form={form} name="name" label={getText('name')} />
      <Checkbox.Group form={form} name="scopes" label={getText('googleCredentialScopes')} defaultValue={["sheets"]}>
        <Checkbox value="sheets">{getText('googleCredentialSheetsScope')}</Checkbox>
        <Checkbox value="analytics">{getText('googleCredentialAnalyticsScope')}</Checkbox>
      </Checkbox.Group>      
      <CredentialsFormButtons isCreating={true} canCancel={false} canReset={false} />
    </Form>
  )
}
