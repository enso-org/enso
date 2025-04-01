/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox, Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { CredentialsFormButtons } from './CredentialsFormButtons'
import { FORM_SCHEMA, submitForm } from './google'
import type { CredentialFormProps } from './types'


/** Dialog for a Google credential. */
export function GoogleCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  
  const form = Form.useForm({
    method: 'dialog',
    schema: FORM_SCHEMA,
    onSubmit: (values) => submitForm(createCredentials, values),
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
