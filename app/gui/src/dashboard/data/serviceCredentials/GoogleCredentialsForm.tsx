/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox, Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { z } from 'zod'
import { CredentialsFormButtons } from './CredentialsFormButtons'

const FORM_SCHEMA = z.object({
  title: z.string(),
  scopes: z.array(z.string()),
})

/** Dialog for a Google credential. */
export function GoogleCredentialsForm() {
  const { getText } = useText()
  
  const form = Form.useForm({
    method: 'dialog',
    schema: FORM_SCHEMA,
    onSubmit: (values) => {
      console.log(values)
    },
  })

  /*
  <Checkbox.Group form={form} name="scopes">
        <Checkbox value="sheets">{getText('googleCredentialSheetsScope')}</Checkbox>
        <Checkbox value="analytics">{getText('googleCredentialAnalyticsScope')}</Checkbox>
      </Checkbox.Group>*/

  return (
    <Form form={form} className="w-full">
      <Input form={form} name="title" label={getText('name')} />
      
      <CredentialsFormButtons isCreating={true} canCancel={false} canReset={false} />
    </Form>
  )
}
