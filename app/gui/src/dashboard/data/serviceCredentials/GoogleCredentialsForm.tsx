/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox, Form, Input } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { z } from 'zod'
import { CredentialsFormButtons } from './CredentialsFormButtons'
import * as i18n from 'enso-common/src/text'

const FORM_SCHEMA = z.object({
  title: z.string().min(1),
  scopes: z.array(z.string()).refine((scopes) => scopes.length > 0, {message: i18n.getText(i18n.resolveDictionary(), 'googleCredentialScopesEmptyError')}),
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

  return (
    <Form form={form} className="w-full">
      <Input form={form} name="title" label={getText('name')} />
      <Checkbox.Group form={form} name="scopes" label={getText('googleCredentialScopes')} defaultValue={["sheets"]}>
        <Checkbox value="sheets">{getText('googleCredentialSheetsScope')}</Checkbox>
        <Checkbox value="analytics">{getText('googleCredentialAnalyticsScope')}</Checkbox>
      </Checkbox.Group>      
      <CredentialsFormButtons isCreating={true} canCancel={false} canReset={false} />
    </Form>
  )
}
