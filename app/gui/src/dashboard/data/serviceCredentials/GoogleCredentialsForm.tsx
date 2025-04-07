/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox, Form, Input } from '#/components/AriaComponents'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '#/providers/TextProvider'
import { CredentialsFormFooter } from './CredentialsFormFooter'
import * as google from './google'
import type { CredentialFormProps } from './types'

/** Dialog for a Google credential. */
export function GoogleCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  return (
    <Form
      method="dialog"
      schema={google.FORM_SCHEMA}
      defaultValues={{
        scopes: ['sheets'],
      }}
      className="w-full"
      onSubmit={async (values) => {
        try {
          await google.submitForm(createCredentials, values)
        } catch (error) {
          toastAndLog(null, error)
        }
      }}
    >
      {(form) => (
        <>
          <Input form={form} name="name" label={getText('name')} />
          <Checkbox.Group form={form} name="scopes" label={getText('googleCredentialScopes')}>
            <Checkbox value="sheets">{getText('googleCredentialSheetsScope')}</Checkbox>
            <Checkbox value="analytics">{getText('googleCredentialAnalyticsScope')}</Checkbox>
          </Checkbox.Group>
          <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
        </>
      )}
    </Form>
  )
}
