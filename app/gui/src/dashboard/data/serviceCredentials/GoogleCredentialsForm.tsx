/**
 * @file
 * Dialog for a Google credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */
import { Checkbox } from '#/components/Checkbox'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { useText } from '$/providers/react'
import { CredentialsFormFooter } from './CredentialsFormFooter'
import * as google from './google'
import type { CredentialFormProps } from './types'

/** Dialog for a Google credential. */
export function GoogleCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()

  return (
    <Form
      method="dialog"
      schema={google.FORM_SCHEMA}
      defaultValues={{ scopes: ['sheets'] }}
      className="w-full"
      onSubmit={async (values) => {
        await google.submitForm(createCredentials, values)
      }}
    >
      <Input name="name" label={getText('name')} />

      <Checkbox.Group name="scopes" label={getText('googleCredentialScopes')}>
        <Checkbox value="sheets">{getText('googleCredentialSheetsScope')}</Checkbox>
        <Checkbox value="analytics">{getText('googleCredentialAnalyticsScope')}</Checkbox>
      </Checkbox.Group>

      <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
    </Form>
  )
}
