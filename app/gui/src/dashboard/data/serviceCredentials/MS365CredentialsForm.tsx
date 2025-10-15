/**
 * @file
 * Dialog for an MS365 credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox } from '#/components/Checkbox'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '$/providers/react'
import { CredentialsFormFooter } from './CredentialsFormFooter'
import * as ms365 from './ms365'
import type { CredentialFormProps } from './types'

/** Dialog for a MS365 credential. */
export function MS365CredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  return (
    <Form
      method="dialog"
      schema={ms365.FORM_SCHEMA}
      defaultValues={{
        scopes: ['User.Read', 'Files.Read'],
      }}
      className="w-full"
      onSubmit={async (values) => {
        try {
          await ms365.submitForm(createCredentials, values)
        } catch (error) {
          toastAndLog(null, error)
        }
      }}
    >
      {(form) => (
        <>
          <Input form={form} name="name" label={getText('name')} />
          <Checkbox.Group form={form} name="scopes" label={getText('ms365CredentialScopes')}>
            <Checkbox value="User.Read">{getText('ms365CredentialUserReadScope')}</Checkbox>
            <Checkbox value="Files.Read">{getText('ms365CredentialFilesReadScope')}</Checkbox>
          </Checkbox.Group>
          <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
        </>
      )}
    </Form>
  )
}
