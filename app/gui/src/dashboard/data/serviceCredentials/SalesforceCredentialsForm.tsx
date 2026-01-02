/**
 * @file
 * Dialog for a Salesforce credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Text } from '#/components/Text'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '$/providers/react'
import { CredentialsFormFooter } from './CredentialsFormFooter'
import * as salesforce from './salesforce'
import type { CredentialFormProps } from './types'

/** Dialog for a Salesforce credential. */
export function SalesforceCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  return (
    <Form
      method="dialog"
      schema={salesforce.FORM_SCHEMA}
      defaultValues={{
        ...salesforce.DEFAULT_FORM_VALUES,
        scopes: [...salesforce.DEFAULT_FORM_VALUES.scopes],
      }}
      className="w-full"
      onSubmit={async (values) => {
        try {
          await salesforce.submitForm(createCredentials, values)
        } catch (error) {
          toastAndLog(null, error)
        }
      }}
    >
      {(form) => (
        <>
          <Input
            form={form}
            name="name"
            label={getText('name')}
            defaultValue={salesforce.DEFAULT_FORM_VALUES.name}
          />
          <Text variant="body" color="primary">
            {getText('salesforceCredentialScopesSummary')}
          </Text>
          <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
        </>
      )}
    </Form>
  )
}
