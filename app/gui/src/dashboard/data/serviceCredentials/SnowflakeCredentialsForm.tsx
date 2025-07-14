/**
 * @file
 * Dialog for a Snowflake credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '$/providers/react'
import { CredentialsFormFooter } from './CredentialsFormFooter'
import * as snowflake from './snowflake'
import type { CredentialFormProps } from './types'

/** Dialog for a Snowflake credential. */
export function SnowflakeCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  return (
    <Form
      method="dialog"
      schema={snowflake.FORM_SCHEMA}
      className="w-full"
      onSubmit={async (values) => {
        try {
          await snowflake.submitForm(createCredentials, values)
        } catch (error) {
          toastAndLog(null, error)
        }
      }}
    >
      {(form) => (
        <>
          <Button
            variant="link"
            href="https://help.enso.org/docs/using-enso/connecting-to-snowflake#oauth-integration"
            target="_blank"
          >
            {getText('snowflakeIntegrationGetHelp')}
          </Button>
          <Input form={form} name="name" label={getText('name')} />
          <Input form={form} name="account" label={getText('snowflakeCredentialAccount')} />
          <Input
            form={form}
            name="clientId"
            label={getText('snowflakeCredentialClientId')}
            autoComplete="off"
          />
          <Input
            form={form}
            name="clientSecret"
            label={getText('snowflakeCredentialClientSecret')}
            type="password"
            autoComplete="new-password"
          />
          <Input form={form} name="role" label={getText('snowflakeCredentialRole')} />
          <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
        </>
      )}
    </Form>
  )
}
