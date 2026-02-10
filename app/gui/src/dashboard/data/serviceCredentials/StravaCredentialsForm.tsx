/**
 * @file
 * Dialog for a Strava credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox } from '#/components/Checkbox'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useRemoteConfig, useText } from '$/providers/react'
import { CredentialsFormFooter } from './CredentialsFormFooter'
import * as strava from './strava'
import type { CredentialFormProps } from './types'

/** Dialog for a Strava credential. */
export function StravaCredentialsForm(props: CredentialFormProps) {
  const { createCredentials } = props
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const apiUrl = useRemoteConfig('ENSO_IDE_API_URL')
  const oauthId = useRemoteConfig('ENSO_IDE_STRAVA_OAUTH_CLIENT_ID')

  return (
    <Form
      method="dialog"
      schema={strava.FORM_SCHEMA}
      defaultValues={{
        scopes: ['read', 'activity:read'],
      }}
      className="w-full"
      onSubmit={async (values) => {
        try {
          await strava.submitForm(apiUrl, oauthId, createCredentials, values)
        } catch (error) {
          toastAndLog(null, error)
        }
      }}
    >
      {(form) => (
        <>
          <Input form={form} name="name" label={getText('name')} />
          <Checkbox.Group form={form} name="scopes" label={getText('stravaCredentialScopes')}>
            <Checkbox value="read">{getText('stravaCredentialReadScope')}</Checkbox>
            <Checkbox value="activity:read">
              {getText('stravaCredentialActivityReadScope')}
            </Checkbox>
          </Checkbox.Group>
          <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
        </>
      )}
    </Form>
  )
}
