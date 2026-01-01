/**
 * @file
 * Dialog for a Salesforce credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox } from '#/components/Checkbox'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Selector } from '#/components/Inputs/Selector/Selector'
import { Text } from '#/components/Text'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '$/providers/react'
import type { TextId } from 'enso-common/src/text'
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
        scopes: ['User.Read'],
        filesPermission: 'Files.ReadWrite.All',
        sitesPermission: 'NoAccess',
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
      {(form) => {
        const filesPermission = form.watch('filesPermission')
        const sitesPermission = form.watch('sitesPermission')

        return (
          <>
            <Input form={form} name="name" label={getText('name')} defaultValue="Salesforce" />
            <Checkbox.Group form={form} name="scopes" label={getText('salesforceCredentialScopes')}>
              <Checkbox value="User.Read">{getText('salesforceCredentialUserReadScope')}</Checkbox>
            </Checkbox.Group>
            <Selector
              form={form}
              name="filesPermission"
              label={getText('salesforceCredentialFilesPermission')}
              items={
                [
                  'Files.ReadWrite.All',
                  'Files.Read.All',
                  'Files.ReadWrite',
                  'Files.Read',
                  'NoAccess',
                ] as const
              }
            >
              {(item) => {
                // eslint-disable-next-line no-restricted-syntax
                const key = `salesforceCredentialFilesPermission${item.replace(/\./g, '')}` as TextId
                return getText(key)
              }}
            </Selector>
            <Text variant="body" color="primary">
              {(() => {
                const key =
                  // eslint-disable-next-line no-restricted-syntax
                  `salesforceCredentialFilesPermission${filesPermission.replace(/\./g, '')}Description` as TextId
                return getText(key)
              })()}
            </Text>
            <Selector
              form={form}
              name="sitesPermission"
              label={getText('salesforceCredentialSitesPermission')}
              items={
                ['Sites.Manage.All', 'Sites.ReadWrite.All', 'Sites.Read.All', 'NoAccess'] as const
              }
            >
              {(item) => {
                // eslint-disable-next-line no-restricted-syntax
                const key = `salesforceCredentialSitesPermission${item.replace(/\./g, '')}` as TextId
                return getText(key)
              }}
            </Selector>
            <Text variant="body" color="primary">
              {(() => {
                const key =
                  // eslint-disable-next-line no-restricted-syntax
                  `salesforceCredentialSitesPermission${sitesPermission.replace(/\./g, '')}Description` as TextId
                return getText(key)
              })()}
            </Text>
            <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
          </>
        )
      }}
    </Form>
  )
}
