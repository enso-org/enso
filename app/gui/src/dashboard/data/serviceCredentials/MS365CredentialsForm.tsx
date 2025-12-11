/**
 * @file
 * Dialog for an MS365 credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Checkbox } from '#/components/Checkbox'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Selector } from '#/components/Inputs/Selector/Selector'
import { Text } from '#/components/Text'
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
        scopes: ['User.Read'],
        filesPermission: 'Files.ReadWrite.All',
        sitesPermission: 'Sites.Read.All',
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
          <Input form={form} name="name" label={getText('name')} defaultValue="Microsoft365" />
          <Checkbox.Group form={form} name="scopes" label={getText('ms365CredentialScopes')}>
            <Checkbox value="User.Read">{getText('ms365CredentialUserReadScope')}</Checkbox>
          </Checkbox.Group>
          <Selector
            form={form}
            name="filesPermission"
            label={getText('ms365CredentialFilesPermission')}
            items={[
              'Files.ReadWrite.All',
              'Files.Read.All',
              'Files.ReadWrite',
              'Files.Read',
              'NoAccess',
            ] as const}
          >
            {(item) => {
              const key = `ms365CredentialFilesPermission${item.replace(/\./g, '')}` as any
              return getText(key)
            }}
          </Selector>
          <Text variant="body" color="primary">
            {getText(
              `ms365CredentialFilesPermission${form.watch('filesPermission').replace(/\./g, '')}Description` as any,
            )}
          </Text>
          <Selector
            form={form}
            name="sitesPermission"
            label={getText('ms365CredentialSitesPermission')}
            items={['Sites.Manage.All', 'Sites.ReadWrite.All', 'Sites.Read.All', 'NoAccess'] as const}
          >
            {(item) => {
              const key = `ms365CredentialSitesPermission${item.replace(/\./g, '')}` as any
              return getText(key)
            }}
          </Selector>
          <Text variant="body" color="primary">
            {getText(
              `ms365CredentialSitesPermission${form.watch('sitesPermission').replace(/\./g, '')}Description` as any,
            )}
          </Text>
          <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
        </>
      )}
    </Form>
  )
}
