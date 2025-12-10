/**
 * @file
 * Dialog for an MS365 credential.
 * Remember to ensure this component is added to `CREDENTIAL_INFOS` in `constants.ts`.
 */

import { Button } from '#/components/Button'
import { Checkbox } from '#/components/Checkbox'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Tooltip } from '#/components/Tooltip'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useText } from '$/providers/react'
import { CredentialsFormFooter } from './CredentialsFormFooter'
import * as ms365 from './ms365'
import type { CredentialFormProps } from './types'

const MS365_SCOPE_OPTIONS = [
  {
    value: 'User.Read',
    labelId: 'ms365CredentialUserReadScope',
    descriptionId: 'ms365CredentialUserReadScopeDescription',
    defaultSelected: true,
  },
  {
    value: 'Files.ReadWrite.All',
    labelId: 'ms365CredentialFilesReadWriteAllScope',
    descriptionId: 'ms365CredentialFilesReadWriteAllScopeDescription',
    defaultSelected: true,
  },
  {
    value: 'Files.Read.All',
    labelId: 'ms365CredentialFilesReadAllScope',
    descriptionId: 'ms365CredentialFilesReadAllScopeDescription',
    defaultSelected: false,
  },
  {
    value: 'Files.ReadWrite',
    labelId: 'ms365CredentialFilesReadWriteScope',
    descriptionId: 'ms365CredentialFilesReadWriteScopeDescription',
    defaultSelected: false,
  },
  {
    value: 'Files.Read',
    labelId: 'ms365CredentialFilesReadScope',
    descriptionId: 'ms365CredentialFilesReadScopeDescription',
    defaultSelected: false,
  },
] as const satisfies ReadonlyArray<{
  readonly value: string
  readonly labelId: string
  readonly descriptionId: string
  readonly defaultSelected: boolean
}>

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
        scopes: MS365_SCOPE_OPTIONS.filter(({ defaultSelected }) => defaultSelected).map(
          ({ value }) => value,
        ),
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
            {MS365_SCOPE_OPTIONS.map(({ value, labelId, descriptionId }) => {
              const label = getText(labelId)
              const description = getText(descriptionId)
              return (
                <div key={value} className="flex items-center gap-1">
                  <Checkbox value={value}>{label}</Checkbox>
                  <Tooltip.Trigger>
                    <Button
                      aria-label={getText('ms365CredentialScopeHelpAriaLabel')}
                      size="xsmall"
                      variant="icon"
                      icon="help"
                      tooltip={false}
                    />
                    <Tooltip>{description}</Tooltip>
                  </Tooltip.Trigger>
                </div>
              )
            })}
          </Checkbox.Group>
          <CredentialsFormFooter isCreating={true} canCancel={false} canReset={false} />
        </>
      )}
    </Form>
  )
}
