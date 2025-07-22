/** @file Modal for setting the organization name. */
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { ORGANIZATION_NAME_MAX_LENGTH, ORGANIZATION_NAME_MIN_LENGTH } from '$/appUtils'
import { useBackends, useText } from '$/providers/react'

/** Modal for setting the organization name. */
export function SetupOrganizationModal() {
  const { getText } = useText()

  return (
    <Dialog title={getText('setupOrganization')} modalProps={{ defaultOpen: true }}>
      <SetupOrganizationForm />
    </Dialog>
  )
}

/** Form for setting the organization name. */
export function SetupOrganizationForm() {
  const { getText } = useText()
  const { remoteBackend } = useBackends()
  const updateOrganization = useMutationCallback(
    backendMutationOptions(remoteBackend, 'updateOrganization'),
  )
  const createDefaultUserGroup = useMutationCallback(
    backendMutationOptions(remoteBackend, 'createUserGroup'),
  )

  return (
    <Form
      gap="medium"
      className="max-w-96"
      defaultValues={{ name: '' }}
      schema={(z) =>
        z.object({
          name: z
            .string()
            .min(ORGANIZATION_NAME_MIN_LENGTH, getText('organizationNameMinLengthError'))
            .max(ORGANIZATION_NAME_MAX_LENGTH),
        })
      }
      onSubmit={({ name }) =>
        updateOrganization([{ name }]).then(() => createDefaultUserGroup([{ name }]))
      }
    >
      <Text>{getText('setOrganizationNameDescription')}</Text>
      <Input
        name="name"
        autoFocus
        inputMode="text"
        autoComplete="off"
        label={getText('organizationNameSettingsInput')}
        description={getText(
          'organizationNameSettingsInputDescription',
          ORGANIZATION_NAME_MAX_LENGTH,
        )}
      />

      <Form.Submit />

      <Form.FormError />
    </Form>
  )
}
