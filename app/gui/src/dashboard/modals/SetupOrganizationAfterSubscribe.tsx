/** @file Modal for setting the organization name. */
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { ORGANIZATION_NAME_MAX_LENGTH, USER_GROUP_NAME_MAX_LENGTH } from '$/appUtils'
import { useBackends, useText } from '$/providers/react'

/** Modal for setting the organization name. */
export function SetOrganizationNameModal() {
  const { getText } = useText()

  return (
    <Dialog title={getText('setupOrganization')} modalProps={{ defaultOpen: true }}>
      <SetOrganizationNameForm />
    </Dialog>
  )
}

/** Form for setting the organization name. */
export function SetOrganizationNameForm() {
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
          name: z.string().min(1).max(ORGANIZATION_NAME_MAX_LENGTH),
        })
      }
      onSubmit={({ name }) =>
        updateOrganization([{ name }]).then(() => createDefaultUserGroup([{ name: name }]))
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

/** Form for creating a user group. */
export function CreateUserGroupForm() {
  const { getText } = useText()
  const { remoteBackend } = useBackends()
  const createDefaultUserGroup = useMutationCallback(
    backendMutationOptions(remoteBackend, 'createUserGroup'),
  )

  return (
    <Form
      schema={(z) => z.object({ groupName: z.string().min(1).max(USER_GROUP_NAME_MAX_LENGTH) })}
      gap="medium"
      className="max-w-96"
      defaultValues={{ groupName: '' }}
      onSubmit={({ groupName }) => createDefaultUserGroup([{ name: groupName }])}
    >
      <Text>{getText('setDefaultUserGroupDescription')}</Text>
      <Input
        name="groupName"
        autoComplete="off"
        label={getText('groupNameSettingsInput')}
        description={getText('groupNameSettingsInputDescription', USER_GROUP_NAME_MAX_LENGTH)}
      />

      <Form.Submit />

      <Form.FormError />
    </Form>
  )
}
