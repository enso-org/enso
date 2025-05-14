/** @file A modal to create a user group. */
import { ButtonGroup, Dialog, Form, Input, Text } from '#/components/AriaComponents'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useRemoteBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { normalizeName } from '#/utilities/string'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useSuspenseQuery } from '@tanstack/react-query'

/** A modal to create a user group. */
export function NewUserGroupForm() {
  const backend = useRemoteBackend()
  const { getText } = useText()
  const { data: userGroups } = useSuspenseQuery(backendQueryOptions(backend, 'listUserGroups', []))
  const userGroupNames = new Set(userGroups.map((group) => normalizeName(group.groupName)))
  const createUserGroup = useMutationCallback(backendMutationOptions(backend, 'createUserGroup'))

  return (
    <Form
      schema={(z) =>
        z.object({
          name: z
            .string()
            .min(1)
            .refine(
              (name) => !userGroupNames.has(normalizeName(name)),
              getText('duplicateUserGroupError'),
            ),
        })
      }
      method="dialog"
      onSubmit={({ name }) => createUserGroup([{ name }])}
    >
      <Text.Heading variant="subtitle">{getText('newUserGroup')}</Text.Heading>
      <Input name="name" label={getText('name')} />
      <ButtonGroup className="relative">
        <Form.Submit />
        <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
      </ButtonGroup>
    </Form>
  )
}
