/** @file A form to create a user group. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs'
import { Text } from '#/components/Text'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { normalizeName } from '#/utilities/string'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends, useText } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'

/** A form to create a user group. */
export function NewUserGroupForm() {
  const { remoteBackend: backend } = useBackends()
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
      <Button.Group className="relative">
        <Form.Submit />
        <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>
      </Button.Group>
      <Form.FormError />
    </Form>
  )
}
