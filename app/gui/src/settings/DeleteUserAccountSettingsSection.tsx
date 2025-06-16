/** @file Settings tab for deleting the current user. */
import { useAuth, useText } from '$/providers/react'
import { Button } from '$/react-components/Button'
import { ConfirmDeleteUserModal } from '$/react-components/ConfirmDeleteUserModal'
import { Dialog } from '$/react-components/Dialog'
import { Text } from '$/react-components/Text'

/** Settings tab for deleting the current user. */
export default function DeleteUserAccountSettingsSection() {
  const { deleteUser } = useAuth()
  const { getText } = useText()

  return (
    <div className="flex flex-col items-start gap-2.5 rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]">
      <Text.Heading color="danger">{getText('dangerZone')}</Text.Heading>
      <div className="flex gap-2">
        <Dialog.Trigger>
          <Button size="medium" variant="delete">
            {getText('deleteUserAccountButtonLabel')}
          </Button>
          <ConfirmDeleteUserModal
            doDelete={async () => {
              await deleteUser()
            }}
          />
        </Dialog.Trigger>
        <Text className="my-auto">{getText('deleteUserAccountWarning')}</Text>
      </div>
    </div>
  )
}
