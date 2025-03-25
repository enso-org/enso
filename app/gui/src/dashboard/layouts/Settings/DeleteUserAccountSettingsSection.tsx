/** @file Settings tab for deleting the current user. */
import { Button, DialogTrigger, Text } from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'
import { ConfirmDeleteUserModal } from '#/modals/ConfirmDeleteUserModal'
import { useAuth } from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'

/** Settings tab for deleting the current user. */
export default function DeleteUserAccountSettingsSection() {
  const { deleteUser } = useAuth()
  const { getText } = useText()

  return (
    <FocusArea direction="vertical">
      {(innerProps) => (
        <div
          className="flex flex-col items-start gap-2.5 rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]"
          {...innerProps}
        >
          <Text.Heading color="danger">{getText('dangerZone')}</Text.Heading>
          <div className="flex gap-2">
            <DialogTrigger>
              <Button size="medium" variant="delete">
                {getText('deleteUserAccountButtonLabel')}
              </Button>
              <ConfirmDeleteUserModal
                doDelete={async () => {
                  await deleteUser()
                }}
              />
            </DialogTrigger>
            <Text className="my-auto">{getText('deleteUserAccountWarning')}</Text>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
