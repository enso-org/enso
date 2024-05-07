/** @file Settings tab for deleting the current user. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/UnstyledButton'

import ConfirmDeleteUserModal from '#/modals/ConfirmDeleteUserModal'

// ========================================
// === DeleteUserAccountSettingsSection ===
// ========================================

/** Settings tab for deleting the current user. */
export default function DeleteUserAccountSettingsSection() {
  const { signOut } = authProvider.useAuth()
  const { setModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]"
          {...innerProps}
        >
          <aria.Heading level={2} className="h-[2.375rem] py-0.5 text-xl font-bold text-danger">
            {getText('dangerZone')}
          </aria.Heading>
          <div className="flex gap-buttons">
            <UnstyledButton
              className="button bg-danger px-delete-user-account-button-x text-inversed opacity-full hover:opacity-full"
              onPress={() => {
                setModal(
                  <ConfirmDeleteUserModal
                    doDelete={async () => {
                      await backend.deleteUser()
                      await signOut()
                    }}
                  />
                )
              }}
            >
              <aria.Text className="text inline-block">
                {getText('deleteUserAccountButtonLabel')}
              </aria.Text>
            </UnstyledButton>
            <aria.Text className="text my-auto">{getText('deleteUserAccountWarning')}</aria.Text>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
