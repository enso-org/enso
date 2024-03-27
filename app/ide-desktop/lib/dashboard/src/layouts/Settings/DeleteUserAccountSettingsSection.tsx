/** @file Settings tab for deleting the current user. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/styled/UnstyledButton'

import ConfirmDeleteUserModal from '#/modals/ConfirmDeleteUserModal'

// ========================================
// === DeleteUserAccountSettingsSection ===
// ========================================

/** Settings tab for deleting the current user. */
export default function DeleteUserAccountSettingsSection() {
  const { signOut } = authProvider.useAuth()
  const { setModal } = modalProvider.useSetModal()
  const { backend } = backendProvider.useBackend()

  return (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div
          ref={ref}
          // This UI element does not appear anywhere else.
          // eslint-disable-next-line no-restricted-syntax
          className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]"
          {...innerProps}
        >
          <aria.Heading className="settings-subheading text-danger">Danger Zone</aria.Heading>
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
              <aria.Text className="text inline-block">Delete this user account</aria.Text>
            </UnstyledButton>
            <aria.Text className="text my-auto">
              Once deleted, it will be gone forever. Please be certain.
            </aria.Text>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
