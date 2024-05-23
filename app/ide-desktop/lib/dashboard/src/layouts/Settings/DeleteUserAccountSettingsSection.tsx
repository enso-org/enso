/** @file Settings tab for deleting the current user. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import SettingsSection from '#/components/styled/settings/SettingsSection'
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
    <SettingsSection
      title={<aria.Text className="text-danger">{getText('dangerZone')}</aria.Text>}
      // This UI element does not appear anywhere else.
      // eslint-disable-next-line no-restricted-syntax
      className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]"
    >
      <div className="flex gap-buttons">
        <ariaComponents.Button
          variant="delete"
          size="medium"
          rounded="full"
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
          {getText('deleteUserAccountButtonLabel')}
        </ariaComponents.Button>
        <aria.Text className="text-md my-auto">{getText('deleteUserAccountWarning')}</aria.Text>
      </div>
    </SettingsSection>
  )
}
