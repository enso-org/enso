/** @file Settings tab for deleting the current user. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import SettingsSection from '#/components/styled/settings/SettingsSection'
import UnstyledButton from '#/components/UnstyledButton'

import ConfirmDeleteUserModal from '#/modals/ConfirmDeleteUserModal'

import type Backend from '#/services/Backend'

// ========================================
// === DeleteUserAccountSettingsSection ===
// ========================================

/** Props for a {@link DeleteUserAccountSettingsSection}. */
export interface DeleteUserAccountSettingsSectionProps {
  readonly backend: Backend
}

/** Settings tab for deleting the current user. */
export default function DeleteUserAccountSettingsSection(
  props: DeleteUserAccountSettingsSectionProps
) {
  const { backend } = props
  const { signOut } = authProvider.useAuth()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()

  return (
    <SettingsSection
      title={<aria.Text className="text-danger">{getText('dangerZone')}</aria.Text>}
      // This UI element does not appear anywhere else.
      // eslint-disable-next-line no-restricted-syntax
      className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]"
    >
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
    </SettingsSection>
  )
}
