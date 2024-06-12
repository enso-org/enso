/** @file Settings tab for deleting the current user. */
import * as React from 'react'

import * as backendHooks from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import SettingsSection from '#/components/styled/settings/SettingsSection'

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

  const deleteUserMutation = backendHooks.useBackendMutation(backend, 'deleteUser')

  return (
    <SettingsSection
      title={<aria.Text className="text-danger">{getText('dangerZone')}</aria.Text>}
      // This UI element does not appear anywhere else.
      // eslint-disable-next-line no-restricted-syntax
      className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]"
    >
      <div className="flex gap-2">
        <ariaComponents.Button
          size="custom"
          variant="custom"
          className="button relative rounded-full bg-danger px-delete-user-account-button-x text-inversed opacity-full before:absolute before:inset-0 before:rounded-full before:transition-all hover:opacity-full before:hover:bg-primary/10"
          onPress={() => {
            setModal(
              <ConfirmDeleteUserModal
                doDelete={async () => {
                  await deleteUserMutation.mutateAsync([])
                  await signOut()
                }}
              />
            )
          }}
        >
          <aria.Text className="text inline-block">
            {getText('deleteUserAccountButtonLabel')}
          </aria.Text>
        </ariaComponents.Button>
        <aria.Text className="text my-auto">{getText('deleteUserAccountWarning')}</aria.Text>
      </div>
    </SettingsSection>
  )
}
