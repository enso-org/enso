/** @file Settings tab for deleting the current user. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'

import ConfirmDeleteUserModal from '#/modals/ConfirmDeleteUserModal'

// ========================================
// === DeleteUserAccountSettingsSection ===
// ========================================

/** Settings tab for deleting the current user. */
export default function DeleteUserAccountSettingsSection() {
  const { signOut, deleteUser } = authProvider.useAuth()
  const { getText } = textProvider.useText()

  return (
    <FocusArea direction="vertical">
      {(innerProps) => (
        <div
          className="flex flex-col items-start gap-settings-section-header rounded-2.5xl border-2 border-danger px-[1rem] pb-[0.9375rem] pt-[0.5625rem]"
          {...innerProps}
        >
          <aria.Heading level={2} className="h-[2.375rem] py-0.5 text-xl font-bold text-danger">
            {getText('dangerZone')}
          </aria.Heading>
          <div className="flex gap-2">
            <ariaComponents.DialogTrigger>
              <ariaComponents.Button size="medium" variant="delete">
                {getText('deleteUserAccountButtonLabel')}
              </ariaComponents.Button>
              <ConfirmDeleteUserModal
                doDelete={async () => {
                  await deleteUser()
                  await signOut()
                }}
              />
            </ariaComponents.DialogTrigger>
            <aria.Text className="my-auto">{getText('deleteUserAccountWarning')}</aria.Text>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
