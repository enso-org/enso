/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import ChangePasswordForm from '#/layouts/Settings/ChangePasswordForm'
import DeleteUserAccountSettingsSection from '#/layouts/Settings/DeleteUserAccountSettingsSection'
import ProfilePictureInput from '#/layouts/Settings/ProfilePictureInput'
import UserAccountSettingsSection from '#/layouts/Settings/UserAccountSettingsSection'

import SettingsSection from '#/components/styled/settings/SettingsSection'

// ==========================
// === AccountSettingsTab ===
// ==========================

/** Settings tab for viewing and editing account information. */
export default function AccountSettingsTab() {
  const { getText } = textProvider.useText()
  const { accessToken } = authProvider.useNonPartialUserSession()

  // The shape of the JWT payload is statically known.
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const username: string | null =
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-non-null-assertion
    accessToken != null ? JSON.parse(atob(accessToken.split('.')[1]!)).username : null
  const canChangePassword = username != null ? !/^Github_|^Google_/.test(username) : false

  return (
    <div className="flex h flex-col gap-settings-section lg:h-auto lg:flex-row">
      <div className="flex w-settings-main-section flex-col gap-settings-subsection">
        <UserAccountSettingsSection />
        {canChangePassword && (
          <SettingsSection title={getText('changePassword')}>
            <ChangePasswordForm />
          </SettingsSection>
        )}
        <DeleteUserAccountSettingsSection />
      </div>
      <SettingsSection title={getText('profilePicture')}>
        <ProfilePictureInput />
      </SettingsSection>
    </div>
  )
}
