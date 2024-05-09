/** @file Settings tab for viewing and editing account information. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'

import ChangePasswordSettingsSection from '#/layouts/Settings/ChangePasswordSettingsSection'
import DeleteUserAccountSettingsSection from '#/layouts/Settings/DeleteUserAccountSettingsSection'
import ProfilePictureSettingsSection from '#/layouts/Settings/ProfilePictureSettingsSection'
import UserAccountSettingsSection from '#/layouts/Settings/UserAccountSettingsSection'

import type Backend from '#/services/Backend'

// ==========================
// === AccountSettingsTab ===
// ==========================

/** Props for a {@link AccountSettingsTab}. */
export interface AccountSettingsTabProps {
  readonly backend: Backend
}

/** Settings tab for viewing and editing account information. */
export default function AccountSettingsTab(props: AccountSettingsTabProps) {
  const { backend } = props
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
        <UserAccountSettingsSection backend={backend} />
        {canChangePassword && <ChangePasswordSettingsSection />}
        <DeleteUserAccountSettingsSection backend={backend} />
      </div>
      <ProfilePictureSettingsSection backend={backend} />
    </div>
  )
}
