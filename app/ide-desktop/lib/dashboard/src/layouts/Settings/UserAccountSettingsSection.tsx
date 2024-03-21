/** @file Settings section for viewing and editing account information. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'

import SettingsInput from '#/layouts/Settings/SettingsInput'

import FocusArea from '#/components/styled/FocusArea'

import * as object from '#/utilities/object'

// ==================================
// === UserAccountSettingsSection ===
// ==================================

/** Settings section for viewing and editing account information. */
export default function UserAccountSettingsSection() {
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setUser } = authProvider.useAuth()
  const { backend } = backendProvider.useBackend()
  const { user } = authProvider.useNonPartialUserSession()

  const doUpdateName = async (newName: string) => {
    const oldName = user?.name ?? ''
    if (newName === oldName) {
      return
    } else {
      try {
        await backend.updateUser({ username: newName })
        setUser(object.merger({ name: newName }))
      } catch (error) {
        toastAndLog(null, error)
      }
      return
    }
  }

  return (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div ref={ref} className="flex flex-col gap-settings-section-header" {...innerProps}>
          <h3 className="settings-subheading">User Account</h3>
          <div className="flex flex-col">
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-user-account-settings-label">Name</span>
              <span className="text my-auto grow font-bold">
                <SettingsInput
                  type="text"
                  initialValue={user?.name ?? ''}
                  onSubmit={doUpdateName}
                />
              </span>
            </div>
            <div className="flex h-row gap-settings-entry">
              <span className="text my-auto w-user-account-settings-label">Email</span>
              <span className="settings-value my-auto grow font-bold">{user?.email ?? ''}</span>
            </div>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
