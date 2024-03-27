/** @file Settings section for viewing and editing account information. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import SettingsInput from '#/components/styled/settings/SettingsInput'
import SettingsSection from '#/components/styled/settings/SettingsSection'

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
  const { getText } = textProvider.useText()

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
    <SettingsSection title={getText('userAccount')}>
      <div className="flex flex-col">
        <aria.TextField defaultValue={user?.name ?? ''} className="flex h-row gap-settings-entry">
          <aria.Label className="text my-auto w-user-account-settings-label">
            {getText('name')}
          </aria.Label>
          <SettingsInput type="text" onSubmit={doUpdateName} />
        </aria.TextField>
        <div className="flex h-row gap-settings-entry">
          <aria.Text className="text my-auto w-user-account-settings-label">
            {getText('email')}
          </aria.Text>
          <aria.Text className="settings-value my-auto grow font-bold">
            {user?.email ?? ''}
          </aria.Text>
        </div>
      </div>
    </SettingsSection>
  )
}
