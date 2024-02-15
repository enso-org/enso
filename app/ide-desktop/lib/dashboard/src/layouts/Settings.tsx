/** @file Settings screen. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'

import SettingsSidebar from '#/layouts/SettingsSidebar'
import AccountSettingsTab from '#/layouts/settingsTab/AccountSettingsTab'
import KeyboardShortcutsSettingsTab from '#/layouts/settingsTab/KeyboardShortcutsSettingsTab'
import MembersSettingsTab from '#/layouts/settingsTab/MembersSettingsTab'
import SettingsTab from '#/layouts/settingsTab/SettingsTab'

// ================
// === Settings ===
// ================

/** Settings screen. */
export default function Settings() {
  const [settingsTab, setSettingsTab] = React.useState(SettingsTab.account)
  const { organization } = authProvider.useNonPartialUserSession()

  let content: JSX.Element
  switch (settingsTab) {
    case SettingsTab.account: {
      content = <AccountSettingsTab />
      break
    }
    case SettingsTab.members: {
      content = <MembersSettingsTab />
      break
    }
    case SettingsTab.keyboardShortcuts: {
      content = <KeyboardShortcutsSettingsTab />
      break
    }
    default: {
      // This case should be removed when all settings tabs are implemented.
      content = <></>
      break
    }
  }

  return (
    <div className="flex flex-col flex-1 gap-8 overflow-hidden">
      <div className="flex gap-2.5 font-bold text-xl h-9.5 px-4.75">
        <span className="py-0.5">Settings for </span>
        <div className="rounded-full leading-144.5 bg-frame h-9 px-2.25 pt-0.5 pb-1.25">
          {organization?.name ?? '(Unknown Organization)'}
        </div>
      </div>
      <div className="flex flex-1 gap-8 pl-3 overflow-hidden">
        <SettingsSidebar settingsTab={settingsTab} setSettingsTab={setSettingsTab} />
        {content}
      </div>
    </div>
  )
}
