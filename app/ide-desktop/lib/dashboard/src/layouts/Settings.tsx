/** @file Settings screen. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'

import AccountSettingsTab from '#/layouts/Settings/AccountSettingsTab'
import KeyboardShortcutsSettingsTab from '#/layouts/Settings/KeyboardShortcutsSettingsTab'
import MembersSettingsTab from '#/layouts/Settings/MembersSettingsTab'
import OrganizationSettingsTab from '#/layouts/Settings/OrganizationSettingsTab'
import SettingsTab from '#/layouts/Settings/SettingsTab'
import SettingsSidebar from '#/layouts/SettingsSidebar'

import * as backendModule from '#/services/Backend'

// ================
// === Settings ===
// ================

/** Settings screen. */
export default function Settings() {
  const [settingsTab, setSettingsTab] = React.useState(SettingsTab.account)
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const { backend } = backendProvider.useBackend()
  const [organization, setOrganization] = React.useState<backendModule.OrganizationInfo>(() => ({
    pk: user?.id ?? backendModule.OrganizationId(''),
    // eslint-disable-next-line @typescript-eslint/naming-convention
    organization_name: null,
    email: null,
    website: null,
    address: null,
    picture: null,
  }))

  React.useEffect(() => {
    void (async () => {
      if (
        sessionType === authProvider.UserSessionType.full &&
        backend.type === backendModule.BackendType.remote
      ) {
        const newOrganization = await backend.getOrganization()
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      }
    })()
  }, [sessionType, backend])

  let content: JSX.Element
  switch (settingsTab) {
    case SettingsTab.account: {
      content = <AccountSettingsTab />
      break
    }
    case SettingsTab.organization: {
      content = (
        <OrganizationSettingsTab organization={organization} setOrganization={setOrganization} />
      )
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
    <div className="flex flex-col flex-1 gap-settings-header overflow-hidden px-page-x">
      <div className="flex font-bold text-xl h-heading px-heading-x">
        <span className="py-heading-y">Settings for </span>
        {/* This UI element does not appear anywhere else. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <div className="rounded-full leading-snug bg-frame h-9 ml-2.5 px-2.25 pt-0.5 pb-1.25">
          {settingsTab !== SettingsTab.organization
            ? user?.name ?? 'your account'
            : organization.organization_name ?? 'your organization'}
        </div>
      </div>
      <div className="flex flex-1 gap-settings overflow-hidden">
        <SettingsSidebar settingsTab={settingsTab} setSettingsTab={setSettingsTab} />
        {content}
      </div>
    </div>
  )
}
