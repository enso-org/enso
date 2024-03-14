/** @file Settings screen. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'

import AccountSettingsTab from '#/layouts/Settings/AccountSettingsTab'
import KeyboardShortcutsSettingsTab from '#/layouts/Settings/KeyboardShortcutsSettingsTab'
import MembersSettingsTab from '#/layouts/Settings/MembersSettingsTab'
import OrganizationSettingsTab from '#/layouts/Settings/OrganizationSettingsTab'
import SettingsTab from '#/layouts/Settings/SettingsTab'
import SettingsSidebar from '#/layouts/SettingsSidebar'

import type * as backendModule from '#/services/Backend'

// ================
// === Settings ===
// ================

/** Settings screen. */
export default function Settings() {
  const [settingsTab, setSettingsTab] = React.useState(SettingsTab.account)
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const [organization, setOrganization] = React.useState<backendModule.SmartOrganization | null>(
    null
  )

  const setOrganizationInfo = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.OrganizationInfo>) => {
      setOrganization(
        oldOrganization =>
          oldOrganization?.withValue(
            oldOrganization.value == null
              ? null
              : typeof valueOrUpdater !== 'function'
                ? valueOrUpdater
                : valueOrUpdater(oldOrganization.value)
          ) ?? null
      )
    },
    []
  )

  React.useEffect(() => {
    void (async () => {
      if (sessionType === authProvider.UserSessionType.full) {
        setOrganization(await user.getOrganization())
      }
    })()
  }, [user, sessionType])

  let content: JSX.Element
  switch (settingsTab) {
    case SettingsTab.account: {
      content = <AccountSettingsTab />
      break
    }
    case SettingsTab.organization: {
      content = (
        <OrganizationSettingsTab
          organization={organization}
          setOrganizationInfo={setOrganizationInfo}
        />
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
    <div className="flex flex-1 flex-col gap-settings-header overflow-hidden px-page-x">
      <div className="flex h-heading px-heading-x text-xl font-bold">
        <span className="py-heading-y">Settings for </span>
        {/* This UI element does not appear anywhere else. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <div className="ml-[0.625rem] h-[2.25rem] rounded-full bg-frame px-[0.5625rem] pb-[0.3125rem] pt-[0.125rem] leading-snug">
          {settingsTab !== SettingsTab.organization
            ? user?.value.name ?? 'your account'
            : organization?.value?.organization_name ?? 'your organization'}
        </div>
      </div>
      <div className="flex flex-1 gap-settings overflow-hidden">
        <SettingsSidebar settingsTab={settingsTab} setSettingsTab={setSettingsTab} />
        {content}
      </div>
    </div>
  )
}
