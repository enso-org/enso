/** @file Settings screen. */
import * as React from 'react'

import * as searchParamsState from '#/hooks/searchParamsStateHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import AccountSettingsTab from '#/layouts/Settings/AccountSettingsTab'
import ActivityLogSettingsTab from '#/layouts/Settings/ActivityLogSettingsTab'
import KeyboardShortcutsSettingsTab from '#/layouts/Settings/KeyboardShortcutsSettingsTab'
import MembersSettingsTab from '#/layouts/Settings/MembersSettingsTab'
import OrganizationSettingsTab from '#/layouts/Settings/OrganizationSettingsTab'
import SettingsTab from '#/layouts/Settings/SettingsTab'
import SettingsSidebar from '#/layouts/SettingsSidebar'

import * as aria from '#/components/aria'

import * as backendModule from '#/services/Backend'

import * as array from '#/utilities/array'

// ================
// === Settings ===
// ================

/** Settings screen. */
export default function Settings() {
  const [settingsTab, setSettingsTab] = searchParamsState.useSearchParamsState(
    'SettingsTab',
    SettingsTab.account,
    (value): value is SettingsTab => array.includes(Object.values(SettingsTab), value)
  )
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const remoteBackend = backendProvider.useRemoteBackendStrict()
  const { getText } = textProvider.useText()
  const [organization, setOrganization] = React.useState<backendModule.OrganizationInfo>(() => ({
    id: user?.organizationId ?? backendModule.OrganizationId(''),
    name: null,
    email: null,
    website: null,
    address: null,
    picture: null,
  }))

  React.useEffect(() => {
    void (async () => {
      if (sessionType === authProvider.UserSessionType.full) {
        const newOrganization = await remoteBackend.getOrganization()
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      }
    })()
  }, [sessionType, remoteBackend])

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
    case SettingsTab.activityLog: {
      content = <ActivityLogSettingsTab />
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
      <aria.Heading level={1} className="flex h-heading px-heading-x text-xl font-bold">
        <aria.Text className="py-heading-y">{getText('settingsFor')}</aria.Text>
        {/* This UI element does not appear anywhere else. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <div className="ml-[0.625rem] h-[2.25rem] rounded-full bg-frame px-[0.5625rem] pb-[0.3125rem] pt-[0.125rem] leading-snug">
          {settingsTab !== SettingsTab.organization
            ? user?.name ?? 'your account'
            : organization.name ?? 'your organization'}
        </div>
      </aria.Heading>
      <div className="flex flex-1 gap-settings overflow-hidden">
        <SettingsSidebar settingsTab={settingsTab} setSettingsTab={setSettingsTab} />
        {content}
      </div>
    </div>
  )
}
