/** @file Settings screen. */
import * as React from 'react'

import BurgerMenuIcon from 'enso-assets/burger_menu.svg'

import * as searchParamsState from '#/hooks/searchParamsStateHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import AccountSettingsTab from '#/layouts/Settings/AccountSettingsTab'
import ActivityLogSettingsTab from '#/layouts/Settings/ActivityLogSettingsTab'
import KeyboardShortcutsSettingsTab from '#/layouts/Settings/KeyboardShortcutsSettingsTab'
import MembersSettingsTab from '#/layouts/Settings/MembersSettingsTab'
import OrganizationSettingsTab from '#/layouts/Settings/OrganizationSettingsTab'
import SettingsTab from '#/layouts/Settings/SettingsTab'
import UserGroupsSettingsTab from '#/layouts/Settings/UserGroupsSettingsTab'
import SettingsSidebar from '#/layouts/SettingsSidebar'

import * as aria from '#/components/aria'
import * as portal from '#/components/Portal'
import Button from '#/components/styled/Button'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as array from '#/utilities/array'

// ================
// === Settings ===
// ================

/** Props for a {@link Settings}. */
export interface SettingsProps {
  readonly backend: Backend | null
}

/** Settings screen. */
export default function Settings(props: SettingsProps) {
  const { backend } = props
  const [settingsTab, setSettingsTab] = searchParamsState.useSearchParamsState(
    'SettingsTab',
    SettingsTab.account,
    array.includesPredicate(Object.values(SettingsTab))
  )
  const { type: sessionType, user } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()
  const root = portal.useStrictPortalContext()
  const [isUserInOrganization, setIsUserInOrganization] = React.useState(true)
  const [isSidebarPopoverOpen, setIsSidebarPopoverOpen] = React.useState(false)
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
        const newOrganization = await backend?.getOrganization()
        setIsUserInOrganization(newOrganization != null)
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      }
    })()
  }, [sessionType, backend])

  let content: JSX.Element | null
  switch (settingsTab) {
    case SettingsTab.account: {
      content = backend == null ? null : <AccountSettingsTab backend={backend} />
      break
    }
    case SettingsTab.organization: {
      content =
        backend == null ? null : (
          <OrganizationSettingsTab
            backend={backend}
            organization={organization}
            setOrganization={setOrganization}
          />
        )
      break
    }
    case SettingsTab.members: {
      content = backend == null ? null : <MembersSettingsTab backend={backend} />
      break
    }
    case SettingsTab.userGroups: {
      content = backend == null ? null : <UserGroupsSettingsTab backend={backend} />
      break
    }
    case SettingsTab.keyboardShortcuts: {
      content = <KeyboardShortcutsSettingsTab />
      break
    }
    case SettingsTab.activityLog: {
      content = backend == null ? null : <ActivityLogSettingsTab backend={backend} />
      break
    }
    default: {
      // This case should be removed when all settings tabs are implemented.
      content = <></>
      break
    }
  }
  const noContent = content == null

  React.useEffect(() => {
    if (noContent) {
      // Set to the first settings page that does not require a backend.
      setSettingsTab(SettingsTab.keyboardShortcuts)
    }
  }, [noContent, setSettingsTab])

  return (
    <div className="mt-4 flex flex-1 flex-col gap-settings-header overflow-hidden px-page-x">
      <aria.Heading level={1} className="flex h-heading px-heading-x text-xl font-bold">
        <aria.MenuTrigger isOpen={isSidebarPopoverOpen} onOpenChange={setIsSidebarPopoverOpen}>
          <Button image={BurgerMenuIcon} buttonClassName="mr-3 sm:hidden" onPress={() => {}} />
          <aria.Popover UNSTABLE_portalContainer={root.current}>
            <SettingsSidebar
              isMenu
              hasBackend={backend != null}
              isUserInOrganization={isUserInOrganization}
              settingsTab={settingsTab}
              setSettingsTab={setSettingsTab}
              onClickCapture={() => {
                setIsSidebarPopoverOpen(false)
              }}
            />
          </aria.Popover>
        </aria.MenuTrigger>
        <aria.Text className="py-heading-y">{getText('settingsFor')}</aria.Text>
        {/* This UI element does not appear anywhere else. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <div className="ml-[0.625rem] h-[2.25rem] rounded-full bg-frame px-[0.5625rem] pb-[0.3125rem] pt-[0.125rem] leading-snug">
          {settingsTab !== SettingsTab.organization &&
          settingsTab !== SettingsTab.members &&
          settingsTab !== SettingsTab.userGroups
            ? user?.name ?? 'your account'
            : organization.name ?? 'your organization'}
        </div>
      </aria.Heading>
      <div className="flex flex-1 gap-settings overflow-hidden">
        <SettingsSidebar
          hasBackend={backend != null}
          isUserInOrganization={isUserInOrganization}
          settingsTab={settingsTab}
          setSettingsTab={setSettingsTab}
        />
        {content}
      </div>
    </div>
  )
}
