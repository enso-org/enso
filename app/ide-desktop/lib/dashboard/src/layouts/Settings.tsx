/** @file Settings screen. */
import * as React from 'react'

import BurgerMenuIcon from 'enso-assets/burger_menu.svg'

import * as backendHooks from '#/hooks/backendHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'

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
import * as ariaComponents from '#/components/AriaComponents'
import * as errorBoundary from '#/components/ErrorBoundary'
import * as loader from '#/components/Loader'
import * as portal from '#/components/Portal'
import Button from '#/components/styled/Button'

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
  const { getText } = textProvider.useText()
  const root = portal.useStrictPortalContext()
  const [isSidebarPopoverOpen, setIsSidebarPopoverOpen] = React.useState(false)
  const user = backendHooks.useBackendUsersMe(backend)
  const organization = backendHooks.useBackendGetOrganization(backend)
  const isUserInOrganization = organization != null

  let content: React.JSX.Element | null

  switch (settingsTab) {
    case SettingsTab.account: {
      content = backend == null ? null : <AccountSettingsTab backend={backend} />
      break
    }
    case SettingsTab.organization: {
      content = backend == null ? null : <OrganizationSettingsTab backend={backend} />
      break
    }
    case SettingsTab.members: {
      content = <MembersSettingsTab />
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
      <aria.Heading level={1} className="flex items-center px-heading-x">
        <aria.MenuTrigger isOpen={isSidebarPopoverOpen} onOpenChange={setIsSidebarPopoverOpen}>
          <Button image={BurgerMenuIcon} buttonClassName="mr-3 sm:hidden" onPress={() => {}} />
          <aria.Popover UNSTABLE_portalContainer={root}>
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
        <ariaComponents.Text.Heading>
          <span>{getText('settingsFor')}</span>
        </ariaComponents.Text.Heading>

        <ariaComponents.Text
          variant="h1"
          truncate="1"
          className="ml-2.5 max-w-lg rounded-full bg-frame px-2.5"
          aria-hidden
        >
          {settingsTab !== SettingsTab.organization &&
          settingsTab !== SettingsTab.members &&
          settingsTab !== SettingsTab.userGroups
            ? user?.name ?? 'your account'
            : organization?.name ?? 'your organization'}
        </ariaComponents.Text>
      </aria.Heading>
      <div className="mt-8 flex flex-1 gap-6 overflow-hidden pr-0.5">
        <aside className="flex h-full flex-col overflow-y-auto overflow-x-hidden pb-12">
          <SettingsSidebar
            hasBackend={backend != null}
            isUserInOrganization={isUserInOrganization}
            settingsTab={settingsTab}
            setSettingsTab={setSettingsTab}
          />
        </aside>
        <errorBoundary.ErrorBoundary>
          <React.Suspense fallback={<loader.Loader size="medium" minHeight="h64" />}>
            <main className="h-full w-full flex-shrink-0 flex-grow basis-0 overflow-y-auto overflow-x-hidden pb-12 pl-1.5 pr-3">
              <div className="w-full max-w-[840px]">{content}</div>
            </main>
          </React.Suspense>
        </errorBoundary.ErrorBoundary>
      </div>
    </div>
  )
}
