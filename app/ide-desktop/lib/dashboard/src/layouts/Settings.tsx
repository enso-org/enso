/** @file Settings screen. */
import * as React from 'react'

import * as searchParamsState from '#/hooks/searchParamsStateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as searchBarProvider from '#/providers/SearchBarProvider'
import * as textProvider from '#/providers/TextProvider'

import SearchBar from '#/layouts/SearchBar'
import * as settingsData from '#/layouts/Settings/settingsData'
import SettingsTab from '#/layouts/Settings/SettingsTab'
import SettingsTabType from '#/layouts/Settings/SettingsTabType'
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
    SettingsTabType.account,
    (value): value is SettingsTabType => array.includes(Object.values(SettingsTabType), value)
  )
  const { type: sessionType, user, accessToken } = authProvider.useNonPartialUserSession()
  const { setUser } = authProvider.useAuth()
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setSearchBar, unsetSearchBar } = searchBarProvider.useSetSearchBar('Settings')
  const [query, setQuery] = React.useState('')
  const [organization, setOrganization] = React.useState<backendModule.OrganizationInfo>(() => ({
    id: user?.organizationId ?? backendModule.OrganizationId(''),
    name: null,
    email: null,
    website: null,
    address: null,
    picture: null,
  }))
  const context = React.useMemo<settingsData.SettingsContext>(
    () => ({
      accessToken,
      user,
      setUser,
      backend,
      organization,
      setOrganization,
      toastAndLog,
    }),
    [accessToken, backend, organization, setUser, toastAndLog, user]
  )

  React.useEffect(() => {
    setSearchBar(
      <SearchBar
        data-testid="settings-search-bar"
        query={query}
        setQuery={setQuery}
        label={getText('settingsSearchBarLabel')}
        placeholder={getText('settingsSearchBarPlaceholder')}
      />
    )
    return () => {
      unsetSearchBar()
    }
  }, [
    query,
    /* should never change */ getText,
    /* should never change */ setSearchBar,
    /* should never change */ unsetSearchBar,
  ])

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

  return (
    <div className="flex flex-1 flex-col gap-settings-header overflow-hidden px-page-x">
      <aria.Heading level={1} className="flex h-heading px-heading-x text-xl font-bold">
        <aria.Text className="py-heading-y">{getText('settingsFor')}</aria.Text>
        {/* This UI element does not appear anywhere else. */}
        {/* eslint-disable-next-line no-restricted-syntax */}
        <div className="ml-[0.625rem] h-[2.25rem] rounded-full bg-frame px-[0.5625rem] pb-[0.3125rem] pt-[0.125rem] leading-snug">
          {settingsTab !== SettingsTabType.organization
            ? user?.name ?? 'your account'
            : organization.name ?? 'your organization'}
        </div>
      </aria.Heading>
      <div className="flex flex-1 gap-settings overflow-hidden">
        <SettingsSidebar settingsTab={settingsTab} setSettingsTab={setSettingsTab} />
        <SettingsTab context={context} data={settingsData.SETTINGS_TAB_DATA[settingsTab]} />
      </div>
    </div>
  )
}
