/** @file Settings screen. */
import * as React from 'react'

import BurgerMenuIcon from 'enso-assets/burger_menu.svg'

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
import * as portal from '#/components/Portal'
import Button from '#/components/styled/Button'

import * as backendModule from '#/services/Backend'

import * as array from '#/utilities/array'
import * as string from '#/utilities/string'

// ================
// === Settings ===
// ================

/** Settings screen. */
export default function Settings() {
  const [tab, setTab] = searchParamsState.useSearchParamsState(
    'SettingsTab',
    SettingsTabType.account,
    array.includesPredicate(Object.values(SettingsTabType))
  )
  const { type: sessionType, user, accessToken } = authProvider.useNonPartialUserSession()
  const { setUser } = authProvider.useAuth()
  const { backend } = backendProvider.useStrictBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { setSearchBar, unsetSearchBar } = searchBarProvider.useSetSearchBar('Settings')
  const [query, setQuery] = React.useState('')
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
    subscription: {},
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
      getText,
    }),
    [accessToken, backend, organization, setUser, toastAndLog, user, getText]
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
        setIsUserInOrganization(newOrganization != null)
        if (newOrganization != null) {
          setOrganization(newOrganization)
        }
      }
    })()
  }, [sessionType, backend])

  const isMatch = React.useMemo(() => {
    const regex = new RegExp(string.regexEscape(query.trim()).replace(/\s+/g, '.+'), 'i')
    return (name: string) => regex.test(name)
  }, [query])

  const doesEntryMatchQuery = React.useCallback(
    (entry: settingsData.SettingsEntryData) => {
      switch (entry.type) {
        case settingsData.SettingsEntryType.input: {
          return isMatch(getText(entry.nameId))
        }
        case settingsData.SettingsEntryType.custom: {
          const doesAliasesIdMatch =
            entry.aliasesId == null ? false : getText(entry.aliasesId).split('\n').some(isMatch)
          if (doesAliasesIdMatch) {
            return true
          } else {
            return entry.getExtraAliases == null
              ? false
              : entry.getExtraAliases(context).some(isMatch)
          }
        }
      }
    },
    [context, getText, isMatch]
  )

  const data = React.useMemo<settingsData.SettingsTabData>(() => {
    const tabData = settingsData.SETTINGS_TAB_DATA[tab]
    if (!/\S/.test(query)) {
      return tabData
    } else {
      if (isMatch(tabData.nameId)) {
        return tabData
      } else {
        const sections = tabData.sections.flatMap(section => {
          const matchingEntries = isMatch(getText(section.nameId))
            ? section.entries
            : section.entries.filter(doesEntryMatchQuery)
          if (matchingEntries.length === 0) {
            return []
          } else {
            return [{ ...section, entries: matchingEntries }]
          }
        })
        return { ...tabData, sections }
      }
    }
  }, [doesEntryMatchQuery, getText, isMatch, query, tab])

  const tabsToShow = React.useMemo<readonly SettingsTabType[]>(() => {
    if (!/\S/.test(query)) {
      return settingsData.ALL_SETTINGS_TABS
    } else {
      return settingsData.SETTINGS_DATA.flatMap(tabSection =>
        tabSection.tabs
          .filter(tabData =>
            isMatch(getText(tabData.nameId)) || isMatch(getText(tabSection.nameId))
              ? true
              : tabData.sections.some(section =>
                  isMatch(getText(section.nameId))
                    ? true
                    : section.entries.some(doesEntryMatchQuery)
                )
          )
          .map(tabData => tabData.settingsTab)
      )
    }
  }, [doesEntryMatchQuery, getText, isMatch, query])

  return (
    <div className="flex flex-1 flex-col gap-settings-header overflow-hidden px-page-x">
      <aria.Heading level={1} className="flex h-heading px-heading-x text-xl font-bold">
        <aria.MenuTrigger isOpen={isSidebarPopoverOpen} onOpenChange={setIsSidebarPopoverOpen}>
          <Button image={BurgerMenuIcon} buttonClassName="mr-3 sm:hidden" onPress={() => {}} />
          <aria.Popover UNSTABLE_portalContainer={root}>
            <SettingsSidebar
              isMenu
              tabsToShow={tabsToShow}
              isUserInOrganization={isUserInOrganization}
              tab={tab}
              setTab={setTab}
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
          {data.organizationOnly === true
            ? organization.name ?? 'your organization'
            : user?.name ?? 'your account'}
        </div>
      </aria.Heading>
      <div className="flex flex-1 gap-settings overflow-hidden">
        <SettingsSidebar
          tabsToShow={tabsToShow}
          isUserInOrganization={isUserInOrganization}
          tab={tab}
          setTab={setTab}
        />
        <SettingsTab context={context} data={data} />
      </div>
    </div>
  )
}
