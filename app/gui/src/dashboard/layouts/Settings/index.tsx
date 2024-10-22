/** @file Settings screen. */
import * as React from 'react'

import { useMutation, useQueryClient } from '@tanstack/react-query'

import BurgerMenuIcon from '#/assets/burger_menu.svg'
import { Heading, MenuTrigger } from '#/components/aria'
import { Button, Popover, Text } from '#/components/AriaComponents'
import { useStrictPortalContext } from '#/components/Portal'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import SearchBar from '#/layouts/SearchBar'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useLocalBackend, useRemoteBackend } from '#/providers/BackendProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { Path } from '#/services/ProjectManager'
import { includesPredicate } from '#/utilities/array'
import { regexEscape } from '#/utilities/string'
import {
  ALL_SETTINGS_TABS,
  SETTINGS_DATA,
  SETTINGS_NO_RESULTS_SECTION_DATA,
  SETTINGS_TAB_DATA,
  SettingsEntryType,
  type SettingsContext,
  type SettingsEntryData,
  type SettingsTabData,
} from './data'
import SettingsSidebar from './Sidebar'
import SettingsTab from './Tab'
import SettingsTabType from './TabType'

// ================
// === Settings ===
// ================

/** Props for a {@link Settings}. */
export interface SettingsProps {
  readonly backend: Backend | null
}

/** Settings screen. */
export default function Settings() {
  const queryClient = useQueryClient()
  const backend = useRemoteBackend()
  const localBackend = useLocalBackend()
  const [tab, setTab] = useSearchParamsState(
    'SettingsTab',
    SettingsTabType.account,
    includesPredicate(Object.values(SettingsTabType)),
  )
  const { user, accessToken } = useFullUserSession()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const [query, setQuery] = React.useState('')
  const root = useStrictPortalContext()
  const [isSidebarPopoverOpen, setIsSidebarPopoverOpen] = React.useState(false)
  const { data: organization = null } = useBackendQuery(backend, 'getOrganization', [])
  const isQueryBlank = !/\S/.test(query)

  const updateUser = useMutation(backendMutationOptions(backend, 'updateUser')).mutateAsync
  const updateOrganization = useMutation(
    backendMutationOptions(backend, 'updateOrganization'),
  ).mutateAsync

  const [, setLocalRootDirectory] = useLocalStorageState('localRootDirectory')
  const updateLocalRootPath = useEventCallback((value: string) => {
    setLocalRootDirectory(value)
    if (localBackend) {
      localBackend.setRootPath(Path(value))
    }
  })
  const resetLocalRootPath = useEventCallback(() => {
    setLocalRootDirectory(undefined)
    localBackend?.resetRootPath()
  })

  const context = React.useMemo<SettingsContext>(
    () => ({
      accessToken,
      user,
      backend,
      localBackend,
      organization,
      updateUser,
      updateOrganization,
      updateLocalRootPath,
      resetLocalRootPath,
      toastAndLog,
      getText,
      queryClient,
    }),
    [
      accessToken,
      backend,
      getText,
      localBackend,
      organization,
      toastAndLog,
      updateLocalRootPath,
      resetLocalRootPath,
      updateOrganization,
      updateUser,
      user,
      queryClient,
    ],
  )

  const isMatch = React.useMemo(() => {
    const regex = new RegExp(regexEscape(query.trim()).replace(/\s+/g, '.+'), 'i')
    return (name: string) => regex.test(name)
  }, [query])

  const doesEntryMatchQuery = React.useCallback(
    (entry: SettingsEntryData) => {
      switch (entry.type) {
        case SettingsEntryType.input: {
          return isMatch(getText(entry.nameId))
        }
        case SettingsEntryType.custom: {
          const doesAliasesIdMatch =
            entry.aliasesId == null ? false : getText(entry.aliasesId).split('\n').some(isMatch)
          if (doesAliasesIdMatch) {
            return true
          } else {
            return entry.getExtraAliases == null ?
                false
              : entry.getExtraAliases(context).some(isMatch)
          }
        }
      }
    },
    [context, getText, isMatch],
  )

  const tabsToShow = React.useMemo<readonly SettingsTabType[]>(() => {
    if (isQueryBlank) {
      return ALL_SETTINGS_TABS
    } else {
      return SETTINGS_DATA.flatMap((tabSection) =>
        tabSection.tabs
          .filter((tabData) =>
            isMatch(getText(tabData.nameId)) || isMatch(getText(tabSection.nameId)) ?
              true
            : tabData.sections.some((section) =>
                isMatch(getText(section.nameId)) ? true : section.entries.some(doesEntryMatchQuery),
              ),
          )
          .map((tabData) => tabData.settingsTab),
      )
    }
  }, [isQueryBlank, doesEntryMatchQuery, getText, isMatch])
  const effectiveTab = tabsToShow.includes(tab) ? tab : tabsToShow[0] ?? SettingsTabType.account

  const data = React.useMemo<SettingsTabData>(() => {
    const tabData = SETTINGS_TAB_DATA[effectiveTab]
    if (isQueryBlank) {
      return tabData
    } else {
      if (isMatch(getText(tabData.nameId))) {
        return tabData
      } else {
        const sections = tabData.sections.flatMap((section) => {
          const matchingEntries =
            isMatch(getText(section.nameId)) ?
              section.entries
            : section.entries.filter(doesEntryMatchQuery)
          if (matchingEntries.length === 0) {
            return []
          } else {
            return [{ ...section, entries: matchingEntries }]
          }
        })
        return {
          ...tabData,
          sections: sections.length === 0 ? [SETTINGS_NO_RESULTS_SECTION_DATA] : sections,
        }
      }
    }
  }, [isQueryBlank, doesEntryMatchQuery, getText, isMatch, effectiveTab])

  return (
    <div className="flex flex-1 flex-col gap-4 overflow-hidden pl-page-x pt-4">
      <Heading level={1} className="flex items-center px-heading-x">
        <MenuTrigger isOpen={isSidebarPopoverOpen} onOpenChange={setIsSidebarPopoverOpen}>
          <Button size="custom" variant="custom" icon={BurgerMenuIcon} className="mr-3 sm:hidden" />
          <Popover UNSTABLE_portalContainer={root}>
            <SettingsSidebar
              isMenu
              context={context}
              tabsToShow={tabsToShow}
              tab={effectiveTab}
              setTab={setTab}
              onClickCapture={() => {
                setIsSidebarPopoverOpen(false)
              }}
            />
          </Popover>
        </MenuTrigger>

        <Text variant="h1" className="font-bold">
          {getText('settingsFor')}
        </Text>

        <Text
          variant="h1"
          truncate="1"
          className="ml-2.5 mr-8 max-w-[min(32rem,_100%)] rounded-full bg-white px-2.5 font-bold"
          aria-hidden
        >
          {data.organizationOnly === true ? organization?.name ?? 'your organization' : user.name}
        </Text>
      </Heading>
      <div className="sm:ml-[14rem]">
        <SearchBar
          data-testid="settings-search-bar"
          query={query}
          setQuery={setQuery}
          label={getText('settingsSearchBarLabel')}
          placeholder={getText('settingsSearchBarPlaceholder')}
        />
      </div>
      <div className="flex sm:ml-[222px]" />
      <div className="flex flex-1 gap-4 overflow-hidden">
        <aside className="hidden h-full shrink-0 basis-[206px] flex-col overflow-y-auto overflow-x-hidden pb-12 sm:flex">
          <SettingsSidebar
            context={context}
            tabsToShow={tabsToShow}
            tab={effectiveTab}
            setTab={setTab}
          />
        </aside>
        <main className="flex flex-1 flex-col overflow-y-auto pb-12 pl-1 scrollbar-gutter-stable">
          <SettingsTab
            context={context}
            data={data}
            onInteracted={() => {
              if (effectiveTab !== tab) {
                setTab(effectiveTab)
              }
            }}
          />
        </main>
      </div>
    </div>
  )
}
