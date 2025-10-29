/** @file Settings screen. */
import { Heading } from '#/components/aria'
import { Button } from '#/components/Button'
import { Popover } from '#/components/Dialog'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Menu } from '#/components/Menu'
import { usePortalContext } from '#/components/Portal'
import { Text } from '#/components/Text'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useLocalStorageState } from '#/hooks/localStoreState'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useLocalRootDirectory } from '#/layouts/Drive/persistentState'
import { useDownloadDirectory } from '#/layouts/Drive/useDownloadDirectory'
import SearchBar from '#/layouts/SearchBar'
import { regexEscape } from '#/utilities/string'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useBackends, useFullUserSession, useSession, useText } from '$/providers/react'
import { includesPredicate } from '$/utils/data/array'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import * as React from 'react'
import {
  ALL_SETTINGS_TABS,
  SETTINGS_DATA,
  SETTINGS_NO_RESULTS_SECTION_DATA,
  SETTINGS_TAB_DATA,
  type SettingsContext,
  type SettingsEntryData,
  type SettingsTabData,
} from './data'
import SettingsSidebar from './Sidebar'
import SettingsTab from './Tab'
import SettingsTabType from './TabType'

/** Settings screen. */
export function Settings() {
  const queryClient = useQueryClient()
  const { remoteBackend: backend, localBackend } = useBackends()
  const [tab, setTab] = useSearchParamsState(
    'SettingsTab',
    SettingsTabType.account,
    includesPredicate(Object.values(SettingsTabType)),
  )
  const { user, accessToken } = useFullUserSession()
  const { changePassword } = useSession()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const [query, setQuery] = React.useState('')
  const root = usePortalContext()
  const { data: organization = null } = useQuery(
    backendQueryOptions(backend, 'getOrganization', []),
  )
  const isQueryBlank = !/\S/.test(query)
  const [preferredTimeZone, setPreferredTimeZone] = useLocalStorageState('preferredTimeZone')

  const updateUser = useMutationCallback(backendMutationOptions(backend, 'updateUser'))
  const updateOrganization = useMutationCallback(
    backendMutationOptions(backend, 'updateOrganization'),
  )

  const localRootDirectory = useLocalRootDirectory() ?? localBackend?.rootPath() ?? null
  const downloadDirectory = useDownloadDirectory()

  const isMatch = React.useMemo(() => {
    const regex = new RegExp(regexEscape(query.trim()).replace(/\s+/g, '.+'), 'i')
    return (name: string) => regex.test(name)
  }, [query])

  const context = React.useMemo<SettingsContext>(
    () => ({
      accessToken,
      user,
      backend,
      localBackend,
      organization,
      updateUser,
      updateOrganization,
      localRootDirectory,
      downloadDirectory,
      toastAndLog,
      getText,
      queryClient,
      isMatch,
      changePassword,
      preferredTimeZone,
      setPreferredTimeZone,
    }),
    [
      accessToken,
      backend,
      getText,
      localBackend,
      organization,
      toastAndLog,
      updateUser,
      updateOrganization,
      localRootDirectory,
      downloadDirectory,
      user,
      queryClient,
      isMatch,
      changePassword,
      preferredTimeZone,
      setPreferredTimeZone,
    ],
  )

  const doesEntryMatchQuery = React.useCallback(
    (entry: SettingsEntryData) => {
      switch (entry.type) {
        case 'form': {
          return entry.inputs.some((input) => isMatch(getText(input.nameId)))
        }
        case 'custom': {
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
  const effectiveTab = tabsToShow.includes(tab) ? tab : (tabsToShow[0] ?? SettingsTabType.account)

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

  const changeTab = useEventCallback(() => {
    if (tab !== effectiveTab) {
      setTab(tab)
    }
  })

  return (
    <ErrorBoundary>
      <div className="flex h-full w-full flex-col gap-4 overflow-hidden pl-page-x pt-4">
        <Heading level={1} className="flex items-center px-heading-x">
          <Menu.Trigger>
            <Button variant="icon" icon="3_dot_menu" className="mr-3 sm:hidden" />
            <Popover size="auto" UNSTABLE_portalContainer={root}>
              <SettingsSidebar
                context={context}
                tabsToShow={tabsToShow}
                tab={effectiveTab}
                setTab={setTab}
              />
            </Popover>
          </Menu.Trigger>

          <Text nowrap variant="h1" className="cursor-default font-bold">
            {getText('settingsFor')}
          </Text>

          <Text
            variant="h1"
            truncate="1"
            className="ml-2.5 mr-8 max-w-[min(32rem,_100%)] cursor-default rounded-full bg-white px-2.5 font-bold"
            aria-hidden
          >
            {data.organizationOnly === true ?
              (organization?.name ?? 'your organization')
            : user.name}
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
            <SettingsTab context={context} data={data} onInteracted={changeTab} />
          </main>
        </div>
      </div>
    </ErrorBoundary>
  )
}
