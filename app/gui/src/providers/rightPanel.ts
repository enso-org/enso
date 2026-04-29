import { useIsFeatureUnderPaywall } from '$/composables/paywall'
import { useBackends } from '$/providers/backends'
import { CATEGORY_BACKEND, isCloudCategory, type Category } from '$/providers/category'
import { proxyRefs, type ToValue } from '$/utils/reactivity'
import { useSyncLocalStorage } from '@/composables/syncLocalStorage'
import { createContextStore } from '@/providers'
import type { Icon } from '@/util/iconMetadata/iconName'
import { useQuery } from '@tanstack/vue-query'
import { AssetType, ProjectId, type AnyAsset } from 'enso-common/src/services/Backend'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { encoding } from 'lib0'
import { computed, reactive, readonly, ref, toValue, type Ref } from 'vue'
import type { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import { panelKey, type Panel } from './container'
import { useText, type TextStore } from './text'

/** Information about content of "Help" panel. */
export interface DisplayedHelp {
  item: Result<SuggestionId | undefined>
  aiMode: boolean
}

/** Possible elements in this screen to spotlight on. */
export type AssetPropertiesSpotlight = 'datalink' | 'secret'

/**
 * Context of right-side panels. See {@link useRightPanel}.
 */
export interface RightPanelContext {
  category?: Category
  // TODO[ao]: Project View could get full information about asset, so this type could be just
  //  `AnyAsset | undefined`
  item?: AnyAsset | ProjectId | undefined
  defaultItem?: AnyAsset | undefined
  spotlightOn?: AssetPropertiesSpotlight | undefined
  help?: DisplayedHelp
}

interface RightPanelTabInfo {
  icon: Icon
  /**
   * If Err, the tab is disabled and the error message is presented to user as reason of
   * disabling.
   */
  enabled: ToValue<Result<void>>
  hidden?: ToValue<boolean>
  title: ToValue<string>
}

function useRightPanelTabs(
  focusedPanel: ToValue<Panel>,
  rightPanelContext: Ref<RightPanelContext | undefined>,
  { textRef, getText }: TextStore,
) {
  const isFeatureUnderPaywall = useIsFeatureUnderPaywall()
  const isCloudDirectoryView = computed(
    () =>
      rightPanelContext.value?.category != null &&
      isCloudCategory(rightPanelContext.value.category),
  )
  const enabledInCloudOnly = computed(() =>
    isCloudDirectoryView.value ? Ok() : Err('Exclusive to Cloud'),
  )
  return new Map([
    [
      'description',
      {
        icon: 'info',
        enabled: enabledInCloudOnly,
        title: 'Description',
      },
    ],
    [
      'contents',
      {
        icon: 'docs',
        enabled: Ok(),
        hidden: true,
        title: 'Contents',
      },
    ],
    [
      'settings',
      {
        icon: 'properties',
        enabled: enabledInCloudOnly,
        title: textRef('properties'),
      },
    ],
    [
      'versions',
      {
        icon: 'history',
        enabled: enabledInCloudOnly,
        title: textRef('versions'),
      },
    ],
    [
      'sessions',
      {
        icon: 'activity',
        enabled: Ok(),
        title: textRef('projectSessions'),
      },
    ],
    [
      'executionsCalendar',
      {
        icon: 'schedule',
        enabled: computed(() => {
          if (!enabledInCloudOnly.value.ok) return enabledInCloudOnly.value
          if (isFeatureUnderPaywall('scheduler'))
            return Err(getText('assetProjectExecutionsCalendar.teamPlanOnly'))
          return Ok()
        }),
        title: textRef('executionsCalendar'),
      },
    ],
    [
      'documentation',
      {
        icon: 'docs',
        enabled: computed(() => {
          const panelType = toValue(focusedPanel).type
          return panelType === 'project' || panelType === 'drive' ?
              Ok()
            : Err('Exclusive to Project and Drive panels')
        }),
        title: textRef('docs'),
      },
    ],
    [
      'help',
      {
        icon: 'help',
        enabled: computed(() =>
          toValue(focusedPanel).type === 'project' ? Ok() : Err('Exclusive to Project'),
        ),
        title: 'Component help',
      },
    ],
  ] as const satisfies [string, RightPanelTabInfo][])
}

export type RightPanelTabId =
  ReturnType<typeof useRightPanelTabs> extends Map<infer K, any> ? K : never

export type RightPanelData = ReturnType<typeof useRightPanel>

function useRightPanel(focusedPanel: ToValue<Panel>, textStore: TextStore = useText()) {
  const { backendForType } = useBackends()
  const contextPerPanel = reactive(new Map<ReturnType<typeof panelKey>, RightPanelContext>())
  const context = computed(() => contextPerPanel.get(panelKey(toValue(focusedPanel))))
  const allTabs = useRightPanelTabs(focusedPanel, context, textStore)
  const fullscreen = ref(false)
  const temporaryTab = ref<RightPanelTabId>()
  const tab = ref<RightPanelTabId>()

  useSyncLocalStorage({
    storageKey: 'rightPanel',
    mapKeyEncoder: (enc) => encoding.writeVarString(enc, panelKey(toValue(focusedPanel))),
    debounce: 200,
    captureState: () => ({
      tab: tab.value,
    }),
    restoreState: (state) => {
      if (state) {
        tab.value = state.tab
      } else {
        tab.value = toValue(focusedPanel).type === 'project' ? 'documentation' : undefined
      }
    },
  })

  const displayedTab = computed(() => {
    const markedTab = temporaryTab.value ?? tab.value
    if (markedTab == null) return undefined
    const tabInfo = allTabs.get(markedTab)
    if (!tabInfo || toValue(tabInfo.hidden)) return undefined
    if (!toValue(tabInfo.enabled)?.ok) return undefined
    return markedTab
  })

  /**
   * Set context from given tab.
   *
   * Every tab may register and update the context assigned to it, which will be active when the
   * tab is selected.
   */
  function setContext(panel: Panel, ctx: RightPanelContext) {
    contextPerPanel.set(panelKey(panel), ctx)
  }

  /**
   * Update context for given tab.
   *
   * If the tab didn't set any context, this method does nothing.
   */
  function updateContext(panel: Panel, f: (ctx: RightPanelContext) => RightPanelContext) {
    const key = panelKey(panel)
    const ctx = contextPerPanel.get(key)
    if (ctx == null) return
    const newCtx = f(ctx)
    contextPerPanel.set(key, newCtx)
  }

  const focusedProject = computed(() => {
    if (typeof context.value?.item === 'string') {
      return context.value.item
    } else if (context.value?.item?.type === AssetType.project) {
      return context.value.item.id
    } else {
      return undefined
    }
  })

  const focusedAsset = computed<AnyAsset | undefined>(() => {
    const currentItem = context.value?.item ?? context.value?.defaultItem
    return typeof currentItem === 'object' ? currentItem : undefined
  })

  const backendType = computed(
    () => context.value?.category && CATEGORY_BACKEND[context.value.category.type],
  )

  const focusedAssetDetailsQuery = useQuery({
    queryKey: [backendType, 'getAssetDetails', focusedAsset] as const,
    queryFn: async (query) => {
      const [backendType, , currentItem] = query.queryKey
      if (!backendType || !currentItem) return null
      return await backendForType(backendType).getAssetDetails(currentItem.id, undefined)
    },
    enabled: () => backendType.value != null && focusedAsset.value != null,
    meta: { persist: false },
  })
  const focusedAssetDetails = focusedAssetDetailsQuery.data

  function setTab(newTab: RightPanelTabId | undefined) {
    tab.value = newTab
    temporaryTab.value = undefined
  }

  function toggleTab(specificTab?: RightPanelTabId | undefined) {
    if (specificTab == null || tab.value == specificTab) {
      setTab(undefined)
    } else {
      setTab(specificTab)
    }
  }

  return proxyRefs({
    allTabs,
    tab: readonly(tab),
    /** Tab which should be displayed (taking temporary tab into consideration). */
    displayedTab,
    setTab,
    toggleTab,
    /**
     * A tab displayed temporarily. It overrides the tab clicked by user.
     *
     * The usages include displaying asset properties when editing Datalink - once the edit stops,
     * the tab is restored to previous state.
     */
    temporaryTab,
    setTemporaryTab: (tab: RightPanelTabId | undefined) => (temporaryTab.value = tab),
    fullscreen,
    context,
    setContext,
    updateContext,
    /**
     * The project being a focus of the right panel, e.g. the currently opened project tab or
     * selected project in Drive View.
     */
    focusedProject,
    /**
     * The asset being a focus of the right panel, e.g. the currently selected asset in Drive View.
     */
    focusedAsset,
    /** The details for `focusedAsset`. */
    focusedAssetDetails,
  })
}

export const [provideRightPanelData, useRightPanelData] = createContextStore(
  'rightPanel',
  useRightPanel,
)
