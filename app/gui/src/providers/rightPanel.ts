import { type PaywallFeatureName } from '#/hooks/billing/FeaturesConfiguration'
import { Category, isCloudCategory } from '#/layouts/CategorySwitcher/Category'
import { AnyAsset, AssetType, ProjectId } from '#/services/Backend'
import { createContextStore } from '@/providers'
import { Err, Ok, Result } from '@/util/data/result'
import { Icon } from '@/util/iconMetadata/iconName'
import { ToValue } from '@/util/reactivity'
import { useLocalStorage } from '@vueuse/core'
import { computed, proxyRefs, reactive, readonly, Ref, ref, toRef, toValue } from 'vue'
import { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import { TabId } from './container'
import { TextStore, useText } from './text'

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

/** Right Panel Data kept in local storage. */
interface RightPanelStore {
  tab: RightPanelTabId | undefined
  width: number | undefined
}

function useRightPanelTabs(
  currentTab: ToValue<TabId>,
  rightPanelContext: Ref<RightPanelContext | undefined>,
  isFeatureUnderPaywall: (feature: PaywallFeatureName) => boolean,
  enableScheduledExecution: ToValue<boolean>,
  { textRef, getText }: TextStore,
) {
  const isDriveView = computed(() => toValue(currentTab) === 'drive')
  const isCloudDirectoryView = computed(
    () =>
      isDriveView.value &&
      rightPanelContext.value?.category != null &&
      isCloudCategory(rightPanelContext.value.category),
  )
  const enabledInCloudOnly = computed(() =>
    isCloudDirectoryView.value ? Ok()
    : isDriveView.value ? Err('Exclusive to Cloud')
    : Err('Exclusive to Cloud category in Drive'),
  )
  return new Map([
    [
      'description',
      {
        icon: 'text',
        enabled: enabledInCloudOnly,
        title: 'Description',
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
        icon: 'versions',
        enabled: enabledInCloudOnly,
        title: textRef('versions'),
      },
    ],
    [
      'sessions',
      {
        icon: 'sessions',
        enabled: enabledInCloudOnly,
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
        hidden: computed(() => !toValue(enableScheduledExecution)),
        title: textRef('executionsCalendar'),
      },
    ],
    [
      'documentation',
      {
        icon: 'docs',
        enabled: Ok(),
        title: textRef('docs'),
      },
    ],
    [
      'help',
      {
        icon: 'help',
        enabled: computed(() => {
          const tab = toValue(currentTab)
          return tab !== 'drive' && tab !== 'settings' ? Ok() : Err('Exclusive to Project view')
        }),
        title: 'Component help',
      },
    ],
  ] as const satisfies [string, RightPanelTabInfo][])
}

export type RightPanelTabId =
  ReturnType<typeof useRightPanelTabs> extends Map<infer K, any> ? K : never

export type RightPanelData = ReturnType<typeof useRightPanel>

function useRightPanel(
  containerTab: ToValue<TabId>,
  isFeatureUnderPaywall: (feature: PaywallFeatureName) => boolean,
  enableScheduledExecution: ToValue<boolean>,
  textStore: TextStore = useText(),
) {
  const contextPerTab = reactive(new Map<TabId, RightPanelContext>())
  const context = computed(() => contextPerTab.get(toValue(containerTab)))
  const allTabs = useRightPanelTabs(
    containerTab,
    context,
    isFeatureUnderPaywall,
    enableScheduledExecution,
    textStore,
  )
  const fullscreen = ref(false)
  const temporaryTab = ref<RightPanelTabId>()

  const store = useLocalStorage<RightPanelStore>('rightPanel', {
    tab: undefined,
    width: undefined,
  })

  const displayedTab = computed(() => {
    const markedTab = temporaryTab.value ?? store.value.tab
    if (markedTab == null) return undefined
    if (!toValue(allTabs.get(markedTab)?.enabled)?.ok) return undefined
    return markedTab
  })

  /**
   * Set context from given tab.
   *
   * Every tab may register and update the context assigned to it, which will be active when the
   * tab is selected.
   */
  function setContext(tab: TabId, ctx: RightPanelContext) {
    contextPerTab.set(tab, ctx)
  }

  /**
   * Update context for given tab.
   *
   * If the tab didn't set any context, this method does nothing.
   */
  function updateContext(tab: TabId, f: (ctx: RightPanelContext) => RightPanelContext) {
    const ctx = contextPerTab.get(tab)
    if (ctx == null) return
    const newCtx = f(ctx)
    contextPerTab.set(tab, newCtx)
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

  function setTab(tab: RightPanelTabId | undefined) {
    store.value.tab = tab
    temporaryTab.value = undefined
  }

  function toggleTab(specificTab?: RightPanelTabId | undefined) {
    if (specificTab == null || store.value.tab == specificTab) {
      setTab(undefined)
    } else {
      setTab(specificTab)
    }
  }

  return proxyRefs({
    allTabs,
    tab: readonly(toRef(store.value, 'tab')),
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
    width: toRef(store.value, 'width'),
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
  })
}

export const [provideRightPanelData, useRightPanelData] = createContextStore(
  'rightPanel',
  useRightPanel,
)
