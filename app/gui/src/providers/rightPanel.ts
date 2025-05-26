import { type PaywallFeatureName } from '#/hooks/billing/FeaturesConfiguration'
import { Category, isCloudCategory } from '#/layouts/CategorySwitcher/Category'
import { AnyAsset, AssetType, ProjectId } from '#/services/Backend'
import {
  AssetProperties,
  AssetVersions,
  ProjectExecutionsCalendar,
  ProjectSessions,
} from '$/components/TabView/reactTabs'
import ComponentDocumentation from '@/components/ComponentDocumentation.vue'
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import { createContextStore } from '@/providers'
import { Err, Ok, Result } from '@/util/data/result'
import { Icon } from '@/util/iconMetadata/iconName'
import { ToValue } from '@/util/reactivity'
import { useLocalStorage } from '@vueuse/core'
import { Component, computed, proxyRefs, reactive, Ref, ref, toRef, toValue } from 'vue'
import { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'
import { TabId } from './container'
import { injectText, TextStore } from './text'

export interface DisplayedHelp {
  item: Result<SuggestionId | undefined>
  aiMode: boolean
}

/** Possible elements in this screen to spotlight on. */
export type AssetPropertiesSpotlight = 'datalink' | 'description' | 'secret'

export interface RightPanelContext {
  category?: Category
  item?: AnyAsset | ProjectId | undefined
  defaultItem?: AnyAsset | undefined
  spotlightOn?: AssetPropertiesSpotlight | undefined
  help?: DisplayedHelp
}

export interface RightPanelTabInfo {
  icon: Icon
  enabled: ToValue<Result<boolean>>
  title: ToValue<string>
  component: Component
}

interface RightPanelStore {
  tab: RightPanelTabId | undefined
  width: number | undefined
}

function useRightPanelTabs(
  currentTab: ToValue<TabId>,
  rightPanelContext: Ref<RightPanelContext | undefined>,
  isFeatureUnderPaywall: (feature: PaywallFeatureName) => boolean,
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
    isCloudDirectoryView.value ? Ok(true)
    : isDriveView.value ? Err('Exclusive to Cloud')
    : Err('Exclusive to Cloud category in Drive'),
  )
  return new Map([
    [
      'settings',
      {
        icon: 'properties',
        enabled: enabledInCloudOnly,
        title: textRef('properties'),
        component: AssetProperties,
      },
    ],
    [
      'versions',
      {
        icon: 'versions',
        enabled: enabledInCloudOnly,
        title: textRef('versions'),
        component: AssetVersions,
      },
    ],
    [
      'sessions',
      {
        icon: 'sessions',
        enabled: enabledInCloudOnly,
        title: textRef('projectSessions'),
        component: ProjectSessions,
      },
    ],
    [
      'executionsCalendar',
      {
        icon: 'schedule',
        enabled: computed(() => {
          if (!enabledInCloudOnly.value.ok) return enabledInCloudOnly.value
          if (!isFeatureUnderPaywall('scheduler'))
            return Err(getText('assetProjectExecutionsCalendar.teamPlanOnly'))
          return Ok(true)
        }),
        title: textRef('executionsCalendar'),
        component: ProjectExecutionsCalendar,
      },
    ],
    [
      'documentation',
      {
        icon: 'docs',
        enabled: Ok(true),
        title: textRef('docs'),
        component: DocumentationEditor,
      },
    ],
    [
      'help',
      {
        icon: 'help',
        enabled: computed(() => {
          const tab = toValue(currentTab)
          return tab !== 'drive' && tab !== 'settings' ? Ok(true) : Err('Exclusive to Project view')
        }),
        title: 'Component help',
        component: ComponentDocumentation,
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
  textStore: TextStore = injectText(),
) {
  const contextPerTab = reactive(new Map<TabId, RightPanelContext>())
  const context = computed(() => contextPerTab.get(toValue(containerTab)))
  const allTabs = useRightPanelTabs(containerTab, context, isFeatureUnderPaywall, textStore)
  const fullscreen = ref(false)
  const temporaryTab = ref<RightPanelTabId>()

  const store = useLocalStorage<RightPanelStore>('rightPanel', {
    tab: undefined,
    width: undefined,
  })

  function setContext(tab: TabId, ctx: RightPanelContext) {
    contextPerTab.set(tab, ctx)
  }

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

  function toggleTab(specificTab?: RightPanelTabId | undefined) {
    if (specificTab == null || store.value.tab == specificTab) {
      store.value.tab = undefined
    } else {
      store.value.tab = specificTab
    }
  }

  return proxyRefs({
    allTabs,
    tab: toRef(store.value, 'tab'),
    toggleTab,
    temporaryTab,
    setTemporaryTab: (tab: RightPanelTabId | undefined) => (temporaryTab.value = tab),
    width: toRef(store.value, 'width'),
    fullscreen,
    context,
    setContext,
    updateContext,
    focusedProject,
    focusedAsset,
  })
}

export const [provideRightPanelData, injectRightPanelData] = createContextStore(
  'rightPanel',
  useRightPanel,
)
