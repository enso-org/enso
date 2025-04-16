import { AssetPropertiesSpotlight } from '#/layouts/AssetPanel/components/AssetProperties'
import { Category, isCloudCategory } from '#/layouts/CategorySwitcher/Category'
import { LaunchedProject } from '#/providers/ProjectsProvider'
import { AnyAsset, AssetType, ProjectId } from '#/services/Backend'
import {
  AssetProperties,
  AssetVersions,
  ProjectExecutionsCalendar,
  ProjectSessions,
} from '$/components/TabView/reactTabs'
import { injectText } from '$/providers/text'
import ComponentDocumentation from '@/components/ComponentDocumentation.vue'
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import { createContextStore } from '@/providers'
import { SuggestionId } from '@/stores/suggestionDatabase/entry'
import { Err, Ok, Result } from '@/util/data/result'
import { Icon } from '@/util/iconMetadata/iconName'
import { ToValue } from '@/util/reactivity'
import { useLocalStorage } from '@vueuse/core'
import { Component, computed, proxyRefs, reactive, ref, Ref, toRef } from 'vue'
import { useRoute, useRouter } from 'vue-router'

export type TabId = 'drive' | 'settings' | ProjectId

export interface DisplayedHelp {
  item: Result<SuggestionId | undefined>
  aiMode: boolean
}

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

function useRightPanelTabs(
  currentTab: Ref<TabId>,
  rightPanelContext: Ref<RightPanelContext | undefined>,
) {
  const { textRef } = injectText()
  const isDriveView = computed(() => currentTab.value === 'drive')
  const isCloudDirectoryView = computed(
    () =>
      isDriveView.value &&
      rightPanelContext.value?.category != null &&
      isCloudCategory(rightPanelContext.value.category),
  )
  const enabledInCloudOnly = () =>
    computed(() =>
      isCloudDirectoryView.value ? Ok(true)
      : isDriveView.value ? Err('Exclusive to Cloud')
      : Err('Exclusive to Cloud category in Drive'),
    )
  return new Map([
    [
      'settings',
      {
        icon: 'properties',
        enabled: enabledInCloudOnly(),
        title: textRef('properties'),
        component: AssetProperties,
      },
    ],
    [
      'versions',
      {
        icon: 'versions',
        enabled: enabledInCloudOnly(),
        title: textRef('versions'),
        component: AssetVersions,
      },
    ],
    [
      'sessions',
      {
        icon: 'sessions',
        enabled: enabledInCloudOnly(),
        title: textRef('projectSessions'),
        component: ProjectSessions,
      },
    ],
    [
      'executionsCalendar',
      {
        icon: 'schedule',
        enabled: enabledInCloudOnly(),
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
        enabled: computed(() =>
          currentTab.value !== 'drive' && currentTab.value !== 'settings' ?
            Ok(true)
          : Err('Exclusive to Project view'),
        ),
        title: 'Component help',
        component: ComponentDocumentation,
      },
    ],
  ] as const satisfies [string, RightPanelTabInfo][])
}

export type RightPanelTabId =
  ReturnType<typeof useRightPanelTabs> extends Map<infer K, any> ? K : never

export type RightPanelData = ReturnType<typeof useRightPanel>
function useRightPanel(containerTab: Ref<TabId>, store: Ref<ContainerStore>) {
  const contextPerTab = reactive(new Map<TabId, RightPanelContext>())
  const context = computed(() => contextPerTab.get(containerTab.value))
  const allTabs = useRightPanelTabs(containerTab, context)
  const fullscreen = ref(false)

  const temporaryTab = ref<RightPanelTabId>()

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
    if (specificTab == null || store.value.rightPanelTab == specificTab) {
      store.value.rightPanelTab = undefined
    } else {
      store.value.rightPanelTab = specificTab
    }
  }

  return proxyRefs({
    allTabs,
    tab: toRef(store.value, 'rightPanelTab'),
    toggleTab,
    temporaryTab,
    setTemporaryTab: (tab: RightPanelTabId | undefined) => (temporaryTab.value = tab),
    width: toRef(store.value, 'rightPanelWidth'),
    fullscreen,
    context,
    setContext,
    updateContext,
    focusedProject,
    focusedAsset,
  })
}

interface ContainerStore {
  // openedProjects: LaunchedProject[]
  rightPanelTab: RightPanelTabId | undefined
  rightPanelWidth: number | undefined
}

export type ContainerData = ReturnType<typeof injectConainerData>
export const [provideContainerData, injectConainerData] = createContextStore(
  'gui-container',
  (launchedProjectsFromReact: ToValue<readonly LaunchedProject[]>) => {
    const router = useRouter()
    const route = useRoute()
    const openedProjects = toRef(launchedProjectsFromReact)

    const isValidTab = (name: typeof route.query.page): name is TabId =>
      name === 'drive' ||
      name === 'settings' ||
      openedProjects.value.find((p) => p.id === name) != null

    const tab = computed<TabId>({
      get: () => (isValidTab(route.query.page) ? route.query.page : 'drive'),
      set: (page) => {
        router.push({ query: { ...route.query, page } })
      },
    })

    const store = useLocalStorage<ContainerStore>('container', {
      rightPanelTab: undefined,
      rightPanelWidth: undefined,
    })

    const rightPanel = useRightPanel(tab, store)

    return {
      openedProjects,
      rightPanel,
      tab,
    }
  },
)
