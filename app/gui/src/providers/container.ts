import LocalStorage from '#/utilities/LocalStorage'
import { proxyRefs } from '$/utils/reactivity'
import type { Opt } from '@/util/data/opt'
import { createGlobalState } from '@vueuse/core'
import { BackendType, isProjectId, Plan } from 'enso-common/src/services/Backend'
import { Err } from 'enso-common/src/utilities/data/result'
import { computed, onScopeDispose, reactive, ref, watchEffect } from 'vue'
import { useRoute, useRouter, type RouteLocation } from 'vue-router'
import * as z from 'zod'
import { useAuth } from './auth'
import { useBackends } from './backends'
import { useFeatureFlag } from './featureFlags'
import { useOpenedProjects, type Project } from './openedProjects'
import {
  PROJECT_ID_SCHEMA,
  RUNNING_PROJECT_INFO_SCHEMA,
  type ProjectInfo,
  type RunningProjectInfo,
} from './openedProjects/projectInfo'

const PROJECT_TAB_SCHEMA = z.object({ type: z.literal('project'), id: PROJECT_ID_SCHEMA })
const SETTINGS_TAB_SCHEMA = z.object({ type: z.literal('settings') })
const TAB_SCHEMA = z.discriminatedUnion('type', [PROJECT_TAB_SCHEMA, SETTINGS_TAB_SCHEMA])
const OPENED_TAB_SCHEMA = z.intersection(
  TAB_SCHEMA,
  z.object({ runningProject: z.optional(RUNNING_PROJECT_INFO_SCHEMA) }),
)

export type ProjectTab = z.infer<typeof PROJECT_TAB_SCHEMA>
export type Tab = z.infer<typeof TAB_SCHEMA>
type OpenedTab = z.infer<typeof OPENED_TAB_SCHEMA>

export type Panel = Tab | { type: 'drive' }

declare module '#/utilities/LocalStorage' {
  interface LocalStorageData {
    readonly openedTabs: (Tab & { runningProject?: RunningProjectInfo | undefined })[]
    readonly rightPanelWidth: number
    readonly leftPanelWidth: number
  }
}

LocalStorage.registerKey('openedTabs', { schema: z.array(OPENED_TAB_SCHEMA) })
LocalStorage.registerKey('rightPanelWidth', { schema: z.number() })
LocalStorage.registerKey('leftPanelWidth', { schema: z.number() })

export function tabFromRoute(route: RouteLocation) {
  switch (route.name) {
    case 'project': {
      if (!isProjectId(route.params.id)) return null
      // const project = openedProjects.get(route.params.id)
      // if (!project) return null
      return { type: 'project' as const, id: route.params.id }
    }
    case 'settings':
      return { type: 'settings' as const }
    default:
      return null
  }
}

export function panelKey(panel: Opt<Panel>) {
  switch (panel?.type) {
    case 'project':
      return `project/${panel.id}`
    case 'settings':
    case 'drive':
      return panel.type
    default:
      return ''
  }
}

export function panelEquals(a: Opt<Panel>, b: Opt<Panel>) {
  return panelKey(a) === panelKey(b)
}

function isProjectShownAsTab(project: Project) {
  return (
    project.nextTask?.process === 'opening' ||
    project.error != null ||
    (project.state.status !== 'not-opened' && project.nextTask?.process !== 'closing')
  )
}

export type ContainerData = ReturnType<typeof useContainerData>
function createContainerStore() {
  const router = useRouter()
  const route = useRoute()
  const openedProjects = useOpenedProjects()
  const localStorage = LocalStorage.getInstance()
  const modesForBackend = useModesForBackend()
  const tabs: Tab[] = reactive([])
  const focusedPanel = ref<Panel>()

  const currentTab = computed<Tab | null>({
    get: () => tabFromRoute(route),
    set: (tab) => {
      if (panelEquals(tab, currentTab.value)) return
      switch (tab?.type) {
        case 'project':
          router.push({ name: 'project', params: { id: tab.id }, query: route.query })
          break
        case 'settings':
          router.push({ name: 'settings', query: route.query })
          break
        case null:
          router.push({ name: 'dashboard', query: route.query })
      }
    },
  })

  const leftPanelWidth = localStorage.ref('leftPanelWidth')
  const rightPanelWidth = localStorage.ref('rightPanelWidth')

  function isTabOpened(tab: Tab) {
    return tabs.some((openedTab) => panelEquals(openedTab, tab))
  }

  function isCurrentTab(tab: Tab) {
    return panelEquals(tab, currentTab.value)
  }

  function openProjectTab(info: ProjectInfo, userAction = true) {
    const tab: Tab = { type: 'project', id: info.id }
    if (!isTabOpened(tab)) {
      const project = openedProjects.openProject(info)
      tabs.push(tab)
      if (userAction) {
        openedProjects.waitForProcess(project).then(() => (currentTab.value = tab))
      }
    }
  }

  /** Checks if project with given backend type may be opened locally. */
  function canOpenProjectLocally(backend: BackendType) {
    return modesForBackend.value.locally[backend] != null
  }

  /** Open project locally, by asset data and backend type. */
  function openProjectLocally(
    info: Omit<ProjectInfo, 'mode'>,
    backend: BackendType,
    userAction = true,
  ) {
    const mode = modesForBackend.value.locally[backend]
    if (mode != null) {
      return openProjectTab({ ...info, mode }, userAction)
    }
  }

  /** Checks if project with given backend type may be opened natively. */
  function canOpenProjectNatively(backend: BackendType) {
    return modesForBackend.value.natively[backend] != null
  }

  /** Open project natively, by asset data and backend type. */
  function openProjectNatively(
    info: Omit<ProjectInfo, 'mode'>,
    backend: BackendType,
    userAction = true,
  ) {
    const mode = modesForBackend.value.natively[backend]
    if (mode != null) {
      return openProjectTab({ ...info, mode }, userAction)
    }
  }

  function openSettingsTab() {
    const tab: Tab = { type: 'settings' }
    if (!isTabOpened(tab)) {
      tabs.push(tab)
    }
    currentTab.value = tab
  }

  function closeTab(tab: Tab) {
    const index = tabs.findIndex((opened) => panelEquals(opened, tab))
    if (index < 0) return Err(`Tab to close not found: ${JSON.stringify(tab)}`)
    tabs.splice(index, 1)
    if (tab.type === 'project') {
      openedProjects.closeProject(tab.id)
    }
  }

  function closeCurrentTab() {
    if (currentTab.value != null) closeTab(currentTab.value)
  }

  /**
   * Read and restore projects from local storage, and then keep the storage up-to-date about
   * currently opened projects.
   */
  function syncWithLocalStorage() {
    for (const tab of localStorage.get('openedTabs') ?? []) {
      if (tab.runningProject != null) openedProjects.restoreProject(tab.runningProject)
      tabs.push(tab)
    }

    return watchEffect(() => {
      const openedTabs: OpenedTab[] = []
      for (const tab of tabs) {
        let runningProject: RunningProjectInfo | undefined
        if (tab.type === 'project') {
          const project = openedProjects.get(tab.id)
          switch (project?.state.status) {
            case 'opened':
            case 'initialized':
            case 'hybrid-closed':
            case 'to-restore':
            case 'closed-by-backend':
              runningProject = project.state.info
              break
          }
        }
        openedTabs.push({ ...tab, runningProject })
      }
      localStorage.set('openedTabs', openedTabs)
    })
  }

  const stopSyncing = syncWithLocalStorage()
  onScopeDispose(stopSyncing)

  return proxyRefs({
    currentTab,
    tabs,
    focusedPanel,
    leftPanelWidth,
    rightPanelWidth,
    isTabOpened,
    isCurrentTab,
    openProjectTab,
    canOpenProjectLocally,
    openProjectLocally,
    canOpenProjectNatively,
    openProjectNatively,
    openSettingsTab,
    closeTab,
    closeCurrentTab,
  })
}

/**
 * Return structure specifying a project opening mode for local/native runs, and if they are
 * available to the user.
 */
function useModesForBackend() {
  const auth = useAuth()
  const enableCloudExecution = useFeatureFlag('enableCloudExecution')
  const backends = useBackends()
  return computed(() => ({
    locally: {
      [BackendType.local]: backends.localBackend != null ? ('local' as const) : null,
      [BackendType.remote]: backends.localBackend != null ? ('hybrid' as const) : null,
    },
    // Local projects can be run natively; only Team plans and above have access to Cloud execution.
    // Local projects: Open normally
    // Cloud projects: Open in Cloud VM
    natively: {
      [BackendType.local]: backends.localBackend != null ? ('local' as const) : null,
      [BackendType.remote]:
        (
          enableCloudExecution.value &&
          (auth.session?.user.plan === Plan.team || auth.session?.user.plan === Plan.enterprise)
        ) ?
          ('cloud' as const)
        : null,
    },
  }))
}

export const useContainerData = createGlobalState(createContainerStore)
