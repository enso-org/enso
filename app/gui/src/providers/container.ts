import { EnsoPath } from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import { createContextStore } from '@/providers'
import { proxyRefs } from '@/util/reactivity'
import { normalizeRouteParamToString } from '@/util/router'
import { filter } from 'enso-common/src/utilities/data/iter'
import { computed, watchEffect } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useOpenedProjects, type Project } from './openedProjects'
import type { RunningProjectInfo } from './openedProjects/projectStates'

/** Tab identifier, equal to the path of the view's URL. */
export type TabId = 'drive' | 'settings' | EnsoPath

/** Check if given {@link TabId} refers to a project tab. */
export function isProjectTab(tab: TabId): tab is EnsoPath {
  switch (tab) {
    case 'drive':
    case 'settings':
      return false
    default:
      DEV: tab satisfies EnsoPath
      return true
  }
}

function isProjectShownAsTab(project: Project) {
  return (
    project.nextTask?.process === 'opening' ||
    project.error != null ||
    (project.state.status !== 'not-opened' &&
      project.state.status !== 'hybrid-closed' &&
      project.state.status !== 'hybrid-uploaded')
  )
}

export type ContainerData = ReturnType<typeof useContainerData>
export const [provideContainerData, useContainerData] = createContextStore(
  'gui-container',
  (fallbackTab: TabId = 'drive') => {
    const router = useRouter()
    const route = useRoute()
    const openedProjects = useOpenedProjects()
    const localStorage = LocalStorage.getInstance()

    const projectsClosedByBackend = []
    for (const project of localStorage.get('openedTabs') ?? []) {
      if (project.mode === 'local' || project.mode === 'hybrid') {
        openedProjects.openProject(project)
      } else {
        projectsClosedByBackend.push(openedProjects)
      }
    }

    const projectTabs = computed(() =>
      Array.from(filter(openedProjects.listProjects(), isProjectShownAsTab), (project) => ({
        ...project,
        shown: computed(() => tab.value === project.state.info.ensoPath),
      })),
    )

    const isValidTab = (name: string | undefined): name is TabId =>
      name === 'drive' ||
      name === 'settings' ||
      projectTabs.value.find((p) => p.state.info.ensoPath === name) != null

    const tab = computed<TabId>({
      get: () => {
        const name = normalizeRouteParamToString(route.params.path)
        return isValidTab(name) ? name : fallbackTab
      },
      set: (page) => {
        router.push({ params: { path: page.split('/') }, query: route.query })
      },
    })

    // When the current tab is no longer valid (e.g. the project was closed), switch to the fallback tab.
    watchEffect(() => {
      const name = normalizeRouteParamToString(route.params.path)
      if (!isValidTab(name)) {
        tab.value = fallbackTab
      }
    })

    watchEffect(() => {
      const openedTabs: RunningProjectInfo[] = []
      const unuploadedProjects: RunningProjectInfo[] = []
      for (const project of openedProjects.listProjects()) {
        if (project.state.status === 'opened' || project.state.status === 'initialized') {
          openedTabs.push(project.state.info)
        } else if (project.state.status === 'hybrid-closed') {
          unuploadedProjects.push(project.state.info)
        }
      }
      localStorage.set('openedTabs', openedTabs)
      localStorage.set('unuploadedProjects', unuploadedProjects)
    })

    return proxyRefs({
      tab,
      projectTabs,
    })
  },
)
