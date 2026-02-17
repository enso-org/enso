import { createContextStore } from '@/providers'
import { proxyRefs } from '@/util/reactivity'
import { normalizeRouteParamToString } from '@/util/router'
import { EnsoPath } from 'enso-common/src/services/Backend'
import { ensoPathEq } from 'enso-common/src/services/Backend/ensoPath'
import { filter } from 'enso-common/src/utilities/data/iter'
import { computed, onScopeDispose, watchEffect } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useOpenedProjects, type Project } from './openedProjects'

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
    (project.state.status !== 'not-opened' && project.nextTask?.process !== 'closing')
  )
}

export type ContainerData = ReturnType<typeof useContainerData>
export const [provideContainerData, useContainerData] = createContextStore(
  'gui-container',
  (fallbackTab: TabId = 'drive') => {
    const router = useRouter()
    const route = useRoute()
    const openedProjects = useOpenedProjects()

    const projectTabs = computed(() =>
      Array.from(filter(openedProjects.listProjects(), isProjectShownAsTab), (project) => ({
        ...project,
        shown: computed(
          () =>
            tab.value !== 'drive' &&
            tab.value !== 'settings' &&
            ensoPathEq(tab.value, project.state.info.ensoPath),
        ),
      })),
    )

    const isValidTab = (name: string | undefined): name is TabId =>
      name === 'drive' ||
      name === 'settings' ||
      projectTabs.value.find((p) => name && ensoPathEq(p.state.info.ensoPath, EnsoPath(name))) !=
        null

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

    const offProjectReady = openedProjects.onProjectReady(
      (project) => (tab.value = project.state.info.ensoPath),
    )
    onScopeDispose(offProjectReady)

    return proxyRefs({
      tab,
      projectTabs,
    })
  },
)
