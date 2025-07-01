import { LaunchedProject } from '#/providers/ProjectsProvider'
import { createContextStore } from '@/providers'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { computed } from 'vue'
import { useRoute, useRouter } from 'vue-router'

export type TabId = 'drive' | 'settings' | `local/${string}` | `cloud/${string}`

export function ensoPathToTabId(path: string): TabId {
  if (path.startsWith('enso://')) {
    return `cloud/${path.slice('enso://'.length)}`
  } else {
    return `local/${path}`
  }
}

function routeParamToTabName(routeParam: string | string[] | undefined) {
  return routeParam instanceof Array ? routeParam.join('/') : routeParam
}

export type ContainerData = ReturnType<typeof useConainerData>
export const [provideContainerData, useConainerData] = createContextStore(
  'gui-container',
  (launchedProjectsFromReact: ToValue<readonly LaunchedProject[]>) => {
    const router = useRouter()
    const route = useRoute()
    const openedProjects = computed(() =>
      toValue(launchedProjectsFromReact).map((lp) => ({
        ...lp,
        shown: computed(() => tab.value === ensoPathToTabId(lp.ensoPath)),
      })),
    )

    const isValidTab = (name: string | undefined): name is TabId =>
      name === 'drive' ||
      name === 'settings' ||
      openedProjects.value.find((p) => ensoPathToTabId(p.ensoPath) === name) != null

    const tab = computed<TabId>({
      get: () => {
        const name = routeParamToTabName(route.params.path)
        return isValidTab(name) ? name : 'drive'
      },
      set: (page) => {
        router.push({ params: { path: page.split('/') }, query: route.query })
      },
    })

    return proxyRefs({
      openedProjects,
      tab,
    })
  },
)
