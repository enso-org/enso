import { LaunchedProject } from '#/providers/ProjectsProvider'
import { ProjectId } from '#/services/Backend'
import { createContextStore } from '@/providers'
import { type ToValue } from '@/util/reactivity'
import { computed, proxyRefs, toRef } from 'vue'
import { LocationQueryValue, useRoute, useRouter } from 'vue-router'

export type TabId = 'drive' | 'settings' | ProjectId

const pageKey = 'cloud-ide_page' as const

export type ContainerData = ReturnType<typeof useConainerData>
export const [provideContainerData, useConainerData] = createContextStore(
  'gui-container',
  (launchedProjectsFromReact: ToValue<readonly LaunchedProject[]>) => {
    const router = useRouter()
    const route = useRoute()
    const openedProjects = toRef(launchedProjectsFromReact)

    const isValidTab = (
      name: LocationQueryValue | LocationQueryValue[] | undefined,
    ): name is TabId =>
      name === 'drive' ||
      name === 'settings' ||
      openedProjects.value.find((p) => p.id === name) != null

    const tab = computed<TabId>({
      get: () => (isValidTab(route?.query[pageKey]) ? route.query[pageKey] : 'drive'),
      set: (page) => {
        router.push({ query: { ...route.query, [pageKey]: page } })
      },
    })

    return proxyRefs({
      openedProjects,
      tab,
    })
  },
)
