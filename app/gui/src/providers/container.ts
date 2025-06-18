import { LaunchedProject } from '#/providers/ProjectsProvider'
import { ProjectId } from '#/services/Backend'
import { createContextStore } from '@/providers'
import { type ToValue } from '@/util/reactivity'
import { computed, proxyRefs, toRef } from 'vue'
import { useRoute, useRouter } from 'vue-router'

export type TabId = 'drive' | 'settings' | ProjectId

export type ContainerData = ReturnType<typeof useConainerData>
export const [provideContainerData, useConainerData] = createContextStore(
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
      get: () => (isValidTab(route?.query.page) ? route.query.page : 'drive'),
      set: (page) => {
        router.push({ query: { ...route.query, page } })
      },
    })

    return proxyRefs({
      openedProjects,
      tab,
    })
  },
)
