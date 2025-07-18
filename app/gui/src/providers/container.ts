import { BackendType, DirectoryId, EnsoPath, ProjectId } from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import { createContextStore } from '@/providers'
import { proxyRefs } from '@/util/reactivity'
import { normalizeRouteParamToString } from '@/util/router'
import { computed } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import * as z from 'zod'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
    readonly launchedProjects: z.infer<typeof LAUNCHED_PROJECT_SCHEMA>
  }
}

const PROJECT_ID_SCHEMA = z.custom<ProjectId>(
  (x) => typeof x === 'string' && x.startsWith('project-'),
)
const DIRECTORY_ID_SCHEMA = z.custom<DirectoryId>(
  (x) => typeof x === 'string' && x.startsWith('directory-'),
)
const PROJECT_SCHEMA = z
  .object({
    id: PROJECT_ID_SCHEMA,
    parentId: DIRECTORY_ID_SCHEMA,
    title: z.string(),
    ensoPath: z.string(),
    type: z.nativeEnum(BackendType),
    hybrid: z.optional(
      z.object({
        cloudProjectId: PROJECT_ID_SCHEMA,
        cloudParentId: DIRECTORY_ID_SCHEMA,
        parentId: DIRECTORY_ID_SCHEMA,
        cloudProjectDirectoryPath: z.string(),
      }),
    ),
  })
  .readonly()
const LAUNCHED_PROJECT_SCHEMA = z.array(PROJECT_SCHEMA).readonly()

/** Launched project information. */
export type LaunchedProject = z.infer<typeof PROJECT_SCHEMA>
/** Launched project ID. */
export type LaunchedProjectId = ProjectId

LocalStorage.registerKey('launchedProjects', {
  isUserSpecific: true,
  schema: LAUNCHED_PROJECT_SCHEMA,
})

/** Tab identifier, equal to the path of the view's URL. */
export type TabId = 'drive' | 'settings' | EnsoPath

export type ContainerData = ReturnType<typeof useContainerData>
export const [provideContainerData, useContainerData] = createContextStore('gui-container', () => {
  const router = useRouter()
  const route = useRoute()
  const localStorage = LocalStorage.getInstance()

  const openedProjects = computed(
    () =>
      localStorage.get('launchedProjects')?.map((lp) => ({
        ...lp,
        shown: computed(() => tab.value === lp.ensoPath),
      })) ?? [],
  )

  const isValidTab = (name: string | undefined): name is TabId =>
    name === 'drive' ||
    name === 'settings' ||
    openedProjects.value.find((p) => p.ensoPath === name) != null

  const tab = computed<TabId>({
    get: () => {
      const name = normalizeRouteParamToString(route.params.path)
      return isValidTab(name) ? name : 'drive'
    },
    set: (page) => {
      router.push({ params: { path: page.split('/') }, query: route.query })
    },
  })

  const addLaunchedProject = (project: LaunchedProject) => {
    updateLaunchedProjects((current) => [...current, project])
  }
  const removeLaunchedProject = (projectId: LaunchedProjectId) => {
    updateLaunchedProjects((current) =>
      current.filter(({ id, hybrid }) => id !== projectId && hybrid?.cloudProjectId !== projectId),
    )
  }
  const updateLaunchedProjects = (
    update: (projects: readonly LaunchedProject[]) => readonly LaunchedProject[],
  ) => {
    localStorage.set('launchedProjects', update(localStorage.get('launchedProjects') ?? []))
  }

  return proxyRefs({
    openedProjects,
    tab,
    addLaunchedProject,
    removeLaunchedProject,
    updateLaunchedProjects,
  })
})
