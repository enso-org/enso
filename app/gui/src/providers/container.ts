import { BackendType, DirectoryId, EnsoPath, ProjectId, ProjectSessionId } from '#/services/Backend'
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
const PROJECT_SESSION_ID_SCHEMA = z.custom<ProjectSessionId>(
  (x) => typeof x === 'string' && x.startsWith('projectsession-'),
)
const DIRECTORY_ID_SCHEMA = z.custom<DirectoryId>(
  (x) => typeof x === 'string' && x.startsWith('directory-'),
)
const ENSO_PATH_SCHEMA = z.custom<EnsoPath>((x) => typeof x === 'string')
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
        cloudProjectSessionId: PROJECT_SESSION_ID_SCHEMA,
        cloudParentId: DIRECTORY_ID_SCHEMA,
        parentId: DIRECTORY_ID_SCHEMA,
        cloudProjectDirectoryPath: ENSO_PATH_SCHEMA,
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

export type ContainerData = ReturnType<typeof useContainerData>
export const [provideContainerData, useContainerData] = createContextStore(
  'gui-container',
  (fallbackTab: TabId = 'drive') => {
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
        return isValidTab(name) ? name : fallbackTab
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
        current.filter(
          ({ id, hybrid }) => id !== projectId && hybrid?.cloudProjectId !== projectId,
        ),
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
  },
)
