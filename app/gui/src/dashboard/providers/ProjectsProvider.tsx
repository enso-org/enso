/** @file The React provider (and associated hooks) for providing reactive events. */
import * as React from 'react'

import invariant from 'tiny-invariant'
import * as z from 'zod'
import * as zustand from 'zustand'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'

import * as localStorageProvider from '#/providers/LocalStorageProvider'

import * as backendModule from '#/services/Backend'

import { useMounted } from '#/hooks/mountHooks'
import * as array from '#/utilities/array'
import LocalStorage from '#/utilities/LocalStorage'

// ===============
// === TabType ===
// ===============

/** Main content of the screen. Only one should be visible at a time. */
export enum TabType {
  drive = 'drive',
  settings = 'settings',
}

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
    readonly page: z.infer<typeof PAGES_SCHEMA>
    readonly launchedProjects: z.infer<typeof LAUNCHED_PROJECT_SCHEMA>
  }
}

// =================
// === Constants ===
// =================

const PROJECT_SCHEMA = z
  .object({
    id: z.custom<backendModule.ProjectId>((x) => typeof x === 'string' && x.startsWith('project-')),
    parentId: z.custom<backendModule.DirectoryId>(
      (x) => typeof x === 'string' && x.startsWith('directory-'),
    ),
    title: z.string(),
    type: z.nativeEnum(backendModule.BackendType),
  })
  .readonly()
const LAUNCHED_PROJECT_SCHEMA = z.array(PROJECT_SCHEMA).readonly()

/** Launched project information. */
export type LaunchedProject = z.infer<typeof PROJECT_SCHEMA>
/** Launched project ID. */
export type LaunchedProjectId = backendModule.ProjectId

LocalStorage.registerKey('launchedProjects', {
  isUserSpecific: true,
  schema: LAUNCHED_PROJECT_SCHEMA,
})

export const PAGES_SCHEMA = z
  .nativeEnum(TabType)
  .or(
    z.custom<LaunchedProjectId>(
      (value) => typeof value === 'string' && value.startsWith('project-'),
    ),
  )

LocalStorage.registerKey('page', { schema: PAGES_SCHEMA })

// =====================
// === ProjectsStore ===
// =====================

/** The state of this zustand store. */
interface ProjectsStore {
  readonly page: LaunchedProjectId | TabType
  readonly setPage: (page: LaunchedProjectId | TabType) => void
  readonly launchedProjects: readonly LaunchedProject[]
  readonly updateLaunchedProjects: (
    update: (projects: readonly LaunchedProject[]) => readonly LaunchedProject[],
  ) => void
  readonly addLaunchedProject: (project: LaunchedProject) => void
  readonly removeLaunchedProject: (projectId: LaunchedProjectId) => void
  readonly clearLaunchedProjects: () => void
}

// =======================
// === ProjectsContext ===
// =======================

/** State contained in a `ProjectsContext`. */
export type ProjectsContextType = zustand.StoreApi<ProjectsStore>

const ProjectsContext = React.createContext<ProjectsContextType | null>(null)

/** Props for a {@link ProjectsProvider}. */
export type ProjectsProviderProps = Readonly<React.PropsWithChildren>

const STORE = zustand.createStore<ProjectsStore>((set) => ({
  page: TabType.drive,
  setPage: (page) => {
    set({ page })
  },
  launchedProjects: [],
  updateLaunchedProjects: (update) => {
    set(({ launchedProjects }) => ({ launchedProjects: update(launchedProjects) }))
  },
  addLaunchedProject: (project) => {
    set(({ launchedProjects }) => ({ launchedProjects: [...launchedProjects, project] }))
  },
  removeLaunchedProject: (projectId) => {
    set(({ launchedProjects }) => ({
      launchedProjects: launchedProjects.filter(({ id }) => id !== projectId),
    }))
  },
  clearLaunchedProjects: () => {
    set({ launchedProjects: [] })
  },
}))

// ========================
// === ProjectsProvider ===
// ========================

/**
 * A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused.
 */
export default function ProjectsProvider(props: ProjectsProviderProps) {
  const { children } = props
  const { localStorage } = localStorageProvider.useLocalStorage()
  const store = STORE

  useMounted(() => {
    const launchedProjects = localStorage.get('launchedProjects')
    if (launchedProjects) {
      store
        .getState()
        .updateLaunchedProjects((projects) => (projects.length === 0 ? launchedProjects : projects))
    }
  })

  return (
    <ProjectsContext.Provider value={store}>
      <PageSynchronizer />
      {children}
    </ProjectsContext.Provider>
  )
}

// ========================
// === PageSynchronizer ===
// ========================

/** A component to synchronize React state with search parmas state. */
function PageSynchronizer() {
  const { localStorage } = localStorageProvider.useLocalStorage()
  const store = useProjectsStore()
  const providerPage = usePage()
  const providerSetPage = useSetPage()
  const [page, privateSetPage] = searchParamsState.useSearchParamsState(
    'page',
    () => store.getState().page,
    (value: unknown): value is LaunchedProjectId | TabType => {
      return (
        array.includes(Object.values(TabType), value) ||
        store.getState().launchedProjects.some((p) => p.id === value)
      )
    },
  )

  React.useEffect(() => {
    providerSetPage(page)
  }, [page, providerSetPage])

  React.useEffect(() => {
    privateSetPage(providerPage)
  }, [providerPage, privateSetPage])

  React.useEffect(() =>
    store.subscribe((state) => {
      localStorage.set('launchedProjects', state.launchedProjects)
    }),
  )

  return null
}

// ========================
// === useProjectsStore ===
// ========================

/** The projects store. */
export function useProjectsStore() {
  const store = React.useContext(ProjectsContext)

  invariant(store, 'Projects store store can only be used inside an `ProjectsProvider`.')

  return store
}

// ===========================
// === useLaunchedProjects ===
// ===========================

/** A function to retrieve all launched projects. */
export function useLaunchedProjects() {
  const store = useProjectsStore()
  return zustand.useStore(store, (state) => state.launchedProjects)
}

// =================================
// === useUpdateLaunchedProjects ===
// =================================

/** A function to update launched projects. */
export function useUpdateLaunchedProjects() {
  const store = useProjectsStore()
  const updateLaunchedProjects = zustand.useStore(store, (state) => state.updateLaunchedProjects)
  return eventCallbacks.useEventCallback(
    (update: (projects: readonly LaunchedProject[]) => readonly LaunchedProject[]) => {
      React.startTransition(() => {
        updateLaunchedProjects(update)
      })
    },
  )
}

// =============================
// === useAddLaunchedProject ===
// =============================

/** A function to add a new launched project. */
export function useAddLaunchedProject() {
  const store = useProjectsStore()
  const addLaunchedProject = zustand.useStore(store, (state) => state.addLaunchedProject)
  return eventCallbacks.useEventCallback((project: LaunchedProject) => {
    React.startTransition(() => {
      addLaunchedProject(project)
    })
  })
}

// ================================
// === useRemoveLaunchedProject ===
// ================================

/** A function to remove a launched project. */
export function useRemoveLaunchedProject() {
  const store = useProjectsStore()
  const removeLaunchedProject = zustand.useStore(store, (state) => state.removeLaunchedProject)
  return eventCallbacks.useEventCallback((projectId: LaunchedProjectId) => {
    React.startTransition(() => {
      removeLaunchedProject(projectId)
    })
  })
}

// ================================
// === useClearLaunchedProjects ===
// ================================

/** A function to remove all launched projects. */
export function useClearLaunchedProjects() {
  const store = useProjectsStore()
  const clearLaunchedProjects = zustand.useStore(store, (state) => state.clearLaunchedProjects)
  return eventCallbacks.useEventCallback(() => {
    React.startTransition(() => {
      clearLaunchedProjects()
    })
  })
}

// ===============
// === usePage ===
// ===============

/** A function to get the current page. */
export function usePage() {
  const store = useProjectsStore()
  return zustand.useStore(store, (state) => state.page)
}
// ==================
// === useSetPage ===
// ==================

/** A function to set the current page. */
export function useSetPage() {
  const store = useProjectsStore()
  const setPage = zustand.useStore(store, (state) => state.setPage)
  return eventCallbacks.useEventCallback((page: LaunchedProjectId | TabType) => {
    React.startTransition(() => {
      setPage(page)
    })
  })
}
