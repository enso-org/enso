/** @file The React provider (and associated hooks) for providing reactive events. */
import * as React from 'react'

import invariant from 'tiny-invariant'
import * as useZustand from 'use-zustand'
import * as z from 'zod'
import * as zustand from 'zustand'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'
import type * as projectHooks from '#/hooks/projectHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'

import * as localStorageProvider from '#/providers/LocalStorageProvider'

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
  }
}

const PAGES_SCHEMA = z
  .nativeEnum(TabType)
  .or(z.custom<projectHooks.ProjectId>(value => typeof value === 'string'))

LocalStorage.registerKey('page', { schema: PAGES_SCHEMA })

// =====================
// === ProjectsStore ===
// =====================

/** The state of this zustand store. */
interface ProjectsStore {
  readonly page: projectHooks.ProjectId | TabType
  readonly setPage: (page: projectHooks.ProjectId | TabType) => void
  readonly launchedProjects: readonly projectHooks.Project[]
  readonly addLaunchedProject: (project: projectHooks.Project) => void
  readonly removeLaunchedProject: (projectId: projectHooks.ProjectId) => void
  readonly clearLaunchedProjects: () => void
}

// =======================
// === ProjectsContext ===
// =======================

/** State contained in a `ProjectsContext`. */
export interface ProjectsContextType extends zustand.StoreApi<ProjectsStore> {}

const ProjectsContext = React.createContext<ProjectsContextType | null>(null)

/** Props for a {@link ProjectsProvider}. */
export interface ProjectsProviderProps extends Readonly<React.PropsWithChildren> {}

// ========================
// === ProjectsProvider ===
// ========================

/** A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused. */
export default function ProjectsProvider(props: ProjectsProviderProps) {
  const { children } = props
  const { localStorage } = localStorageProvider.useLocalStorage()
  const [store] = React.useState(() =>
    zustand.createStore<ProjectsStore>((set, get) => ({
      page: localStorage.get('page') ?? TabType.drive,
      setPage: page => {
        localStorage.set('page', page)
      },
      launchedProjects: localStorage.get('launchedProjects') ?? [],
      addLaunchedProject: project => {
        set({ launchedProjects: [...get().launchedProjects, project] })
      },
      removeLaunchedProject: projectId => {
        set({ launchedProjects: get().launchedProjects.filter(({ id }) => id !== projectId) })
      },
      clearLaunchedProjects: () => {
        set({ launchedProjects: [] })
      },
    }))
  )

  const [page, privateSetPage] = searchParamsState.useSearchParamsState(
    'page',
    () => localStorage.get('page') ?? TabType.drive,
    (value: unknown): value is projectHooks.ProjectId | TabType => {
      return (
        array.includes(Object.values(TabType), value) ||
        store.getState().launchedProjects.some(p => p.id === value)
      )
    }
  )

  const setPage = eventCallbacks.useEventCallback((newPage: projectHooks.ProjectId | TabType) => {
    privateSetPage(newPage)
    localStorage.set('page', newPage)
  })

  const contextValue = React.useMemo(() => ({ ...store, page, setPage }), [page, setPage, store])

  return (
    <ProjectsContext.Provider value={contextValue}>
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
    () => providerPage,
    (value: unknown): value is projectHooks.ProjectId | TabType => {
      return (
        array.includes(Object.values(TabType), value) ||
        store.getState().launchedProjects.some(p => p.id === value)
      )
    }
  )

  React.useEffect(() => {
    providerSetPage(page)
  }, [page, providerSetPage])

  React.useEffect(() => {
    privateSetPage(providerPage)
    localStorage.set('page', providerPage)
  }, [localStorage, privateSetPage, providerPage])

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

// =============================
// === useAddLaunchedProject ===
// =============================

/** A function to retrieve all launched projects. */
export function useLaunchedProjects() {
  const store = useProjectsStore()
  return useZustand.useZustand(store, state => state.launchedProjects)
}

// =============================
// === useAddLaunchedProject ===
// =============================

/** A function to add a new launched projoect. */
export function useAddLaunchedProject() {
  const store = useProjectsStore()
  const addLaunchedProject = useZustand.useZustand(store, state => state.addLaunchedProject)
  return eventCallbacks.useEventCallback((project: projectHooks.Project) => {
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
  const removeLaunchedProject = useZustand.useZustand(store, state => state.removeLaunchedProject)
  return eventCallbacks.useEventCallback((projectId: projectHooks.ProjectId) => {
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
  const clearLaunchedProjects = useZustand.useZustand(store, state => state.clearLaunchedProjects)
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
  return useZustand.useZustand(store, state => state.page)
}
// ==================
// === useSetPage ===
// ==================

/** A function to set the current page. */
export function useSetPage() {
  const store = useProjectsStore()
  const setPage = useZustand.useZustand(store, state => state.setPage)
  return eventCallbacks.useEventCallback((page: projectHooks.ProjectId | TabType) => {
    React.startTransition(() => {
      setPage(page)
    })
  })
}
