/** @file Provider for the list of opened projects. */
import { createContext, useContext, useMemo, type PropsWithChildren } from 'react'

import invariant from 'tiny-invariant'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { EMPTY_ARRAY } from '#/utilities/array'
import {
  useLaunchedProjectsState,
  validatePage,
  type LaunchedProject,
  type LaunchedProjectId,
  type TabType,
} from './ProjectsProvider/projectsLocalStorage'
export {
  TAB_TYPES,
  type LaunchedProject,
  type LaunchedProjectId,
  type TabType,
} from './ProjectsProvider/projectsLocalStorage'

/** State contained in a `ProjectsContext`. */
export interface ProjectsContextType {
  readonly setLaunchedProjects: (launchedProjects: readonly LaunchedProject[]) => void
  readonly addLaunchedProject: (project: LaunchedProject) => void
  readonly removeLaunchedProject: (projectId: LaunchedProjectId) => void
  readonly updateLaunchedProjects: (
    update: (projects: readonly LaunchedProject[]) => readonly LaunchedProject[],
  ) => void
  readonly getState: () => {
    readonly launchedProjects: readonly LaunchedProject[]
    readonly page: LaunchedProjectId | TabType
  }
  readonly setPage: (page: LaunchedProjectId | TabType) => void
}

const ProjectsContext = createContext<ProjectsContextType | null>(null)
const PageContext = createContext<LaunchedProjectId | TabType | null>(null)
const LaunchedProjectsContext = createContext<readonly LaunchedProject[] | null>(null)

/** Props for a {@link ProjectsProvider}. */
export type ProjectsProviderProps = Readonly<PropsWithChildren>

/** Provider for the list of opened projects. */
export default function ProjectsProvider(props: ProjectsProviderProps) {
  const { children } = props

  const [launchedProjects, setLaunchedProjects] = useLaunchedProjectsState(EMPTY_ARRAY)
  const [page, setPage] = useSearchParamsState(
    'page',
    (): LaunchedProjectId | TabType => 'drive',
    validatePage,
  )

  const addLaunchedProject = useEventCallback((project: LaunchedProject) => {
    setLaunchedProjects((current) => [...current, project])
  })
  const removeLaunchedProject = useEventCallback((projectId: LaunchedProjectId) => {
    setLaunchedProjects((current) => current.filter(({ id }) => id !== projectId))
  })
  const updateLaunchedProjects = useEventCallback(
    (update: (projects: readonly LaunchedProject[]) => readonly LaunchedProject[]) => {
      setLaunchedProjects((current) => update(current))
    },
  )

  const getState = useEventCallback(() => ({
    launchedProjects,
    page,
  }))

  const projectsContextValue = useMemo(
    () => ({
      updateLaunchedProjects,
      addLaunchedProject,
      removeLaunchedProject,
      setLaunchedProjects,
      setPage,
      getState,
    }),
    [
      updateLaunchedProjects,
      addLaunchedProject,
      removeLaunchedProject,
      setLaunchedProjects,
      setPage,
      getState,
    ],
  )

  return (
    <ProjectsContext.Provider value={projectsContextValue}>
      <PageContext.Provider value={page}>
        <LaunchedProjectsContext.Provider value={launchedProjects}>
          {children}
        </LaunchedProjectsContext.Provider>
      </PageContext.Provider>
    </ProjectsContext.Provider>
  )
}

/** The projects store. */
export function useProjectsStore() {
  const context = useContext(ProjectsContext)
  invariant(context != null, 'Projects store can only be used inside an `ProjectsProvider`.')
  return context
}

/** The page context. */
export function usePage() {
  const context = useContext(PageContext)
  invariant(context != null, 'Page context can only be used inside an `ProjectsProvider`.')
  return context
}

/** A function to set the current page. */
export function useSetPage() {
  const { setPage } = useProjectsStore()
  return useEventCallback((page: LaunchedProjectId | TabType) => {
    setPage(page)
  })
}

/** Returns the launched projects context. */
export function useLaunchedProjects() {
  const context = useContext(LaunchedProjectsContext)
  invariant(
    context != null,
    'Launched projects context can only be used inside an `ProjectsProvider`.',
  )
  return context
}

/** A function to update launched projects. */
export function useUpdateLaunchedProjects() {
  const { updateLaunchedProjects } = useProjectsStore()
  return updateLaunchedProjects
}

/** A function to add a new launched project. */
export function useAddLaunchedProject() {
  const { addLaunchedProject } = useProjectsStore()
  return addLaunchedProject
}

/** A function to remove a launched project. */
export function useRemoveLaunchedProject() {
  const { removeLaunchedProject } = useProjectsStore()
  return removeLaunchedProject
}

/** A function to remove all launched projects. */
export function useClearLaunchedProjects() {
  const { setLaunchedProjects } = useProjectsStore()
  return useEventCallback(() => {
    setLaunchedProjects(EMPTY_ARRAY)
  })
}
