/** @file The React provider (and associated hooks) for providing reactive events. */
import * as eventCallbacks from '#/hooks/eventCallbackHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as array from '#/utilities/array'
import * as React from 'react'
import {
  LaunchedProjectsContext,
  PageContext,
  ProjectsContext,
  TAB_TYPES,
  type LaunchedProject,
  type LaunchedProjectId,
  type TabType,
} from './constants'

/** Props for a {@link ProjectsProvider}. */
export type ProjectsProviderProps = Readonly<React.PropsWithChildren>

/**
 * A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused.
 */
export function ProjectsProvider(props: ProjectsProviderProps) {
  const { children } = props

  const [launchedProjects, setLaunchedProjects] = localStorageProvider.useLocalStorageState(
    'launchedProjects',
    array.EMPTY_ARRAY,
  )
  const [page, setPage] = searchParamsState.useSearchParamsState(
    'page',
    (): LaunchedProjectId | TabType => 'drive',
    (value: unknown): value is LaunchedProjectId | TabType => {
      return array.includes(TAB_TYPES, value) || launchedProjects.some((p) => p.id === value)
    },
  )

  const addLaunchedProject = eventCallbacks.useEventCallback((project: LaunchedProject) => {
    setLaunchedProjects((current) => [...current, project])
  })
  const removeLaunchedProject = eventCallbacks.useEventCallback((projectId: LaunchedProjectId) => {
    setLaunchedProjects((current) => current.filter(({ id }) => id !== projectId))
  })
  const updateLaunchedProjects = eventCallbacks.useEventCallback(
    (update: (projects: readonly LaunchedProject[]) => readonly LaunchedProject[]) => {
      setLaunchedProjects((current) => update(current))
    },
  )

  const getState = eventCallbacks.useEventCallback(() => ({
    launchedProjects,
    page,
  }))

  const projectsContextValue = React.useMemo(
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
