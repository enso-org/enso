/** @file Hooks for `ProjectsProvider`. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import {
  LaunchedProjectsContext,
  PageContext,
  ProjectsContext,
  type LaunchedProjectId,
  type TabType,
} from './constants'

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
    setLaunchedProjects([])
  })
}
