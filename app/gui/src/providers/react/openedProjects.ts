import type { ProjectId } from '#/services/Backend'
import * as react from 'react'
import type { OpenedProjectsStore } from '../openedProjects'
import { useInReactFunction, useVueValue } from './common'

export const OpenedProjectsContext = react.createContext<OpenedProjectsStore | null>(null)
export const useOpenedProjects = useInReactFunction(OpenedProjectsContext)

export function useIsProjectOpening(id: ProjectId) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () => openedProjects.get(id)?.nextTask?.process === 'opening',
      [openedProjects, id],
    ),
  )
}

export function useIsProjectClosing(id: ProjectId | null) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () => (id != null ? openedProjects.get(id)?.nextTask?.process === 'closing' : false),
      [openedProjects, id],
    ),
  )
}

export function useAreOtherProjectsOpening(id: ProjectId) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () =>
        [...openedProjects.listProjects()].some(
          (proj) => proj.state.info.id !== id && proj.nextTask?.process === 'opening',
        ),
      [openedProjects, id],
    ),
  )
}
