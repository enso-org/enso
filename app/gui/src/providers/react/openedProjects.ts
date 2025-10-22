import type { ProjectAsset, ProjectId } from '#/services/Backend'
import * as react from 'react'
import type { OpenedProjectsStore } from '../openedProjects'
import { useInReactFunction, useVueValue } from './common'

export const OpenedProjectsContext = react.createContext<OpenedProjectsStore | null>(null)
export const useOpenedProjects = useInReactFunction(OpenedProjectsContext)

export function useIsProjectOpening(asset: ProjectAsset) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () => openedProjects.isProjectOpening(asset),
      [openedProjects, asset, asset?.projectState, asset?.projectState.type],
    ),
  )
}

export function useIsProjectOpened(asset: ProjectAsset | null) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(() => {
      const result = asset != null ? openedProjects.isProjectOpened(asset) : false
      console.debug('Returning', result)
      return result
    }, [openedProjects, asset, asset?.projectState, asset?.projectState.type]),
    false,
    true,
  )
}

export function useIsProjectClosing(id: ProjectId | null) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () => (id != null ? openedProjects.isProjectClosing(id) : false),
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
