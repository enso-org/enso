import type { ProjectAsset, ProjectId } from 'enso-common/src/services/Backend'
import * as react from 'react'
import type { OpenedProjectsStore } from '../openedProjects'
import { useInReactFunction, useVueValue } from './common'

export const OpenedProjectsContext = react.createContext<OpenedProjectsStore | null>(null)
export const useOpenedProjects = useInReactFunction(OpenedProjectsContext)

/** Check if given asset is in process of opening, either by us or in backend only. */
export function useIsProjectOpening(asset: ProjectAsset) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () => openedProjects.isProjectOpening(asset),
      [openedProjects, asset, asset?.projectState, asset?.projectState.type],
    ),
  )
}

/** Check if given asset is opened, either by us or in backend only. */
export function useIsProjectOpened(asset: ProjectAsset | null) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () => (asset != null ? openedProjects.isProjectOpened(asset) : false),
      [openedProjects, asset, asset?.projectState, asset?.projectState.type],
    ),
  )
}

/** Check if given asset is in process of closing, either by us or in backend only. */
export function useIsProjectClosing(id: ProjectId | null) {
  const openedProjects = useOpenedProjects()
  return useVueValue(
    react.useCallback(
      () => (id != null ? openedProjects.isProjectClosing(id) : false),
      [openedProjects, id],
    ),
  )
}

/** Check if there is any other project being opened by us. */
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
