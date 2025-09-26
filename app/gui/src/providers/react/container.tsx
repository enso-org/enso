import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { EnsoPath, ProjectId } from '#/services/Backend'
import { useContainerData as useContainerDataVue, type ContainerData } from '$/providers/container'
import {
  useRightPanelData as useRightPanelDataVue,
  type RightPanelData,
} from '$/providers/rightPanel'
import type { Opt } from '@/util/data/opt'
import { reactComponent } from '@/util/react'
import * as react from 'react'
import { useInReactFunction, useVueValue } from './common'

const RightPanelDataContext = react.createContext<RightPanelData | null>(null)
export const useRightPanelData = useInReactFunction(RightPanelDataContext)

const ContainerDataContext = react.createContext<ContainerData | null>(null)
export const useContainerData = useInReactFunction(ContainerDataContext)

export const ContainerDataProviderForReact = reactComponent(
  ({ value, children }: react.PropsWithChildren<{ value: ContainerData }>) => {
    return <ContainerDataContext.Provider value={value}>{children}</ContainerDataContext.Provider>
  },
  {
    useInjectPropsFromWrapper: () => {
      const result = {
        value: useContainerDataVue(),
      }
      // Avoid annoying warning about __veauryInjectedProps__ property by returning a function.
      return () => result
    },
  },
) as any

export const RightPanelDataProviderForReact = reactComponent(
  ({ value, children }: react.PropsWithChildren<{ value: RightPanelData }>) => {
    return <RightPanelDataContext.Provider value={value}>{children}</RightPanelDataContext.Provider>
  },
  {
    useInjectPropsFromWrapper: () => {
      const result = {
        value: useRightPanelDataVue(),
      }
      // Avoid annoying warning about __veauryInjectedProps__ property by returning a function.
      return () => result
    },
  },
) as any

/**
 * A hook to read currently focused asset for right panel, e.g. the currently selected asset
 * in Drive View.
 */
export function useRightPanelFocusedAsset() {
  const rightPanel = useRightPanelData()
  return useVueValue(react.useCallback(() => rightPanel.focusedAsset, [rightPanel]))
}

/** A hook reading current category set for right panel context. */
export function useRightPanelContextCategory() {
  const rightPanel = useRightPanelData()
  return useVueValue(react.useCallback(() => rightPanel.context?.category, [rightPanel]))
}

/** Returns the launched projects context. */
export function useLaunchedProjects() {
  const container = useContainerData()
  return useVueValue(react.useCallback(() => container.openedProjects, [container]))
}

/** A function to update launched projects. */
export function useUpdateLaunchedProjects() {
  const { updateLaunchedProjects } = useContainerData()
  return updateLaunchedProjects
}

/** A function to add a new launched project. */
export function useAddLaunchedProject() {
  const { addLaunchedProject } = useContainerData()
  return addLaunchedProject
}

/** A function to remove a launched project. */
export function useRemoveLaunchedProject() {
  const { removeLaunchedProject } = useContainerData()
  return removeLaunchedProject
}

/** A function to remove all launched projects. */
export function useClearLaunchedProjects() {
  const { updateLaunchedProjects } = useContainerData()

  return useEventCallback(() => {
    updateLaunchedProjects(() => [])
  })
}

/** A function to add project to "opening projects" list */
export function useAddOpeningProject() {
  const { openingProjects } = useContainerData()
  return useEventCallback((id: ProjectId, ensoPath: string) => {
    openingProjects.set(id, EnsoPath(ensoPath))
  })
}

/** A function to remove project from "opening projects" list */
export function useRemoveOpeningProject() {
  const { openingProjects } = useContainerData()
  return useEventCallback((id: ProjectId) => {
    openingProjects.delete(id)
  })
}

/** A hook returning information if given project is opened. */
export function useIsProjectOpening(id: Opt<ProjectId>) {
  const { openingProjects } = useContainerData()
  return useVueValue(
    react.useCallback(() => (id != null ? openingProjects.has(id) : false), [openingProjects, id]),
  )
}

/** A hook returning information if any other project is opened. */
export function useAreOtherProjectsOpening(id: ProjectId) {
  const { openingProjects } = useContainerData()
  return useVueValue(
    react.useCallback(
      () => openingProjects.size !== 0 && !openingProjects.has(id),
      [openingProjects, id],
    ),
  )
}

/** A function to add project to "closing projects" list */
export function useAddClosingProject() {
  const { closingProjects } = useContainerData()
  return useEventCallback((id: ProjectId) => {
    closingProjects.add(id)
  })
}

/** A function to remove project from "closing projects" list */
export function useRemoveClosingProject() {
  const { closingProjects } = useContainerData()
  return useEventCallback((id: ProjectId) => {
    closingProjects.delete(id)
  })
}

/** A hook returning information if given project is closing. */
export function useIsProjectClosing(id: Opt<ProjectId>) {
  const { closingProjects } = useContainerData()
  return useVueValue(
    react.useCallback(() => (id != null ? closingProjects.has(id) : false), [closingProjects, id]),
  )
}
