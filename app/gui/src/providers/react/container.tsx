import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { ContainerData, useContainerData as useContainerDataVue } from '$/providers/container'
import { RightPanelData, useRightPanelData as useRightPanelDataVue } from '$/providers/rightPanel'
import * as react from 'react'
import { applyPureReactInVue } from 'veaury'
import { useInReactFunction, useVueValue } from './common'

const RightPanelDataContext = react.createContext<RightPanelData | null>(null)
export const useRightPanelData = useInReactFunction(RightPanelDataContext)

const ContainerDataContext = react.createContext<ContainerData | null>(null)
export const useContainerData = useInReactFunction(ContainerDataContext)

export const ContainerDataProviderForReact = applyPureReactInVue(
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
)

export const RightPanelDataProviderForReact = applyPureReactInVue(
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
)

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
