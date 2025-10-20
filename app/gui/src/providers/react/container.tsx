import { useContainerData as useContainerDataVue, type ContainerData } from '$/providers/container'
import {
  useRightPanelData as useRightPanelDataVue,
  type RightPanelData,
} from '$/providers/rightPanel'
import { reactComponent } from '@/util/react'
import * as react from 'react'
import { useInReactFunction, useVueValue } from './common'

const RightPanelDataContext = react.createContext<RightPanelData | null>(null)
export const useRightPanelData = useInReactFunction(RightPanelDataContext)

const ContainerDataContext = react.createContext<ContainerData | null>(null)
export const useContainerData = useInReactFunction(ContainerDataContext)

export const ContainerProviderForReact = reactComponent(
  ({
    container,
    rightPanel,
    children,
  }: react.PropsWithChildren<{ container: ContainerData; rightPanel: RightPanelData }>) => {
    return (
      <ContainerDataContext.Provider value={container}>
        <RightPanelDataContext.Provider value={rightPanel}>
          {children}
        </RightPanelDataContext.Provider>
      </ContainerDataContext.Provider>
    )
  },
  {
    useInjectPropsFromWrapper: () => {
      const result = {
        container: useContainerDataVue(),
        rightPanel: useRightPanelDataVue(),
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
