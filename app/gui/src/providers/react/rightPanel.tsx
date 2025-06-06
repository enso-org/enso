import { RightPanelData, useRightPanelData as useRightPanelDataVue } from '$/providers/rightPanel'
import * as react from 'react'
import { applyPureReactInVue } from 'veaury'
import { useInReactFunction, useVueValue } from './common'

const RightPanelDataContext = react.createContext<RightPanelData | null>(null)
export const useRightPanelData = useInReactFunction(RightPanelDataContext)

export const RightPanelDataProviderForReact = applyPureReactInVue(
  ({ value, children }: react.PropsWithChildren<{ value: RightPanelData }>) => {
    return <RightPanelDataContext.Provider value={value}>{children}</RightPanelDataContext.Provider>
  },
  {
    useInjectPropsFromWrapper: () => {
      return {
        value: useRightPanelDataVue(),
      }
    },
  },
)

export function useRightPanelFocusedAsset() {
  const rightPanel = useRightPanelData()
  return useVueValue(react.useCallback(() => rightPanel.focusedAsset, [rightPanel]))
}

export function useRightPanelContextCategory() {
  const rightPanel = useRightPanelData()
  return useVueValue(react.useCallback(() => rightPanel.context?.category, [rightPanel]))
}
