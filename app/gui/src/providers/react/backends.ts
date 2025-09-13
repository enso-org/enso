import { BackendsStore } from '$/providers/backends'
import { useInReactFunction, useVueValue } from '$/providers/react/common'
import * as react from 'react'

export const BackendsContext = react.createContext<BackendsStore | null>(null)
export const useBackends = useInReactFunction(BackendsContext)

/** A hook returning true if Project Manager failed.  */
export function useDidLoadingProjectManagerFail() {
  const backends = useBackends()
  return useVueValue(react.useCallback(() => backends.didLoadingProjectManagerFail, [backends]))
}
