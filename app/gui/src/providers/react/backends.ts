import type { BackendsStore } from '$/providers/backends'
import { useInReactFunction } from '$/providers/react/common'
import * as react from 'react'

export const BackendsContext = react.createContext<BackendsStore | null>(null)
export const useBackends = useInReactFunction(BackendsContext)
