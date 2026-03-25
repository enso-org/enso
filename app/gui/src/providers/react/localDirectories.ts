import * as react from 'react'
import type { LocalDirectoriesStore } from '../localDirectories'
import { useInReactFunction, useVueValue } from './common'

export const LocalDirectoriesContext = react.createContext<LocalDirectoriesStore | null>(null)
export const useLocalDirectories = useInReactFunction(LocalDirectoriesContext)

export function useLocalRootDirectory() {
  const store = useLocalDirectories()
  return useVueValue(react.useCallback(() => store.localRootDirectory, [store]))
}

export function useDownloadDirectory() {
  const store = useLocalDirectories()
  return useVueValue(react.useCallback(() => store.downloadDirectory, [store]))
}
