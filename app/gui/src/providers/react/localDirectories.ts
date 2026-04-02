import * as react from 'react'
import type { LocalPathsStore } from '../localDirectories'
import { useInReactFunction, useVueValue } from './common'

export const LocalDirectoriesContext = react.createContext<LocalPathsStore | null>(null)
export const useLocalDirectories = useInReactFunction(LocalDirectoriesContext)

/** A hook reading current root path for local projects. */
export function useLocalRootDirectory() {
  const store = useLocalDirectories()
  return useVueValue(react.useCallback(() => store.localRootDirectory, [store]))
}

/** A hook reading local "downloads" directory . */
export function useDownloadDirectory() {
  const store = useLocalDirectories()
  return useVueValue(react.useCallback(() => store.downloadDirectory, [store]))
}
