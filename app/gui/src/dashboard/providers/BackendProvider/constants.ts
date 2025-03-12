/** @file Constants for `BackendProvider`. */
import type { LocalBackend } from '#/services/LocalBackend'
import type { RemoteBackend } from '#/services/RemoteBackend'
import { createContext } from 'react'

/** State contained in a `BackendContext`. */
export interface BackendContextType {
  readonly remoteBackend: RemoteBackend | null
  readonly localBackend: LocalBackend | null
}

export const BackendContext = createContext<BackendContextType>({
  remoteBackend: null,
  localBackend: null,
})

/** State contained in a `ProjectManagerContext`. */
export interface ProjectManagerContextType {
  readonly didLoadingProjectManagerFail: boolean
  readonly reconnectToProjectManager: () => void
}

export const ProjectManagerContext = createContext<ProjectManagerContextType>({
  didLoadingProjectManagerFail: false,
  reconnectToProjectManager: () => {},
})
