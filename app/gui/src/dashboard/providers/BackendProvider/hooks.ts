/**
 * @file The React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context.
 */
import * as React from 'react'

import invariant from 'tiny-invariant'

import * as common from 'enso-common'

import { type Category, isCloudCategory } from '#/layouts/CategorySwitcher/Category'

import { BackendType } from '#/services/Backend'
import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'

/** State contained in a `BackendContext`. */
export interface BackendContextType {
  readonly remoteBackend: RemoteBackend | null
  readonly localBackend: LocalBackend | null
}

export const BackendContext = React.createContext<BackendContextType>({
  remoteBackend: null,
  localBackend: null,
})

/** State contained in a `ProjectManagerContext`. */
export interface ProjectManagerContextType {
  readonly didLoadingProjectManagerFail: boolean
  readonly reconnectToProjectManager: () => void
}

export const ProjectManagerContext = React.createContext<ProjectManagerContextType>({
  didLoadingProjectManagerFail: false,
  reconnectToProjectManager: () => {},
})

/**
 * Get the Remote Backend.
 * @throws {Error} when no Remote Backend exists. This should never happen.
 */
export function useRemoteBackend() {
  const remoteBackend = React.useContext(BackendContext).remoteBackend

  if (remoteBackend == null) {
    throw new Error('This component requires a Cloud Backend to function.')
  }

  return remoteBackend
}

/** Get the Local Backend. */
export function useLocalBackend() {
  return React.useContext(BackendContext).localBackend
}

/**
 * Get the corresponding backend for the given category.
 */
export function useBackend(category: Category) {
  const remoteBackend = useRemoteBackend()
  const localBackend = useLocalBackend()

  return pickBackend(category, remoteBackend, localBackend)
}

/**
 * Pick the backend for the given category.
 * @throws {Error} when a Local Backend is requested for a non-local project.
 */
export function pickBackend(
  category: Category,
  remoteBackend: RemoteBackend,
  localBackend: LocalBackend | null,
) {
  if (isCloudCategory(category)) {
    return remoteBackend
  }

  invariant(
    localBackend != null,
    `This distribution of ${common.PRODUCT_NAME} does not support the Local Backend.`,
  )

  return localBackend
}

/**
 * Get the backend for the given project type.
 * @throws {Error} when a Local Backend is requested for a non-local project.
 */
export function useBackendForProjectType(projectType: BackendType) {
  const remoteBackend = useRemoteBackend()
  const localBackend = useLocalBackend()

  switch (projectType) {
    case BackendType.remote:
      return remoteBackend
    case BackendType.local:
      invariant(
        localBackend,
        'Attempted to get a local backend for local project, but no local backend was provided.',
      )
      return localBackend
  }
}

/** Whether connecting to the Project Manager failed. */
export function useDidLoadingProjectManagerFail() {
  return React.useContext(ProjectManagerContext).didLoadingProjectManagerFail
}

/** Reconnect to the Project Manager. */
export function useReconnectToProjectManager() {
  return React.useContext(ProjectManagerContext).reconnectToProjectManager
}
