/** @file Hooks for `BackendProvider`. */
import { type Category, isCloudCategory } from '#/layouts/Drive/CategorySwitcher/Category'
import { BackendType } from '#/services/Backend'
import { PRODUCT_NAME } from 'enso-common'
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { BackendContext, ProjectManagerContext } from './constants'

/**
 * Get the Remote Backend.
 * @throws {Error} when no Remote Backend exists. This should never happen.
 */
export function useRemoteBackend() {
  const remoteBackend = useContext(BackendContext).remoteBackend

  if (remoteBackend == null) {
    throw new Error('This component requires a Cloud Backend to function.')
  }

  return remoteBackend
}

/** Get the Local Backend. */
export function useLocalBackend() {
  return useContext(BackendContext).localBackend
}

/**
 * Get the corresponding backend for the given property.
 * @throws {Error} when neither the Remote Backend nor the Local Backend are supported.
 * This should never happen unless the build is misconfigured.
 */
export function useBackend(category: Category) {
  const remoteBackend = useRemoteBackend()
  const localBackend = useLocalBackend()

  if (isCloudCategory(category)) {
    return remoteBackend
  } else {
    invariant(
      localBackend != null,
      `This distribution of ${PRODUCT_NAME} does not support the Local Backend.`,
    )
    return localBackend
  }
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
  return useContext(ProjectManagerContext).didLoadingProjectManagerFail
}

/** Reconnect to the Project Manager. */
export function useReconnectToProjectManager() {
  return useContext(ProjectManagerContext).reconnectToProjectManager
}
