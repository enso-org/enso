/** @file The React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context. */
import * as React from 'react'

import invariant from 'tiny-invariant'

import * as common from 'enso-common'

import type Category from '#/layouts/CategorySwitcher/Category'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'

import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'

// ======================
// === BackendContext ===
// ======================

/** State contained in a `BackendContext`. */
export interface BackendContextType {
  readonly remoteBackend: RemoteBackend | null
  readonly localBackend: LocalBackend | null
}

const BackendContext = React.createContext<BackendContextType>({
  remoteBackend: null,
  localBackend: null,
})

/** Props for a {@link BackendProvider}. */
export interface BackendProviderProps extends Readonly<React.PropsWithChildren> {
  readonly remoteBackend: RemoteBackend | null
  readonly localBackend: LocalBackend | null
}

// =======================
// === BackendProvider ===
// =======================

/** A React Provider that lets components get and set the current backend. */
export default function BackendProvider(props: BackendProviderProps) {
  const { remoteBackend, localBackend, children } = props

  return (
    <BackendContext.Provider value={{ remoteBackend, localBackend }}>
      {children}
    </BackendContext.Provider>
  )
}

// ========================
// === useRemoteBackend ===
// ========================

/**
 * Get the Remote Backend. Since the RemoteBackend is always defined, `null` is never returned.
 * @deprecated Use {@link useRemoteBackendStrict} instead.
 */
export function useRemoteBackend() {
  return React.useContext(BackendContext).remoteBackend
}

// ==============================
// === useRemoteBackendStrict ===
// ==============================

/**
 * Get the Remote Backend.
 * @throws {Error} when no Remote Backend exists. This should never happen.
 */
export function useRemoteBackendStrict() {
  const remoteBackend = React.useContext(BackendContext).remoteBackend
  if (remoteBackend == null) {
    // eslint-disable-next-line no-restricted-syntax
    throw new Error('This component requires a Cloud Backend to function.')
  }
  return remoteBackend
}

// =======================
// === useLocalBackend ===
// =======================

/** Get the Local Backend. */
export function useLocalBackend() {
  return React.useContext(BackendContext).localBackend
}

// ==================
// === useBackend ===
// ==================

/** Get the corresponding backend for the given property.
 * @throws {Error} when neither the Remote Backend nor the Local Backend are supported.
 * This should never happen unless the build is misconfigured. */
export function useBackend(category: Category) {
  const remoteBackend = useRemoteBackendStrict()
  const localBackend = useLocalBackend()

  if (categoryModule.isCloudCategory(category)) {
    return remoteBackend
  } else {
    invariant(
      localBackend != null,
      `This distribution of ${common.PRODUCT_NAME} does not support the Local Backend.`,
    )
    return localBackend
  }
}
