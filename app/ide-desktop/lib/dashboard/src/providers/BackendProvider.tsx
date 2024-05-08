/** @file The React provider for the project manager `Backend`, along with hooks to use the
 * provider via the shared React context. */
import * as React from 'react'

import * as ensoCommon from 'enso-common'

import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'

import type Backend from '#/services/Backend'

// ======================
// === BackendContext ===
// ======================

/** State contained in a `BackendContext`. */
export interface BackendContextType {
  readonly remoteBackend: Backend | null
  readonly localBackend: Backend | null
}

// @ts-expect-error The default value will never be exposed
// as `backend` will always be accessed using `useBackend`.
const BackendContext = React.createContext<BackendContextType>(null)

/** Props for a {@link BackendProvider}. */
export interface BackendProviderProps extends Readonly<React.PropsWithChildren> {
  readonly remoteBackend: Backend | null
  readonly localBackend: Backend | null
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

/** Get the Remote Backend. */
export function useRemoteBackend() {
  return React.useContext(BackendContext).remoteBackend
}

// ==============================
// === useRemoteBackendStrict ===
// ==============================

/** Get the Remote Backend. */
export function useRemoteBackendStrict() {
  const remoteBackend = React.useContext(BackendContext).remoteBackend
  if (remoteBackend == null) {
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
  const remoteBackend = useRemoteBackend()
  const localBackend = useLocalBackend()
  const backend = categoryModule.isCloud(category) ? remoteBackend : localBackend
  if (backend == null) {
    throw new Error(
      `This distribution of ${ensoCommon.PRODUCT_NAME} supports neither the Cloud Backend nor the Local Backend.`
    )
  }
  return backend
}
