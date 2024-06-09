/** @file The React provider (and associated hooks) storing whether the local backend is supported. */
import * as React from 'react'

// ===================================
// === SupportsLocalBackendContext ===
// ===================================

const SupportsLocalBackendContext = React.createContext(false)

/** Props for a {@link SupportsLocalBackendProvider}. */
export interface SupportsLocalBackendProviderProps extends Readonly<React.PropsWithChildren> {
  readonly supportsLocalBackend: boolean
}

// ====================================
// === SupportsLocalBackendProvider ===
// ====================================

/** A React provider (and associated hooks) for determining whether the current focus contex
 * is vertical or horizontal. */
export default function SupportsLocalBackendProvider(props: SupportsLocalBackendProviderProps) {
  const { supportsLocalBackend, children } = props
  return (
    <SupportsLocalBackendContext.Provider value={supportsLocalBackend}>
      {children}
    </SupportsLocalBackendContext.Provider>
  )
}

/** Whether the Local Backend is supported. */
export function useSupportsLocalBackend() {
  return React.useContext(SupportsLocalBackendContext)
}
