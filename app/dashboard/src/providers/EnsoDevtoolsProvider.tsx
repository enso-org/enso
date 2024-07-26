/** @file The React provider (and associated hooks) for Data Catalog state. */
import * as React from 'react'

import invariant from 'tiny-invariant'
import * as zustand from 'zustand'

// =========================
// === EnsoDevtoolsStore ===
// =========================

/** The state of this zustand store. */
interface EnsoDevtoolsStore {
  readonly showVersionChecker: boolean | null
  readonly setEnableVersionChecker: (showVersionChecker: boolean | null) => void
}

// =======================
// === ProjectsContext ===
// =======================

/** State contained in a `ProjectsContext`. */
export interface ProjectsContextType extends zustand.StoreApi<EnsoDevtoolsStore> {}

const EnsoDevtoolsContext = React.createContext<ProjectsContextType | null>(null)

/** Props for a {@link EnsoDevtoolsProvider}. */
export interface ProjectsProviderProps extends Readonly<React.PropsWithChildren> {}

// ========================
// === ProjectsProvider ===
// ========================

/** A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused. */
export default function EnsoDevtoolsProvider(props: ProjectsProviderProps) {
  const { children } = props
  const [store] = React.useState(() => {
    return zustand.createStore<EnsoDevtoolsStore>(set => ({
      showVersionChecker: false,
      setEnableVersionChecker: showVersionChecker => {
        set({ showVersionChecker })
      },
    }))
  })

  return <EnsoDevtoolsContext.Provider value={store}>{children}</EnsoDevtoolsContext.Provider>
}

// ============================
// === useEnsoDevtoolsStore ===
// ============================

/** The Enso devtools store. */
function useEnsoDevtoolsStore() {
  const store = React.useContext(EnsoDevtoolsContext)

  invariant(store, 'Enso Devtools store can only be used inside an `EnsoDevtoolsProvider`.')

  return store
}

// ===============================
// === useEnableVersionChecker ===
// ===============================

/** A function to set whether the version checker is forcibly shown/hidden. */
export function useEnableVersionChecker() {
  const store = useEnsoDevtoolsStore()
  return zustand.useStore(store, state => state.showVersionChecker)
}

// ==================================
// === useSetEnableVersionChecker ===
// ==================================

/** A function to set whether the version checker is forcibly shown/hidden. */
export function useSetEnableVersionChecker() {
  const store = useEnsoDevtoolsStore()
  return zustand.useStore(store, state => state.setEnableVersionChecker)
}
