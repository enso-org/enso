/** @file The React provider (and associated hooks) for Data Catalog state. */
import { createContext, useContext, useState, type PropsWithChildren } from 'react'

import invariant from 'tiny-invariant'
import { createStore, useStore, type StoreApi } from 'zustand'

/** The state of this zustand store. */
interface EnsoDevtoolsStore {
  readonly enableVersionChecker: boolean | null
  readonly setEnableVersionChecker: (enableVersionChecker: boolean | null) => void
}

/** State contained in a `EnsoDevtools`. */
export interface EnsoDevtoolsContextType extends StoreApi<EnsoDevtoolsStore> {}

const EnsoDevtoolsContext = createContext<EnsoDevtoolsContextType | null>(null)

/** Props for a {@link EnsoDevtoolsProvider}. */
export interface ProjectsProviderProps extends Readonly<PropsWithChildren> {}

/** A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused. */
export default function EnsoDevtoolsProvider(props: ProjectsProviderProps) {
  const { children } = props
  const [store] = useState(() => {
    return createStore<EnsoDevtoolsStore>((set) => ({
      enableVersionChecker: null,
      setEnableVersionChecker: (enableVersionChecker) => {
        set({ enableVersionChecker })
      },
    }))
  })

  return <EnsoDevtoolsContext.Provider value={store}>{children}</EnsoDevtoolsContext.Provider>
}

/** The Enso devtools store. */
function useEnsoDevtoolsStore() {
  const store = useContext(EnsoDevtoolsContext)

  invariant(store, 'Enso Devtools store can only be used inside an `EnsoDevtoolsProvider`.')

  return store
}

/** Whether the version checker is forcibly shown/hidden. */
export function useEnableVersionChecker() {
  const store = useEnsoDevtoolsStore()
  return useStore(store, (state) => state.enableVersionChecker)
}

/** A function to set whether the version checker is forcibly shown/hidden. */
export function useSetEnableVersionChecker() {
  const store = useEnsoDevtoolsStore()
  return useStore(store, (state) => state.setEnableVersionChecker)
}
