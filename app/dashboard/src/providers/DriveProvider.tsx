/** @file The React provider (and associated hooks) for providing reactive events. */
import * as React from 'react'

import invariant from 'tiny-invariant'
import * as zustand from 'zustand'

import type { DirectoryAsset } from 'enso-common/src/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'

// ==================
// === DriveStore ===
// ==================

/** The state of this zustand store. */
interface DriveStore {
  readonly targetDirectory: AssetTreeNode<DirectoryAsset> | null
  readonly setTargetDirectory: (targetDirectory: AssetTreeNode<DirectoryAsset> | null) => void
}

// =======================
// === ProjectsContext ===
// =======================

/** State contained in a `ProjectsContext`. */
export interface ProjectsContextType extends zustand.StoreApi<DriveStore> {}

const DriveContext = React.createContext<ProjectsContextType | null>(null)

/** Props for a {@link DriveProvider}. */
export interface ProjectsProviderProps extends Readonly<React.PropsWithChildren> {}

// ========================
// === ProjectsProvider ===
// ========================

/** A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused. */
export default function DriveProvider(props: ProjectsProviderProps) {
  const { children } = props
  const [store] = React.useState(() => {
    return zustand.createStore<DriveStore>((set) => ({
      targetDirectory: null,
      setTargetDirectory: (targetDirectory) => {
        set({ targetDirectory })
      },
    }))
  })

  return <DriveContext.Provider value={store}>{children}</DriveContext.Provider>
}

// ========================
// === useProjectsStore ===
// ========================

/** The projects store. */
export function useProjectsStore() {
  const store = React.useContext(DriveContext)

  invariant(store, 'Projects store store can only be used inside an `ProjectsProvider`.')

  return store
}

// ==========================
// === useTargetDirectory ===
// ==========================

/** A function to get the target directory of the Asset Table selection. */
export function useTargetDirectory() {
  const store = useProjectsStore()
  return zustand.useStore(store, (state) => state.targetDirectory)
}

// =============================
// === useSetTargetDirectory ===
// =============================

/** A function to set the target directory of the Asset Table selection. */
export function useSetTargetDirectory() {
  const store = useProjectsStore()
  return zustand.useStore(store, (state) => state.setTargetDirectory)
}
