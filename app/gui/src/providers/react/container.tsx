import { useDriveLocation as useDriveStoreVue, type DriveLocationStore } from '$/providers//drive'
import { useContainerData as useContainerDataVue, type ContainerData } from '$/providers/container'
import { useInReactFunction, useVueRef, useVueValue } from '$/providers/react/common'
import {
  useRightPanelData as useRightPanelDataVue,
  type RightPanelData,
} from '$/providers/rightPanel'
import { reactComponent } from '@/util/react'
import * as react from 'react'
import { toRef } from 'vue'
import { useCategories } from '.'

const RightPanelDataContext = react.createContext<RightPanelData | null>(null)
export const useRightPanelData = useInReactFunction(RightPanelDataContext)

const ContainerDataContext = react.createContext<ContainerData | null>(null)
export const useContainerData = useInReactFunction(ContainerDataContext)

const DriveLocationStoreContext = react.createContext<DriveLocationStore | null>(null)
export const useDriveLocation = useInReactFunction(DriveLocationStoreContext)

export const ContainerProviderForReact = reactComponent(
  ({
    container,
    rightPanel,
    driveStore,
    children,
  }: react.PropsWithChildren<{
    container: ContainerData
    rightPanel: RightPanelData
    driveStore: DriveLocationStore
  }>) => {
    return (
      <ContainerDataContext.Provider value={container}>
        <RightPanelDataContext.Provider value={rightPanel}>
          <DriveLocationStoreContext.Provider value={driveStore}>
            {children}
          </DriveLocationStoreContext.Provider>
        </RightPanelDataContext.Provider>
      </ContainerDataContext.Provider>
    )
  },
  {
    useInjectPropsFromWrapper: () => {
      const result = {
        container: useContainerDataVue(),
        rightPanel: useRightPanelDataVue(),
        driveStore: useDriveStoreVue(),
      }
      // Avoid annoying warning about __veauryInjectedProps__ property by returning a function.
      return () => result
    },
  },
) as any

/**
 * A hook to read currently focused asset for right panel, e.g. the currently selected asset
 * in Drive View.
 */
export function useRightPanelFocusedAsset() {
  const rightPanel = useRightPanelData()
  return useVueValue(react.useCallback(() => rightPanel.focusedAsset, [rightPanel]))
}

/** A hook reading current category set for right panel context. */
export function useRightPanelContextCategory() {
  const rightPanel = useRightPanelData()
  return useVueValue(react.useCallback(() => rightPanel.context?.category, [rightPanel]))
}

export function useDriveCurrentCategory() {
  const drive = useDriveLocation()
  return useVueRef(react.useCallback(() => toRef(drive, 'currentCategory'), [drive]))
}

export function useDriveCurrentBackend() {
  const drive = useDriveLocation()
  return useVueValue(react.useCallback(() => drive.associatedBackend, [drive]))
}

export function useDriveCurrentDirectory() {
  const drive = useDriveLocation()
  return useVueRef(react.useCallback(() => toRef(drive, 'currentDirectory'), [drive]))
}

export function useDriveCurrentRootPath() {
  const drive = useDriveLocation()
  const { categoryRootPath } = useCategories()
  return useVueValue(
    react.useCallback(() => categoryRootPath(drive.currentCategory), [drive, categoryRootPath]),
  )
}
