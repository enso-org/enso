/**
 * @file Switches between {@link AssetContextMenu}
 * and {@link AssetsTableContextMenu} as necessary.
 */
import { ContextMenuApi } from '#/components/ContextMenu'
import { useStore } from '#/hooks/storeHooks'
import { AssetContextMenu } from '#/layouts/AssetContextMenu'
import { AssetsTableContextMenu } from '#/layouts/AssetsTableContextMenu'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useDriveStore } from '#/providers/DriveProvider'
import { DirectoryId } from '#/services/Backend'
import { ForwardedRef, forwardRef } from 'react'

export interface AssetsTableCombinedContextMenuProps {
  readonly currentDirectoryId: DirectoryId
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (newParentKey: DirectoryId, newParentId: DirectoryId) => void
}

export const AssetsTableCombinedContextMenu = forwardRef(function AssetsTableCombinedContextMenu(
  props: AssetsTableCombinedContextMenuProps,
  ref: ForwardedRef<ContextMenuApi>,
) {
  const { currentDirectoryId, doCopy, doCut, doPaste } = props

  const driveStore = useDriveStore()

  const singleSelectedItemId = useStore(
    driveStore,
    (state) =>
      state.selectedIds.size === 1 ? state.selectedIds[Symbol.iterator]().next().value : undefined,
    {
      unsafeEnableTransition: true,
    },
  )
  const contextMenuData = useStore(driveStore, (state) => state.contextMenuData)
  const getAsset = useGetAsset()
  const asset = singleSelectedItemId ? getAsset(singleSelectedItemId) : undefined

  if (asset) {
    return (
      contextMenuData?.triggerRef && (
        <AssetContextMenu
          ref={ref}
          asset={asset}
          currentDirectoryId={currentDirectoryId}
          doCopy={doCopy}
          doCut={doCut}
          doPaste={doPaste}
          triggerRef={contextMenuData.triggerRef}
          initialPosition={contextMenuData.initialContextMenuPosition}
        />
      )
    )
  } else {
    return (
      <AssetsTableContextMenu
        ref={ref}
        currentDirectoryId={currentDirectoryId}
        doCopy={doCopy}
        doCut={doCut}
        doPaste={doPaste}
      />
    )
  }
})
