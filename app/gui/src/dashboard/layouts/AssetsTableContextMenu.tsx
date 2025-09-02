/**
 * @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected.
 */
import type { ContextMenuApi } from '#/components/ContextMenu'
import { ContextMenu } from '#/components/ContextMenu'
import {
  deleteAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useUploadFileToCloudMutation, useUploadFileToLocal } from '#/hooks/backendUploadFilesHooks'
import { useCopy } from '#/hooks/copyHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { defineMenuEntry, useMenuEntries } from '#/hooks/menuHooks'
import {
  canTransferBetweenCategories,
  type Category,
  isCloudCategory,
} from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useGlobalContextMenuEntries } from '#/layouts/useGlobalContextMenuEntries'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useExportArchive } from '#/pages/useExportArchive'
import { useDriveStore, useSelectedAssets, useSetSelectedAssets } from '#/providers/DriveProvider'
import { setModal } from '#/providers/ModalProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useStore } from '#/utilities/zustand'
import { useBackends, useText, useUser } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import * as React from 'react'
import invariant from 'tiny-invariant'

/** Props for an {@link AssetsTableContextMenu}. */
export interface AssetsTableContextMenuProps {
  readonly backend: Backend
  readonly category: Category
  readonly currentDirectoryId: backendModule.DirectoryId
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
  ) => void
}

/**
 * A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected.
 */
export const AssetsTableContextMenu = React.forwardRef(function AssetsTableContextMenu(
  props: AssetsTableContextMenuProps,
  ref: React.ForwardedRef<ContextMenuApi>,
) {
  const { backend, category, currentDirectoryId, doCopy, doCut, doPaste } = props

  const { getText } = useText()

  const { localBackend } = useBackends()
  const user = useUser()
  const isCloud = isCloudCategory(category)
  const getAsset = useGetAsset()
  const selectedAssets = useSelectedAssets()
  const setSelectedAssets = useSetSelectedAssets()
  const driveStore = useDriveStore()
  const deleteAssets = useMutationCallback(deleteAssetsMutationOptions(backend))
  const restoreAssets = useMutationCallback(restoreAssetsMutationOptions(backend))
  const showDeveloperIds = useFeatureFlag('showDeveloperIds')
  const copyMutation = useCopy()
  const uploadFileToCloudMutation = useUploadFileToCloudMutation()
  const uploadFileToLocal = useUploadFileToLocal(category)
  const exportArchive = useExportArchive({ backend })

  const canUploadToCloud = user.plan !== backendModule.Plan.free

  const globalContextMenuEntries = useGlobalContextMenuEntries({
    backend,
    category,
    currentDirectoryId,
    directoryId: null,
    doPaste,
  })

  const canUploadAllProjectsToCloud = useStore(
    driveStore,
    (state) =>
      !isCloud &&
      localBackend != null &&
      [...state.selectedIds].every(
        (id) => backendModule.getAssetTypeFromId(id) === backendModule.AssetType.project,
      ),
  )
  const canDownloadAllProjectsToLocal = useStore(
    driveStore,
    (state) =>
      isCloud &&
      localBackend != null &&
      [...state.selectedIds].every(
        (id) => backendModule.getAssetTypeFromId(id) === backendModule.AssetType.project,
      ),
  )

  const uploadFilesToCloudCallback = useEventCallback(async () => {
    invariant(localBackend != null, 'Cannot upload to cloud when not on Local backend')
    const selectedIds = [...driveStore.getState().selectedIds]
    const files = selectedIds.flatMap((id) => {
      const asset = getAsset(id)
      return asset ? [asset] : []
    })
    await uploadFileToCloudMutation(localBackend, {
      assets: [...files],
      targetDirectoryId: user.rootDirectoryId,
    })
  })

  const downloadFilesToLocalCallback = useEventCallback(async () => {
    const selectedIds = [...driveStore.getState().selectedIds]
    const files = selectedIds.flatMap((id) => {
      const asset = getAsset(id)
      return asset ? [asset] : []
    })
    await uploadFileToLocal(files)
  })

  const hasPasteData = useStore(driveStore, ({ pasteData }) => {
    const effectivePasteData =
      (
        pasteData?.data.backendType === backend.type &&
        canTransferBetweenCategories(pasteData.data.category, category)
      ) ?
        pasteData
      : null
    return (effectivePasteData?.data.assets.length ?? 0) > 0
  })

  // This is not a React component even though it contains JSX.
  const doDeleteAll = useEventCallback(() => {
    const selectedIds = selectedAssets.map((asset) => asset.id)
    const deleteAll = async () => {
      setSelectedAssets([])
      await deleteAssets([selectedIds, false])
    }
    const firstKey = selectedIds[0]
    const soleAssetName =
      firstKey != null ? (getAsset(firstKey)?.title ?? '(unknown)') : '(unknown)'
    setModal(
      <ConfirmDeleteModal
        defaultOpen
        actionText={
          selectedIds.length === 1 ?
            getText('deleteSelectedAssetActionText', soleAssetName)
          : getText('deleteSelectedAssetsActionText', selectedIds.length)
        }
        onConfirm={deleteAll}
      />,
    )
  })

  const copyIdsMenuEntry = defineMenuEntry(
    showDeveloperIds && {
      action: 'copyId',
      color: 'accent',
      doAction: () => {
        copyMutation.mutate(selectedAssets.map((asset) => asset.id).join('\n'))
      },
    },
  )

  const pasteAllMenuEntry = defineMenuEntry(
    hasPasteData && {
      action: 'paste',
      doAction: () => {
        const selected = selectedAssets[0]
        if (selected?.type === backendModule.AssetType.directory) {
          doPaste(selected.id, selected.id)
        } else {
          doPaste(currentDirectoryId, currentDirectoryId)
        }
      },
    },
  )

  const entries = useMenuEntries(
    category.type === 'recent' ? [copyIdsMenuEntry]
    : category.type === 'trash' ?
      selectedAssets.length === 0 ?
        []
      : [
          pasteAllMenuEntry,
          {
            action: 'undelete',
            label: getText('restoreFromTrashShortcut'),
            doAction: () => {
              void restoreAssets({
                ids: selectedAssets.map((asset) => asset.id),
                parentId: null,
              })
            },
          },
          {
            action: 'delete',
            label: getText('deleteForeverShortcut'),
            doAction: () => {
              const asset = selectedAssets[0]
              const soleAssetName = asset?.title ?? '(unknown)'
              setModal(
                <ConfirmDeleteModal
                  defaultOpen
                  actionText={
                    selectedAssets.length === 1 ?
                      getText('deleteSelectedAssetForeverActionText', soleAssetName)
                    : getText('deleteSelectedAssetsForeverActionText', selectedAssets.length)
                  }
                  onConfirm={async () => {
                    setSelectedAssets([])
                    await deleteAssets([selectedAssets.map((otherAsset) => otherAsset.id), true])
                  }}
                />,
              )
            },
          },
          copyIdsMenuEntry,
        ]
    : [
        selectedAssets.length !== 0 &&
          canUploadAllProjectsToCloud && {
            isUnderPaywall: !canUploadToCloud,
            action: 'uploadToCloud',
            feature: 'uploadToCloud',
            doAction: () => {
              void uploadFilesToCloudCallback()
            },
          },
        selectedAssets.length !== 0 &&
          canDownloadAllProjectsToLocal && {
            action: 'downloadToLocal',
            doAction: () => {
              void downloadFilesToLocalCallback()
            },
          },
        selectedAssets.length !== 0 && {
          action: 'exportArchive',
          doAction: () => {
            void exportArchive()
          },
        },
        selectedAssets.length !== 0 && isCloud && { action: 'copy', doAction: doCopy },
        selectedAssets.length !== 0 && { action: 'cut', doAction: doCut },
        pasteAllMenuEntry,
        ...globalContextMenuEntries,
        selectedAssets.length !== 0 && {
          action: 'delete',
          label: isCloud ? getText('moveToTrashShortcut') : getText('deleteShortcut'),
          doAction: doDeleteAll,
        },
        copyIdsMenuEntry,
      ],
  )

  return (
    <ContextMenu ref={ref} aria-label={getText('assetsTableContextMenuLabel')} entries={entries} />
  )
})
