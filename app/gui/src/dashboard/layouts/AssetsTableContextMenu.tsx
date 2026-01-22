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
import { useUploadFileToCloud, useUploadFileToLocal } from '#/hooks/backendUploadFilesHooks'
import { useCopy } from '#/hooks/copyHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { defineMenuEntry, useMenuEntries } from '#/hooks/menuHooks'
import { canTransferBetweenCategories, isCloudCategory } from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useCategoriesAPI } from '#/layouts/Drive/Categories'
import { useGlobalContextMenuEntries } from '#/layouts/useGlobalContextMenuEntries'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useExportArchive } from '#/pages/useExportArchive'
import { useDriveStore, useSelectedAssets, useSetSelectedAssets } from '#/providers/DriveProvider'
import { setModal } from '#/providers/ModalProvider'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useStore } from '#/utilities/zustand'
import { useBackends, useRouter, useText, useUser } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import * as backendModule from 'enso-common/src/services/Backend'
import * as React from 'react'
import invariant from 'tiny-invariant'

/** Props for an {@link AssetsTableContextMenu}. */
export interface AssetsTableContextMenuProps {
  readonly currentDirectoryId: backendModule.DirectoryId
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (newParentId: backendModule.DirectoryId) => void
}

/**
 * A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected.
 */
export const AssetsTableContextMenu = React.forwardRef(function AssetsTableContextMenu(
  props: AssetsTableContextMenuProps,
  ref: React.ForwardedRef<ContextMenuApi>,
) {
  const { currentDirectoryId, doCopy, doCut, doPaste } = props

  const { category, associatedBackend: backend } = useCategoriesAPI()
  const { getText } = useText()
  const { router } = useRouter()
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
  const uploadFileToCloudMutation = useUploadFileToCloud()
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

  const goToDrive = async () => {
    if (router.currentRoute.value.path === '/drive') return
    await router.push({ ...router.currentRoute.value, path: '/drive' })
  }

  const copyIdsMenuEntry = defineMenuEntry(
    showDeveloperIds && {
      action: 'copyId',
      color: 'accent',
      doAction: () => {
        void goToDrive()
        copyMutation.mutate(selectedAssets.map((asset) => asset.id).join('\n'))
      },
    },
  )

  const pasteAllMenuEntry = defineMenuEntry(
    hasPasteData && {
      action: 'paste',
      doAction: () => {
        void goToDrive()
        const selected = selectedAssets[0]
        const id =
          selected?.type === backendModule.AssetType.directory ? selected.id : currentDirectoryId
        doPaste(id)
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
              void goToDrive()
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
              void goToDrive()
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
              void goToDrive()
              void uploadFilesToCloudCallback()
            },
          },
        selectedAssets.length !== 0 &&
          canDownloadAllProjectsToLocal && {
            action: 'downloadToLocal',
            doAction: () => {
              void goToDrive()
              void downloadFilesToLocalCallback()
            },
          },
        selectedAssets.length !== 0 && {
          action: 'exportArchive',
          doAction: () => {
            void goToDrive()
            void exportArchive()
          },
        },
        selectedAssets.length !== 0 && isCloud && { action: 'copy', doAction: doCopy },
        selectedAssets.length !== 0 && {
          action: 'cut',
          doAction: () => {
            void goToDrive()
            doCut()
          },
        },
        pasteAllMenuEntry,
        ...globalContextMenuEntries,
        selectedAssets.length !== 0 && {
          action: 'delete',
          label: isCloud ? getText('moveToTrashShortcut') : getText('deleteShortcut'),
          doAction: () => {
            void goToDrive()
            doDeleteAll()
          },
        },
        copyIdsMenuEntry,
      ],
  )

  return (
    <ContextMenu ref={ref} aria-label={getText('assetsTableContextMenuLabel')} entries={entries} />
  )
})
