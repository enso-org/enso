/**
 * @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected.
 */
import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import { ContextMenuEntry as PaywallContextMenuEntry } from '#/components/Paywall'
import { Separator } from '#/components/Separator'
import {
  deleteAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useUploadFileToCloudMutation, useUploadFileToLocal } from '#/hooks/backendUploadFilesHooks'
import { useCopy } from '#/hooks/copyHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import {
  canTransferBetweenCategories,
  type Category,
  isCloudCategory,
} from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useDriveStore, useSelectedAssets, useSetSelectedAssets } from '#/providers/DriveProvider'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { useStore } from '#/utilities/zustand'
import { useBackends, useText, useUser } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { useMutation } from '@tanstack/react-query'
import * as React from 'react'
import invariant from 'tiny-invariant'
import { GlobalContextMenu } from './GlobalContextMenu'

/** Props for an {@link AssetsTableContextMenu}. */
export interface AssetsTableContextMenuProps {
  readonly hidden?: boolean
  readonly backend: Backend
  readonly category: Category
  readonly currentDirectoryId: backendModule.DirectoryId
  readonly event: Pick<React.MouseEvent<Element, MouseEvent>, 'pageX' | 'pageY'>
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId,
  ) => void
  readonly rootRef?: React.RefObject<HTMLElement>
}

/**
 * A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected.
 */
export default function AssetsTableContextMenu(props: AssetsTableContextMenuProps) {
  // eslint-disable-next-line react-compiler/react-compiler
  'use no memo'
  const {
    hidden = false,
    backend,
    category,
    currentDirectoryId,
    event,
    doCopy,
    doCut,
    doPaste,
    rootRef,
  } = props

  const { getText } = useText()

  const { localBackend } = useBackends()
  const user = useUser()
  const isCloud = isCloudCategory(category)
  const getAsset = useGetAsset()
  const selectedAssets = useSelectedAssets()
  const setSelectedAssets = useSetSelectedAssets()
  const driveStore = useDriveStore()
  const deleteAssetsMutation = useMutation(deleteAssetsMutationOptions(backend))
  const restoreAssetsMutation = useMutation(restoreAssetsMutationOptions(backend))
  const showDeveloperIds = useFeatureFlag('showDeveloperIds')
  const copyMutation = useCopy()
  const uploadFileToCloudMutation = useUploadFileToCloudMutation()
  const uploadFileToLocal = useUploadFileToLocal(category)

  const canUploadToCloud = user.plan !== backendModule.Plan.free

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
      await deleteAssetsMutation.mutateAsync([selectedIds, false])
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

  const copyIdsMenuEntry = showDeveloperIds && (
    <ContextMenuEntry
      bindingFocusScope={rootRef}
      hidden={hidden}
      action="copyId"
      color="accent"
      label={getText('copyAllIdsShortcut')}
      doAction={() => copyMutation.mutateAsync(selectedAssets.map((asset) => asset.id).join('\n'))}
    />
  )

  const pasteAllMenuEntry = hasPasteData && (
    <ContextMenuEntry
      bindingFocusScope={rootRef}
      hidden={hidden}
      action="paste"
      label={getText('pasteAllShortcut')}
      doAction={() => {
        const selected = selectedAssets[0]
        if (selected?.type === backendModule.AssetType.directory) {
          doPaste(selected.id, selected.id)
        } else {
          doPaste(currentDirectoryId, currentDirectoryId)
        }
      }}
    />
  )

  if (category.type === 'trash') {
    return (
      selectedAssets.length > 1 && (
        <ContextMenu
          aria-label={getText('assetsTableContextMenuLabel')}
          hidden={hidden}
          event={event}
        >
          {copyIdsMenuEntry}
          <ContextMenuEntry
            bindingFocusScope={rootRef}
            hidden={hidden}
            action="undelete"
            label={getText('restoreAllFromTrashShortcut')}
            doAction={() => {
              unsetModal()
              restoreAssetsMutation.mutate({
                ids: selectedAssets.map((asset) => asset.id),
                parentId: null,
              })
            }}
          />
          <ContextMenuEntry
            bindingFocusScope={rootRef}
            hidden={hidden}
            action="delete"
            label={getText('deleteAllForeverShortcut')}
            doAction={() => {
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
                    await deleteAssetsMutation.mutateAsync([
                      selectedAssets.map((otherAsset) => otherAsset.id),
                      true,
                    ])
                  }}
                />,
              )
            }}
          />
          {pasteAllMenuEntry}
        </ContextMenu>
      )
    )
  }

  if (category.type === 'recent') {
    return (
      showDeveloperIds && (
        <ContextMenu
          aria-label={getText('assetsTableContextMenuLabel')}
          hidden={hidden}
          event={event}
        >
          {copyIdsMenuEntry}
        </ContextMenu>
      )
    )
  }

  return (
    <ContextMenu aria-label={getText('assetsTableContextMenuLabel')} hidden={hidden} event={event}>
      <>
        {copyIdsMenuEntry}
        {selectedAssets.length !== 0 && (
          <ContextMenuEntry
            bindingFocusScope={rootRef}
            hidden={hidden}
            action="delete"
            label={isCloud ? getText('moveAllToTrashShortcut') : getText('deleteAllShortcut')}
            doAction={doDeleteAll}
          />
        )}
        {selectedAssets.length !== 0 && canUploadAllProjectsToCloud && (
          <PaywallContextMenuEntry
            hidden={hidden}
            bindingFocusScope={rootRef}
            isUnderPaywall={!canUploadToCloud}
            action="uploadToCloud"
            feature="uploadToCloud"
            label={getText('uploadAllToCloudShortcut')}
            doAction={uploadFilesToCloudCallback}
          />
        )}
        {selectedAssets.length !== 0 && canDownloadAllProjectsToLocal && (
          <ContextMenuEntry
            bindingFocusScope={rootRef}
            hidden={hidden}
            action="downloadToLocal"
            label={getText('downloadAllToLocalShortcut')}
            doAction={downloadFilesToLocalCallback}
          />
        )}
        {selectedAssets.length !== 0 && isCloud && (
          <ContextMenuEntry
            bindingFocusScope={rootRef}
            hidden={hidden}
            action="copy"
            label={getText('copyAllShortcut')}
            doAction={doCopy}
          />
        )}
        {selectedAssets.length !== 0 && (
          <ContextMenuEntry
            bindingFocusScope={rootRef}
            hidden={hidden}
            action="cut"
            label={getText('cutAllShortcut')}
            doAction={doCut}
          />
        )}
        {pasteAllMenuEntry}
      </>

      {!hidden && <Separator className="my-2 first:hidden" />}

      <GlobalContextMenu
        noWrapper
        hidden={hidden}
        backend={backend}
        category={category}
        currentDirectoryId={currentDirectoryId}
        directoryId={null}
        doPaste={doPaste}
        event={event}
        bindingFocusScope={rootRef}
      />
    </ContextMenu>
  )
}
