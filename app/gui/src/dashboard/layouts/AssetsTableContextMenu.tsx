/**
 * @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected.
 */
import * as React from 'react'

import { useStore } from '#/utilities/zustand'

import { useDriveStore, useSelectedAssets, useSetSelectedAssets } from '#/providers/DriveProvider'

import {
  canTransferBetweenCategories,
  type Category,
  isCloudCategory,
} from '#/layouts/CategorySwitcher/Category'
import { GlobalContextMenu } from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import { Separator } from '#/components/AriaComponents'
import {
  deleteAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { useMutation } from '@tanstack/react-query'
import { twJoin } from '../utilities/tailwindMerge'

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
  } = props

  const { setModal, unsetModal } = useSetModal()
  const { getText } = useText()

  const isCloud = isCloudCategory(category)
  const getAsset = useGetAsset()
  const selectedAssets = useSelectedAssets()
  const setSelectedAssets = useSetSelectedAssets()
  const driveStore = useDriveStore()
  const deleteAssetsMutation = useMutation(deleteAssetsMutationOptions(backend))
  const restoreAssetsMutation = useMutation(restoreAssetsMutationOptions(backend))

  const hasPasteData = useStore(driveStore, ({ pasteData }) => {
    const effectivePasteData =
      (
        pasteData?.data.backendType === backend.type &&
        canTransferBetweenCategories(pasteData.data.category, category)
      ) ?
        pasteData
      : null
    return (effectivePasteData?.data.ids.size ?? 0) > 0
  })

  // This is not a React component even though it contains JSX.
  const doDeleteAll = useEventCallback(async () => {
    const selectedIds = selectedAssets.map((asset) => asset.id)
    const deleteAll = async () => {
      unsetModal()
      setSelectedAssets([])

      await deleteAssetsMutation.mutateAsync([selectedIds, false])
    }
    if (
      isCloud &&
      selectedIds.every((key) => getAsset(key)?.type !== backendModule.AssetType.directory)
    ) {
      await deleteAll()
    } else {
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
          doDelete={deleteAll}
        />,
      )
    }
  })

  const pasteAllMenuEntry = hasPasteData && (
    <ContextMenuEntry
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
          <ContextMenuEntry
            hidden={hidden}
            action="undelete"
            label={getText('restoreAllFromTrashShortcut')}
            doAction={() => {
              unsetModal()
              restoreAssetsMutation.mutate(selectedAssets.map((asset) => asset.id))
            }}
          />
          <ContextMenuEntry
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
                  doDelete={async () => {
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
    return null
  }

  return (
    <ContextMenu aria-label={getText('assetsTableContextMenuLabel')} hidden={hidden} event={event}>
      <>
        {selectedAssets.length !== 0 && (
          <ContextMenuEntry
            hidden={hidden}
            action="delete"
            label={isCloud ? getText('moveAllToTrashShortcut') : getText('deleteAllShortcut')}
            doAction={doDeleteAll}
          />
        )}
        {selectedAssets.length !== 0 && isCloud && (
          <ContextMenuEntry
            hidden={hidden}
            action="copy"
            label={getText('copyAllShortcut')}
            doAction={doCopy}
          />
        )}
        {selectedAssets.length !== 0 && (
          <ContextMenuEntry
            hidden={hidden}
            action="cut"
            label={getText('cutAllShortcut')}
            doAction={doCut}
          />
        )}
        {pasteAllMenuEntry}
      </>

      <Separator className={twJoin('my-2 first:hidden', hidden && 'hidden')} />

      <GlobalContextMenu
        noWrapper
        hidden={hidden}
        backend={backend}
        category={category}
        currentDirectoryId={currentDirectoryId}
        directoryId={null}
        doPaste={doPaste}
        event={event}
      />
    </ContextMenu>
  )
}
