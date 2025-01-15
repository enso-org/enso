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

import Separator from '#/components/styled/Separator'
import {
  deleteAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import * as permissions from '#/utilities/permissions'
import { useMutation } from '@tanstack/react-query'

// =================
// === Constants ===
// =================

/** Props for an {@link AssetsTableContextMenu}. */
export interface AssetsTableContextMenuProps {
  readonly hidden?: boolean
  readonly backend: Backend
  readonly category: Category
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly nodeMapRef: React.MutableRefObject<
    ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>
  >
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
  const { hidden = false, backend, category } = props
  const { nodeMapRef, event, rootDirectoryId } = props
  const { doCopy, doCut, doPaste } = props

  const { user } = useFullUserSession()
  const { setModal, unsetModal } = useSetModal()
  const { getText } = useText()

  const isCloud = isCloudCategory(category)
  const selectedAssets = useSelectedAssets()
  const setSelectedAssets = useSetSelectedAssets()
  const driveStore = useDriveStore()
  const deleteAssetsMutation = useMutation(deleteAssetsMutationOptions(backend))
  const restoreAssetsMutation = useMutation(restoreAssetsMutationOptions(backend))

  const hasPasteData = useStore(driveStore, ({ pasteData }) => {
    const effectivePasteData =
      (
        pasteData?.data.backendType === backend.type &&
        canTransferBetweenCategories(pasteData.data.category, category, user)
      ) ?
        pasteData
      : null
    return (effectivePasteData?.data.ids.size ?? 0) > 0
  })

  const ownsAllSelectedAssets =
    !isCloud ||
    selectedAssets.every(
      ({ id }) =>
        permissions.tryFindSelfPermission(user, nodeMapRef.current.get(id)?.item.permissions)
          ?.permission === permissions.PermissionAction.own,
    )

  // This is not a React component even though it contains JSX.
  const doDeleteAll = useEventCallback(async () => {
    const selectedKeys = selectedAssets.map((asset) => asset.id)
    const deleteAll = async () => {
      unsetModal()
      setSelectedAssets([])

      await deleteAssetsMutation.mutateAsync([selectedKeys, false])
    }
    if (
      isCloud &&
      selectedKeys.every(
        (key) => nodeMapRef.current.get(key)?.item.type !== backendModule.AssetType.directory,
      )
    ) {
      await deleteAll()
    } else {
      const firstKey = selectedKeys[0]
      const soleAssetName =
        firstKey != null ?
          (nodeMapRef.current.get(firstKey)?.item.title ?? '(unknown)')
        : '(unknown)'
      setModal(
        <ConfirmDeleteModal
          defaultOpen
          actionText={
            selectedKeys.length === 1 ?
              getText('deleteSelectedAssetActionText', soleAssetName)
            : getText('deleteSelectedAssetsActionText', selectedKeys.length)
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
          doPaste(rootDirectoryId, rootDirectoryId)
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
          {isCloud && (
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
          )}
          {pasteAllMenuEntry}
        </ContextMenu>
      )
    )
  } else if (category.type === 'recent') {
    return null
  } else {
    const shouldShowAssetMenu = selectedAssets.length !== 0 || pasteAllMenuEntry !== false
    const shouldShowGlobalMenu =
      category.type !== 'cloud' || user.plan == null || user.plan === backendModule.Plan.solo
    if (!shouldShowAssetMenu && !shouldShowGlobalMenu) {
      return null
    } else {
      return (
        <ContextMenu
          aria-label={getText('assetsTableContextMenuLabel')}
          hidden={hidden}
          event={event}
        >
          {shouldShowAssetMenu && (
            <>
              {selectedAssets.length !== 0 && ownsAllSelectedAssets && (
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
              {selectedAssets.length !== 0 && ownsAllSelectedAssets && (
                <ContextMenuEntry
                  hidden={hidden}
                  action="cut"
                  label={getText('cutAllShortcut')}
                  doAction={doCut}
                />
              )}
              {pasteAllMenuEntry}
            </>
          )}
          {shouldShowAssetMenu && shouldShowGlobalMenu && <Separator hidden={hidden} />}
          {shouldShowGlobalMenu && (
            <GlobalContextMenu
              noWrapper
              hidden={hidden}
              backend={backend}
              category={category}
              rootDirectoryId={rootDirectoryId}
              directoryId={null}
              path={null}
              doPaste={doPaste}
              event={event}
            />
          )}
        </ContextMenu>
      )
    }
  }
}
