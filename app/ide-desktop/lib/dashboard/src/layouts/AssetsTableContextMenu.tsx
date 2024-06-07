/** @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as store from '#/store'

import * as backendHooks from '#/hooks/backendHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as permissions from '#/utilities/permissions'
import * as set from '#/utilities/set'
import * as uniqueString from '#/utilities/uniqueString'

// =================
// === Constants ===
// =================

/** Props for an {@link AssetsTableContextMenu}. */
export interface AssetsTableContextMenuProps {
  readonly hidden?: boolean
  readonly backend: Backend
  readonly category: Category
  readonly rootDirectoryId: backendModule.DirectoryId
  readonly event: Pick<React.MouseEvent<Element, MouseEvent>, 'pageX' | 'pageY'>
  readonly doPaste: (newParentId: backendModule.DirectoryId) => void
}

/** A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
export default function AssetsTableContextMenu(props: AssetsTableContextMenuProps) {
  const { hidden = false, backend, category, event, rootDirectoryId } = props
  const { doPaste } = props
  const queryClient = reactQuery.useQueryClient()
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const assetPasteData = store.useStore(storeState => storeState.assetPasteData)
  const selectedAssetIds = store.useStore(storeState => storeState.getSelectedAssetIds())
  const setSelectedAssetIds = store.useStore(storeState => storeState.setSelectedAssetIds)
  const setAssetPasteData = store.useStore(storeState => storeState.setAssetPasteData)
  const isCloud = categoryModule.isCloud(category)

  const deleteAssetMutation = backendHooks.useBackendMutation(backend, 'deleteAsset')
  const undoDeleteAssetMutation = backendHooks.useBackendMutation(backend, 'undoDeleteAsset')

  const getAllAssetsMap = () => {
    const allAssets = Object.values(
      backendHooks.getBackendAllKnownDirectories(queryClient, user, backend)
    ).flat()
    return new Map(allAssets.map(asset => [asset.id, asset]))
  }

  // This works because all items are mutated, ensuring their value stays
  // up to date.
  const ownsAllSelectedAssets =
    !isCloud ||
    (user != null &&
      Array.from(selectedAssetIds, key => {
        const userPermissions = getAllAssetsMap().get(key)?.permissions
        const selfPermission = userPermissions?.find(
          backendModule.isUserPermissionAnd(permission => permission.user.userId === user.userId)
        )
        return selfPermission?.permission === permissions.PermissionAction.own
      }).every(isOwner => isOwner))

  // This is not a React component even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const doDeleteAll = () => {
    const deleteAll = () => {
      for (const key of selectedAssetIds) {
        deleteAssetMutation.mutate([key, { force: false }])
      }
    }
    if (isCloud) {
      deleteAll()
    } else {
      const [firstKey] = selectedAssetIds
      const getSoleAssetName = () => {
        return firstKey != null
          ? getAllAssetsMap().get(firstKey)?.title ?? '(unknown)'
          : '(unknown)'
      }
      setModal(
        <ConfirmDeleteModal
          actionText={
            selectedAssetIds.size === 1
              ? getText('deleteSelectedAssetActionText', getSoleAssetName())
              : getText('deleteSelectedAssetsActionText', selectedAssetIds.size)
          }
          doDelete={() => {
            setSelectedAssetIds(set.EMPTY)
            deleteAll()
          }}
        />
      )
    }
  }

  if (category === Category.trash) {
    return selectedAssetIds.size === 0 ? null : (
      <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
        <ContextMenu aria-label={getText('assetsTableContextMenuLabel')} hidden={hidden}>
          <ContextMenuEntry
            hidden={hidden}
            action="undelete"
            label={getText('restoreAllFromTrashShortcut')}
            doAction={() => {
              for (const id of selectedAssetIds) {
                undoDeleteAssetMutation.mutate([id])
              }
            }}
          />
          {isCloud && (
            <ContextMenuEntry
              hidden={hidden}
              action="delete"
              label={getText('deleteAllForeverShortcut')}
              doAction={() => {
                const [firstKey] = selectedAssetIds
                const getSoleAssetName = () => {
                  return firstKey != null
                    ? getAllAssetsMap().get(firstKey)?.title ?? '(unknown)'
                    : '(unknown)'
                }
                setModal(
                  <ConfirmDeleteModal
                    actionText={
                      selectedAssetIds.size === 1
                        ? getText('deleteSelectedAssetForeverActionText', getSoleAssetName())
                        : getText('deleteSelectedAssetsForeverActionText', selectedAssetIds.size)
                    }
                    doDelete={() => {
                      setSelectedAssetIds(set.EMPTY)
                      for (const id of selectedAssetIds) {
                        deleteAssetMutation.mutate([id, { force: true }])
                      }
                    }}
                  />
                )
              }}
            />
          )}
        </ContextMenu>
      </ContextMenus>
    )
  } else if (category !== Category.cloud && category !== Category.local) {
    return null
  } else {
    return (
      <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
        {selectedAssetIds.size !== 0 && (
          <ContextMenu aria-label={getText('assetsTableContextMenuLabel')} hidden={hidden}>
            {ownsAllSelectedAssets && (
              <ContextMenuEntry
                hidden={hidden}
                action="delete"
                label={isCloud ? getText('moveAllToTrashShortcut') : getText('deleteAllShortcut')}
                doAction={doDeleteAll}
              />
            )}
            {isCloud && (
              <ContextMenuEntry
                hidden={hidden}
                action="copy"
                label={getText('copyAllShortcut')}
                doAction={() => {
                  setAssetPasteData({ action: 'copy', ids: selectedAssetIds })
                }}
              />
            )}
            {ownsAllSelectedAssets && (
              <ContextMenuEntry
                hidden={hidden}
                action="cut"
                label={getText('cutAllShortcut')}
                doAction={() => {
                  setAssetPasteData({ action: 'cut', ids: selectedAssetIds })
                }}
              />
            )}
            {assetPasteData != null && assetPasteData.ids.size > 0 && (
              <ContextMenuEntry
                hidden={hidden}
                action="paste"
                label={getText('pasteAllShortcut')}
                doAction={() => {
                  const [firstKey] = selectedAssetIds
                  const selectedNode =
                    selectedAssetIds.size === 1 && firstKey != null
                      ? getAllAssetsMap().get(firstKey)
                      : null
                  if (selectedNode?.type === backendModule.AssetType.directory) {
                    doPaste(selectedNode.id)
                  } else {
                    doPaste(rootDirectoryId)
                  }
                }}
              />
            )}
          </ContextMenu>
        )}
        <GlobalContextMenu
          hidden={hidden}
          backend={backend}
          hasPasteData={assetPasteData != null}
          rootDirectoryId={rootDirectoryId}
          directoryId={null}
          doPaste={doPaste}
        />
      </ContextMenus>
    )
  }
}
