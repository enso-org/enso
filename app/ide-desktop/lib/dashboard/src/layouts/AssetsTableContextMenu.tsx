/** @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
import * as React from 'react'

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

import type * as pasteDataModule from '#/utilities/pasteData'
import * as permissions from '#/utilities/permissions'
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
  readonly pasteData: pasteDataModule.PasteData<ReadonlySet<backendModule.AssetId>> | null
  readonly nodeMapRef: React.MutableRefObject<
    ReadonlyMap<backendModule.AssetId, backendModule.AnyAsset>
  >
  readonly event: Pick<React.MouseEvent<Element, MouseEvent>, 'pageX' | 'pageY'>
  readonly doPaste: (newParentId: backendModule.DirectoryId) => void
}

/** A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
export default function AssetsTableContextMenu(props: AssetsTableContextMenuProps) {
  const { hidden = false, backend, category, pasteData, nodeMapRef, event, rootDirectoryId } = props
  const { doPaste } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const selectedIds = store.useStore(storeState => storeState.getSelectedAssetIds(backend.type))
  const setSelectedIds = store.useStore(storeState => storeState.setSelectedAssetIds)
  const setAssetPasteData = store.useStore(storeState => storeState.setAssetPasteData)
  const isCloud = categoryModule.isCloud(category)

  const deleteAssetMutation = backendHooks.useBackendMutation(backend, 'deleteAsset')
  const undoDeleteAssetMutation = backendHooks.useBackendMutation(backend, 'undoDeleteAsset')

  // This works because all items are mutated, ensuring their value stays
  // up to date.
  const ownsAllSelectedAssets =
    !isCloud ||
    (user != null &&
      Array.from(selectedIds, key => {
        const userPermissions = nodeMapRef.current.get(key)?.permissions
        const selfPermission = userPermissions?.find(
          backendModule.isUserPermissionAnd(permission => permission.user.userId === user.userId)
        )
        return selfPermission?.permission === permissions.PermissionAction.own
      }).every(isOwner => isOwner))

  // This is not a React component even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const doDeleteAll = () => {
    const deleteAll = () => {
      for (const key of selectedIds) {
        deleteAssetMutation.mutate([key, { force: false }])
      }
    }
    if (isCloud) {
      deleteAll()
    } else {
      const [firstKey] = selectedIds
      const soleAssetName =
        firstKey != null ? nodeMapRef.current.get(firstKey)?.title ?? '(unknown)' : '(unknown)'
      setModal(
        <ConfirmDeleteModal
          actionText={
            selectedIds.length === 1
              ? getText('deleteSelectedAssetActionText', soleAssetName)
              : getText('deleteSelectedAssetsActionText', selectedIds.length)
          }
          doDelete={() => {
            setSelectedIds(backend.type, [])
            deleteAll()
          }}
        />
      )
    }
  }

  if (category === Category.trash) {
    return selectedIds.length === 0 ? (
      <></>
    ) : (
      <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
        <ContextMenu aria-label={getText('assetsTableContextMenuLabel')} hidden={hidden}>
          <ContextMenuEntry
            hidden={hidden}
            action="undelete"
            label={getText('restoreAllFromTrashShortcut')}
            doAction={() => {
              for (const id of selectedIds) {
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
                const [firstKey] = selectedIds
                const soleAssetName =
                  firstKey != null
                    ? nodeMapRef.current.get(firstKey)?.title ?? '(unknown)'
                    : '(unknown)'
                setModal(
                  <ConfirmDeleteModal
                    actionText={
                      selectedIds.length === 1
                        ? getText('deleteSelectedAssetForeverActionText', soleAssetName)
                        : getText('deleteSelectedAssetsForeverActionText', selectedIds.length)
                    }
                    doDelete={() => {
                      setSelectedIds(backend.type, [])
                      for (const id of selectedIds) {
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
        {selectedIds.length !== 0 && (
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
                  setAssetPasteData(backend.type, 'copy', selectedIds)
                }}
              />
            )}
            {ownsAllSelectedAssets && (
              <ContextMenuEntry
                hidden={hidden}
                action="cut"
                label={getText('cutAllShortcut')}
                doAction={() => {
                  setAssetPasteData(backend.type, 'cut', selectedIds)
                }}
              />
            )}
            {pasteData != null && pasteData.data.size > 0 && (
              <ContextMenuEntry
                hidden={hidden}
                action="paste"
                label={getText('pasteAllShortcut')}
                doAction={() => {
                  const [firstKey] = selectedIds
                  const selectedNode =
                    selectedIds.length === 1 && firstKey != null
                      ? nodeMapRef.current.get(firstKey)
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
          hasPasteData={pasteData != null}
          rootDirectoryId={rootDirectoryId}
          directoryId={null}
          doPaste={doPaste}
        />
      </ContextMenus>
    )
  }
}
