/** @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'

import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import type * as assetTreeNode from '#/utilities/AssetTreeNode'
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
  readonly selectedKeys: ReadonlySet<backendModule.AssetId>
  readonly clearSelectedKeys: () => void
  readonly nodeMapRef: React.MutableRefObject<
    ReadonlyMap<backendModule.AssetId, assetTreeNode.AnyAssetTreeNode>
  >
  readonly event: Pick<React.MouseEvent<Element, MouseEvent>, 'pageX' | 'pageY'>
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  readonly dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (
    newParentKey: backendModule.DirectoryId,
    newParentId: backendModule.DirectoryId
  ) => void
}

/** A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
export default function AssetsTableContextMenu(props: AssetsTableContextMenuProps) {
  const { hidden = false, backend, category, pasteData, selectedKeys, clearSelectedKeys } = props
  const { nodeMapRef, event, dispatchAssetEvent, dispatchAssetListEvent, rootDirectoryId } = props
  const { doCopy, doCut, doPaste } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const isCloud = categoryModule.isCloud(category)

  // This works because all items are mutated, ensuring their value stays
  // up to date.
  const ownsAllSelectedAssets =
    !isCloud ||
    (user != null &&
      Array.from(selectedKeys, key => {
        const userPermissions = nodeMapRef.current.get(key)?.item.permissions
        const selfPermission = userPermissions?.find(
          backendModule.isUserPermissionAnd(permission => permission.user.userId === user.userId)
        )
        return selfPermission?.permission === permissions.PermissionAction.own
      }).every(isOwner => isOwner))

  // This is not a React component even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const doDeleteAll = () => {
    if (isCloud) {
      unsetModal()
      dispatchAssetEvent({ type: AssetEventType.delete, ids: selectedKeys })
    } else {
      const [firstKey] = selectedKeys
      const soleAssetName =
        firstKey != null ? nodeMapRef.current.get(firstKey)?.item.title ?? '(unknown)' : '(unknown)'
      setModal(
        <ConfirmDeleteModal
          actionText={
            selectedKeys.size === 1
              ? getText('deleteSelectedAssetActionText', soleAssetName)
              : getText('deleteSelectedAssetsActionText', selectedKeys.size)
          }
          doDelete={() => {
            clearSelectedKeys()
            dispatchAssetEvent({ type: AssetEventType.delete, ids: selectedKeys })
          }}
        />
      )
    }
  }

  if (category === Category.trash) {
    return selectedKeys.size === 0 ? (
      <></>
    ) : (
      <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
        <ContextMenu aria-label={getText('assetsTableContextMenuLabel')} hidden={hidden}>
          <ContextMenuEntry
            hidden={hidden}
            action="undelete"
            label={getText('restoreAllFromTrashShortcut')}
            doAction={() => {
              unsetModal()
              dispatchAssetEvent({ type: AssetEventType.restore, ids: selectedKeys })
            }}
          />
          {isCloud && (
            <ContextMenuEntry
              hidden={hidden}
              action="delete"
              label={getText('deleteAllForeverShortcut')}
              doAction={() => {
                const [firstKey] = selectedKeys
                const soleAssetName =
                  firstKey != null
                    ? nodeMapRef.current.get(firstKey)?.item.title ?? '(unknown)'
                    : '(unknown)'
                setModal(
                  <ConfirmDeleteModal
                    actionText={
                      selectedKeys.size === 1
                        ? getText('deleteSelectedAssetForeverActionText', soleAssetName)
                        : getText('deleteSelectedAssetsForeverActionText', selectedKeys.size)
                    }
                    doDelete={() => {
                      clearSelectedKeys()
                      dispatchAssetEvent({ type: AssetEventType.deleteForever, ids: selectedKeys })
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
        {selectedKeys.size !== 0 && (
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
                doAction={doCopy}
              />
            )}
            {ownsAllSelectedAssets && (
              <ContextMenuEntry
                hidden={hidden}
                action="cut"
                label={getText('cutAllShortcut')}
                doAction={doCut}
              />
            )}
            {pasteData != null && pasteData.data.size > 0 && (
              <ContextMenuEntry
                hidden={hidden}
                action="paste"
                label={getText('pasteAllShortcut')}
                doAction={() => {
                  const [firstKey] = selectedKeys
                  const selectedNode =
                    selectedKeys.size === 1 && firstKey != null
                      ? nodeMapRef.current.get(firstKey)
                      : null
                  if (selectedNode?.type === backendModule.AssetType.directory) {
                    doPaste(selectedNode.key, selectedNode.item.id)
                  } else {
                    doPaste(rootDirectoryId, rootDirectoryId)
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
          directoryKey={null}
          directoryId={null}
          dispatchAssetListEvent={dispatchAssetListEvent}
          doPaste={doPaste}
        />
      </ContextMenus>
    )
  }
}
