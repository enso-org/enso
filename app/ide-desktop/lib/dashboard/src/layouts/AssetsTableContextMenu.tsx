/** @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'

import Category from '#/layouts/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenus from '#/components/ContextMenus'

import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import * as backendModule from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as permissions from '#/utilities/permissions'
import * as uniqueString from '#/utilities/uniqueString'

// =================
// === Constants ===
// =================

/** Props for an {@link AssetsTableContextMenu}. */
export interface AssetsTableContextMenuProps {
  readonly hidden?: boolean
  readonly isCloud: boolean
  readonly category: Category
  readonly hasPasteData: boolean
  readonly selectedKeys: ReadonlySet<backendModule.AssetId>
  readonly clearSelectedKeys: () => void
  readonly nodeMapRef: React.MutableRefObject<ReadonlyMap<backendModule.AssetId, AssetTreeNode>>
  readonly event: Pick<React.MouseEvent<Element, MouseEvent>, 'pageX' | 'pageY'>
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  readonly dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (newParentKey: backendModule.AssetId) => void
}

/** A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
export default function AssetsTableContextMenu(props: AssetsTableContextMenuProps) {
  const { category, isCloud, hasPasteData, selectedKeys, clearSelectedKeys, nodeMapRef } = props
  const { event, dispatchAssetEvent, dispatchAssetListEvent, hidden = false } = props
  const { doCopy, doCut, doPaste } = props
  const { user } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const rootDirectory = React.useMemo(() => user?.rootDirectory(), [user])
  const { getText } = textProvider.useText()

  // This works because all items are mutated, ensuring their value stays
  // up to date.
  const ownsAllSelectedAssets =
    !isCloud ||
    (user != null &&
      Array.from(selectedKeys, key => {
        const userPermissions = nodeMapRef.current.get(key)?.item.value.permissions
        const selfPermission = userPermissions?.find(
          permission => permission.user.user_email === user.value.email
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
      setModal(
        <ConfirmDeleteModal
          actionText={
            selectedKeys.size === 1
              ? getText('deleteSelectedAssetActionText')
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
        <ContextMenu hidden={hidden}>
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
                setModal(
                  <ConfirmDeleteModal
                    actionText={
                      selectedKeys.size === 1
                        ? getText('deleteSelectedAssetForeverActionText')
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
  } else if (category !== Category.home) {
    return null
  } else {
    return (
      <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
        {selectedKeys.size !== 0 && (
          <ContextMenu hidden={hidden}>
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
            {isCloud && ownsAllSelectedAssets && (
              <ContextMenuEntry
                hidden={hidden}
                action="cut"
                label={getText('cutAllShortcut')}
                doAction={doCut}
              />
            )}
            {hasPasteData && (
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
                  if (selectedNode?.item.type === backendModule.AssetType.directory) {
                    doPaste(selectedNode.key)
                  } else if (rootDirectory != null) {
                    doPaste(rootDirectory.value.id)
                  }
                }}
              />
            )}
          </ContextMenu>
        )}
        <GlobalContextMenu
          hidden={hidden}
          isCloud={isCloud}
          hasPasteData={hasPasteData}
          directoryKey={null}
          directory={null}
          dispatchAssetListEvent={dispatchAssetListEvent}
          doPaste={doPaste}
        />
      </ContextMenus>
    )
  }
}
