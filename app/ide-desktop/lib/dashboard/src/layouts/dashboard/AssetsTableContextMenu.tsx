/** @file A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'

import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import GlobalContextMenu from '#/layouts/dashboard/GlobalContextMenu'

import ContextMenu from '#/components/ContextMenu'
import ContextMenus from '#/components/ContextMenus'
import ConfirmDeleteModal from '#/components/dashboard/ConfirmDeleteModal'
import MenuEntry from '#/components/MenuEntry'

import * as backendModule from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'
import type * as pasteDataModule from '#/utilities/pasteData'
import * as permissions from '#/utilities/permissions'
import * as shortcutManager from '#/utilities/ShortcutManager'
import * as string from '#/utilities/string'
import * as uniqueString from '#/utilities/uniqueString'

// =================
// === Constants ===
// =================

/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'item'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'items'
// This is a function, even though does not look like one.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)

/** Props for an {@link AssetsTableContextMenu}. */
export interface AssetsTableContextMenuProps {
  hidden?: boolean
  category: Category
  pasteData: pasteDataModule.PasteData<Set<backendModule.AssetId>> | null
  selectedKeys: Set<backendModule.AssetId>
  setSelectedKeys: (items: Set<backendModule.AssetId>) => void
  nodeMapRef: React.MutableRefObject<ReadonlyMap<backendModule.AssetId, AssetTreeNode>>
  event: Pick<React.MouseEvent<Element, MouseEvent>, 'pageX' | 'pageY'>
  dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
  doCopy: () => void
  doCut: () => void
  doPaste: (newParentKey: backendModule.AssetId, newParentId: backendModule.DirectoryId) => void
}

/** A context menu for an `AssetsTable`, when no row is selected, or multiple rows
 * are selected. */
export default function AssetsTableContextMenu(props: AssetsTableContextMenuProps) {
  const { category, pasteData, selectedKeys, setSelectedKeys, nodeMapRef, event } = props
  const { dispatchAssetEvent, dispatchAssetListEvent, hidden = false } = props
  const { doCopy, doCut, doPaste } = props
  const { backend } = backendProvider.useBackend()
  const { organization } = authProvider.useNonPartialUserSession()
  const { setModal, unsetModal } = modalProvider.useSetModal()
  const rootDirectoryId = React.useMemo(
    () => organization?.rootDirectoryId ?? backendModule.DirectoryId(''),
    [organization]
  )
  const isCloud = backend.type === backendModule.BackendType.remote

  const pluralized = pluralize(selectedKeys.size)
  // This works because all items are mutated, ensuring their value stays
  // up to date.
  const ownsAllSelectedAssets =
    !isCloud ||
    (organization != null &&
      Array.from(selectedKeys, key => {
        const userPermissions = nodeMapRef.current.get(key)?.item.permissions
        const selfPermission = userPermissions?.find(
          permission => permission.user.user_email === organization.email
        )
        return selfPermission?.permission === permissions.PermissionAction.own
      }).every(isOwner => isOwner))
  // This is not a React component even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const doDeleteAll = () => {
    if (isCloud) {
      unsetModal()
      dispatchAssetEvent({
        type: AssetEventType.delete,
        ids: selectedKeys,
      })
    } else {
      setModal(
        <ConfirmDeleteModal
          description={`${selectedKeys.size} selected ${pluralized}`}
          doDelete={() => {
            setSelectedKeys(new Set())
            dispatchAssetEvent({
              type: AssetEventType.delete,
              ids: selectedKeys,
            })
          }}
        />
      )
    }
  }
  // This is not a React component even though it contains JSX.
  // eslint-disable-next-line no-restricted-syntax
  const doRestoreAll = () => {
    unsetModal()
    dispatchAssetEvent({
      type: AssetEventType.restore,
      ids: selectedKeys,
    })
  }
  if (category === Category.trash) {
    return selectedKeys.size === 0 ? (
      <></>
    ) : (
      <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
        <ContextMenu hidden={hidden}>
          <MenuEntry
            hidden={hidden}
            action={shortcutManager.KeyboardAction.restoreAllFromTrash}
            doAction={doRestoreAll}
          />
        </ContextMenu>
      </ContextMenus>
    )
  } else if (category !== Category.home) {
    return null
  } else {
    const deleteAction = isCloud
      ? shortcutManager.KeyboardAction.moveAllToTrash
      : shortcutManager.KeyboardAction.deleteAll
    return (
      <ContextMenus key={uniqueString.uniqueString()} hidden={hidden} event={event}>
        {selectedKeys.size !== 0 && (
          <ContextMenu hidden={hidden}>
            {ownsAllSelectedAssets && (
              <MenuEntry hidden={hidden} action={deleteAction} doAction={doDeleteAll} />
            )}
            {isCloud && (
              <MenuEntry
                hidden={hidden}
                action={shortcutManager.KeyboardAction.copyAll}
                doAction={doCopy}
              />
            )}
            {isCloud && ownsAllSelectedAssets && (
              <MenuEntry
                hidden={hidden}
                action={shortcutManager.KeyboardAction.cutAll}
                doAction={doCut}
              />
            )}
            {pasteData != null && pasteData.data.size > 0 && (
              <MenuEntry
                hidden={hidden}
                action={shortcutManager.KeyboardAction.pasteAll}
                doAction={() => {
                  const [firstKey] = selectedKeys
                  const selectedNode =
                    selectedKeys.size === 1 && firstKey != null
                      ? nodeMapRef.current.get(firstKey)
                      : null
                  if (selectedNode?.item.type === backendModule.AssetType.directory) {
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
          hasCopyData={pasteData != null}
          directoryKey={null}
          directoryId={null}
          dispatchAssetListEvent={dispatchAssetListEvent}
          doPaste={doPaste}
        />
      </ContextMenus>
    )
  }
}
