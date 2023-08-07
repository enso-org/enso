/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'

import * as assetsTable from './assetsTable'
import * as tableRow from './tableRow'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import ContextMenuSeparator from './contextMenuSeparator'
import ContextMenus from './contextMenus'
import GlobalContextMenu from './globalContextMenu'
import ManagePermissionsModal from './managePermissionsModal'

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps<T extends backendModule.AnyAsset> {
    innerProps: tableRow.TableRowInnerProps<
        T,
        assetsTable.AssetsTableState,
        assetsTable.AssetRowState,
        T['id']
    >
    event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
    eventTarget: HTMLElement | null
    doDelete: () => Promise<void>
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps<backendModule.AnyAsset>) {
    const {
        innerProps: {
            item,
            state: { dispatchAssetEvent, dispatchAssetListEvent },
            setRowState,
        },
        event,
        eventTarget,
        doDelete,
    } = props
    const { setModal, unsetModal } = modalProvider.useSetModal()
    return (
        <ContextMenus key={props.innerProps.item.id} event={event}>
            <ContextMenu>
                {item.type === backendModule.AssetType.project && (
                    <ContextMenuEntry
                        action={shortcuts.KeyboardAction.open}
                        doAction={() => {
                            unsetModal()
                            dispatchAssetEvent({
                                type: assetEventModule.AssetEventType.openProject,
                                id: item.id,
                            })
                        }}
                    />
                )}
                <ContextMenuEntry
                    disabled={
                        item.type !== backendModule.AssetType.project &&
                        item.type !== backendModule.AssetType.directory
                    }
                    action={shortcuts.KeyboardAction.rename}
                    doAction={() => {
                        setRowState(oldRowState => ({
                            ...oldRowState,
                            isEditingName: true,
                        }))
                        unsetModal()
                    }}
                />
                <ContextMenuEntry
                    disabled
                    action={shortcuts.KeyboardAction.snapshot}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    action={shortcuts.KeyboardAction.moveToTrash}
                    doAction={() => {
                        setModal(
                            <ConfirmDeleteModal
                                description={`the ${item.type} '${item.title}'`}
                                doDelete={doDelete}
                            />
                        )
                    }}
                />
                <ContextMenuSeparator />
                <ContextMenuEntry
                    action={shortcuts.KeyboardAction.share}
                    doAction={() => {
                        setModal(
                            <ManagePermissionsModal
                                asset={item}
                                eventTarget={eventTarget}
                                initialPermissions={[]}
                                emailsOfUsersWithPermission={
                                    new Set(
                                        item.permissions?.map(
                                            permission => permission.user.user_email
                                        )
                                    )
                                }
                                onSubmit={() => {
                                    // TODO[sb]: Update the asset's permissions list.
                                }}
                            />
                        )
                    }}
                />
                <ContextMenuEntry
                    disabled
                    action={shortcuts.KeyboardAction.label}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuSeparator />
                <ContextMenuEntry
                    disabled
                    action={shortcuts.KeyboardAction.duplicate}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    disabled
                    action={shortcuts.KeyboardAction.copy}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    disabled
                    action={shortcuts.KeyboardAction.cut}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    disabled
                    action={shortcuts.KeyboardAction.download}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
            </ContextMenu>
            <GlobalContextMenu
                directoryId={item.parentId}
                dispatchAssetListEvent={dispatchAssetListEvent}
            />
        </ContextMenus>
    )
}
