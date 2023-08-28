/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'
import * as toast from 'react-toastify'

import * as assetEventModule from '../events/assetEvent'
import * as assetTreeNode from '../assetTreeNode'
import * as backendModule from '../backend'
import * as hooks from '../../hooks'
import * as http from '../../http'
import * as remoteBackendModule from '../remoteBackend'
import * as shortcuts from '../shortcuts'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import * as assetsTable from './assetsTable'
import * as tableRow from './tableRow'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuSeparator from './contextMenuSeparator'
import ContextMenus from './contextMenus'
import GlobalContextMenu from './globalContextMenu'
import ManagePermissionsModal from './managePermissionsModal'
import MenuEntry from './menuEntry'

// ========================
// === AssetContextMenu ===
// ========================

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps {
    hidden?: boolean
    innerProps: tableRow.TableRowInnerProps<
        assetTreeNode.AssetTreeNode,
        assetsTable.AssetsTableState,
        assetsTable.AssetRowState
    >
    event: Pick<React.MouseEvent, 'pageX' | 'pageY'>
    eventTarget: HTMLElement | null
    doDelete: () => Promise<void>
}

/** The context menu for an arbitrary {@link backendModule.Asset}. */
export default function AssetContextMenu(props: AssetContextMenuProps) {
    const {
        hidden = false,
        innerProps: {
            item,
            setItem,
            state: { dispatchAssetEvent, dispatchAssetListEvent },
            setRowState,
        },
        event,
        eventTarget,
        doDelete,
    } = props
    const logger = loggerProvider.useLogger()
    const { organization, accessToken } = authProvider.useNonPartialUserSession()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const asset = item.item
    const self = asset.permissions?.find(
        permission => permission.user.user_email === organization?.email
    )
    const managesThisAsset =
        self?.permission === backendModule.PermissionAction.own ||
        self?.permission === backendModule.PermissionAction.admin
    const canExecute =
        self?.permission != null && backendModule.PERMISSION_ACTION_CAN_EXECUTE[self.permission]
    const isOtherUserUsingProject =
        backendModule.assetIsProject(asset) &&
        organization != null &&
        asset.projectState.opened_by !== organization.email
    const setAsset = React.useCallback(
        (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
            if (typeof valueOrUpdater === 'function') {
                setItem(oldItem => ({
                    ...oldItem,
                    item: valueOrUpdater(oldItem.item),
                }))
            } else {
                setItem(oldItem => ({ ...oldItem, item: valueOrUpdater }))
            }
        },
        [/* should never change */ setItem]
    )
    return (
        <ContextMenus hidden={hidden} key={asset.id} event={event}>
            <ContextMenu hidden={hidden}>
                {asset.type === backendModule.AssetType.project &&
                    canExecute &&
                    !isOtherUserUsingProject && (
                        <MenuEntry
                            hidden={hidden}
                            action={shortcuts.KeyboardAction.open}
                            doAction={() => {
                                unsetModal()
                                dispatchAssetEvent({
                                    type: assetEventModule.AssetEventType.openProject,
                                    id: asset.id,
                                    shouldAutomaticallySwitchPage: true,
                                })
                            }}
                        />
                    )}
                {asset.type === backendModule.AssetType.project &&
                    backend.type === backendModule.BackendType.local && (
                        <MenuEntry
                            hidden={hidden}
                            action={shortcuts.KeyboardAction.uploadToCloud}
                            doAction={async () => {
                                unsetModal()
                                if (accessToken == null) {
                                    toastAndLog('Cannot upload to cloud in offline mode')
                                } else {
                                    try {
                                        const headers = new Headers([
                                            ['Authorization', `Bearer ${accessToken}`],
                                        ])
                                        const client = new http.Client(headers)
                                        const remoteBackend = new remoteBackendModule.RemoteBackend(
                                            client,
                                            logger
                                        )
                                        const projectResponse = await fetch(
                                            `./api/project-manager/projects/${asset.id}/enso-project`
                                        )
                                        // This DOES NOT update the cloud assets list when it
                                        // completes, as the current backend is not the remote
                                        // (cloud) backend. The user may change to the cloud backend
                                        // while this request is in progress, however this is
                                        // uncommon enough that it is not worth the added complexity.
                                        await remoteBackend.uploadFile(
                                            {
                                                fileName: `${asset.title}.enso-project`,
                                                fileId: null,
                                                parentDirectoryId: null,
                                            },
                                            await projectResponse.blob()
                                        )
                                        toast.toast.success(
                                            'Successfully uploaded local project to cloud!'
                                        )
                                    } catch (error) {
                                        toastAndLog(
                                            'Could not upload local project to cloud',
                                            error
                                        )
                                    }
                                }
                            }}
                        />
                    )}
                {canExecute && !isOtherUserUsingProject && (
                    <MenuEntry
                        hidden={hidden}
                        disabled={
                            asset.type !== backendModule.AssetType.project &&
                            asset.type !== backendModule.AssetType.directory
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
                )}
                <MenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.snapshot}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                {managesThisAsset && !isOtherUserUsingProject && (
                    <MenuEntry
                        hidden={hidden}
                        action={shortcuts.KeyboardAction.moveToTrash}
                        doAction={() => {
                            setModal(
                                <ConfirmDeleteModal
                                    description={`the ${asset.type} '${asset.title}'`}
                                    doDelete={doDelete}
                                />
                            )
                        }}
                    />
                )}
                <ContextMenuSeparator hidden={hidden} />
                {managesThisAsset && (
                    <MenuEntry
                        hidden={hidden}
                        action={shortcuts.KeyboardAction.share}
                        doAction={() => {
                            setModal(
                                <ManagePermissionsModal
                                    item={asset}
                                    setItem={setAsset}
                                    self={self}
                                    eventTarget={eventTarget}
                                    doRemoveSelf={() => {
                                        dispatchAssetEvent({
                                            type: assetEventModule.AssetEventType.removeSelf,
                                            id: asset.id,
                                        })
                                    }}
                                />
                            )
                        }}
                    />
                )}
                <MenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.label}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuSeparator hidden={hidden} />
                <MenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.duplicate}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <MenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.copy}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                {!isOtherUserUsingProject && (
                    <MenuEntry
                        hidden={hidden}
                        disabled
                        action={shortcuts.KeyboardAction.cut}
                        doAction={() => {
                            // No backend support yet.
                        }}
                    />
                )}
                <MenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.download}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
            </ContextMenu>
            {asset.type === backendModule.AssetType.directory ? (
                <GlobalContextMenu
                    hidden={hidden}
                    // This is SAFE, as this only exists when the item is a directory.
                    // eslint-disable-next-line no-restricted-syntax
                    directoryKey={item.key as backendModule.DirectoryId}
                    directoryId={asset.id}
                    dispatchAssetListEvent={dispatchAssetListEvent}
                />
            ) : null}
        </ContextMenus>
    )
}
