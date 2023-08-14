/** @file The context menu for an arbitrary {@link backendModule.Asset}. */
import * as React from 'react'
import * as toast from 'react-toastify'

import * as assetEventModule from '../events/assetEvent'
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
import ContextMenuEntry from './contextMenuEntry'
import ContextMenuSeparator from './contextMenuSeparator'
import ContextMenus from './contextMenus'
import GlobalContextMenu from './globalContextMenu'
import ManagePermissionsModal from './managePermissionsModal'

// ========================
// === AssetContextMenu ===
// ========================

/** Props for a {@link AssetContextMenu}. */
export interface AssetContextMenuProps<T extends backendModule.AnyAsset> {
    hidden?: boolean
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
        hidden = false,
        innerProps: {
            key,
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
    const self = item.permissions?.find(
        permission => permission.user.user_email === organization?.email
    )
    const managesThisAsset =
        self?.permission === backendModule.PermissionAction.own ||
        self?.permission === backendModule.PermissionAction.admin
    return (
        <ContextMenus hidden={hidden} key={props.innerProps.item.id} event={event}>
            <ContextMenu hidden={hidden}>
                {item.type === backendModule.AssetType.project && (
                    <ContextMenuEntry
                        hidden={hidden}
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
                {item.type === backendModule.AssetType.project &&
                    backend.type === backendModule.BackendType.local && (
                        <ContextMenuEntry
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
                                            `./api/project-manager/projects/${item.id}/enso-project`
                                        )
                                        // This DOES NOT update the cloud assets list when it
                                        // completes, as the current backend is not the remote
                                        // (cloud) backend. The user may change to the cloud backend
                                        // while this request is in progress, however this is
                                        // uncommon enough that it is not worth the added complexity.
                                        await remoteBackend.uploadFile(
                                            {
                                                fileName: `${item.title}.enso-project`,
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
                <ContextMenuEntry
                    hidden={hidden}
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
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.snapshot}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    hidden={hidden}
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
                <ContextMenuSeparator hidden={hidden} />
                {managesThisAsset && (
                    <ContextMenuEntry
                        hidden={hidden}
                        action={shortcuts.KeyboardAction.share}
                        doAction={() => {
                            setModal(
                                <ManagePermissionsModal
                                    item={item}
                                    setItem={setItem}
                                    self={self}
                                    eventTarget={eventTarget}
                                    doRemoveSelf={() => {
                                        dispatchAssetEvent({
                                            type: assetEventModule.AssetEventType.removeSelf,
                                            id: item.id,
                                        })
                                    }}
                                />
                            )
                        }}
                    />
                )}
                <ContextMenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.label}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuSeparator hidden={hidden} />
                <ContextMenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.duplicate}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.copy}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.cut}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
                <ContextMenuEntry
                    hidden={hidden}
                    disabled
                    action={shortcuts.KeyboardAction.download}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
            </ContextMenu>
            {item.type === backendModule.AssetType.directory ? (
                <GlobalContextMenu
                    hidden={hidden}
                    // This is SAFE, as this only exists when the item is a directory.
                    // eslint-disable-next-line no-restricted-syntax
                    directoryKey={key as backendModule.DirectoryId}
                    directoryId={item.id}
                    dispatchAssetListEvent={dispatchAssetListEvent}
                />
            ) : null}
        </ContextMenus>
    )
}
