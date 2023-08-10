/** @file The context menu for a {@link backendModule.ProjectAsset}. */
import * as React from 'react'
import * as toast from 'react-toastify'

import * as assetEventModule from '../events/assetEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'
import * as http from '../../http'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as remoteBackendModule from '../remoteBackend'

import * as assetContextMenu from './assetContextMenu'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'

// =================
// === Constants ===
// =================

/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'project'

// ==========================
// === ProjectContextMenu ===
// ==========================

/** Props for a {@link ProjectContextMenu}. */
export interface ProjectContextMenuProps
    extends assetContextMenu.AssetContextMenuProps<backendModule.ProjectAsset> {}

/** The context menu for a {@link backendModule.ProjectAsset}. */
export default function ProjectContextMenu(props: ProjectContextMenuProps) {
    const {
        innerProps: { item, setRowState },
        event,
        dispatchAssetEvent,
        doDelete,
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const { accessToken } = authProvider.useNonPartialUserSession()
    const toastAndLog = hooks.useToastAndLog()

    const doOpenForEditing = () => {
        unsetModal()
        dispatchAssetEvent({
            type: assetEventModule.AssetEventType.openProject,
            id: item.id,
        })
    }
    const doUploadToCloud = async () => {
        unsetModal()
        if (accessToken == null) {
            toastAndLog('Cannot upload to cloud in offline mode')
        } else {
            try {
                const headers = new Headers([['Authorization', `Bearer ${accessToken}`]])
                const client = new http.Client(headers)
                const remoteBackend = new remoteBackendModule.RemoteBackend(client, logger)
                const projectResponse = await fetch(
                    `./api/project-manager/projects/${item.id}/enso-project`
                )
                await remoteBackend.uploadFile(
                    {
                        fileName: `${item.title}.enso-project`,
                        fileId: null,
                        parentDirectoryId: null,
                    },
                    await projectResponse.blob()
                )
                toast.toast.success('Successfully uploaded local project to cloud!')
            } catch (error) {
                toastAndLog('Could not upload local project to cloud', error)
            }
        }
    }
    const doRename = () => {
        setRowState(oldRowState => ({
            ...oldRowState,
            isEditingName: true,
        }))
        unsetModal()
    }
    return (
        <ContextMenu key={item.id} event={event}>
            <ContextMenuEntry onClick={doOpenForEditing}>Open for editing</ContextMenuEntry>
            {backend.type === backendModule.BackendType.local && (
                <ContextMenuEntry onClick={doUploadToCloud}>Upload to cloud</ContextMenuEntry>
            )}
            <ContextMenuEntry onClick={doRename}>Rename</ContextMenuEntry>
            <ContextMenuEntry
                onClick={() => {
                    setModal(
                        <ConfirmDeleteModal
                            description={`the ${ASSET_TYPE_NAME} '${item.title}'`}
                            doDelete={doDelete}
                        />
                    )
                }}
            >
                <span className="text-red-700">Delete</span>
            </ContextMenuEntry>
        </ContextMenu>
    )
}
