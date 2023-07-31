/** @file The context menu for a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'

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
        innerProps: { item, rowState, setRowState },
        event,
        dispatchAssetEvent,
        doDelete,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const isDeleteDisabled = backend.type === backendModule.BackendType.local && rowState.isRunning
    const doOpenForEditing = () => {
        unsetModal()
        dispatchAssetEvent({
            type: assetEventModule.AssetEventType.openProject,
            id: item.id,
        })
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
            <ContextMenuEntry onClick={doRename}>Rename</ContextMenuEntry>
            <ContextMenuEntry
                disabled={isDeleteDisabled}
                {...(isDeleteDisabled
                    ? {
                          title: 'A running local project cannot be removed.',
                      }
                    : {})}
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
