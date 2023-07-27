/** @file The context menu for a {@link backendModule.ProjectAsset}. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'

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
        <ContextMenu>
            <ContextMenuEntry action={shortcuts.KeyboardAction.open} onClick={doOpenForEditing} />
            <ContextMenuEntry action={shortcuts.KeyboardAction.rename} onClick={doRename} />
            <ContextMenuEntry
                disabled={isDeleteDisabled}
                action={shortcuts.KeyboardAction.moveToTrash}
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
            />
        </ContextMenu>
    )
}
