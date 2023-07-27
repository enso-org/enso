/** @file The context menu for a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'

import * as backendModule from '../backend'
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
const ASSET_TYPE_NAME = 'folder'

// ============================
// === DirectoryContextMenu ===
// ============================

/** Props for a {@link DirectoryContextMenu}. */
export interface DirectoryContextMenuProps
    extends assetContextMenu.AssetContextMenuProps<backendModule.DirectoryAsset> {}

/** The context menu for a {@link backendModule.DirectoryAsset}. */
export default function DirectoryContextMenu(props: DirectoryContextMenuProps) {
    const {
        innerProps: { item, setRowState },
        doDelete,
    } = props
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const doRename = () => {
        setRowState(oldRowState => ({
            ...oldRowState,
            isEditingName: true,
        }))
        unsetModal()
    }

    return (
        <ContextMenu>
            <ContextMenuEntry action={shortcuts.KeyboardAction.rename} onClick={doRename} />
            <ContextMenuEntry
                action={shortcuts.KeyboardAction.moveToTrash}
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
