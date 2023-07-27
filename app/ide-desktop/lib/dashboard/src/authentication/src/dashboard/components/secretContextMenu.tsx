/** @file The context menu for a {@link backendModule.SecretAsset}. */
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
const ASSET_TYPE_NAME = 'secret'

// =========================
// === SecretContextMenu ===
// =========================

/** Props for a {@link SecretContextMenu}. */
export interface SecretContextMenuProps
    extends assetContextMenu.AssetContextMenuProps<backendModule.SecretAsset> {}

/** The context menu for a {@link backendModule.SecretAsset}. */
export default function SecretContextMenu(props: SecretContextMenuProps) {
    const {
        innerProps: { item },
        doDelete,
    } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <ContextMenu>
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
