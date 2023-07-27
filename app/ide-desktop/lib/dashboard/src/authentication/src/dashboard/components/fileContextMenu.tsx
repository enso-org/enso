/** @file The context menu for a {@link backendModule.FileAsset}. */
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

const ASSET_TYPE_NAME = 'file'

// =======================
// === FileContextMenu ===
// =======================

/** Props for a {@link FileContextMenu}. */
export interface FileContextMenuProps
    extends assetContextMenu.AssetContextMenuProps<backendModule.FileAsset> {}

/** The context menu for a {@link backendModule.FileAsset}. */
export default function FileContextMenu(props: FileContextMenuProps) {
    const {
        innerProps: { item },
        doDelete,
    } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <ContextMenu>
            {/*<ContextMenuEntry disabled onClick={doCopy}>
                                Copy
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doCut}>
                                Cut
                            </ContextMenuEntry>*/}
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
            <ContextMenuEntry
                disabled
                action={shortcuts.KeyboardAction.download}
                onClick={() => {
                    // No backend support yet.
                }}
            />
            {/*<ContextMenuEntry disabled onClick={doDownload}>
                                Download
                            </ContextMenuEntry>*/}
        </ContextMenu>
    )
}
