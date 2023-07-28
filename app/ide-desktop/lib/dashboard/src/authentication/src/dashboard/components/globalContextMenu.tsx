/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import * as assetListEventModule from '../events/assetListEvent'
import * as backend from '../backend'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'

import * as assetContextMenu from './assetContextMenu'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps
    extends assetContextMenu.AssetContextMenuProps<backend.AnyAsset> {}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
    const {
        innerProps: {
            item,
            state: { dispatchAssetListEvent },
        },
    } = props
    const { unsetModal } = modalProvider.useSetModal()
    const filesInputRef = React.useRef<HTMLInputElement>(null)
    return (
        <ContextMenu>
            <input
                ref={filesInputRef}
                multiple
                type="file"
                id="context_menu_file_input"
                className="hidden"
                onInput={event => {
                    if (event.currentTarget.files != null) {
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.uploadFiles,
                            parentId: item.parentId,
                            files: event.currentTarget.files,
                        })
                    }
                }}
            ></input>
            <ContextMenuEntry
                action={shortcuts.KeyboardAction.uploadFiles}
                onClick={() => {
                    unsetModal()
                    filesInputRef.current?.click()
                }}
            />
            <ContextMenuEntry
                action={shortcuts.KeyboardAction.newProject}
                onClick={() => {
                    unsetModal()
                    dispatchAssetListEvent({
                        type: assetListEventModule.AssetListEventType.newProject,
                        parentId: item.parentId,
                        templateId: null,
                    })
                }}
            />
            <ContextMenuEntry
                action={shortcuts.KeyboardAction.newFolder}
                onClick={() => {
                    unsetModal()
                    dispatchAssetListEvent({
                        type: assetListEventModule.AssetListEventType.newFolder,
                        parentId: item.parentId,
                    })
                }}
            />
            <ContextMenuEntry
                disabled
                action={shortcuts.KeyboardAction.newDataConnector}
                onClick={() => {
                    // No backend support yet.
                }}
            />
        </ContextMenu>
    )
}
