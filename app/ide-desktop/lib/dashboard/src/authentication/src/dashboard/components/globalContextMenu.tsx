/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'

import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
    directoryId: backendModule.DirectoryId
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
    const { directoryId, dispatchAssetListEvent } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const filesInputRef = React.useRef<HTMLInputElement>(null)
    return (
        <ContextMenu>
            {backend.type !== backendModule.BackendType.local && (
                <>
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
                                    parentId: directoryId,
                                    files: Array.from(event.currentTarget.files),
                                })
                                unsetModal()
                            }
                        }}
                    ></input>
                    <ContextMenuEntry
                        action={shortcuts.KeyboardAction.uploadFiles}
                        doAction={() => {
                            filesInputRef.current?.click()
                        }}
                    />
                </>
            )}
            <ContextMenuEntry
                action={shortcuts.KeyboardAction.newProject}
                doAction={() => {
                    unsetModal()
                    dispatchAssetListEvent({
                        type: assetListEventModule.AssetListEventType.newProject,
                        parentId: directoryId,
                        templateId: null,
                        onSpinnerStateChange: null,
                    })
                }}
            />
            {backend.type !== backendModule.BackendType.local && (
                <ContextMenuEntry
                    action={shortcuts.KeyboardAction.newFolder}
                    doAction={() => {
                        unsetModal()
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.newFolder,
                            parentId: directoryId,
                        })
                    }}
                />
            )}
            {backend.type !== backendModule.BackendType.local && (
                <ContextMenuEntry
                    disabled
                    action={shortcuts.KeyboardAction.newDataConnector}
                    doAction={() => {
                        // No backend support yet.
                    }}
                />
            )}
        </ContextMenu>
    )
}
