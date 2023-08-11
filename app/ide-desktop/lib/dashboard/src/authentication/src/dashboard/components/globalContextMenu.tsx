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
    hidden?: boolean
    directoryKey: backendModule.DirectoryId | null
    directoryId: backendModule.DirectoryId | null
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
    const { hidden = false, directoryKey, directoryId, dispatchAssetListEvent } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const filesInputRef = React.useRef<HTMLInputElement>(null)
    return (
        <ContextMenu hidden={hidden}>
            {!hidden && (
                <input
                    ref={filesInputRef}
                    multiple
                    type="file"
                    id="context_menu_file_input"
                    {...(backend.type !== backendModule.BackendType.local
                        ? {}
                        : { accept: '.enso-project' })}
                    className="hidden"
                    onInput={event => {
                        if (event.currentTarget.files != null) {
                            dispatchAssetListEvent({
                                type: assetListEventModule.AssetListEventType.uploadFiles,
                                parentKey: directoryKey,
                                parentId: directoryId,
                                files: Array.from(event.currentTarget.files),
                            })
                            unsetModal()
                        }
                    }}
                ></input>
            )}
            <ContextMenuEntry
                hidden={hidden}
                action={shortcuts.KeyboardAction.uploadFiles}
                doAction={() => {
                    if (filesInputRef.current?.isConnected === true) {
                        filesInputRef.current.click()
                    } else {
                        const input = document.createElement('input')
                        input.type = 'file'
                        input.style.display = 'none'
                        document.body.appendChild(input)
                        input.addEventListener('input', () => {
                            if (input.files != null) {
                                dispatchAssetListEvent({
                                    type: assetListEventModule.AssetListEventType.uploadFiles,
                                    parentKey: directoryKey,
                                    parentId: directoryId,
                                    files: Array.from(input.files),
                                })
                                unsetModal()
                            }
                        })
                        input.click()
                        input.remove()
                    }
                }}
            />
            <ContextMenuEntry
                hidden={hidden}
                action={shortcuts.KeyboardAction.newProject}
                doAction={() => {
                    unsetModal()
                    dispatchAssetListEvent({
                        type: assetListEventModule.AssetListEventType.newProject,
                        parentKey: directoryKey,
                        parentId: directoryId,
                        templateId: null,
                        onSpinnerStateChange: null,
                    })
                }}
            />
            {backend.type !== backendModule.BackendType.local && (
                <ContextMenuEntry
                    hidden={hidden}
                    action={shortcuts.KeyboardAction.newFolder}
                    doAction={() => {
                        unsetModal()
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.newFolder,
                            parentKey: directoryKey,
                            parentId: directoryId,
                        })
                    }}
                />
            )}
            {backend.type !== backendModule.BackendType.local && (
                <ContextMenuEntry
                    hidden={hidden}
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
