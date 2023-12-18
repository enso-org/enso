/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'

import ContextMenu from './contextMenu'
import MenuEntry from './menuEntry'
import NewDataConnectorModal from './newDataConnectorModal'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
    hidden?: boolean
    hasCopyData: boolean
    directoryKey: backendModule.DirectoryId | null
    directoryId: backendModule.DirectoryId | null
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    doPaste: (
        newParentKey: backendModule.AssetId | null,
        newParentId: backendModule.DirectoryId | null
    ) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
    const {
        hidden = false,
        hasCopyData,
        directoryKey,
        directoryId,
        dispatchAssetListEvent,
        doPaste,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const filesInputRef = React.useRef<HTMLInputElement>(null)
    const isCloud = backend.type === backendModule.BackendType.remote
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
            <MenuEntry
                hidden={hidden}
                action={
                    backend.type === backendModule.BackendType.local
                        ? shortcuts.KeyboardAction.uploadProjects
                        : shortcuts.KeyboardAction.uploadFiles
                }
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
            {isCloud && (
                <MenuEntry
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
            )}
            {isCloud && (
                <MenuEntry
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
            {isCloud && (
                <MenuEntry
                    hidden={hidden}
                    action={shortcuts.KeyboardAction.newDataConnector}
                    doAction={() => {
                        setModal(
                            <NewDataConnectorModal
                                doCreate={(name, value) => {
                                    dispatchAssetListEvent({
                                        type: assetListEventModule.AssetListEventType
                                            .newDataConnector,
                                        parentKey: directoryKey,
                                        parentId: directoryId,
                                        name,
                                        value,
                                    })
                                }}
                            />
                        )
                    }}
                />
            )}
            {isCloud && directoryKey == null && hasCopyData && (
                <MenuEntry
                    hidden={hidden}
                    action={shortcuts.KeyboardAction.paste}
                    doAction={() => {
                        unsetModal()
                        doPaste(null, null)
                    }}
                />
            )}
        </ContextMenu>
    )
}
