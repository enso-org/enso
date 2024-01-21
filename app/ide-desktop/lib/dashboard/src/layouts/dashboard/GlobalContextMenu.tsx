/** @file A context menu available everywhere in the directory. */
import * as React from 'react'

import type * as assetListEventModule from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'
import UpsertSecretModal from '#/layouts/dashboard/UpsertSecretModal'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/backend'
import * as shortcuts from '#/utilities/shortcuts'

import ContextMenu from '#/components/ContextMenu'
import MenuEntry from '#/components/MenuEntry'

/** Props for a {@link GlobalContextMenu}. */
export interface GlobalContextMenuProps {
    hidden?: boolean
    hasCopyData: boolean
    directoryKey: backendModule.DirectoryId | null
    directoryId: backendModule.DirectoryId | null
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    doPaste: (newParentKey: backendModule.AssetId, newParentId: backendModule.DirectoryId) => void
}

/** A context menu available everywhere in the directory. */
export default function GlobalContextMenu(props: GlobalContextMenuProps) {
    const { hidden = false, hasCopyData, directoryKey, directoryId, dispatchAssetListEvent } = props
    const { doPaste } = props
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const rootDirectoryId = React.useMemo(
        () => organization?.rootDirectoryId ?? backendModule.DirectoryId(''),
        [organization]
    )
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
                                type: AssetListEventType.uploadFiles,
                                parentKey: directoryKey ?? rootDirectoryId,
                                parentId: directoryId ?? rootDirectoryId,
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
                                    type: AssetListEventType.uploadFiles,
                                    parentKey: directoryKey ?? rootDirectoryId,
                                    parentId: directoryId ?? rootDirectoryId,
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
                            type: AssetListEventType.newProject,
                            parentKey: directoryKey ?? rootDirectoryId,
                            parentId: directoryId ?? rootDirectoryId,
                            templateId: null,
                            templateName: null,
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
                            type: AssetListEventType.newFolder,
                            parentKey: directoryKey ?? rootDirectoryId,
                            parentId: directoryId ?? rootDirectoryId,
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
                            <UpsertSecretModal
                                id={null}
                                name={null}
                                doCreate={(name, value) => {
                                    dispatchAssetListEvent({
                                        type: AssetListEventType.newDataConnector,
                                        parentKey: directoryKey ?? rootDirectoryId,
                                        parentId: directoryId ?? rootDirectoryId,
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
                        doPaste(rootDirectoryId, rootDirectoryId)
                    }}
                />
            )}
        </ContextMenu>
    )
}
