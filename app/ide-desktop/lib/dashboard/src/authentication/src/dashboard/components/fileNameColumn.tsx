/** @file The icon and name of a {@link backendModule.FileAsset}. */
import * as React from 'react'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as eventModule from '../event'
import * as fileInfo from '../../fileInfo'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as presence from '../presence'
import * as shortcuts from '../shortcuts'

import * as column from '../column'
import EditableSpan from './editableSpan'

// ================
// === FileName ===
// ================

/** Props for a {@link FileNameColumn}. */
export interface FileNameColumnProps extends column.AssetColumnProps<backendModule.FileAsset> {}

/** The icon and name of a {@link backendModule.FileAsset}. */
export default function FileNameColumn(props: FileNameColumnProps) {
    const {
        keyProp: key,
        item,
        setItem,
        selected,
        state: { assetEvents, dispatchAssetListEvent, getDepth },
        rowState,
        setRowState,
    } = props
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()

    // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
    // context menu entry should be re-added.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async () => {
        return await Promise.resolve(null)
    }

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.createProject:
            case assetEventModule.AssetEventType.createDirectory:
            case assetEventModule.AssetEventType.createSecret:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects:
            case assetEventModule.AssetEventType.deleteMultiple:
            case assetEventModule.AssetEventType.downloadSelected: {
                // Ignored. These events should all be unrelated to projects.
                // `deleteMultiple` and `downloadSelected` are handled by `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.uploadFiles: {
                const file = event.files.get(key)
                if (file != null) {
                    rowState.setPresence(presence.Presence.inserting)
                    try {
                        const createdFile = await backend.uploadFile(
                            {
                                fileId: null,
                                fileName: item.title,
                                parentDirectoryId: item.parentId,
                            },
                            file
                        )
                        rowState.setPresence(presence.Presence.present)
                        const newItem: backendModule.FileAsset = {
                            ...item,
                            ...createdFile,
                        }
                        setItem(newItem)
                    } catch (error) {
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.delete,
                            id: key,
                        })
                        toastAndLog('Could not upload file', error)
                    }
                }
                break
            }
        }
    })

    return (
        <div
            className={`flex text-left items-center align-middle whitespace-nowrap ${indent.indentClass(
                getDepth(key)
            )}`}
            onClick={event => {
                if (
                    eventModule.isSingleClick(event) &&
                    (selected ||
                        shortcuts.SHORTCUT_REGISTRY.matchesMouseAction(
                            shortcuts.MouseAction.editName,
                            event
                        ))
                ) {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: true,
                    }))
                }
            }}
        >
            <img src={fileInfo.fileIcon()} />
            <EditableSpan
                editable={false}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                    if (newTitle !== item.title) {
                        const oldTitle = item.title
                        setItem(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(/* newTitle */)
                        } catch {
                            setItem(oldItem => ({ ...oldItem, title: oldTitle }))
                        }
                    }
                }}
                onCancel={() => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                }}
                className="bg-transparent grow px-2"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}
