/** @file The icon and name of a {@link backendModule.DirectoryAsset}. */
import * as React from 'react'

import DirectoryIcon from 'enso-assets/directory.svg'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as column from '../column'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as presence from '../presence'
import * as shortcuts from '../shortcuts'

import * as backendProvider from '../../providers/backend'

import EditableSpan from './editableSpan'

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryNameColumn}. */
export interface DirectoryNameColumnProps
    extends column.AssetColumnProps<backendModule.DirectoryAsset> {}

/** The icon and name of a {@link backendModule.DirectoryAsset}. */
export default function DirectoryNameColumn(props: DirectoryNameColumnProps) {
    const {
        keyProp: key,
        item,
        setItem,
        selected,
        setSelected,
        state: { assetEvents, dispatchAssetListEvent, doToggleDirectoryExpansion, getDepth },
        rowState,
        setRowState,
    } = props
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()

    const doRename = async (newName: string) => {
        if (backend.type !== backendModule.BackendType.local) {
            try {
                await backend.updateDirectory(item.id, { title: newName }, item.title)
                return
            } catch (error) {
                toastAndLog('Error renaming folder', error)
                throw error
            }
        }
    }

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.createProject:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.createSecret:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects:
            case assetEventModule.AssetEventType.deleteMultiple: {
                // Ignored. These events should all be unrelated to directories.
                break
            }
            case assetEventModule.AssetEventType.createDirectory: {
                if (key === event.placeholderId) {
                    if (backend.type !== backendModule.BackendType.remote) {
                        toastAndLog('Folders cannot be created on the local backend')
                    } else {
                        rowState.setPresence(presence.Presence.inserting)
                        try {
                            const createdDirectory = await backend.createDirectory({
                                parentId: item.parentId,
                                title: item.title,
                            })
                            rowState.setPresence(presence.Presence.present)
                            const newItem: backendModule.DirectoryAsset = {
                                ...item,
                                ...createdDirectory,
                            }
                            setItem(newItem)
                        } catch (error) {
                            dispatchAssetListEvent({
                                type: assetListEventModule.AssetListEventType.delete,
                                id: key,
                            })
                            toastAndLog('Error creating new folder', error)
                        }
                    }
                }
                break
            }
        }
    })

    return (
        <div
            className={`flex text-left items-center whitespace-nowrap ${indent.indentClass(
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
                } else if (eventModule.isDoubleClick(event)) {
                    if (!rowState.isEditingName) {
                        // This must be processed on the next tick, otherwise it will be overridden
                        // by the default click handler.
                        setTimeout(() => {
                            setSelected(false)
                        }, 0)
                        doToggleDirectoryExpansion(item, key)
                    }
                }
            }}
        >
            <img src={DirectoryIcon} />
            <EditableSpan
                editable={rowState.isEditingName}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                    if (newTitle !== item.title) {
                        const oldTitle = item.title
                        setItem(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(newTitle)
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
                className="cursor-pointer bg-transparent grow px-2"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}
