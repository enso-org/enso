/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import SecretIcon from 'enso-assets/secret.svg'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as presence from '../presence'
import * as shortcuts from '../shortcuts'

import * as column from '../column'
import EditableSpan from './editableSpan'

// ==================
// === SecretName ===
// ==================

/** Props for a {@link SecretNameColumn}. */
export interface SecretNameColumnProps extends column.AssetColumnProps<backendModule.SecretAsset> {}

/** The icon and name of a {@link backendModule.SecretAsset}. */
export default function SecretNameColumn(props: SecretNameColumnProps) {
    const {
        keyProp: key,
        item,
        setItem,
        selected,
        state: { assetEvent, dispatchAssetListEvent, getDepth },
        rowState,
        setRowState,
    } = props
    const toastAndLog = hooks.useToastAndLog()
    const { backend } = backendProvider.useBackend()

    // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
    // context menu entry should be re-added.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async (/* _newName: string */) => {
        await Promise.resolve(null)
    }

    hooks.useEventHandler(assetEvent, async event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.createProject:
            case assetEventModule.AssetEventType.createDirectory:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects:
            case assetEventModule.AssetEventType.deleteMultiple: {
                // Ignored. These events should all be unrelated to secrets.
                // `deleteMultiple` is handled in `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.createSecret: {
                if (key === event.placeholderId) {
                    if (backend.type !== backendModule.BackendType.remote) {
                        toastAndLog('Secrets cannot be created on the local backend')
                    } else {
                        rowState.setPresence(presence.Presence.inserting)
                        try {
                            const createdSecret = await backend.createSecret({
                                parentDirectoryId: item.parentId,
                                secretName: item.title,
                                secretValue: event.value,
                            })
                            rowState.setPresence(presence.Presence.present)
                            const newItem: backendModule.SecretAsset = {
                                ...item,
                                ...createdSecret,
                            }
                            setItem(newItem)
                        } catch (error) {
                            dispatchAssetListEvent({
                                type: assetListEventModule.AssetListEventType.delete,
                                id: key,
                            })
                            toastAndLog('Error creating new secret', error)
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
                }
            }}
        >
            <img src={SecretIcon} />{' '}
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
