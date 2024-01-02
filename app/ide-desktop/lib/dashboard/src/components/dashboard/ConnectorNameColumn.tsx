/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import ConnectorIcon from 'enso-assets/connector.svg'

import * as assetEvent from '#/events/assetEvent'
import * as assetListEvent from '#/events/assetListEvent'
import * as hooks from '#/hooks'
import * as backendProvider from '#/providers/backendProvider'
import * as shortcutsProvider from '#/providers/shortcutsProvider'
import * as backendModule from '#/services/backend'
import * as assetTreeNode from '#/utilities/assetTreeNode'
import * as eventModule from '#/utilities/event'
import * as indent from '#/utilities/indent'
import * as shortcutsModule from '#/utilities/shortcuts'
import * as visibility from '#/utilities/visibility'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'

// =====================
// === ConnectorName ===
// =====================

/** Props for a {@link ConnectorNameColumn}. */
export interface ConnectorNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.SecretAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.SecretAsset}.
 * This should never happen. */
export default function ConnectorNameColumn(props: ConnectorNameColumnProps) {
    const {
        item,
        setItem,
        selected,
        state: { assetEvents, dispatchAssetListEvent },
        rowState,
        setRowState,
    } = props
    const toastAndLog = hooks.useToastAndLog()
    const { backend } = backendProvider.useBackend()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const asset = item.item
    if (asset.type !== backendModule.AssetType.secret) {
        // eslint-disable-next-line no-restricted-syntax
        throw new Error('`ConnectorNameColumn` can only display data connector assets.')
    }
    const setAsset = assetTreeNode.useSetAsset(asset, setItem)

    // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
    // context menu entry should be re-added.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async () => {
        await Promise.resolve(null)
    }

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            case assetEvent.AssetEventType.newProject:
            case assetEvent.AssetEventType.newFolder:
            case assetEvent.AssetEventType.uploadFiles:
            case assetEvent.AssetEventType.openProject:
            case assetEvent.AssetEventType.closeProject:
            case assetEvent.AssetEventType.cancelOpeningAllProjects:
            case assetEvent.AssetEventType.cut:
            case assetEvent.AssetEventType.cancelCut:
            case assetEvent.AssetEventType.move:
            case assetEvent.AssetEventType.delete:
            case assetEvent.AssetEventType.restore:
            case assetEvent.AssetEventType.download:
            case assetEvent.AssetEventType.downloadSelected:
            case assetEvent.AssetEventType.removeSelf:
            case assetEvent.AssetEventType.temporarilyAddLabels:
            case assetEvent.AssetEventType.temporarilyRemoveLabels:
            case assetEvent.AssetEventType.addLabels:
            case assetEvent.AssetEventType.removeLabels:
            case assetEvent.AssetEventType.deleteLabel: {
                // Ignored. These events should all be unrelated to secrets.
                // `deleteMultiple`, `restoreMultiple`, `download`,
                // and `downloadSelected` are handled by `AssetRow`.
                break
            }
            case assetEvent.AssetEventType.newDataConnector: {
                if (item.key === event.placeholderId) {
                    if (backend.type !== backendModule.BackendType.remote) {
                        toastAndLog('Data connectors cannot be created on the local backend')
                    } else {
                        rowState.setVisibility(visibility.Visibility.faded)
                        try {
                            const createdSecret = await backend.createSecret({
                                parentDirectoryId: asset.parentId,
                                secretName: asset.title,
                                secretValue: event.value,
                            })
                            rowState.setVisibility(visibility.Visibility.visible)
                            setAsset({
                                ...asset,
                                id: createdSecret.id,
                            })
                        } catch (error) {
                            dispatchAssetListEvent({
                                type: assetListEvent.AssetListEventType.delete,
                                key: item.key,
                            })
                            toastAndLog('Error creating new data connector', error)
                        }
                    }
                }
                break
            }
        }
    })

    return (
        <div
            className={`flex text-left items-center whitespace-nowrap rounded-l-full gap-1 px-1.5 py-1 min-w-max ${indent.indentClass(
                item.depth
            )}`}
            onKeyDown={event => {
                if (rowState.isEditingName && event.key === 'Enter') {
                    event.stopPropagation()
                }
            }}
            onClick={event => {
                if (
                    eventModule.isSingleClick(event) &&
                    (selected ||
                        shortcuts.matchesMouseAction(shortcutsModule.MouseAction.editName, event))
                ) {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: true,
                    }))
                }
            }}
        >
            <img src={ConnectorIcon} className="m-1" />
            <EditableSpan
                editable={false}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                    if (newTitle !== asset.title) {
                        const oldTitle = asset.title
                        setAsset(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(/* newTitle */)
                        } catch {
                            setAsset(oldItem => ({ ...oldItem, title: oldTitle }))
                        }
                    }
                }}
                onCancel={() => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                }}
                className="bg-transparent grow leading-170 h-6 py-px"
            >
                {asset.title}
            </EditableSpan>
        </div>
    )
}
