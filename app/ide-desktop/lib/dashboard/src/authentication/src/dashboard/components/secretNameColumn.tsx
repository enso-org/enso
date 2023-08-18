/** @file The icon and name of a {@link backendModule.SecretAsset}. */
import * as React from 'react'

import SecretIcon from 'enso-assets/secret.svg'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as assetTreeNode from '../assetTreeNode'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as presence from '../presence'
import * as shortcutsModule from '../shortcuts'
import * as shortcutsProvider from '../../providers/shortcuts'

import * as column from '../column'
import EditableSpan from './editableSpan'
import SvgMask from '../../authentication/components/svgMask'

// ==================
// === SecretName ===
// ==================

/** Props for a {@link SecretNameColumn}. */
export interface SecretNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.SecretAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.SecretAsset}.
 * This should never happen. */
export default function SecretNameColumn(props: SecretNameColumnProps) {
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
        throw new Error('`SecretNameColumn` can only display secret assets.')
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
            case assetEventModule.AssetEventType.newProject:
            case assetEventModule.AssetEventType.newFolder:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.closeProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects:
            case assetEventModule.AssetEventType.deleteMultiple:
            case assetEventModule.AssetEventType.downloadSelected:
            case assetEventModule.AssetEventType.removeSelf: {
                // Ignored. These events should all be unrelated to secrets.
                // `deleteMultiple` and `downloadSelected` are handled by `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.newSecret: {
                if (item.key === event.placeholderId) {
                    if (backend.type !== backendModule.BackendType.remote) {
                        toastAndLog('Secrets cannot be created on the local backend')
                    } else {
                        rowState.setPresence(presence.Presence.inserting)
                        try {
                            const createdSecret = await backend.createSecret({
                                parentDirectoryId: asset.parentId,
                                secretName: asset.title,
                                secretValue: event.value,
                            })
                            rowState.setPresence(presence.Presence.present)
                            setAsset({
                                ...asset,
                                id: createdSecret.id,
                            })
                        } catch (error) {
                            dispatchAssetListEvent({
                                type: assetListEventModule.AssetListEventType.delete,
                                key: item.key,
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
            className={`flex text-left items-center whitespace-nowrap rounded-l-full gap-1 px-1.5 py-1 min-w-max ${indent.indentClass(
                item.depth
            )}`}
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
            <SvgMask src={SecretIcon} />
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
