/** @file The icon and name of a {@link backendModule.FileAsset}. */
import * as React from 'react'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'
import * as hooks from '#/hooks'
import * as backendProvider from '#/providers/BackendProvider'
import * as shortcutsProvider from '#/providers/ShortcutsProvider'
import * as backendModule from '#/services/backend'
import * as assetTreeNode from '#/utilities/assetTreeNode'
import * as eventModule from '#/utilities/event'
import * as fileIcon from '#/utilities/fileIcon'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as shortcutsModule from '#/utilities/shortcuts'
import Visibility from '#/utilities/visibility'

import type * as column from '#/components/dashboard/column'
import EditableSpan from '#/components/EditableSpan'
import SvgMask from '#/components/SvgMask'

// ================
// === FileName ===
// ================

/** Props for a {@link FileNameColumn}. */
export interface FileNameColumnProps extends column.AssetColumnProps {}

/** The icon and name of a {@link backendModule.FileAsset}.
 * @throws {Error} when the asset is not a {@link backendModule.FileAsset}.
 * This should never happen. */
export default function FileNameColumn(props: FileNameColumnProps) {
    const { item, setItem, selected, state, rowState, setRowState } = props
    const { assetEvents, dispatchAssetListEvent } = state
    const toastAndLog = hooks.useToastAndLog()
    const { backend } = backendProvider.useBackend()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const asset = item.item
    if (asset.type !== backendModule.AssetType.file) {
        // eslint-disable-next-line no-restricted-syntax
        throw new Error('`FileNameColumn` can only display file assets.')
    }
    const setAsset = assetTreeNode.useSetAsset(asset, setItem)

    // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
    // context menu entry should be re-added.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async () => {
        return await Promise.resolve(null)
    }

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            case AssetEventType.newProject:
            case AssetEventType.newFolder:
            case AssetEventType.newDataConnector:
            case AssetEventType.openProject:
            case AssetEventType.closeProject:
            case AssetEventType.cancelOpeningAllProjects:
            case AssetEventType.copy:
            case AssetEventType.cut:
            case AssetEventType.cancelCut:
            case AssetEventType.move:
            case AssetEventType.delete:
            case AssetEventType.restore:
            case AssetEventType.download:
            case AssetEventType.downloadSelected:
            case AssetEventType.removeSelf:
            case AssetEventType.temporarilyAddLabels:
            case AssetEventType.temporarilyRemoveLabels:
            case AssetEventType.addLabels:
            case AssetEventType.removeLabels:
            case AssetEventType.deleteLabel: {
                // Ignored. These events should all be unrelated to projects.
                // `deleteMultiple`, `restoreMultiple`, `download`, and `downloadSelected`
                // are handled by `AssetRow`.
                break
            }
            case AssetEventType.uploadFiles: {
                const file = event.files.get(item.key)
                if (file != null) {
                    rowState.setVisibility(Visibility.faded)
                    try {
                        const createdFile = await backend.uploadFile(
                            {
                                fileId: null,
                                fileName: asset.title,
                                parentDirectoryId: asset.parentId,
                            },
                            file
                        )
                        rowState.setVisibility(Visibility.visible)
                        setAsset(object.merge(asset, { id: createdFile.id }))
                    } catch (error) {
                        dispatchAssetListEvent({
                            type: AssetListEventType.delete,
                            key: item.key,
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
            className={`flex text-left items-center align-middle whitespace-nowrap rounded-l-full gap-1 px-1.5 py-1 min-w-max ${indent.indentClass(
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
                    setRowState(object.merger({ isEditingName: true }))
                }
            }}
        >
            <SvgMask src={fileIcon.fileIcon()} className="m-1" />
            <EditableSpan
                editable={false}
                onSubmit={async newTitle => {
                    setRowState(object.merger({ isEditingName: false }))
                    if (newTitle !== asset.title) {
                        const oldTitle = asset.title
                        setAsset(object.merger({ title: newTitle }))
                        try {
                            await doRename()
                        } catch {
                            setAsset(object.merger({ title: oldTitle }))
                        }
                    }
                }}
                onCancel={() => {
                    setRowState(object.merger({ isEditingName: false }))
                }}
                className="bg-transparent grow leading-170 h-6 py-px"
            >
                {asset.title}
            </EditableSpan>
        </div>
    )
}
