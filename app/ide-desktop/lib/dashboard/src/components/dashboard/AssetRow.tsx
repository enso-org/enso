/** @file A table row for an arbitrary asset. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'
import * as eventHooks from '#/hooks/eventHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import AssetContextMenu from '#/layouts/dashboard/AssetContextMenu'
import type * as assetsTable from '#/layouts/dashboard/AssetsTable'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/backend'
import * as assetTreeNode from '#/utilities/assetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as download from '#/utilities/download'
import * as drag from '#/utilities/drag'
import * as errorModule from '#/utilities/error'
import * as indent from '#/utilities/indent'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as set from '#/utilities/set'
import Visibility, * as visibilityModule from '#/utilities/visibility'

import type * as column from '#/components/dashboard/column'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

// =================
// === Constants ===
// =================

/** The amount of time (in milliseconds) the drag item must be held over this component
 * to make a directory row expand. */
const DRAG_EXPAND_DELAY_MS = 500

/** Placeholder row for directories that are empty. */
const EMPTY_DIRECTORY_PLACEHOLDER = <span className="px-2 opacity-75">This folder is empty.</span>

// ================
// === AssetRow ===
// ================

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
    key: backendModule.AssetId
    item: assetTreeNode.AssetTreeNode
    setItem: React.Dispatch<React.SetStateAction<assetTreeNode.AssetTreeNode>>
    state: assetsTable.AssetsTableState
    rowState: assetsTable.AssetRowState
    setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for an {@link AssetRow}. */
export interface AssetRowProps
    extends Omit<JSX.IntrinsicElements['tr'], 'onClick' | 'onContextMenu'> {
    keyProp: backendModule.AssetId
    tableRowRef?: React.RefObject<HTMLTableRowElement>
    item: assetTreeNode.AssetTreeNode
    state: assetsTable.AssetsTableState
    hidden: boolean
    initialRowState: assetsTable.AssetRowState
    columns: column.AssetColumn[]
    selected: boolean
    setSelected: (selected: boolean) => void
    isSoleSelectedItem: boolean
    allowContextMenu: boolean
    onClick: (props: AssetRowInnerProps, event: React.MouseEvent) => void
    onContextMenu?: (
        props: AssetRowInnerProps,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

/** A row containing an {@link backendModule.AnyAsset}. */
export default function AssetRow(props: AssetRowProps) {
    const { keyProp: key, item: rawItem, initialRowState, hidden, selected } = props
    const { isSoleSelectedItem, setSelected, allowContextMenu, onContextMenu, state } = props
    const { tableRowRef, columns, onClick } = props
    const { visibilities, assetEvents, dispatchAssetEvent, dispatchAssetListEvent } = state
    const { setAssetSettingsPanelProps, doToggleDirectoryExpansion, doCopy, doCut, doPaste } = state

    const { organization, user } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const toastAndLog = toastAndLogHooks.useToastAndLog()
    const [isDraggedOver, setIsDraggedOver] = React.useState(false)
    const [item, setItem] = React.useState(rawItem)
    const dragOverTimeoutHandle = React.useRef<number | null>(null)
    const asset = item.item
    const [insertionVisibility, setInsertionVisibility] = React.useState(Visibility.visible)
    const [rowState, setRowState] = React.useState<assetsTable.AssetRowState>(() =>
        object.merge(initialRowState, { setVisibility: setInsertionVisibility })
    )
    const isCloud = backend.type === backendModule.BackendType.remote
    const visibility = visibilities.get(key) ?? insertionVisibility

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])
    React.useEffect(() => {
        // Mutation is HIGHLY INADVISABLE in React, however it is useful here as we want to avoid
        // re - rendering the parent.
        rawItem.item = asset
    }, [asset, rawItem])
    const setAsset = assetTreeNode.useSetAsset(asset, setItem)

    React.useEffect(() => {
        if (selected && insertionVisibility !== Visibility.visible) {
            setSelected(false)
        }
    }, [selected, insertionVisibility, /* should never change */ setSelected])

    const doCopyOnBackend = React.useCallback(
        async (newParentId: backendModule.DirectoryId | null) => {
            try {
                setAsset(oldAsset =>
                    object.merge(oldAsset, {
                        title: oldAsset.title + ' (copy)',
                        labels: [],
                        permissions: permissions.tryGetSingletonOwnerPermission(organization, user),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                    })
                )
                const copiedAsset = await backend.copyAsset(
                    asset.id,
                    newParentId ?? organization?.rootDirectoryId ?? backendModule.DirectoryId(''),
                    asset.title,
                    null
                )
                setAsset(
                    // This is SAFE, as the type of the copied asset is guaranteed to be the same
                    // as the type of the original asset.
                    // eslint-disable-next-line no-restricted-syntax
                    object.merger(copiedAsset.asset as Partial<backendModule.AnyAsset>)
                )
            } catch (error) {
                toastAndLog(`Could not copy '${asset.title}'`, error)
                // Delete the new component representing the asset that failed to insert.
                dispatchAssetListEvent({
                    type: AssetListEventType.delete,
                    key: item.key,
                })
            }
        },
        [
            backend,
            organization,
            user,
            asset,
            item.key,
            /* should never change */ setAsset,
            /* should never change */ toastAndLog,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    const doMove = React.useCallback(
        async (
            newParentKey: backendModule.AssetId | null,
            newParentId: backendModule.DirectoryId | null
        ) => {
            const rootDirectoryId = organization?.rootDirectoryId ?? backendModule.DirectoryId('')
            const nonNullNewParentKey = newParentKey ?? rootDirectoryId
            const nonNullNewParentId = newParentId ?? rootDirectoryId
            try {
                dispatchAssetListEvent({
                    type: AssetListEventType.move,
                    newParentKey: nonNullNewParentKey,
                    newParentId: nonNullNewParentId,
                    key: item.key,
                    item: asset,
                })
                setItem(oldItem =>
                    oldItem.with({
                        directoryKey: nonNullNewParentKey,
                        directoryId: nonNullNewParentId,
                    })
                )
                await backend.updateAsset(
                    asset.id,
                    { parentDirectoryId: newParentId ?? rootDirectoryId, description: null },
                    asset.title
                )
            } catch (error) {
                toastAndLog(`Could not move '${asset.title}'`, error)
                setItem(oldItem =>
                    oldItem.with({ directoryKey: item.directoryKey, directoryId: item.directoryId })
                )
                // Move the asset back to its original position.
                dispatchAssetListEvent({
                    type: AssetListEventType.move,
                    newParentKey: item.directoryKey,
                    newParentId: item.directoryId,
                    key: item.key,
                    item: asset,
                })
            }
        },
        [
            backend,
            organization,
            asset,
            item.directoryId,
            item.directoryKey,
            item.key,
            /* should never change */ toastAndLog,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    React.useEffect(() => {
        if (isSoleSelectedItem) {
            setAssetSettingsPanelProps({ item, setItem })
        }
    }, [item, isSoleSelectedItem, /* should never change */ setAssetSettingsPanelProps])

    const doDelete = React.useCallback(async () => {
        setInsertionVisibility(Visibility.hidden)
        if (asset.type === backendModule.AssetType.directory) {
            dispatchAssetListEvent({
                type: AssetListEventType.closeFolder,
                id: asset.id,
                // This is SAFE, as this asset is already known to be a directory.
                // eslint-disable-next-line no-restricted-syntax
                key: item.key as backendModule.DirectoryId,
            })
        }
        try {
            dispatchAssetListEvent({
                type: AssetListEventType.willDelete,
                key: item.key,
            })
            if (
                asset.type === backendModule.AssetType.project &&
                backend.type === backendModule.BackendType.local
            ) {
                if (
                    asset.projectState.type !== backendModule.ProjectState.placeholder &&
                    asset.projectState.type !== backendModule.ProjectState.closed
                ) {
                    await backend.openProject(asset.id, null, asset.title)
                }
                try {
                    await backend.closeProject(asset.id, asset.title)
                } catch {
                    // Ignored. The project was already closed.
                }
            }
            await backend.deleteAsset(asset.id, asset.title)
            dispatchAssetListEvent({
                type: AssetListEventType.delete,
                key: item.key,
            })
        } catch (error) {
            setInsertionVisibility(Visibility.visible)
            toastAndLog(
                errorModule.tryGetMessage(error)?.slice(0, -1) ??
                    `Could not delete ${backendModule.ASSET_TYPE_NAME[asset.type]}`
            )
        }
    }, [
        backend,
        dispatchAssetListEvent,
        asset,
        /* should never change */ item.key,
        /* should never change */ toastAndLog,
    ])

    const doRestore = React.useCallback(async () => {
        // Visually, the asset is deleted from the Trash view.
        setInsertionVisibility(Visibility.hidden)
        try {
            await backend.undoDeleteAsset(asset.id, asset.title)
            dispatchAssetListEvent({
                type: AssetListEventType.delete,
                key: item.key,
            })
        } catch (error) {
            setInsertionVisibility(Visibility.visible)
            toastAndLog(`Unable to restore ${backendModule.ASSET_TYPE_NAME[asset.type]}`, error)
        }
    }, [
        backend,
        dispatchAssetListEvent,
        asset,
        /* should never change */ item.key,
        /* should never change */ toastAndLog,
    ])

    eventHooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            // These events are handled in the specific `NameColumn` files.
            case AssetEventType.newProject:
            case AssetEventType.newFolder:
            case AssetEventType.uploadFiles:
            case AssetEventType.newDataConnector:
            case AssetEventType.openProject:
            case AssetEventType.closeProject:
            case AssetEventType.cancelOpeningAllProjects: {
                break
            }
            case AssetEventType.copy: {
                if (event.ids.has(item.key)) {
                    await doCopyOnBackend(event.newParentId)
                }
                break
            }
            case AssetEventType.cut: {
                if (event.ids.has(item.key)) {
                    setInsertionVisibility(Visibility.faded)
                }
                break
            }
            case AssetEventType.cancelCut: {
                if (event.ids.has(item.key)) {
                    setInsertionVisibility(Visibility.visible)
                }
                break
            }
            case AssetEventType.move: {
                if (event.ids.has(item.key)) {
                    setInsertionVisibility(Visibility.visible)
                    await doMove(event.newParentKey, event.newParentId)
                }
                break
            }
            case AssetEventType.delete: {
                if (event.ids.has(item.key)) {
                    await doDelete()
                }
                break
            }
            case AssetEventType.restore: {
                if (event.ids.has(item.key)) {
                    await doRestore()
                }
                break
            }
            case AssetEventType.download: {
                if (event.ids.has(item.key)) {
                    if (isCloud) {
                        if (asset.type !== backendModule.AssetType.file) {
                            toastAndLog('Cannot download assets that are not files')
                        } else {
                            try {
                                const details = await backend.getFileDetails(asset.id, asset.title)
                                const file = details.file
                                download.download(download.s3URLToHTTPURL(file.path), asset.title)
                            } catch (error) {
                                toastAndLog('Could not download file', error)
                            }
                        }
                    } else {
                        download.download(
                            `./api/project-manager/projects/${asset.id}/enso-project`,
                            `${asset.title}.enso-project`
                        )
                    }
                }
                break
            }
            case AssetEventType.downloadSelected: {
                if (selected) {
                    if (isCloud) {
                        if (asset.type !== backendModule.AssetType.file) {
                            toastAndLog('Cannot download assets that are not files')
                        } else {
                            try {
                                const details = await backend.getFileDetails(asset.id, asset.title)
                                const file = details.file
                                download.download(download.s3URLToHTTPURL(file.path), asset.title)
                            } catch (error) {
                                toastAndLog('Could not download selected files', error)
                            }
                        }
                    } else {
                        download.download(
                            `./api/project-manager/projects/${asset.id}/enso-project`,
                            `${asset.title}.enso-project`
                        )
                    }
                }
                break
            }
            case AssetEventType.removeSelf: {
                // This is not triggered from the asset list, so it uses `item.id` instead of `key`.
                if (event.id === asset.id && user != null) {
                    setInsertionVisibility(Visibility.hidden)
                    try {
                        await backend.createPermission({
                            action: null,
                            resourceId: asset.id,
                            userSubjects: [user.id],
                        })
                        dispatchAssetListEvent({
                            type: AssetListEventType.delete,
                            key: item.key,
                        })
                    } catch (error) {
                        setInsertionVisibility(Visibility.visible)
                        toastAndLog(null, error)
                    }
                }
                break
            }
            case AssetEventType.temporarilyAddLabels: {
                const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === labels &&
                    oldRowState.temporarilyRemovedLabels === set.EMPTY
                        ? oldRowState
                        : object.merge(oldRowState, {
                              temporarilyAddedLabels: labels,
                              temporarilyRemovedLabels: set.EMPTY,
                          })
                )
                break
            }
            case AssetEventType.temporarilyRemoveLabels: {
                const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === set.EMPTY &&
                    oldRowState.temporarilyRemovedLabels === labels
                        ? oldRowState
                        : object.merge(oldRowState, {
                              temporarilyAddedLabels: set.EMPTY,
                              temporarilyRemovedLabels: labels,
                          })
                )
                break
            }
            case AssetEventType.addLabels: {
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === set.EMPTY
                        ? oldRowState
                        : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
                )
                const labels = asset.labels
                if (
                    event.ids.has(item.key) &&
                    (labels == null || [...event.labelNames].some(label => !labels.includes(label)))
                ) {
                    const newLabels = [
                        ...(labels ?? []),
                        ...[...event.labelNames].filter(label => labels?.includes(label) !== true),
                    ]
                    setAsset(object.merger({ labels: newLabels }))
                    try {
                        await backend.associateTag(asset.id, newLabels, asset.title)
                    } catch (error) {
                        setAsset(object.merger({ labels }))
                        toastAndLog(null, error)
                    }
                }
                break
            }
            case AssetEventType.removeLabels: {
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === set.EMPTY
                        ? oldRowState
                        : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
                )
                const labels = asset.labels
                if (
                    event.ids.has(item.key) &&
                    labels != null &&
                    [...event.labelNames].some(label => labels.includes(label))
                ) {
                    const newLabels = labels.filter(label => !event.labelNames.has(label))
                    setAsset(object.merger({ labels: newLabels }))
                    try {
                        await backend.associateTag(asset.id, newLabels, asset.title)
                    } catch (error) {
                        setAsset(object.merger({ labels }))
                        toastAndLog(null, error)
                    }
                }
                break
            }
            case AssetEventType.deleteLabel: {
                setAsset(oldAsset => {
                    // The IIFE is required to prevent TypeScript from narrowing this value.
                    let found = (() => false)()
                    const labels =
                        oldAsset.labels?.filter(label => {
                            if (label === event.labelName) {
                                found = true
                                return false
                            } else {
                                return true
                            }
                        }) ?? null
                    return found ? object.merge(oldAsset, { labels }) : oldAsset
                })
                break
            }
        }
    })

    const clearDragState = React.useCallback(() => {
        setIsDraggedOver(false)
        setRowState(oldRowState =>
            oldRowState.temporarilyAddedLabels === set.EMPTY
                ? oldRowState
                : object.merge(oldRowState, { temporarilyAddedLabels: set.EMPTY })
        )
    }, [])

    const onDragOver = (event: React.DragEvent<Element>) => {
        const directoryKey =
            item.item.type === backendModule.AssetType.directory ? item.key : item.directoryKey
        const payload = drag.ASSET_ROWS.lookup(event)
        if (payload != null && payload.every(innerItem => innerItem.key !== directoryKey)) {
            event.preventDefault()
            if (item.item.type === backendModule.AssetType.directory) {
                setIsDraggedOver(true)
            }
        }
    }

    switch (asset.type) {
        case backendModule.AssetType.directory:
        case backendModule.AssetType.project:
        case backendModule.AssetType.file:
        case backendModule.AssetType.secret: {
            const innerProps: AssetRowInnerProps = {
                key,
                item,
                setItem,
                state,
                rowState,
                setRowState,
            }
            return (
                <>
                    {!hidden && insertionVisibility !== Visibility.hidden && (
                        <tr
                            ref={tableRowRef}
                            tabIndex={-1}
                            onClick={event => {
                                unsetModal()
                                onClick(innerProps, event)
                            }}
                            onContextMenu={event => {
                                if (allowContextMenu) {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    onContextMenu?.(innerProps, event)
                                    setModal(
                                        <AssetContextMenu
                                            innerProps={innerProps}
                                            event={event}
                                            eventTarget={
                                                event.target instanceof HTMLElement
                                                    ? event.target
                                                    : event.currentTarget
                                            }
                                            doCopy={doCopy}
                                            doCut={doCut}
                                            doPaste={doPaste}
                                            doDelete={doDelete}
                                        />
                                    )
                                } else {
                                    onContextMenu?.(innerProps, event)
                                }
                            }}
                            className={`h-8 transition duration-300 ease-in-out ${
                                visibilityModule.CLASS_NAME[visibility]
                            } ${isDraggedOver || selected ? 'selected' : ''}`}
                            onDragEnter={event => {
                                if (dragOverTimeoutHandle.current != null) {
                                    window.clearTimeout(dragOverTimeoutHandle.current)
                                }
                                if (backendModule.assetIsDirectory(asset)) {
                                    dragOverTimeoutHandle.current = window.setTimeout(() => {
                                        doToggleDirectoryExpansion(
                                            asset.id,
                                            item.key,
                                            asset.title,
                                            true
                                        )
                                    }, DRAG_EXPAND_DELAY_MS)
                                }
                                // Required because `dragover` does not fire on `mouseenter`.
                                onDragOver(event)
                            }}
                            onDragOver={event => {
                                props.onDragOver?.(event)
                                onDragOver(event)
                            }}
                            onDragEnd={event => {
                                clearDragState()
                                props.onDragEnd?.(event)
                            }}
                            onDragLeave={event => {
                                if (
                                    dragOverTimeoutHandle.current != null &&
                                    (!(event.relatedTarget instanceof Node) ||
                                        !event.currentTarget.contains(event.relatedTarget))
                                ) {
                                    window.clearTimeout(dragOverTimeoutHandle.current)
                                }
                                clearDragState()
                                props.onDragLeave?.(event)
                            }}
                            onDrop={event => {
                                props.onDrop?.(event)
                                clearDragState()
                                const [directoryKey, directoryId, directoryTitle] =
                                    item.item.type === backendModule.AssetType.directory
                                        ? [item.key, item.item.id, asset.title]
                                        : [item.directoryKey, item.directoryId, null]
                                const payload = drag.ASSET_ROWS.lookup(event)
                                if (
                                    payload != null &&
                                    payload.every(innerItem => innerItem.key !== directoryKey)
                                ) {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    unsetModal()
                                    doToggleDirectoryExpansion(
                                        directoryId,
                                        directoryKey,
                                        directoryTitle,
                                        true
                                    )
                                    dispatchAssetEvent({
                                        type: AssetEventType.move,
                                        newParentKey: directoryKey,
                                        newParentId: directoryId,
                                        ids: new Set(payload.map(dragItem => dragItem.key)),
                                    })
                                }
                            }}
                        >
                            {columns.map(column => {
                                // This is a React component even though it does not contain JSX.
                                // eslint-disable-next-line no-restricted-syntax
                                const Render = column.render
                                return (
                                    <td key={column.id} className={column.className ?? ''}>
                                        <Render
                                            keyProp={key}
                                            item={item}
                                            setItem={setItem}
                                            selected={selected}
                                            setSelected={setSelected}
                                            isSoleSelectedItem={isSoleSelectedItem}
                                            state={state}
                                            rowState={rowState}
                                            setRowState={setRowState}
                                        />
                                    </td>
                                )
                            })}
                        </tr>
                    )}
                    {selected && allowContextMenu && insertionVisibility !== Visibility.hidden && (
                        // This is a copy of the context menu, since the context menu registers keyboard
                        // shortcut handlers. This is a bit of a hack, however it is preferable to duplicating
                        // the entire context menu (once for the keyboard actions, once for the JSX).
                        <AssetContextMenu
                            hidden
                            innerProps={{
                                key,
                                item,
                                setItem,
                                state,
                                rowState,
                                setRowState,
                            }}
                            event={{ pageX: 0, pageY: 0 }}
                            eventTarget={null}
                            doCopy={doCopy}
                            doCut={doCut}
                            doPaste={doPaste}
                            doDelete={doDelete}
                        />
                    )}
                </>
            )
        }
        case backendModule.AssetType.specialLoading: {
            return hidden ? null : (
                <tr>
                    <td colSpan={columns.length} className="rounded-rows-skip-level border-r p-0">
                        <div
                            className={`flex justify-center rounded-full h-8 py-1 ${indent.indentClass(
                                item.depth
                            )}`}
                        >
                            <StatelessSpinner
                                size={24}
                                state={statelessSpinner.SpinnerState.loadingMedium}
                            />
                        </div>
                    </td>
                </tr>
            )
        }
        case backendModule.AssetType.specialEmpty: {
            return hidden ? null : (
                <tr>
                    <td colSpan={columns.length} className="rounded-rows-skip-level border-r p-0">
                        <div
                            className={`flex items-center rounded-full h-8 py-2 ${indent.indentClass(
                                item.depth
                            )}`}
                        >
                            <img src={BlankIcon} />
                            {EMPTY_DIRECTORY_PLACEHOLDER}
                        </div>
                    </td>
                </tr>
            )
        }
    }
}
