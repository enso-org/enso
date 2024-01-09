/** @file A {@link TableRow} for an arbitrary asset. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as assetTreeNode from '../assetTreeNode'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as download from '../../download'
import * as drag from '../drag'
import * as errorModule from '../../error'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as modalProvider from '../../providers/modal'
import * as set from '../set'
import * as visibilityModule from '../visibility'

import * as assetsTable from './assetsTable'
import type * as tableRow from './tableRow'
import StatelessSpinner, * as statelessSpinner from './statelessSpinner'
import AssetContextMenu from './assetContextMenu'
import TableRow from './tableRow'

// =================
// === Constants ===
// =================

/** The amount of time (in milliseconds) the drag item must be held over this component
 * to make a directory row expand. */
const DRAG_EXPAND_DELAY_MS = 500

// ================
// === AssetRow ===
// ================

/** Props for an {@link AssetRow}. */
export interface AssetRowProps
    extends tableRow.TableRowProps<
        assetTreeNode.AssetTreeNode,
        assetsTable.AssetsTableState,
        assetsTable.AssetRowState,
        backendModule.AssetId
    > {}

/** A row containing an {@link backendModule.AnyAsset}. */
export default function AssetRow(props: AssetRowProps) {
    const {
        keyProp: key,
        item: rawItem,
        initialRowState,
        hidden,
        selected,
        isSoleSelectedItem,
        setSelected,
        allowContextMenu,
        onContextMenu,
        state,
        columns,
    } = props
    const {
        visibilities,
        assetEvents,
        dispatchAssetEvent,
        dispatchAssetListEvent,
        setAssetSettingsPanelProps,
        doToggleDirectoryExpansion,
        doCut,
        doPaste,
    } = state
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()
    const { user } = authProvider.useNonPartialUserSession()
    const toastAndLog = hooks.useToastAndLog()
    const [isDraggedOver, setIsDraggedOver] = React.useState(false)
    const [item, setItem] = React.useState(rawItem)
    const dragOverTimeoutHandle = React.useRef<number | null>(null)
    const asset = item.item
    const [insertionVisibility, setInsertionVisibility] = React.useState(
        visibilityModule.Visibility.visible
    )
    const [rowState, setRowState] = React.useState<assetsTable.AssetRowState>(() => ({
        ...initialRowState,
        setVisibility: setInsertionVisibility,
    }))
    const visibility = visibilities.get(key) ?? insertionVisibility

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])
    React.useEffect(() => {
        // Mutation is HIGHLY INADVISABLE in React, however it is useful here as we want to avoid re-rendering the
        // parent.
        rawItem.item = asset
    }, [asset, rawItem])
    const setAsset = assetTreeNode.useSetAsset(asset, setItem)

    React.useEffect(() => {
        if (selected && insertionVisibility !== visibilityModule.Visibility.visible) {
            setSelected(false)
        }
    }, [selected, insertionVisibility, /* should never change */ setSelected])

    const doMove = React.useCallback(
        async (
            newParentKey: backendModule.AssetId | null,
            newParentId: backendModule.DirectoryId | null
        ) => {
            // The asset is effectivly being deleted from the current position, and created at the new
            // position, from the viewpoint of the asset list.
            try {
                dispatchAssetListEvent({
                    type: assetListEventModule.AssetListEventType.move,
                    newParentKey,
                    newParentId,
                    key: item.key,
                    item: asset,
                })
                setItem(oldItem => ({
                    ...oldItem,
                    directoryKey: newParentKey,
                    directoryId: newParentId,
                }))
                await backend.updateAsset(
                    asset.id,
                    {
                        parentDirectoryId: newParentId ?? backend.rootDirectoryId(organization),
                        description: null,
                    },
                    asset.title
                )
            } catch (error) {
                toastAndLog(`Could not move '${asset.title}'`, error)
                setItem(oldItem => ({
                    ...oldItem,
                    directoryKey: item.directoryKey,
                    directoryId: item.directoryId,
                }))
                // Move the asset back to its original position.
                dispatchAssetListEvent({
                    type: assetListEventModule.AssetListEventType.move,
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
        setInsertionVisibility(visibilityModule.Visibility.hidden)
        if (asset.type === backendModule.AssetType.directory) {
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.closeFolder,
                id: asset.id,
                // This is SAFE, as this asset is already known to be a directory.
                // eslint-disable-next-line no-restricted-syntax
                key: item.key as backendModule.DirectoryId,
            })
        }
        try {
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.willDelete,
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
                type: assetListEventModule.AssetListEventType.delete,
                key: item.key,
            })
        } catch (error) {
            setInsertionVisibility(visibilityModule.Visibility.visible)
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
        setInsertionVisibility(visibilityModule.Visibility.hidden)
        try {
            await backend.undoDeleteAsset(asset.id, asset.title)
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.delete,
                key: item.key,
            })
        } catch (error) {
            setInsertionVisibility(visibilityModule.Visibility.visible)
            toastAndLog(`Unable to restore ${backendModule.ASSET_TYPE_NAME[asset.type]}`, error)
        }
    }, [
        backend,
        dispatchAssetListEvent,
        asset,
        /* should never change */ item.key,
        /* should never change */ toastAndLog,
    ])

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            // These events are handled in the specific `NameColumn` files.
            case assetEventModule.AssetEventType.newProject:
            case assetEventModule.AssetEventType.newFolder:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.newDataConnector:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.closeProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects: {
                break
            }
            case assetEventModule.AssetEventType.cut: {
                if (event.ids.has(item.key)) {
                    setInsertionVisibility(visibilityModule.Visibility.faded)
                }
                break
            }
            case assetEventModule.AssetEventType.cancelCut: {
                if (event.ids.has(item.key)) {
                    setInsertionVisibility(visibilityModule.Visibility.visible)
                }
                break
            }
            case assetEventModule.AssetEventType.move: {
                if (event.ids.has(item.key)) {
                    setInsertionVisibility(visibilityModule.Visibility.visible)
                    await doMove(event.newParentKey, event.newParentId)
                }
                break
            }
            case assetEventModule.AssetEventType.delete: {
                if (event.ids.has(item.key)) {
                    await doDelete()
                }
                break
            }
            case assetEventModule.AssetEventType.restore: {
                if (event.ids.has(item.key)) {
                    await doRestore()
                }
                break
            }
            case assetEventModule.AssetEventType.download: {
                if (event.ids.has(item.key)) {
                    download.download(
                        `./api/project-manager/projects/${asset.id}/enso-project`,
                        `${asset.title}.enso-project`
                    )
                }
                break
            }
            case assetEventModule.AssetEventType.downloadSelected: {
                if (selected) {
                    download.download(
                        `./api/project-manager/projects/${asset.id}/enso-project`,
                        `${asset.title}.enso-project`
                    )
                }
                break
            }
            case assetEventModule.AssetEventType.removeSelf: {
                // This is not triggered from the asset list, so it uses `item.id` instead of `key`.
                if (event.id === asset.id && user != null) {
                    setInsertionVisibility(visibilityModule.Visibility.hidden)
                    try {
                        await backend.createPermission({
                            action: null,
                            resourceId: asset.id,
                            userSubjects: [user.id],
                        })
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.delete,
                            key: item.key,
                        })
                    } catch (error) {
                        setInsertionVisibility(visibilityModule.Visibility.visible)
                        toastAndLog(null, error)
                    }
                }
                break
            }
            case assetEventModule.AssetEventType.temporarilyAddLabels: {
                const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === labels &&
                    oldRowState.temporarilyRemovedLabels === set.EMPTY
                        ? oldRowState
                        : {
                              ...oldRowState,
                              temporarilyAddedLabels: labels,
                              temporarilyRemovedLabels: set.EMPTY,
                          }
                )
                break
            }
            case assetEventModule.AssetEventType.temporarilyRemoveLabels: {
                const labels = event.ids.has(item.key) ? event.labelNames : set.EMPTY
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === set.EMPTY &&
                    oldRowState.temporarilyRemovedLabels === labels
                        ? oldRowState
                        : {
                              ...oldRowState,
                              temporarilyAddedLabels: set.EMPTY,
                              temporarilyRemovedLabels: labels,
                          }
                )
                break
            }
            case assetEventModule.AssetEventType.addLabels: {
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === set.EMPTY
                        ? oldRowState
                        : { ...oldRowState, temporarilyAddedLabels: set.EMPTY }
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
                    setAsset(oldAsset => ({ ...oldAsset, labels: newLabels }))
                    try {
                        await backend.associateTag(asset.id, newLabels, asset.title)
                    } catch (error) {
                        setAsset(oldAsset => ({ ...oldAsset, labels }))
                        toastAndLog(null, error)
                    }
                }
                break
            }
            case assetEventModule.AssetEventType.removeLabels: {
                setRowState(oldRowState =>
                    oldRowState.temporarilyAddedLabels === set.EMPTY
                        ? oldRowState
                        : { ...oldRowState, temporarilyAddedLabels: set.EMPTY }
                )
                const labels = asset.labels
                if (
                    event.ids.has(item.key) &&
                    labels != null &&
                    [...event.labelNames].some(label => labels.includes(label))
                ) {
                    const newLabels = labels.filter(label => !event.labelNames.has(label))
                    setAsset(oldAsset => ({ ...oldAsset, labels: newLabels }))
                    try {
                        await backend.associateTag(asset.id, newLabels, asset.title)
                    } catch (error) {
                        setAsset(oldAsset => ({ ...oldAsset, labels }))
                        toastAndLog(null, error)
                    }
                }
                break
            }
            case assetEventModule.AssetEventType.deleteLabel: {
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
                    return found ? { ...oldAsset, labels } : oldAsset
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
                : { ...oldRowState, temporarilyAddedLabels: set.EMPTY }
        )
    }, [])

    switch (asset.type) {
        case backendModule.AssetType.directory:
        case backendModule.AssetType.project:
        case backendModule.AssetType.file:
        case backendModule.AssetType.secret: {
            return (
                <>
                    <TableRow
                        className={`${visibilityModule.CLASS_NAME[visibility]} ${
                            isDraggedOver ? 'selected' : ''
                        }`}
                        {...props}
                        hidden={
                            hidden || insertionVisibility === visibilityModule.Visibility.hidden
                        }
                        onContextMenu={(innerProps, event) => {
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
                                        doCut={doCut}
                                        doPaste={doPaste}
                                        doDelete={doDelete}
                                    />
                                )
                            } else {
                                onContextMenu?.(innerProps, event)
                            }
                        }}
                        item={item}
                        setItem={setItem}
                        initialRowState={rowState}
                        setRowState={setRowState}
                        onDragEnter={() => {
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
                        }}
                        onDragOver={event => {
                            props.onDragOver?.(event)
                            if (item.item.type === backendModule.AssetType.directory) {
                                const payload = drag.ASSET_ROWS.lookup(event)
                                if (
                                    payload != null &&
                                    payload.every(innerItem => innerItem.key !== item.key)
                                ) {
                                    event.preventDefault()
                                    setIsDraggedOver(true)
                                }
                            }
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
                            if (item.item.type === backendModule.AssetType.directory) {
                                const payload = drag.ASSET_ROWS.lookup(event)
                                if (
                                    payload != null &&
                                    payload.every(innerItem => innerItem.key !== item.key)
                                ) {
                                    event.preventDefault()
                                    event.stopPropagation()
                                    unsetModal()
                                    dispatchAssetEvent({
                                        type: assetEventModule.AssetEventType.move,
                                        newParentKey: item.key,
                                        newParentId: item.item.id,
                                        ids: new Set(payload.map(dragItem => dragItem.key)),
                                    })
                                }
                            }
                        }}
                    />
                    {selected &&
                        allowContextMenu &&
                        insertionVisibility !== visibilityModule.Visibility.hidden && (
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
                            {assetsTable.EMPTY_DIRECTORY_PLACEHOLDER}
                        </div>
                    </td>
                </tr>
            )
        }
    }
}
