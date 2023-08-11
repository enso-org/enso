/** @file A {@link TableRow} for an arbitrary asset. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as download from '../../download'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as modalProvider from '../../providers/modal'
import * as presenceModule from '../presence'

import * as assetsTable from './assetsTable'
import StatelessSpinner, * as statelessSpinner from './statelessSpinner'
import TableRow, * as tableRow from './tableRow'
import AssetContextMenu from './assetContextMenu'

// ================
// === AssetRow ===
// ================

/** Props for an {@link AssetRow}. */
export interface AssetRowProps<T extends backendModule.AnyAsset>
    extends tableRow.TableRowProps<
        T,
        assetsTable.AssetsTableState,
        assetsTable.AssetRowState,
        T['id']
    > {}

/** A row containing an {@link backendModule.AnyAsset}. */
export default function AssetRow(props: AssetRowProps<backendModule.AnyAsset>) {
    const {
        keyProp: key,
        item: rawItem,
        initialRowState,
        hidden,
        selected,
        allowContextMenu,
        onContextMenu,
        state,
        columns,
    } = props
    const { assetEvents, dispatchAssetListEvent, getDepth } = state
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const { user } = authProvider.useNonPartialUserSession()
    const toastAndLog = hooks.useToastAndLog()
    const [item, setItem] = React.useState(rawItem)
    const [presence, setPresence] = React.useState(presenceModule.Presence.present)
    const [rowState, setRowState] = React.useState<assetsTable.AssetRowState>(() => ({
        ...initialRowState,
        setPresence,
    }))

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])

    const doDelete = React.useCallback(async () => {
        setPresence(presenceModule.Presence.deleting)
        try {
            if (
                item.type === backendModule.AssetType.project &&
                backend.type === backendModule.BackendType.local
            ) {
                if (
                    item.projectState.type !== backendModule.ProjectState.placeholder &&
                    item.projectState.type !== backendModule.ProjectState.closed
                ) {
                    await backend.openProject(item.id, null, item.title)
                }
                try {
                    await backend.closeProject(item.id, item.title)
                } catch {
                    // Ignored. The project was already closed.
                }
            }
            await backend.deleteAsset(item)
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.delete,
                id: key,
            })
        } catch (error) {
            setPresence(presenceModule.Presence.present)
            toastAndLog('Unable to delete project', error)
        }
    }, [backend, dispatchAssetListEvent, item, key, toastAndLog])

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            // These events are handled in the specific NameColumn files.
            case assetEventModule.AssetEventType.newProject:
            case assetEventModule.AssetEventType.newFolder:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.newSecret:
            case assetEventModule.AssetEventType.openProject:
            case assetEventModule.AssetEventType.cancelOpeningAllProjects: {
                break
            }
            case assetEventModule.AssetEventType.deleteMultiple: {
                if (event.ids.has(key)) {
                    await doDelete()
                }
                break
            }
            case assetEventModule.AssetEventType.downloadSelected: {
                if (selected) {
                    download.download(
                        './api/project-manager/' + `projects/${item.id}/enso-project`,
                        `${item.title}.enso-project`
                    )
                }
                break
            }
            case assetEventModule.AssetEventType.removeSelf: {
                // This is not triggered from the asset list, so it uses `item.id` instead of `key`.
                if (event.id === item.id && user != null) {
                    setPresence(presenceModule.Presence.deleting)
                    try {
                        await backend.createPermission({
                            action: null,
                            resourceId: item.id,
                            userSubjects: [user.id],
                        })
                        dispatchAssetListEvent({
                            type: assetListEventModule.AssetListEventType.delete,
                            id: key,
                        })
                    } catch (error) {
                        setPresence(presenceModule.Presence.present)
                        toastAndLog('Unable to delete project', error)
                    }
                }
                break
            }
        }
    })

    switch (item.type) {
        case backendModule.AssetType.directory:
        case backendModule.AssetType.project:
        case backendModule.AssetType.file:
        case backendModule.AssetType.secret: {
            return (
                <>
                    <TableRow
                        className={presenceModule.CLASS_NAME[presence]}
                        {...props}
                        hidden={hidden || presence === presenceModule.Presence.deleting}
                        onContextMenu={(innerProps, event) => {
                            if (allowContextMenu) {
                                event.preventDefault()
                                event.stopPropagation()
                                onContextMenu?.(innerProps, event)
                                setModal(
                                    <AssetContextMenu
                                        innerProps={innerProps}
                                        event={event}
                                        eventTarget={event.currentTarget}
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
                    />
                    {selected &&
                        allowContextMenu &&
                        presence !== presenceModule.Presence.deleting && (
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
                                doDelete={doDelete}
                            />
                        )}
                </>
            )
        }
        case backendModule.AssetType.specialLoading: {
            return hidden ? null : (
                <tr>
                    <td colSpan={columns.length} className="p-0 rounded-full border-r">
                        <div
                            className={`flex justify-center py-2 ${indent.indentClass(
                                getDepth(key)
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
                    <td colSpan={columns.length} className="p-0 rounded-full border-r">
                        <div
                            className={`flex items-center h-10 py-2 ${indent.indentClass(
                                getDepth(key)
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
