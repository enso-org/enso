/** @file Table displaying a list of projects. */
import * as React from 'react'

import * as common from 'enso-common'

import BlankIcon from 'enso-assets/blank.svg'

import * as array from '../array'
import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as download from '../../download'
import * as hooks from '../../hooks'
import * as indent from '../indent'
import * as modalProvider from '../../providers/modal'
import * as permissions from '../permissions'
import * as presenceModule from '../presence'
import * as string from '../../string'
import * as uniqueString from '../../uniqueString'

import StatelessSpinner, * as statelessSpinner from './statelessSpinner'
import TableRow, * as tableRow from './tableRow'
import AssetContextMenu from './assetContextMenu'
import Button from './button'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import Table from './table'

// =================
// === Constants ===
// =================

/** The `localStorage` key under which the ID of the current directory is stored. */
const EXTRA_COLUMNS_KEY =
    common.PRODUCT_NAME.toLowerCase() + '-dashboard-directory-list-extra-columns'

/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'item'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'items'
// This is a function, even though it is not syntactically a function.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)
/** Placeholder row. */
const PLACEHOLDER = (
    <span className="opacity-75">
        You have no projects yet. Go ahead and create one using the button above, or open a template
        from the home screen.
    </span>
)
/** Placeholder row for directories that are empty. */
const EMPTY_DIRECTORY_PLACEHOLDER = <span className="px-2 opacity-75">This folder is empty.</span>

/** The {@link RegExp} matching a directory name following the default naming convention. */
const DIRECTORY_NAME_REGEX = /^New_Folder_(?<directoryIndex>\d+)$/
/** The default prefix of an automatically generated directory. */
const DIRECTORY_NAME_DEFAULT_PREFIX = 'New_Folder_'

// =============
// === Types ===
// =============

/** An interface containing only the depth of an item (an integer). */
interface Depth {
    depth: number
}

/** The type of items in an {@link AssetsTable}. */
export type AssetTableItem<T extends backendModule.AnyAsset = backendModule.AnyAsset> = Depth & T

// ================
// === AssetRow ===
// ================

/** Props for an {@link AssetRow}. */
export interface AssetRowProps<T extends backendModule.AnyAsset>
    extends tableRow.TableRowProps<T, AssetsTableState, AssetRowState, T['id']> {}

/** A row containing a {@link backendModule.ProjectAsset}.
 * @throws {Error} when `item` is not a {@link backendModule.ProjectAsset}. */
function AssetRow(props: AssetRowProps<backendModule.AnyAsset>) {
    const {
        keyProp: key,
        item: rawItem,
        initialRowState,
        columns,
        selected,
        state: { assetEvents, dispatchAssetEvent, dispatchAssetListEvent, getDepth },
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const toastAndLog = hooks.useToastAndLog()
    const [item, setItem] = React.useState(rawItem)
    const [presence, setPresence] = React.useState(presenceModule.Presence.present)

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])

    const doDelete = async () => {
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
    }

    hooks.useEventHandler(assetEvents, async event => {
        switch (event.type) {
            // These events are handled in the specific NameColumn files.
            case assetEventModule.AssetEventType.createProject:
            case assetEventModule.AssetEventType.createDirectory:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.createSecret:
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
        }
    })

    switch (item.type) {
        case backendModule.AssetType.directory:
        case backendModule.AssetType.project:
        case backendModule.AssetType.file:
        case backendModule.AssetType.secret: {
            return presence === presenceModule.Presence.deleting ? (
                <></>
            ) : (
                <TableRow
                    className={presenceModule.CLASS_NAME[presence]}
                    {...props}
                    onContextMenu={(innerProps, event) => {
                        event.preventDefault()
                        event.stopPropagation()
                        setModal(
                            <AssetContextMenu
                                innerProps={innerProps}
                                event={event}
                                dispatchAssetEvent={dispatchAssetEvent}
                                doDelete={doDelete}
                            />
                        )
                    }}
                    item={item}
                    setItem={setItem}
                    initialRowState={{ ...initialRowState, setPresence }}
                />
            )
        }
        case backendModule.AssetType.specialLoading: {
            return (
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
            return (
                <tr>
                    <td colSpan={columns.length} className="p-0 rounded-full border-r">
                        <div
                            className={`flex items-center h-10 py-2 ${indent.indentClass(
                                getDepth(key)
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

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
    appRunner: AppRunner | null
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    getDepth: (id: backendModule.AssetId) => number
    doToggleDirectoryExpansion: (
        directory: backendModule.DirectoryAsset,
        key: backendModule.AssetId
    ) => void
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
    setPresence: (presence: presenceModule.Presence) => void
    isEditingName: boolean
}

/** The default {@link AssetRowState} associated with a {@link AssetRow}. */
export const INITIAL_ROW_STATE: AssetRowState = Object.freeze({
    setPresence: () => {
        // Ignored. This MUST be replaced by the row component. It should also update `presence`.
    },
    isEditingName: false,
})

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
    appRunner: AppRunner | null
    items: backendModule.AnyAsset[]
    filter: ((item: backendModule.AnyAsset) => boolean) | null
    isLoading: boolean
    assetEvents: assetEventModule.AssetEvent[]
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    assetListEvents: assetListEventModule.AssetListEvent[]
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
    const {
        appRunner,
        items: rawItems,
        filter,
        isLoading,
        assetEvents,
        dispatchAssetEvent,
        assetListEvents,
        dispatchAssetListEvent,
        doOpenIde,
        doCloseIde: rawDoCloseIde,
    } = props
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [initialized, setInitialized] = React.useState(false)
    const [items, setItems] = React.useState(rawItems)
    const [extraColumns, setExtraColumns] = React.useState(
        () => new Set<columnModule.ExtraColumn>()
    )
    // Items in the root directory have a depth of 0.
    const itemDepthsRef = React.useRef(new Map<backendModule.AssetId, number>())

    React.useEffect(() => {
        setInitialized(true)
        const extraColumnsJson = localStorage.getItem(EXTRA_COLUMNS_KEY)
        if (extraColumnsJson != null) {
            const savedExtraColumns: unknown = JSON.parse(extraColumnsJson)
            if (
                Array.isArray(savedExtraColumns) &&
                savedExtraColumns.every(array.includesPredicate(columnModule.EXTRA_COLUMNS))
            ) {
                setExtraColumns(new Set(savedExtraColumns))
            }
        }
    }, [])

    React.useEffect(() => {
        if (initialized) {
            localStorage.setItem(EXTRA_COLUMNS_KEY, JSON.stringify(Array.from(extraColumns)))
        }
    }, [extraColumns, initialized])

    const getDepth = React.useCallback(
        (id: backendModule.AssetId) => itemDepthsRef.current.get(id) ?? 0,
        []
    )

    React.useEffect(() => {
        setItems(rawItems)
    }, [rawItems])

    const visibleItems = React.useMemo(
        () => (filter != null ? items.filter(filter) : items),
        [items, filter]
    )

    React.useEffect(() => {
        // Remove unused keys.
        const oldDepths = itemDepthsRef.current
        itemDepthsRef.current = new Map(
            items.map(backendModule.getAssetId).flatMap(key => {
                const depth = oldDepths.get(key)
                return depth != null ? [[key, depth]] : []
            })
        )
    }, [items])

    const expandedDirectoriesRef = React.useRef(new Set<backendModule.DirectoryId>())
    const doToggleDirectoryExpansion = React.useCallback(
        (directory: backendModule.DirectoryAsset, key: backendModule.AssetId) => {
            const set = expandedDirectoriesRef.current
            if (set.has(directory.id)) {
                set.delete(directory.id)
                const foldersToCollapse = new Set([directory.id])
                setItems(
                    items.filter(item => {
                        const shouldKeep = !foldersToCollapse.has(item.parentId)
                        if (item.type === backendModule.AssetType.directory && !shouldKeep) {
                            foldersToCollapse.add(item.id)
                        }
                        return shouldKeep
                    })
                )
            } else {
                const childDepth = getDepth(key) + 1
                set.add(directory.id)
                const loadingAssetId = backendModule.LoadingAssetId(uniqueString.uniqueString())
                itemDepthsRef.current.set(loadingAssetId, childDepth)
                setItems(
                    array.splicedAfter(
                        items,
                        [
                            {
                                type: backendModule.AssetType.specialLoading,
                                title: '',
                                id: loadingAssetId,
                                modifiedAt: dateTime.toRfc3339(new Date()),
                                parentId: directory.id,
                                permissions: [],
                                projectState: null,
                            },
                        ],
                        item => item.id === directory.id
                    )
                )
                void (async () => {
                    const returnedItems = await backend.listDirectory(
                        { parentId: directory.id },
                        directory.title
                    )
                    const childItems: backendModule.AnyAsset[] =
                        returnedItems.length !== 0
                            ? returnedItems
                            : [
                                  {
                                      type: backendModule.AssetType.specialEmpty,
                                      title: '',
                                      id: backendModule.EmptyAssetId(uniqueString.uniqueString()),
                                      modifiedAt: dateTime.toRfc3339(new Date()),
                                      parentId: directory.id,
                                      permissions: [],
                                      projectState: null,
                                  },
                              ]
                    for (const childItem of childItems) {
                        itemDepthsRef.current.set(childItem.id, childDepth)
                    }
                    setItems(oldItems =>
                        array.splicedReplacing(
                            oldItems,
                            childItems,
                            item => item.id === loadingAssetId
                        )
                    )
                })()
            }
        },
        [items, backend, getDepth]
    )

    const getNewProjectName = React.useCallback(
        (templateId?: string | null) => {
            const prefix = `${templateId ?? 'New_Project'}_`
            const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
            const projectIndices = items
                .map(project => projectNameTemplate.exec(project.title)?.groups?.projectIndex)
                .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
            return `${prefix}${Math.max(0, ...projectIndices) + 1}`
        },
        [items]
    )

    hooks.useEventHandler(assetListEvents, event => {
        switch (event.type) {
            case assetListEventModule.AssetListEventType.createDirectory: {
                const directoryIndices = items
                    .map(item => DIRECTORY_NAME_REGEX.exec(item.title))
                    .map(match => match?.groups?.directoryIndex)
                    .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
                const title = `${DIRECTORY_NAME_DEFAULT_PREFIX}${
                    Math.max(0, ...directoryIndices) + 1
                }`
                const placeholderItem: backendModule.DirectoryAsset = {
                    id: backendModule.DirectoryId(uniqueString.uniqueString()),
                    title,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId ?? backendModule.DirectoryId(''),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization),
                    projectState: null,
                    type: backendModule.AssetType.directory,
                }
                const typeOrder = backendModule.ASSET_TYPE_ORDER[placeholderItem.type]
                setItems(oldItems =>
                    array.splicedBefore(
                        oldItems,
                        [placeholderItem],
                        item =>
                            item.parentId === event.parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= typeOrder
                    )
                )
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.createDirectory,
                    placeholderId: placeholderItem.id,
                })
                break
            }
            case assetListEventModule.AssetListEventType.createProject: {
                const projectName = getNewProjectName(event.templateId)
                const dummyId = backendModule.ProjectId(uniqueString.uniqueString())
                const placeholderItem: backendModule.ProjectAsset = {
                    id: dummyId,
                    title: projectName,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId ?? backendModule.DirectoryId(''),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization),
                    projectState: { type: backendModule.ProjectState.placeholder },
                    type: backendModule.AssetType.project,
                }
                const typeOrder = backendModule.ASSET_TYPE_ORDER[placeholderItem.type]
                setItems(oldItems =>
                    array.splicedBefore(
                        oldItems,
                        [placeholderItem],
                        item =>
                            item.parentId === event.parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= typeOrder
                    )
                )
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.createProject,
                    placeholderId: dummyId,
                    templateId: event.templateId,
                    onSpinnerStateChange: event.onSpinnerStateChange,
                })
                break
            }
            case assetListEventModule.AssetListEventType.uploadFiles: {
                const reversedFiles = Array.from(event.files).reverse()
                const placeholderFiles: backendModule.FileAsset[] = reversedFiles
                    .filter(backendModule.fileIsNotProject)
                    .map(file => ({
                        type: backendModule.AssetType.file,
                        id: backendModule.FileId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId: event.parentId ?? backendModule.DirectoryId(''),
                        permissions: permissions.tryGetSingletonOwnerPermission(organization),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: null,
                    }))
                const placeholderProjects: backendModule.ProjectAsset[] = reversedFiles
                    .filter(backendModule.fileIsProject)
                    .map(file => ({
                        type: backendModule.AssetType.project,
                        id: backendModule.ProjectId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId: event.parentId ?? backendModule.DirectoryId(''),
                        permissions: permissions.tryGetSingletonOwnerPermission(organization),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: {
                            type: backendModule.ProjectState.new,
                        },
                    }))
                const fileTypeOrder = backendModule.ASSET_TYPE_ORDER[backendModule.AssetType.file]
                const projectTypeOrder =
                    backendModule.ASSET_TYPE_ORDER[backendModule.AssetType.project]
                setItems(oldItems => {
                    const ret = array.spliceBefore(
                        array.splicedBefore(
                            oldItems,
                            placeholderFiles,
                            item =>
                                item.parentId === event.parentId &&
                                backendModule.ASSET_TYPE_ORDER[item.type] >= fileTypeOrder
                        ),
                        placeholderProjects,
                        item =>
                            item.parentId === event.parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= projectTypeOrder
                    )
                    return ret
                })
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.uploadFiles,
                    files: new Map(
                        [...placeholderFiles, ...placeholderProjects].map((placeholderItem, i) => [
                            placeholderItem.id,
                            // This is SAFE, as `placeholderItems` is created using a map on
                            // `event.files`.
                            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                            event.files[i]!,
                        ])
                    ),
                })
                break
            }
            case assetListEventModule.AssetListEventType.createSecret: {
                const placeholderItem: backendModule.SecretAsset = {
                    id: backendModule.SecretId(uniqueString.uniqueString()),
                    title: event.name,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: event.parentId ?? backendModule.DirectoryId(''),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization),
                    projectState: null,
                    type: backendModule.AssetType.secret,
                }
                const typeOrder = backendModule.ASSET_TYPE_ORDER[placeholderItem.type]
                setItems(oldItems =>
                    array.splicedBefore(
                        oldItems,
                        [placeholderItem],
                        item =>
                            item.parentId === event.parentId &&
                            backendModule.ASSET_TYPE_ORDER[item.type] >= typeOrder
                    )
                )
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.createSecret,
                    placeholderId: placeholderItem.id,
                    value: event.value,
                })
                break
            }
            case assetListEventModule.AssetListEventType.delete: {
                setItems(oldItems => oldItems.filter(item => item.id !== event.id))
                break
            }
        }
    })

    const doOpenManually = React.useCallback(
        (projectId: backendModule.ProjectId) => {
            dispatchAssetEvent({
                type: assetEventModule.AssetEventType.openProject,
                id: projectId,
            })
        },
        [/* should never change */ dispatchAssetEvent]
    )

    const doCloseIde = React.useCallback(() => {
        dispatchAssetEvent({
            type: assetEventModule.AssetEventType.cancelOpeningAllProjects,
        })
        rawDoCloseIde()
    }, [rawDoCloseIde, /* should never change */ dispatchAssetEvent])

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): AssetsTableState => ({
            appRunner,
            assetEvents,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            getDepth,
            doToggleDirectoryExpansion,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        }),
        [
            appRunner,
            assetEvents,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            getDepth,
            doToggleDirectoryExpansion,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    return (
        <div className="flex-1 overflow-auto">
            <div className="flex flex-col w-min min-w-full">
                <div className="h-0">
                    <div className="block sticky right-0 px-2 py-1 ml-auto mt-3 w-29 z-10">
                        <div className="inline-flex gap-3">
                            {columnModule.EXTRA_COLUMNS.map(column => (
                                <Button
                                    key={column}
                                    active={extraColumns.has(column)}
                                    image={columnModule.EXTRA_COLUMN_IMAGES[column]}
                                    onClick={() => {
                                        const newExtraColumns = new Set(extraColumns)
                                        if (extraColumns.has(column)) {
                                            newExtraColumns.delete(column)
                                        } else {
                                            newExtraColumns.add(column)
                                        }
                                        setExtraColumns(newExtraColumns)
                                    }}
                                />
                            ))}
                        </div>
                    </div>
                </div>
                <Table<
                    backendModule.AnyAsset,
                    AssetsTableState,
                    AssetRowState,
                    backendModule.AssetId
                >
                    rowComponent={AssetRow}
                    items={visibleItems}
                    isLoading={isLoading}
                    state={state}
                    initialRowState={INITIAL_ROW_STATE}
                    getKey={backendModule.getAssetId}
                    placeholder={PLACEHOLDER}
                    columns={columnModule.getColumnList(backend.type, extraColumns).map(column => ({
                        id: column,
                        className: columnModule.COLUMN_CSS_CLASS[column],
                        heading: columnModule.COLUMN_HEADING[column],
                        render: columnModule.COLUMN_RENDERER[column],
                    }))}
                    onContextMenu={(selectedKeys, event, setSelectedKeys) => {
                        event.preventDefault()
                        event.stopPropagation()
                        const pluralized = pluralize(selectedKeys.size)
                        // This is not a React component even though it contains JSX.
                        // eslint-disable-next-line no-restricted-syntax
                        const doDeleteAll = () => {
                            setModal(
                                <ConfirmDeleteModal
                                    description={`${selectedKeys.size} selected ${pluralized}`}
                                    doDelete={() => {
                                        setSelectedKeys(new Set())
                                        dispatchAssetEvent({
                                            type: assetEventModule.AssetEventType.deleteMultiple,
                                            ids: selectedKeys,
                                        })
                                        return Promise.resolve()
                                    }}
                                />
                            )
                        }
                        setModal(
                            <ContextMenu key={uniqueString.uniqueString()} event={event}>
                                <ContextMenuEntry onClick={doDeleteAll}>
                                    <span className="text-red-700">
                                        Delete {selectedKeys.size} {pluralized}
                                    </span>
                                </ContextMenuEntry>
                            </ContextMenu>
                        )
                    }}
                />
            </div>
        </div>
    )
}
