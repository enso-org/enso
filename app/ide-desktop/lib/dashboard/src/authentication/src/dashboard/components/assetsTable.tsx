/** @file Table displaying a list of projects. */
import * as React from 'react'
import toast from 'react-hot-toast'

import PlusIcon from 'enso-assets/plus.svg'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as errorModule from '../../error'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as permissions from '../permissions'
import * as presence from '../presence'
import * as string from '../../string'
import * as uniqueString from '../../uniqueString'

import TableRow, * as tableRow from './tableRow'
import AssetContextMenu from './assetContextMenu'
import AssetName from './assetName'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import Table from './table'

// =================
// === Constants ===
// =================

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

// ===========================
// === ProjectActionButton ===
// ===========================

/** Props for a {@link ProjectActionButton}. */
export interface ProjectActionButtonProps {
    project: backendModule.ProjectAsset
    rowState: AssetRowState
    setRowState: React.Dispatch<React.SetStateAction<AssetRowState>>
    assetEvent: assetEventModule.AssetEvent | null
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    onClose: () => void
    appRunner: AppRunner | null
    openIde: () => void
}

// ==========================
// === ProjectNameHeading ===
// ==========================

/** Props for a {@link ProjectNameHeading}. */
interface InternalProjectNameHeadingProps {
    doCreateProject: () => void
}

/** The column header for the "name" column for the table of project assets. */
function ProjectNameHeading(props: InternalProjectNameHeadingProps) {
    const { doCreateProject } = props

    const onClick = (event: React.MouseEvent) => {
        event.stopPropagation()
        doCreateProject()
    }

    return (
        <div className="inline-flex">
            {string.capitalizeFirst(ASSET_TYPE_NAME_PLURAL)}
            <button className="mx-1" onClick={onClick}>
                <img src={PlusIcon} />
            </button>
        </div>
    )
}

// ================
// === AssetRow ===
// ================

/** Props for an {@link AssetRow}. */
export interface AssetRowProps<T extends backendModule.AnyAsset>
    extends tableRow.TableRowProps<T, T['id'], AssetsTableState, AssetRowState> {}

/** A row containing a {@link backendModule.ProjectAsset}.
 * @throws {Error} when `item` is not a {@link backendModule.ProjectAsset}. */
function AssetRow(props: AssetRowProps<backendModule.AnyAsset>) {
    const {
        keyProp: key,
        item: rawItem,
        state: {
            assetEvent,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            markItemAsHidden,
            markItemAsVisible,
        },
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [item, setItem] = React.useState(rawItem)
    const [status, setStatus] = React.useState(presence.Presence.present)

    if (item.type !== backendModule.AssetType.project) {
        // eslint-disable-next-line no-restricted-syntax
        throw new Error('This component should only be rendered when the item is a project.')
    }

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])

    const doDelete = async () => {
        setStatus(presence.Presence.deleting)
        markItemAsHidden(key)
        try {
            await backend.deleteAsset(item)
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.delete,
                id: key,
            })
        } catch (error) {
            setStatus(presence.Presence.present)
            markItemAsVisible(key)
            const message = errorModule.tryGetMessage(error) ?? 'Unable to delete project.'
            toast.error(message)
            logger.error(message)
        }
    }

    return (
        <TableRow
            className={presence.CLASS_NAME[status]}
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
        />
    )
}

// ===================
// === AssetsTable ===
// ===================

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
    appRunner: AppRunner | null
    assetEvent: assetEventModule.AssetEvent | null
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    markItemAsHidden: (key: string) => void
    markItemAsVisible: (key: string) => void
    toggleDirectoryExpansion: (directory: backendModule.DirectoryAsset) => void
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** Data associated with a {@link AssetRow}, used for rendering. */
export interface AssetRowState {
    isRunning: boolean
    isEditingName: boolean
}

/** The default {@link AssetRowState} associated with a {@link AssetRow}. */
export const INITIAL_ROW_STATE: AssetRowState = Object.freeze({
    isRunning: false,
    isEditingName: false,
})

/** Props for a {@link AssetsTable}. */
export interface AssetsTableProps {
    appRunner: AppRunner | null
    directoryId: backendModule.DirectoryId | null
    items: backendModule.AnyAsset[]
    filter: ((item: backendModule.Asset) => boolean) | null
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    assetEvent: assetEventModule.AssetEvent | null
    dispatchAssetEvent: (event: assetEventModule.AssetEvent) => void
    assetListEvent: assetListEventModule.AssetListEvent | null
    dispatchAssetListEvent: (event: assetListEventModule.AssetListEvent) => void
    doCreateProject: () => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** The table of project assets. */
export default function AssetsTable(props: AssetsTableProps) {
    const {
        appRunner,
        directoryId,
        items: rawItems,
        filter,
        isLoading,
        assetEvent,
        dispatchAssetEvent,
        assetListEvent,
        dispatchAssetListEvent,
        doCreateProject,
        doOpenIde,
        doCloseIde: rawDoCloseIde,
    } = props
    const { organization } = authProvider.useNonPartialUserSession()
    const { setModal } = modalProvider.useSetModal()
    const [items, setItems] = React.useState(rawItems)
    // Items in the root directory have a depth of 0.
    const itemDepthsRef = React.useRef(new WeakMap<backendModule.Asset, number>())
    const expandedDirectoriesRef = React.useRef(new Set<backendModule.DirectoryId>())

    React.useEffect(() => {
        setItems(rawItems)
        for (const item of rawItems) {
            itemDepthsRef.current.set(item, 0)
        }
    }, [rawItems])

    const visibleItems = React.useMemo(
        () => (filter != null ? items.filter(filter) : items),
        [items, filter]
    )

    // === Tracking number of visually hidden items ===

    const [shouldForceShowPlaceholder, setShouldForceShowPlaceholder] = React.useState(false)
    const keysOfHiddenItemsRef = React.useRef(new Set<string>())

    const updateShouldForceShowPlaceholder = React.useCallback(() => {
        setShouldForceShowPlaceholder(
            visibleItems.every(item => keysOfHiddenItemsRef.current.has(item.id))
        )
    }, [visibleItems])

    React.useEffect(updateShouldForceShowPlaceholder, [updateShouldForceShowPlaceholder])

    React.useEffect(() => {
        const oldKeys = keysOfHiddenItemsRef.current
        keysOfHiddenItemsRef.current = new Set(
            items.map(backendModule.getAssetId).filter(key => oldKeys.has(key))
        )
    }, [items])

    const markItemAsHidden = React.useCallback(
        (key: string) => {
            keysOfHiddenItemsRef.current.add(key)
            updateShouldForceShowPlaceholder()
        },
        [updateShouldForceShowPlaceholder]
    )

    const markItemAsVisible = React.useCallback(
        (key: string) => {
            keysOfHiddenItemsRef.current.delete(key)
            updateShouldForceShowPlaceholder()
        },
        [updateShouldForceShowPlaceholder]
    )

    // === End tracking number of visually hidden items ===

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

    hooks.useEventHandler(assetListEvent, event => {
        switch (event.type) {
            case assetListEventModule.AssetListEventType.createProject: {
                const projectName = getNewProjectName(event.templateId)
                const dummyId = backendModule.ProjectId(uniqueString.uniqueString())
                const placeholderItem: backendModule.ProjectAsset = {
                    id: dummyId,
                    title: projectName,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: directoryId ?? backendModule.DirectoryId(''),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization),
                    projectState: { type: backendModule.ProjectState.new },
                    type: backendModule.AssetType.project,
                }
                setItems(oldProjectAssets => [placeholderItem, ...oldProjectAssets])
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.createProject,
                    placeholderId: dummyId,
                    templateId: event.templateId,
                })
                break
            }
            case assetListEventModule.AssetListEventType.uploadFiles: {
                const placeholderItems: backendModule.FileAsset[] = Array.from(event.files)
                    .reverse()
                    .map(file => ({
                        type: backendModule.AssetType.file,
                        id: backendModule.FileId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId: directoryId ?? backendModule.DirectoryId(''),
                        permissions: permissions.tryGetSingletonOwnerPermission(organization),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: null,
                    }))
                setItems(oldItems => [...placeholderItems, ...oldItems])
                dispatchAssetEvent({
                    type: assetEventModule.AssetEventType.uploadFiles,
                    files: new Map(
                        placeholderItems.map((placeholderItem, i) => [
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
            assetEvent,
            dispatchAssetEvent,
            dispatchAssetListEvent,
            markItemAsHidden,
            markItemAsVisible,
            toggleDirectoryExpansion,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        }),
        [
            appRunner,
            assetEvent,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            markItemAsHidden,
            markItemAsVisible,
            toggleDirectoryExpansion,
            /* should never change */ dispatchAssetEvent,
            /* should never change */ dispatchAssetListEvent,
        ]
    )

    return (
        <Table<backendModule.AnyAsset, backendModule.AssetId, AssetsTableState, AssetRowState>
            rowComponent={AssetRow}
            items={visibleItems}
            isLoading={isLoading}
            state={state}
            initialRowState={INITIAL_ROW_STATE}
            getKey={backendModule.getAssetId}
            placeholder={PLACEHOLDER}
            forceShowPlaceholder={shouldForceShowPlaceholder}
            columns={columns.map(column =>
                column === columnModule.Column.name
                    ? {
                          id: column,
                          className: columnModule.COLUMN_CSS_CLASS[column],
                          heading: <ProjectNameHeading doCreateProject={doCreateProject} />,
                          render: AssetName,
                      }
                    : {
                          id: column,
                          className: columnModule.COLUMN_CSS_CLASS[column],
                          heading: <>{columnModule.COLUMN_NAME[column]}</>,
                          render: columnModule.COLUMN_RENDERER[column],
                      }
            )}
            onContextMenu={(selectedKeys, event, setSelectedKeys) => {
                event.preventDefault()
                event.stopPropagation()
                // This is not a React component even though it contains JSX.
                // eslint-disable-next-line no-restricted-syntax
                const doDeleteAll = () => {
                    setModal(
                        <ConfirmDeleteModal
                            description={`${selectedKeys.size} selected projects`}
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
                const pluralized = pluralize(selectedKeys.size)
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
    )
}
