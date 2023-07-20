/** @file Table displaying a list of directories. */
import * as React from 'react'
import toast from 'react-hot-toast'

import DirectoryIcon from 'enso-assets/directory.svg'
import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as directoryEventModule from '../events/directoryEvent'
import * as directoryListEventModule from '../events/directoryListEvent'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as permissions from '../permissions'
import * as presence from '../presence'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
import * as uniqueString from '../../uniqueString'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import * as tableColumn from './tableColumn'
import TableRow, * as tableRow from './tableRow'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'
import Table from './table'

// =================
// === Constants ===
// =================

/** The {@link RegExp} matching a directory name following the default naming convention. */
const DIRECTORY_NAME_REGEX = /^New_Folder_(?<directoryIndex>\d+)$/
/** The default prefix of an automatically generated directory. */
const DIRECTORY_NAME_DEFAULT_PREFIX = 'New_Folder_'
/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'folder'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'folders'
// This is a function, even though it is not syntactically a function.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)
/** Placeholder row when the search query is not empty. */
const PLACEHOLDER_WITH_FILTER = (
    <span className="opacity-75">
        This folder does not contain any sub{ASSET_TYPE_NAME_PLURAL} matching your query.
    </span>
)
/** Placeholder row when the search query is empty. */
const PLACEHOLDER_WITHOUT_FILTER = (
    <span className="opacity-75">
        This folder does not contain any sub{ASSET_TYPE_NAME_PLURAL}.
    </span>
)

// ============================
// === DirectoryNameHeading ===
// ============================

/** Props for a {@link DirectoryNameHeading}. */
interface InternalDirectoryNameHeadingProps {
    doCreateDirectory: () => void
}

/** The column header for the "name" column for the table of directory assets. */
function DirectoryNameHeading(props: InternalDirectoryNameHeadingProps) {
    const { doCreateDirectory } = props

    return (
        <div className="inline-flex">
            {string.capitalizeFirst(ASSET_TYPE_NAME_PLURAL)}
            <button
                className="mx-1"
                onClick={event => {
                    event.stopPropagation()
                    doCreateDirectory()
                }}
            >
                <img src={PlusIcon} />
            </button>
        </div>
    )
}

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryName}. */
interface InternalDirectoryNameProps
    extends tableColumn.TableColumnProps<
        backendModule.DirectoryAsset,
        DirectoriesTableState,
        DirectoryRowState
    > {}

/** The icon and name of a specific directory asset. */
function DirectoryName(props: InternalDirectoryNameProps) {
    const {
        item,
        setItem,
        selected,
        state: { enterDirectory },
        rowState,
        setRowState,
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()

    const doRename = async (newName: string) => {
        if (backend.type !== backendModule.BackendType.local) {
            try {
                await backend.updateDirectory(item.id, { title: newName }, item.title)
                return
            } catch (error) {
                const message = `Error renaming folder: ${
                    errorModule.tryGetMessage(error) ?? 'unknown error'
                }`
                toast.error(message)
                logger.error(message)
                throw error
            }
        }
    }

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
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
                    enterDirectory(item)
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

// ===============================
// === DirectoryRowContextMenu ===
// ===============================

/** Props for a {@link DirectoryRowContextMenu}. */
interface InternalDirectoryRowContextMenuProps {
    innerProps: tableRow.TableRowInnerProps<
        backendModule.DirectoryAsset,
        backendModule.DirectoryId,
        DirectoryRowState
    >
    event: React.MouseEvent
    doDelete: () => Promise<void>
}

/** The context menu for a row of a {@link DirectorysTable}. */
function DirectoryRowContextMenu(props: InternalDirectoryRowContextMenuProps) {
    const {
        innerProps: { item, setRowState },
        event,
        doDelete,
    } = props
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const doRename = () => {
        setRowState(oldRowState => ({
            ...oldRowState,
            isEditingName: true,
        }))
        unsetModal()
    }
    return (
        <ContextMenu key={item.id} event={event}>
            <ContextMenuEntry onClick={doRename}>Rename</ContextMenuEntry>
            <ContextMenuEntry
                onClick={() => {
                    setModal(
                        <ConfirmDeleteModal
                            description={`the ${ASSET_TYPE_NAME} '${item.title}'`}
                            doDelete={doDelete}
                        />
                    )
                }}
            >
                <span className="text-red-700">Delete</span>
            </ContextMenuEntry>
        </ContextMenu>
    )
}

// ====================
// === DirectoryRow ===
// ====================

/** A row in a {@link DirectoriesTable}. */
function DirectoryRow(
    props: tableRow.TableRowProps<
        backendModule.DirectoryAsset,
        backendModule.DirectoryId,
        DirectoriesTableState,
        DirectoryRowState
    >
) {
    const {
        keyProp: key,
        item: rawItem,
        state: { directoryEvent, dispatchDirectoryListEvent, markItemAsHidden, markItemAsVisible },
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [item, setItem] = React.useState(rawItem)
    const [status, setStatus] = React.useState(presence.Presence.present)

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])

    const doDelete = async () => {
        if (backend.type !== backendModule.BackendType.local) {
            setStatus(presence.Presence.deleting)
            markItemAsHidden(key)
            try {
                await backend.deleteDirectory(item.id, item.title)
                dispatchDirectoryListEvent({
                    type: directoryListEventModule.DirectoryListEventType.delete,
                    directoryId: key,
                })
            } catch (error) {
                setStatus(presence.Presence.present)
                markItemAsVisible(key)
                const message = `Unable to delete directory: ${
                    errorModule.tryGetMessage(error) ?? 'unknown error.'
                }`
                toast.error(message)
                logger.error(message)
            }
        }
    }

    hooks.useEventHandler(directoryEvent, async event => {
        switch (event.type) {
            case directoryEventModule.DirectoryEventType.create: {
                if (key === event.placeholderId) {
                    if (backend.type !== backendModule.BackendType.remote) {
                        const message = 'Folders cannot be created on the local backend.'
                        toast.error(message)
                        logger.error(message)
                    } else {
                        setStatus(presence.Presence.inserting)
                        try {
                            const createdDirectory = await backend.createDirectory({
                                parentId: item.parentId,
                                title: item.title,
                            })
                            setStatus(presence.Presence.present)
                            const newItem: backendModule.DirectoryAsset = {
                                ...item,
                                ...createdDirectory,
                            }
                            setItem(newItem)
                        } catch (error) {
                            dispatchDirectoryListEvent({
                                type: directoryListEventModule.DirectoryListEventType.delete,
                                directoryId: key,
                            })
                            const message = `Error creating new folder: ${
                                errorModule.tryGetMessage(error) ?? 'unknown error.'
                            }`
                            toast.error(message)
                            logger.error(message)
                        }
                    }
                }
                break
            }
            case directoryEventModule.DirectoryEventType.deleteMultiple: {
                if (event.directoryIds.has(key)) {
                    await doDelete()
                }
                break
            }
        }
    })

    return (
        <TableRow
            className={presence.CLASS_NAME[status]}
            {...props}
            onContextMenu={(innerProps, event) => {
                event.preventDefault()
                event.stopPropagation()
                setModal(
                    <DirectoryRowContextMenu
                        innerProps={innerProps}
                        event={event}
                        doDelete={doDelete}
                    />
                )
            }}
            item={item}
        />
    )
}

// ========================
// === DirectoriesTable ===
// ========================

/** State passed through from a {@link DirectoriesTable} to every cell. */
interface DirectoriesTableState {
    directoryEvent: directoryEventModule.DirectoryEvent | null
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
    dispatchDirectoryListEvent: (event: directoryListEventModule.DirectoryListEvent) => void
    markItemAsHidden: (key: string) => void
    markItemAsVisible: (key: string) => void
}

/** Data associated with a {@link DirectoryRow}, used for rendering. */
export interface DirectoryRowState {
    isEditingName: boolean
}

/** The default {@link DirectoryRowState} associated with a {@link DirectoryRow}. */
export const INITIAL_ROW_STATE: DirectoryRowState = Object.freeze({
    isEditingName: false,
})

/** Props for a {@link DirectoriesTable}. */
export interface DirectoriesTableProps {
    directoryId: backendModule.DirectoryId | null
    items: backendModule.DirectoryAsset[]
    filter: ((item: backendModule.DirectoryAsset) => boolean) | null
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
}

/** The table of directory assets. */
function DirectoriesTable(props: DirectoriesTableProps) {
    const {
        directoryId,
        items: rawItems,
        filter,
        isLoading,
        columnDisplayMode,
        enterDirectory,
    } = props
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [items, setItems] = React.useState(rawItems)
    const [directoryEvent, dispatchDirectoryEvent] =
        hooks.useEvent<directoryEventModule.DirectoryEvent>()
    const [directoryListEvent, dispatchDirectoryListEvent] =
        hooks.useEvent<directoryListEventModule.DirectoryListEvent>()

    React.useEffect(() => {
        setItems(rawItems)
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

    const createNewDirectory = React.useCallback(() => {
        dispatchDirectoryListEvent({
            type: directoryListEventModule.DirectoryListEventType.create,
        })
    }, [/* should never change */ dispatchDirectoryListEvent])

    hooks.useEventHandler(directoryListEvent, event => {
        switch (event.type) {
            case directoryListEventModule.DirectoryListEventType.create: {
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
                    parentId: directoryId ?? backendModule.DirectoryId(''),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization),
                    projectState: null,
                    type: backendModule.AssetType.directory,
                }
                setItems(oldItems => [placeholderItem, ...oldItems])
                dispatchDirectoryEvent({
                    type: directoryEventModule.DirectoryEventType.create,
                    placeholderId: placeholderItem.id,
                })
                break
            }
            case directoryListEventModule.DirectoryListEventType.delete: {
                setItems(oldItems => oldItems.filter(item => item.id !== event.directoryId))
                break
            }
        }
    })

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): DirectoriesTableState => ({
            directoryEvent,
            enterDirectory,
            dispatchDirectoryListEvent,
            markItemAsHidden,
            markItemAsVisible,
        }),
        [
            directoryEvent,
            enterDirectory,
            dispatchDirectoryListEvent,
            markItemAsHidden,
            markItemAsVisible,
        ]
    )

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<
                backendModule.DirectoryAsset,
                backendModule.DirectoryId,
                DirectoriesTableState,
                DirectoryRowState
            >
                rowComponent={DirectoryRow}
                items={visibleItems}
                isLoading={isLoading}
                state={state}
                initialRowState={INITIAL_ROW_STATE}
                getKey={backendModule.getAssetId}
                placeholder={filter != null ? PLACEHOLDER_WITH_FILTER : PLACEHOLDER_WITHOUT_FILTER}
                forceShowPlaceholder={shouldForceShowPlaceholder}
                columns={columnModule.columnsFor(columnDisplayMode, backend.type).map(column =>
                    column === columnModule.Column.name
                        ? {
                              id: column,
                              className: columnModule.COLUMN_CSS_CLASS[column],
                              heading: (
                                  <DirectoryNameHeading doCreateDirectory={createNewDirectory} />
                              ),
                              render: DirectoryName,
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
                    const doDeleteAll = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={
                                    `${selectedKeys.size} selected ` + ASSET_TYPE_NAME_PLURAL
                                }
                                doDelete={() => {
                                    dispatchDirectoryEvent({
                                        type: directoryEventModule.DirectoryEventType
                                            .deleteMultiple,
                                        directoryIds: selectedKeys,
                                    })
                                    setSelectedKeys(new Set())
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
}

export default DirectoriesTable
