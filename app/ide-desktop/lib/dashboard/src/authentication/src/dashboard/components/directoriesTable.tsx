/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import DirectoryIcon from 'enso-assets/directory.svg'
import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as directoryListEventModule from '../events/directoryListEvent'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
import * as toastPromise from '../toastPromise'
import * as toastPromiseMultiple from '../../toastPromiseMultiple'
import * as uniqueString from '../../uniqueString'

import Table, * as table from './table'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'
import RenameModal from './renameModal'

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
/** Messages to be passed to {@link toastPromiseMultiple.toastPromiseMultiple}. */
const TOAST_PROMISE_MULTIPLE_MESSAGES: toastPromiseMultiple.ToastPromiseMultipleMessages<backendModule.DirectoryAsset> =
    {
        begin: total => `Deleting ${total} ${pluralize(total)}...`,
        inProgress: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        end: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        error: asset => `Could not delete ${ASSET_TYPE_NAME} '${asset.title}'.`,
    }

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
            Folder
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

/** State passed through from a {@link DirectoriesTable} to every cell. */
interface DirectoryNamePropsState {
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
}

/** Props for a {@link DirectoryName}. */
interface InternalDirectoryNameProps
    extends table.ColumnProps<backendModule.DirectoryAsset, DirectoryNamePropsState> {}

/** The icon and name of a specific directory asset. */
function DirectoryName(props: InternalDirectoryNameProps) {
    const {
        item,
        setItem,
        selected,
        state: { enterDirectory },
    } = props
    const { backend } = backendProvider.useBackend()
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    const doRename = async (newName: string) => {
        if (backend.type !== backendModule.BackendType.local) {
            await toastPromise.toastPromise(
                backend.updateDirectory(item.id, { title: newName }, item.title),
                {
                    loading: 'Renaming folder...',
                    success: 'Renamed folder',
                    error: error =>
                        `Error renaming folder: ${
                            errorModule.tryGetMessage(error) ?? 'unknown error'
                        }`,
                }
            )
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
                    setIsNameEditable(true)
                } else if (eventModule.isDoubleClick(event)) {
                    enterDirectory(item)
                }
            }}
        >
            <img src={DirectoryIcon} />
            <EditableSpan
                editable={isNameEditable}
                onSubmit={async newTitle => {
                    setIsNameEditable(false)
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
                    setIsNameEditable(false)
                }}
                className="cursor-pointer bg-transparent grow px-2"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}

// =====================
// === DirectoryRows ===
// =====================

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
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    const [items, setItems] = React.useState(rawItems)
    const [directoryListEvent, dispatchDirectoryListEvent] =
        React.useState<directoryListEventModule.DirectoryListEvent | null>(null)

    React.useEffect(() => {
        setItems(rawItems)
    }, [rawItems])

    const visibleItems = React.useMemo(
        () => (filter != null ? items.filter(filter) : items),
        [items, filter]
    )

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): DirectoryNamePropsState => ({ enterDirectory }),
        [enterDirectory]
    )

    const createNewDirectory = React.useCallback(() => {
        dispatchDirectoryListEvent({
            type: directoryListEventModule.DirectoryListEventType.create,
        })
    }, [])

    hooks.useEvent(directoryListEvent, async event => {
        switch (event.type) {
            case directoryListEventModule.DirectoryListEventType.create: {
                if (backend.type !== backendModule.BackendType.remote) {
                    const message = 'Folders cannot be created on the local backend.'
                    toast.error(message)
                    logger.error(message)
                } else {
                    const directoryIndices = items
                        .map(item => DIRECTORY_NAME_REGEX.exec(item.title))
                        .map(match => match?.groups?.directoryIndex)
                        .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
                    const title = `${DIRECTORY_NAME_DEFAULT_PREFIX}${
                        Math.max(...directoryIndices) + 1
                    }`
                    const placeholderItem: backendModule.DirectoryAsset = {
                        title,
                        type: backendModule.AssetType.directory,
                        id: backendModule.DirectoryId(uniqueString.uniqueString()),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        parentId: directoryId ?? backendModule.DirectoryId(''),
                        permissions: [],
                        projectState: null,
                    }
                    setItems(oldItems => [placeholderItem, ...oldItems])
                    const createDirectoryPromise = backend.createDirectory({
                        parentId: directoryId,
                        title,
                    })
                    const createdDirectory = await toastPromise.toastPromise(
                        createDirectoryPromise,
                        {
                            loading: 'Creating folder...',
                            success: 'Sucessfully created folder.',
                            // This is UNSAFE, as the original function's parameter is
                            // of type `any`.
                            error: (promiseError: Error) =>
                                `Error creating new folder: ${promiseError.message}`,
                        }
                    )
                    const newItem: backendModule.DirectoryAsset = {
                        ...placeholderItem,
                        ...createdDirectory,
                    }
                    setItems(oldItems =>
                        oldItems.map(item => (item !== placeholderItem ? item : newItem))
                    )
                }
                break
            }
            case directoryListEventModule.DirectoryListEventType.delete: {
                setItems(oldItems => oldItems.filter(item => item.id !== event.directoryId))
                break
            }
        }
    })

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<backendModule.DirectoryAsset, DirectoryNamePropsState>
                items={visibleItems}
                isLoading={isLoading}
                state={state}
                getKey={backendModule.getAssetId}
                placeholder={filter != null ? PLACEHOLDER_WITH_FILTER : PLACEHOLDER_WITHOUT_FILTER}
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
                onContextMenu={(directories, event, setSelectedItems) => {
                    event.preventDefault()
                    event.stopPropagation()
                    const doDeleteAll = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={`${directories.size} selected folders`}
                                assetType="folders"
                                doDelete={async () => {
                                    setSelectedItems(new Set())
                                    await toastPromiseMultiple.toastPromiseMultiple(
                                        logger,
                                        [...directories],
                                        directory =>
                                            backend.deleteDirectory(directory.id, directory.title),
                                        TOAST_PROMISE_MULTIPLE_MESSAGES
                                    )
                                }}
                            />
                        )
                    }
                    const pluralized = pluralize(directories.size)
                    setModal(
                        <ContextMenu key={uniqueString.uniqueString()} event={event}>
                            <ContextMenuEntry onClick={doDeleteAll}>
                                <span className="text-red-700">
                                    Delete {directories.size} {pluralized}
                                </span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
                onRowContextMenu={(innerProps, event) => {
                    const { item } = innerProps
                    event.preventDefault()
                    event.stopPropagation()
                    const doRename = () => {
                        const innerDoRename = async (newName: string) => {
                            await backend.updateDirectory(item.id, { title: newName }, item.title)
                        }
                        setModal(
                            <RenameModal
                                name={item.title}
                                assetType={item.type}
                                doRename={innerDoRename}
                            />
                        )
                    }
                    const doDelete = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={item.title}
                                assetType={item.type}
                                doDelete={() => backend.deleteDirectory(item.id, item.title)}
                            />
                        )
                    }
                    setModal(
                        <ContextMenu key={item.id} event={event}>
                            <ContextMenuEntry onClick={doRename}>Rename</ContextMenuEntry>
                            <ContextMenuEntry onClick={doDelete}>
                                <span className="text-red-700">Delete</span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
            />
        )
    }
}

export default DirectoriesTable
