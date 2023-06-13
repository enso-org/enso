/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as error from '../../error'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'
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

/** Placeholder row when the search query is not empty. */
const PLACEHOLDER_WITH_QUERY = (
    <span className="opacity-75">
        This folder does not contain any subfolders matching your query.
    </span>
)
/** Placeholder row when the search query is empty. */
const PLACEHOLDER_WITHOUT_QUERY = (
    <span className="opacity-75">This folder does not contain any subfolders.</span>
)
/** Messages to be passed to {@link toastPromiseMultiple.toastPromiseMultiple}. */
const TOAST_PROMISE_MULTIPLE_MESSAGES: toastPromiseMultiple.ToastPromiseMultipleMessages<backendModule.DirectoryAsset> =
    {
        begin: expectedCount =>
            `Deleting ${expectedCount} ${expectedCount === 1 ? 'folder' : 'folders'}...`,
        inProgress: (successCount, expectedCount) =>
            `Deleted ${successCount}/${expectedCount} ${
                expectedCount === 1 ? 'folder' : 'folders'
            }.`,
        end: (successCount, expectedCount) =>
            `Deleted ${successCount}/${expectedCount} ${
                expectedCount === 1 ? 'folder' : 'folders'
            }.`,
        error: directory => `Could not delete folder '${directory.title}'.`,
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
                {svg.ADD_ICON}
            </button>
        </div>
    )
}

// =====================
// === DirectoryName ===
// =====================

/** State passed through from a {@link DirectoriesTable} to every cell. */
interface DirectoryNamePropsState {
    onRename: () => void
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
}

/** Props for a {@link DirectoryName}. */
interface InternalDirectoryNameProps
    extends table.ColumnProps<backendModule.DirectoryAsset, DirectoryNamePropsState> {}

/** The icon and name of a specific directory asset. */
function DirectoryName(props: InternalDirectoryNameProps) {
    const {
        item,
        selected,
        state: { onRename, enterDirectory },
    } = props
    const { backend } = backendProvider.useBackend()
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    // TODO: Wait for backend implementation.
    const doRename = async (newName: string) => {
        if (backend.type !== backendModule.BackendType.local) {
            await toast.promise(backend.updateDirectory(item.id, { title: newName }), {
                loading: 'Renaming folder...',
                success: 'Renamed folder',
                error: reason => `Error renaming folder: ${error.unsafeIntoErrorMessage(reason)}`,
            })
        }
        onRename()
    }

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
            onClick={event => {
                if (
                    event.detail === 1 &&
                    (selected ||
                        (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey))
                ) {
                    setIsNameEditable(true)
                } else if (event.detail === 2) {
                    enterDirectory(item)
                }
            }}
        >
            {svg.DIRECTORY_ICON}
            <EditableSpan
                editable={isNameEditable}
                onSubmit={async newTitle => {
                    setIsNameEditable(false)
                    if (newTitle !== item.title) {
                        // Mutation is UNSAFE as it does not cause a re-render.
                        // However, `setIsNameEditable` causes a re-render, and
                        // using a `useState` is not an option as it will not get overwritten by
                        // the updated value from the server.
                        item.title = newTitle
                        await doRename(newTitle)
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
    items: backendModule.DirectoryAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    doCreateDirectory: () => void
    onRename: () => void
    onDelete: () => void
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
}

/** The table of directory assets. */
function DirectoriesTable(props: DirectoriesTableProps) {
    const {
        items,
        isLoading,
        columnDisplayMode,
        query,
        doCreateDirectory,
        onRename,
        onDelete,
        enterDirectory,
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<backendModule.DirectoryAsset, DirectoryNamePropsState>
                items={items}
                isLoading={isLoading}
                state={{ enterDirectory, onRename }}
                getKey={backendModule.getAssetId}
                placeholder={query ? PLACEHOLDER_WITH_QUERY : PLACEHOLDER_WITHOUT_QUERY}
                columns={columnModule.columnsFor(columnDisplayMode, backend.type).map(column =>
                    column === columnModule.Column.name
                        ? {
                              id: column,
                              className: columnModule.COLUMN_CSS_CLASS[column],
                              heading: (
                                  <DirectoryNameHeading doCreateDirectory={doCreateDirectory} />
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
                onContextMenu={(directories, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    // This is not a React component even though it contains JSX.
                    // eslint-disable-next-line no-restricted-syntax
                    const doDeleteAll = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={`${directories.size} selected folders`}
                                assetType="folders"
                                shouldShowToast={false}
                                doDelete={async () => {
                                    await toastPromiseMultiple.toastPromiseMultiple(
                                        logger,
                                        [...directories],
                                        directory => backend.deleteDirectory(directory.id),
                                        TOAST_PROMISE_MULTIPLE_MESSAGES
                                    )
                                }}
                                onSuccess={onDelete}
                            />
                        )
                    }
                    const directoriesText = directories.size === 1 ? 'folder' : 'folders'
                    setModal(
                        <ContextMenu key={uniqueString.uniqueString()} event={event}>
                            <ContextMenuEntry onClick={doDeleteAll}>
                                <span className="text-red-700">
                                    Delete {directories.size} {directoriesText}
                                </span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
                onRowContextMenu={(directory, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    // This is not a React component even though it contains JSX.
                    // eslint-disable-next-line no-restricted-syntax
                    const doRename = () => {
                        const innerDoRename = async (newName: string) => {
                            await backend.updateDirectory(directory.id, { title: newName })
                        }
                        setModal(
                            <RenameModal
                                name={directory.title}
                                assetType={directory.type}
                                doRename={innerDoRename}
                                onSuccess={onRename}
                            />
                        )
                    }
                    // This is not a React component even though it contains JSX.
                    // eslint-disable-next-line no-restricted-syntax
                    const doDelete = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={directory.title}
                                assetType={directory.type}
                                doDelete={() => backend.deleteDirectory(directory.id)}
                                onSuccess={onDelete}
                            />
                        )
                    }
                    setModal(
                        <ContextMenu key={directory.id} event={event}>
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
