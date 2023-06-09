/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import Table, * as table from './table'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'

// ============================
// === DirectoryNameHeading ===
// ============================

/** Props for a {@link DirectoryNameHeading}. */
export interface DirectoryNameHeadingProps {
    doCreateDirectory: () => void
}

/** The column header for the "name" column for the table of directory assets. */
function DirectoryNameHeading(props: DirectoryNameHeadingProps) {
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
export interface DirectoryNamePropsState {
    onRename: () => void
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
}

/** Props for a {@link DirectoryName}. */
export interface DirectoryNameProps
    extends table.ColumnProps<backendModule.DirectoryAsset, DirectoryNamePropsState> {}

/** The icon and name of a specific directory asset. */
function DirectoryName(props: DirectoryNameProps) {
    const {
        item,
        selected,
        state: { onRename, enterDirectory },
    } = props
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    // TODO: Wait for backend implementation.
    const doRename = async (/* _newName: string */) => {
        onRename()
        return await Promise.resolve(null)
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
                onBlur={async event => {
                    setIsNameEditable(false)
                    if (event.target.value === item.title) {
                        toast.success('The folder name is unchanged.')
                    } else {
                        await doRename(/* event.target.value */)
                    }
                }}
                onCancel={() => {
                    setIsNameEditable(false)
                }}
                className="px-2 bg-transparent"
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
    onAssetClick: (
        asset: backendModule.DirectoryAsset,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
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
        onAssetClick,
        enterDirectory,
    } = props
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
                placeholder={
                    <span className="opacity-75">
                        This directory does not contain any subdirectories
                        {query ? ' matching your query' : ''}.
                    </span>
                }
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
                onContextMenu={(_directories, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    setModal(
                        <ContextMenu key={backendModule.AssetType.directory} event={event}>
                            <ContextMenuEntry
                                disabled
                                onClick={() => {
                                    // Ignored.
                                }}
                            >
                                No actions available
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
                onRowClick={onAssetClick}
                onRowContextMenu={(directory, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    setModal(
                        <ContextMenu key={directory.id} event={event}>
                            <ContextMenuEntry
                                disabled
                                onClick={() => {
                                    // Ignored.
                                }}
                            >
                                No actions available
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
            />
        )
    }
}

export default DirectoriesTable
