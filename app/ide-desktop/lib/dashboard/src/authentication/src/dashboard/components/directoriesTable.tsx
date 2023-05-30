/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import CreateForm, * as createForm from './createForm'
import Table, * as table from './table'
import ContextMenu from './contextMenu'
import EditableSpan from './editableSpan'

// ===========================
// === DirectoryCreateForm ===
// ===========================

/** Props for a {@link DirectoryCreateForm}. */
export interface DirectoryCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

/** A form to create a new directory asset. */
function DirectoryCreateForm(props: DirectoryCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const [name, setName] = React.useState<string | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = async (event: React.FormEvent) => {
            event.preventDefault()
            if (name == null) {
                toast.error('Please provide a folder name.')
            } else {
                unsetModal()
                await toast
                    .promise(
                        backend.createDirectory({
                            parentId: directoryId,
                            title: name,
                        }),
                        {
                            loading: 'Creating folder...',
                            success: 'Sucessfully created folder.',
                            error: error.unsafeIntoErrorMessage,
                        }
                    )
                    .then(onSuccess)
            }
        }

        return (
            <CreateForm title="New Folder" onSubmit={onSubmit} {...passThrough}>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="directory_name">
                        Name
                    </label>
                    <input
                        id="directory_name"
                        type="text"
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        onChange={event => {
                            setName(event.target.value)
                        }}
                    />
                </div>
            </CreateForm>
        )
    }
}

// ============================
// === DirectoryNameHeading ===
// ============================

/** Props for a {@link DirectoryNameHeading}. */
export interface ProjectNameHeadingProps {
    directoryId: backendModule.DirectoryId
    onCreate: () => void
}

/** The column header for the "name" column for the table of directory assets. */
function DirectoryNameHeading(props: ProjectNameHeadingProps) {
    const { directoryId, onCreate } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div className="inline-flex">
            Folder
            <button
                className="mx-1"
                onClick={event => {
                    event.stopPropagation()
                    const buttonPosition = event.currentTarget.getBoundingClientRect()
                    setModal(() => (
                        <DirectoryCreateForm
                            left={buttonPosition.left + window.scrollX}
                            top={buttonPosition.top + window.scrollY}
                            directoryId={directoryId}
                            onSuccess={onCreate}
                        />
                    ))
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
    const doRename = async (_newName: string) => {
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
                        await doRename(event.target.value)
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
    directoryId: backendModule.DirectoryId
    items: backendModule.DirectoryAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    onCreate: () => void
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
        directoryId,
        items,
        isLoading,
        columnDisplayMode,
        query,
        onCreate,
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
                                  <DirectoryNameHeading
                                      directoryId={directoryId}
                                      onCreate={onCreate}
                                  />
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
                onClick={onAssetClick}
                onContextMenu={(_directory, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    setModal(() => <ContextMenu event={event}></ContextMenu>)
                }}
            />
        )
    }
}

export default DirectoriesTable
