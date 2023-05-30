/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as error from '../../error'
import * as fileInfo from '../../fileInfo'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import CreateForm, * as createForm from './createForm'
import Table, * as table from './table'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'

// ======================
// === FileCreateForm ===
// ======================

/** Props for a {@link FileCreateForm}. */
export interface FileCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

/** A form to create a new file asset. */
function FileCreateForm(props: FileCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const [name, setName] = React.useState<string | null>(null)
    const [file, setFile] = React.useState<File | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = async (event: React.FormEvent) => {
            event.preventDefault()
            if (file == null) {
                // TODO[sb]: Uploading a file may be a mistake when creating a new file.
                toast.error('Please select a file to upload.')
            } else {
                unsetModal()
                await toast
                    .promise(
                        backend.uploadFile(
                            {
                                parentDirectoryId: directoryId,
                                fileName: name ?? file.name,
                            },
                            file
                        ),
                        {
                            loading: 'Uploading file...',
                            success: 'Sucessfully uploaded file.',
                            error: error.unsafeIntoErrorMessage,
                        }
                    )
                    .then(onSuccess)
            }
        }

        return (
            <CreateForm title="New File" onSubmit={onSubmit} {...passThrough}>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="file_name">
                        Name
                    </label>
                    <input
                        id="file_name"
                        type="text"
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        onChange={event => {
                            setName(event.target.value)
                        }}
                        defaultValue={name ?? file?.name ?? ''}
                    />
                </div>
                <div className="flex flex-row flex-nowrap m-1">
                    <div className="inline-block flex-1 grow m-1">File</div>
                    <div className="inline-block bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1">
                        <label className="bg-transparent rounded-full w-full" htmlFor="file_file">
                            <div className="inline-block bg-gray-300 hover:bg-gray-400 rounded-l-full px-2 -ml-2">
                                <u>𐌣</u>
                            </div>
                            <div className="inline-block px-2 -mr-2">
                                {file?.name ?? 'No file chosen'}
                            </div>
                        </label>
                        <input
                            id="file_file"
                            type="file"
                            className="hidden"
                            onChange={event => {
                                setFile(event.target.files?.[0] ?? null)
                            }}
                        />
                    </div>
                </div>
            </CreateForm>
        )
    }
}

// =======================
// === FileNameHeading ===
// =======================

/** Props for a {@link FileNameHeading}. */
export interface FileNameHeadingProps {
    directoryId: backendModule.DirectoryId
    onCreate: () => void
}

/** The column header for the "name" column for the table of file assets. */
function FileNameHeading(props: FileNameHeadingProps) {
    const { directoryId, onCreate } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div className="inline-flex">
            File
            <button
                className="mx-1"
                onClick={event => {
                    event.stopPropagation()
                    const buttonPosition = event.currentTarget.getBoundingClientRect()
                    setModal(() => (
                        <FileCreateForm
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

// ================
// === FileName ===
// ================

/** State passed through from a {@link DirectoryRows} to every cell. */
export interface FileNamePropsState {
    onRename: () => void
}

/** Props for a {@link FileName}. */
export interface FileNameProps
    extends table.ColumnProps<backendModule.FileAsset, FileNamePropsState> {}

/** The icon and name of a specific file asset. */
function FileName(props: FileNameProps) {
    const {
        item,
        selected,
        state: { onRename },
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
                }
            }}
        >
            {fileInfo.fileIcon()}
            <EditableSpan
                editable={isNameEditable}
                onBlur={async event => {
                    setIsNameEditable(false)
                    if (event.target.value === item.title) {
                        toast.success('The file name is unchanged.')
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

// ================
// === FileRows ===
// ================

/** Props for a {@link FilesTable}. */
export interface FilesTableProps {
    directoryId: backendModule.DirectoryId
    items: backendModule.FileAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    onCreate: () => void
    onRename: () => void
    onDelete: () => void
    onAssetClick: (
        asset: backendModule.FileAsset,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

/** The table of file assets. */
function FilesTable(props: FilesTableProps) {
    const {
        directoryId,
        items,
        isLoading,
        columnDisplayMode,
        query,
        onCreate,
        onRename,
        onDelete,
        onAssetClick,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<backendModule.FileAsset, FileNamePropsState>
                items={items}
                isLoading={isLoading}
                state={{ onRename }}
                getKey={backendModule.getAssetId}
                placeholder={
                    <span className="opacity-75">
                        This directory does not contain any files
                        {query ? ' matching your query' : ''}.
                    </span>
                }
                columns={columnModule.columnsFor(columnDisplayMode, backend.type).map(column =>
                    column === columnModule.Column.name
                        ? {
                              id: column,
                              className: columnModule.COLUMN_CSS_CLASS[column],
                              heading: (
                                  <FileNameHeading directoryId={directoryId} onCreate={onCreate} />
                              ),
                              render: FileName,
                          }
                        : {
                              id: column,
                              className: columnModule.COLUMN_CSS_CLASS[column],
                              heading: <>{columnModule.COLUMN_NAME[column]}</>,
                              render: columnModule.COLUMN_RENDERER[column],
                          }
                )}
                onClick={onAssetClick}
                onContextMenu={(file, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    const doCopy = () => {
                        /** TODO: Wait for backend endpoint. */
                    }
                    const doCut = () => {
                        /** TODO: Wait for backend endpoint. */
                    }
                    // This is not a React component even though it contains JSX.
                    // eslint-disable-next-line no-restricted-syntax
                    const doDelete = () => {
                        setModal(() => (
                            <ConfirmDeleteModal
                                name={file.title}
                                assetType={file.type}
                                doDelete={() => backend.deleteFile(file.id)}
                                onSuccess={onDelete}
                            />
                        ))
                    }
                    const doDownload = () => {
                        /** TODO: Wait for backend endpoint. */
                    }
                    setModal(() => (
                        <ContextMenu event={event}>
                            <ContextMenuEntry disabled onClick={doCopy}>
                                Copy
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doCut}>
                                Cut
                            </ContextMenuEntry>
                            <ContextMenuEntry onClick={doDelete}>
                                <span className="text-red-700">Delete</span>
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doDownload}>
                                Download
                            </ContextMenuEntry>
                        </ContextMenu>
                    ))
                }}
            />
        )
    }
}

export default FilesTable
