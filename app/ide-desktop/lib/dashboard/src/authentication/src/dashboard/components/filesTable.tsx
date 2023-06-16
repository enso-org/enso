/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as error from '../../error'
import * as fileInfo from '../../fileInfo'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'
import * as toastPromiseMultiple from '../../toastPromiseMultiple'
import * as uniqueString from '../../uniqueString'

import CreateForm, * as createForm from './createForm'
import Table, * as table from './table'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'

// =================
// === Constants ===
// =================

/** Placeholder row when the search query is not empty. */
const PLACEHOLDER_WITH_QUERY = (
    <span className="opacity-75">This folder does not contain any files matching your query.</span>
)
/** Placeholder row when the search query is empty. */
const PLACEHOLDER_WITHOUT_QUERY = (
    <span className="opacity-75">This folder does not contain any files.</span>
)
/** Messages to be passed to {@link toastPromiseMultiple.toastPromiseMultiple}. */
const TOAST_PROMISE_MULTIPLE_MESSAGES: toastPromiseMultiple.ToastPromiseMultipleMessages<backendModule.FileAsset> =
    {
        begin: expectedCount =>
            `Deleting ${expectedCount} ${expectedCount === 1 ? 'file' : 'files'}...`,
        inProgress: (successCount, expectedCount) =>
            `Deleted ${successCount}/${expectedCount} ${expectedCount === 1 ? 'file' : 'files'}.`,
        end: (successCount, expectedCount) =>
            `Deleted ${successCount}/${expectedCount} ${expectedCount === 1 ? 'file' : 'files'}.`,
        error: file => `Could not delete file '${file.title}'.`,
    }

// ======================
// === FileCreateForm ===
// ======================

/** Props for a {@link FileCreateForm}. */
interface InternalFileCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId | null
    onSuccess: () => void
}

/** A form to create a new file asset. */
function FileCreateForm(props: InternalFileCreateFormProps) {
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
                                fileId: null,
                                fileName: name ?? file.name,
                                parentDirectoryId: directoryId,
                            },
                            file
                        ),
                        {
                            loading: 'Uploading file...',
                            success: 'Sucessfully uploaded file.',
                            error: error.tryGetMessage,
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
interface InternalFileNameHeadingProps {
    directoryId: backendModule.DirectoryId | null
    onCreate: () => void
}

/** The column header for the "name" column for the table of file assets. */
function FileNameHeading(props: InternalFileNameHeadingProps) {
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
                    setModal(
                        <FileCreateForm
                            left={buttonPosition.left + window.scrollX}
                            top={buttonPosition.top + window.scrollY}
                            directoryId={directoryId}
                            onSuccess={onCreate}
                        />
                    )
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

/** Props for a {@link FileName}. */
interface InternalFileNameProps extends table.ColumnProps<backendModule.FileAsset> {}

/** The icon and name of a specific file asset. */
function FileName(props: InternalFileNameProps) {
    const { item, selected } = props
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    // TODO: Wait for backend implementation.
    const doRename = async () => {
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
                onSubmit={async newTitle => {
                    setIsNameEditable(false)
                    if (newTitle !== item.title) {
                        // Mutation is UNSAFE as it does not cause a re-render.
                        // However, `setIsNameEditable` causes a re-render, and
                        // using a `useState` is not an option as it will not get overwritten by
                        // the updated value from the server.
                        item.title = newTitle
                        await doRename(/* newTitle */)
                    }
                }}
                onCancel={() => {
                    setIsNameEditable(false)
                }}
                className="bg-transparent grow px-2"
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
    directoryId: backendModule.DirectoryId | null
    items: backendModule.FileAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    onCreate: () => void
    onDelete: () => void
}

/** The table of file assets. */
function FilesTable(props: FilesTableProps) {
    const { directoryId, items, isLoading, columnDisplayMode, query, onCreate, onDelete } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<backendModule.FileAsset>
                items={items}
                isLoading={isLoading}
                getKey={backendModule.getAssetId}
                placeholder={query ? PLACEHOLDER_WITH_QUERY : PLACEHOLDER_WITHOUT_QUERY}
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
                onContextMenu={(files, event, setSelectedItems) => {
                    event.preventDefault()
                    event.stopPropagation()
                    const doDeleteAll = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={`${files.size} selected files`}
                                assetType="files"
                                doDelete={async () => {
                                    setSelectedItems(new Set())
                                    await toastPromiseMultiple.toastPromiseMultiple(
                                        logger,
                                        [...files],
                                        file => backend.deleteFile(file.id, file.title),
                                        TOAST_PROMISE_MULTIPLE_MESSAGES
                                    )
                                }}
                                onSuccess={onDelete}
                            />
                        )
                    }
                    const filesText = files.size === 1 ? 'file' : 'files'
                    setModal(
                        <ContextMenu key={uniqueString.uniqueString()} event={event}>
                            {/*<ContextMenuEntry disabled onClick={doCopyAll}>
                                Copy {files.size} files
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doCutAll}>
                                Cut {files.size} files
                            </ContextMenuEntry>*/}
                            <ContextMenuEntry onClick={doDeleteAll}>
                                <span className="text-red-700">
                                    Delete {files.size} {filesText}
                                </span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
                onRowContextMenu={(file, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    const doDelete = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={`'${file.title}'`}
                                assetType={file.type}
                                doDelete={() => backend.deleteFile(file.id, file.title)}
                                onSuccess={onDelete}
                            />
                        )
                    }
                    setModal(
                        <ContextMenu key={file.id} event={event}>
                            {/*<ContextMenuEntry disabled onClick={doCopy}>
                                Copy
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doCut}>
                                Cut
                            </ContextMenuEntry>*/}
                            <ContextMenuEntry onClick={doDelete}>
                                <span className="text-red-700">Delete</span>
                            </ContextMenuEntry>
                            {/*<ContextMenuEntry disabled onClick={doDownload}>
                                Download
                            </ContextMenuEntry>*/}
                        </ContextMenu>
                    )
                }}
            />
        )
    }
}

export default FilesTable
