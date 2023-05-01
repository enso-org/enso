/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backend from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as error from '../../error'
import * as fileInfo from '../../fileInfo'
import * as modalProvider from '../../providers/modal'
import * as platform from '../../platform'
import * as svg from '../../components/svg'

import CreateForm, * as createForm from './createForm'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import RenameModal from './renameModal'
import Rows from './rows'

// ======================
// === FileCreateForm ===
// ======================

/** Props for a {@link FileCreateForm}. */
export interface FileCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backend.DirectoryId
    onSuccess: () => void
}

function FileCreateForm(props: FileCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const [name, setName] = React.useState<string | null>(null)
    const [file, setFile] = React.useState<File | null>(null)

    if (backend.platform === platform.Platform.desktop) {
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

export interface FileNameHeadingProps {
    directoryId: backend.DirectoryId
    onCreate: () => void
}

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
                            left={buttonPosition.left}
                            top={buttonPosition.top}
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

/** Props for a {@link FileName}. */
export interface FileNameProps {
    item: backend.FileAsset
    onRename: () => void
}

function FileName(props: FileNameProps) {
    const { item, onRename } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
            onClick={event => {
                if (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey) {
                    setModal(() => (
                        <RenameModal
                            assetType={item.type}
                            name={item.title}
                            // TODO: Wait for backend implementation.
                            doRename={() => Promise.resolve()}
                            onSuccess={onRename}
                        />
                    ))
                }
            }}
        >
            {fileInfo.fileIcon(fileInfo.fileExtension(item.title))}{' '}
            <span className="px-2">{item.title}</span>
        </div>
    )
}

// ================
// === FileRows ===
// ================

/** Props for a {@link FileRows}. */
export interface FileRowsProps {
    directoryId: backend.DirectoryId
    items: backend.FileAsset[]
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    onCreate: () => void
    onRename: () => void
    onDelete: () => void
    onAssetClick: (asset: backend.FileAsset, event: React.MouseEvent<HTMLTableRowElement>) => void
}

function FileRows(props: FileRowsProps) {
    const {
        directoryId,
        items,
        columnDisplayMode,
        query,
        onCreate,
        onRename,
        onDelete,
        onAssetClick,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    if (backend.platform === platform.Platform.desktop) {
        return <></>
    } else {
        return (
            <>
                <tr className="h-10" />
                <Rows<backend.FileAsset>
                    items={items}
                    getKey={file => file.id}
                    placeholder={
                        <span className="opacity-75">
                            This directory does not contain any files
                            {query ? ' matching your query' : ''}.
                        </span>
                    }
                    columns={columnModule.COLUMNS_FOR[columnDisplayMode].map(column =>
                        column === columnModule.Column.name
                            ? {
                                  id: column,
                                  heading: (
                                      <FileNameHeading
                                          directoryId={directoryId}
                                          onCreate={onCreate}
                                      />
                                  ),
                                  render: innerProps => (
                                      <FileName item={innerProps.item} onRename={onRename} />
                                  ),
                              }
                            : {
                                  id: column,
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
            </>
        )
    }
}

export default FileRows
