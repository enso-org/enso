/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as fileInfo from '../../fileInfo'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
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

/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'file'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'files'
// This is a function, even though it is not syntactically a function.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)
/** Placeholder row when the search query is not empty. */
const PLACEHOLDER_WITH_QUERY = (
    <span className="opacity-75">
        This folder does not contain any {ASSET_TYPE_NAME_PLURAL} matching your query.
    </span>
)
/** Placeholder row when the search query is empty. */
const PLACEHOLDER_WITHOUT_QUERY = (
    <span className="opacity-75">This folder does not contain any {ASSET_TYPE_NAME_PLURAL}.</span>
)
/** Messages to be passed to {@link toastPromiseMultiple.toastPromiseMultiple}. */
const TOAST_PROMISE_MULTIPLE_MESSAGES: toastPromiseMultiple.ToastPromiseMultipleMessages<backendModule.FileAsset> =
    {
        begin: total => `Deleting ${total} ${pluralize(total)}...`,
        inProgress: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        end: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        error: asset => `Could not delete ${ASSET_TYPE_NAME} '${asset.title}'.`,
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
                            error: errorModule.tryGetMessage,
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
    const [, doRefresh] = hooks.useRefresh()
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    // TODO[sb]: Wait for backend implementation.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async () => {
        return await Promise.resolve(null)
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
                }
            }}
        >
            {fileInfo.fileIcon()}
            <EditableSpan
                editable={isNameEditable}
                onSubmit={async newTitle => {
                    setIsNameEditable(false)
                    if (newTitle !== item.title) {
                        const oldTitle = item.title
                        // Mutation is bad practice as it does not cause a re-render. However, a
                        // `useState` is not an option because a new value may come from the props.
                        // `doRefresh()` ensures that a re-render always happens.
                        item.title = newTitle
                        doRefresh()
                        try {
                            await doRename(/* newTitle */)
                        } catch {
                            item.title = oldTitle
                            doRefresh()
                        }
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
                    const pluralized = pluralize(files.size)
                    setModal(
                        <ContextMenu key={uniqueString.uniqueString()} event={event}>
                            {/*<ContextMenuEntry disabled onClick={doCopyAll}>
                                Copy {files.size} {pluralized}
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doCutAll}>
                                Cut {files.size} {pluralized}
                            </ContextMenuEntry>*/}
                            <ContextMenuEntry onClick={doDeleteAll}>
                                <span className="text-red-700">
                                    Delete {files.size} {pluralized}
                                </span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
                onRowContextMenu={(innerProps, event) => {
                    const { item } = innerProps
                    event.preventDefault()
                    event.stopPropagation()
                    const doDelete = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={`'${item.title}'`}
                                assetType={item.type}
                                doDelete={() => backend.deleteFile(item.id, item.title)}
                                onSuccess={onDelete}
                            />
                        )
                    }
                    setModal(
                        <ContextMenu key={item.id} event={event}>
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
