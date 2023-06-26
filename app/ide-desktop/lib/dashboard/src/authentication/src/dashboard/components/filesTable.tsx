/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as eventModule from '../event'
import * as fileInfo from '../../fileInfo'
import * as fileListEventModule from '../events/fileListEvent'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
import * as toastPromiseMultiple from '../../toastPromiseMultiple'
import * as uniqueString from '../../uniqueString'

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

// =======================
// === FileNameHeading ===
// =======================

/** Props for a {@link FileNameHeading}. */
interface InternalFileNameHeadingProps {
    directoryId: backendModule.DirectoryId | null
    dispatchFileListEvent: (fileListEvent: fileListEventModule.FileListEvent) => void
}

/** The column header for the "name" column for the table of file assets. */
function FileNameHeading(props: InternalFileNameHeadingProps) {
    const { directoryId, dispatchFileListEvent } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()

    const uploadFiles = React.useCallback(
        (event: React.FormEvent<HTMLInputElement>) => {
            if (backend.type === backendModule.BackendType.local) {
                // TODO[sb]: Allow uploading `.enso-project`s
                // https://github.com/enso-org/cloud-v2/issues/510
                const message = 'Files cannot be uploaded to the local backend.'
                toast.error(message)
                logger.error(message)
            } else if (
                event.currentTarget.files == null ||
                event.currentTarget.files.length === 0
            ) {
                toast.success('No files selected to upload.')
            } else if (directoryId == null) {
                // This should never happen, however display a nice error message in case
                // it somehow does.
                const message = 'Files cannot be uploaded while offline.'
                toast.error(message)
                logger.error(message)
            } else {
                dispatchFileListEvent({
                    type: fileListEventModule.FileListEventType.uploadMultiple,
                    files: event.currentTarget.files,
                })
            }
        },
        [
            backend.type,
            directoryId,
            /* should not change */ logger,
            /* should never change */ dispatchFileListEvent,
        ]
    )

    return (
        <div className="inline-flex">
            File
            <input
                type="file"
                id="files_table_upload_files_input"
                name="files_table_upload_files_input"
                multiple
                className="w-0 h-0"
                onInput={uploadFiles}
            />
            <label htmlFor="files_table_upload_files_input" className="cursor-pointer mx-1">
                <img src={PlusIcon} />
            </label>
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
    const { item, setItem, selected } = props
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
                        setItem(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(/* newTitle */)
                        } catch {
                            setItem(oldItem => ({ ...oldItem, title: oldTitle }))
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

// ==================
// === FilesTable ===
// ==================

/** Props for a {@link FilesTable}. */
export interface FilesTableProps {
    directoryId: backendModule.DirectoryId | null
    items: backendModule.FileAsset[]
    filter: ((item: backendModule.FileAsset) => boolean) | null
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    fileListEvent: fileListEventModule.FileListEvent | null
    dispatchFileListEvent: (projectListEvent: fileListEventModule.FileListEvent) => void
}

/** The table of file assets. */
function FilesTable(props: FilesTableProps) {
    const {
        directoryId,
        items: rawItems,
        filter,
        isLoading,
        columnDisplayMode,
        fileListEvent,
        dispatchFileListEvent,
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [items, setItems] = React.useState(rawItems)

    React.useEffect(() => {
        setItems(rawItems)
    }, [rawItems])

    const visibleItems = React.useMemo(
        () => (filter != null ? items.filter(filter) : items),
        [items, filter]
    )

    hooks.useEvent(fileListEvent, event => {
        switch (event.type) {
            case fileListEventModule.FileListEventType.uploadMultiple: {
                const placeholderItems: backendModule.FileAsset[] = Array.from(event.files)
                    .reverse()
                    .map(file => ({
                        type: backendModule.AssetType.file,
                        id: backendModule.FileId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId: directoryId ?? backendModule.DirectoryId(''),
                        permissions: [],
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: null,
                    }))
                setItems(oldItems => [...placeholderItems, ...oldItems])
                // FIXME: dispatchFileEvent
                break
            }
            case fileListEventModule.FileListEventType.delete: {
                setItems(oldItems => oldItems.filter(item => item.id !== event.fileId))
                break
            }
        }
    })

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<backendModule.FileAsset>
                items={visibleItems}
                isLoading={isLoading}
                getKey={backendModule.getAssetId}
                placeholder={filter != null ? PLACEHOLDER_WITH_QUERY : PLACEHOLDER_WITHOUT_QUERY}
                columns={columnModule.columnsFor(columnDisplayMode, backend.type).map(column =>
                    column === columnModule.Column.name
                        ? {
                              id: column,
                              className: columnModule.COLUMN_CSS_CLASS[column],
                              heading: (
                                  <FileNameHeading
                                      directoryId={directoryId}
                                      dispatchFileListEvent={dispatchFileListEvent}
                                  />
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
                                doDelete={async () => {
                                    await backend.deleteFile(item.id, item.title)
                                }}
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
