/** @file Table displaying a list of files. */
import * as React from 'react'
import toast from 'react-hot-toast'

import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as errorModule from '../../error'
import * as fileEventModule from '../events/fileEvent'
import * as fileListEventModule from '../events/fileListEvent'
import * as hooks from '../../hooks'
import * as permissions from '../permissions'
import * as presence from '../presence'
import * as string from '../../string'
import * as uniqueString from '../../uniqueString'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import TableRow, * as tableRow from './tableRow'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import Table from './table'

// =================
// === Constants ===
// =================

/** The user-facing name of this asset type. */
export const ASSET_TYPE_NAME = 'file'
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

// ===============
// === FileRow ===
// ===============

/** A row in a {@link FilesTable}. */
function FileRow(
    props: tableRow.TableRowProps<
        backendModule.FileAsset,
        backendModule.FileId,
        FilesTableState,
        FileRowState
    >
) {
    const {
        keyProp: key,
        item: rawItem,
        state: { fileEvent, dispatchFileListEvent, markItemAsHidden, markItemAsVisible },
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
                await backend.deleteFile(item.id, item.title)
                dispatchFileListEvent({
                    type: fileListEventModule.FileListEventType.delete,
                    fileId: key,
                })
            } catch (error) {
                setStatus(presence.Presence.present)
                markItemAsVisible(key)
                const message = `Unable to delete file: ${
                    errorModule.tryGetMessage(error) ?? 'unknown error.'
                }`
                toast.error(message)
                logger.error(message)
            }
        }
    }

    hooks.useEventHandler(fileEvent, async event => {
        switch (event.type) {
            case fileEventModule.FileEventType.createMultiple: {
                const file = event.files.get(key)
                if (file != null) {
                    if (backend.type !== backendModule.BackendType.remote) {
                        const message = 'Files cannot be uploaded on the local backend.'
                        toast.error(message)
                        logger.error(message)
                    } else {
                        setStatus(presence.Presence.inserting)
                        try {
                            const createdFile = await backend.uploadFile(
                                {
                                    fileId: null,
                                    fileName: item.title,
                                    parentDirectoryId: item.parentId,
                                },
                                file
                            )
                            setStatus(presence.Presence.present)
                            const newItem: backendModule.FileAsset = {
                                ...item,
                                ...createdFile,
                            }
                            setItem(newItem)
                        } catch (error) {
                            dispatchFileListEvent({
                                type: fileListEventModule.FileListEventType.delete,
                                fileId: key,
                            })
                            const message = `Error creating new file: ${
                                errorModule.tryGetMessage(error) ?? 'unknown error.'
                            }`
                            toast.error(message)
                            logger.error(message)
                        }
                    }
                }
                break
            }
            case fileEventModule.FileEventType.deleteMultiple: {
                if (event.fileIds.has(key)) {
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
                    <FileContextMenu innerProps={innerProps} event={event} doDelete={doDelete} />
                )
            }}
            item={item}
        />
    )
}

// ==================
// === FilesTable ===
// ==================

/** State passed through from a {@link FilesTable} to every cell. */
export interface FilesTableState {
    fileEvent: fileEventModule.FileEvent | null
    dispatchFileListEvent: (event: fileListEventModule.FileListEvent) => void
    markItemAsHidden: (key: string) => void
    markItemAsVisible: (key: string) => void
}

/** Data associated with a {@link FileRow}, used for rendering. */
export interface FileRowState {
    isEditingName: boolean
}

/** The default {@link FileRowState} associated with a {@link FileRow}. */
const INITIAL_ROW_STATE: FileRowState = Object.freeze({
    isEditingName: false,
})

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
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [items, setItems] = React.useState(rawItems)
    const [fileEvent, dispatchFileEvent] = hooks.useEvent<fileEventModule.FileEvent>()

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

    hooks.useEventHandler(fileListEvent, event => {
        switch (event.type) {
            case fileListEventModule.FileListEventType.uploadMultiple: {
                const placeholderItems: backendModule.FileAsset[] = Array.from(event.files)
                    .reverse()
                    .map(file => ({
                        type: backendModule.AssetType.file,
                        id: backendModule.FileId(uniqueString.uniqueString()),
                        title: file.name,
                        parentId: directoryId ?? backendModule.DirectoryId(''),
                        permissions: permissions.tryGetSingletonOwnerPermission(organization),
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        projectState: null,
                    }))
                setItems(oldItems => [...placeholderItems, ...oldItems])
                dispatchFileEvent({
                    type: fileEventModule.FileEventType.createMultiple,
                    files: new Map(
                        placeholderItems.map((placeholderItem, i) => [
                            placeholderItem.id,
                            // This is SAFE, as `placeholderItems` is created using a map on
                            // `event.files`.
                            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                            event.files[i]!,
                        ])
                    ),
                })
                break
            }
            case fileListEventModule.FileListEventType.delete: {
                setItems(oldItems => oldItems.filter(item => item.id !== event.fileId))
                break
            }
        }
    })

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): FilesTableState => ({
            fileEvent,
            dispatchFileListEvent,
            markItemAsHidden,
            markItemAsVisible,
        }),
        [fileEvent, dispatchFileListEvent, markItemAsHidden, markItemAsVisible]
    )

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<backendModule.FileAsset, backendModule.FileId, FilesTableState, FileRowState>
                rowComponent={FileRow}
                items={visibleItems}
                isLoading={isLoading}
                state={state}
                initialRowState={INITIAL_ROW_STATE}
                getKey={backendModule.getAssetId}
                placeholder={filter != null ? PLACEHOLDER_WITH_QUERY : PLACEHOLDER_WITHOUT_QUERY}
                forceShowPlaceholder={shouldForceShowPlaceholder}
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
                                    dispatchFileEvent({
                                        type: fileEventModule.FileEventType.deleteMultiple,
                                        fileIds: selectedKeys,
                                    })
                                    setSelectedKeys(new Set())
                                }}
                            />
                        )
                    }
                    const pluralized = pluralize(selectedKeys.size)
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

export default FilesTable
