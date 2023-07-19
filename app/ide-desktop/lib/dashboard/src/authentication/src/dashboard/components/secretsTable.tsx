/** @file Table displaying a list of secrets. */
import * as React from 'react'
import toast from 'react-hot-toast'

import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as errorModule from '../../error'
import * as hooks from '../../hooks'
import * as permissions from '../permissions'
import * as presence from '../presence'
import * as secretEventModule from '../events/secretEvent'
import * as secretListEventModule from '../events/secretListEvent'
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
export const ASSET_TYPE_NAME = 'secret'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'secrets'
// This is a function, even though it is not syntactically a function.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)
/** Placeholder row when the search query is not empty. */
const PLACEHOLDER_WITH_QUERY = (
    <span className="opacity-75">
        This folder does not contain any secrets matching your query.
    </span>
)
/** Placeholder row when the search query is empty. */
const PLACEHOLDER_WITHOUT_QUERY = (
    <span className="opacity-75">This folder does not contain any secrets.</span>
)

// =================
// === SecretRow ===
// =================

/** A row in a {@link SecretsTable}. */
function SecretRow(
    props: tableRow.TableRowProps<
        backendModule.SecretAsset,
        backendModule.SecretId,
        SecretsTableState,
        SecretRowState
    >
) {
    const {
        keyProp: key,
        item: rawItem,
        state: { secretEvent, dispatchSecretListEvent, markItemAsHidden, markItemAsVisible },
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
                await backend.deleteSecret(item.id, item.title)
                dispatchSecretListEvent({
                    type: secretListEventModule.SecretListEventType.delete,
                    secretId: key,
                })
            } catch (error) {
                setStatus(presence.Presence.present)
                markItemAsVisible(key)
                const message = `Unable to delete secret: ${
                    errorModule.tryGetMessage(error) ?? 'unknown error.'
                }`
                toast.error(message)
                logger.error(message)
            }
        }
    }

    hooks.useEventHandler(secretEvent, async event => {
        switch (event.type) {
            case secretEventModule.SecretEventType.create: {
                if (key === event.placeholderId) {
                    if (backend.type !== backendModule.BackendType.remote) {
                        const message = 'Secrets cannot be created on the local backend.'
                        toast.error(message)
                        logger.error(message)
                    } else {
                        setStatus(presence.Presence.inserting)
                        try {
                            const createdSecret = await backend.createSecret({
                                parentDirectoryId: item.parentId,
                                secretName: item.title,
                                secretValue: event.value,
                            })
                            setStatus(presence.Presence.present)
                            const newItem: backendModule.SecretAsset = {
                                ...item,
                                ...createdSecret,
                            }
                            setItem(newItem)
                        } catch (error) {
                            dispatchSecretListEvent({
                                type: secretListEventModule.SecretListEventType.delete,
                                secretId: key,
                            })
                            const message = `Error creating new secret: ${
                                errorModule.tryGetMessage(error) ?? 'unknown error.'
                            }`
                            toast.error(message)
                            logger.error(message)
                        }
                    }
                }
                break
            }
            case secretEventModule.SecretEventType.deleteMultiple: {
                if (event.secretIds.has(key)) {
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
                    <SecretRowContextMenu
                        innerProps={innerProps}
                        event={event}
                        doDelete={doDelete}
                    />
                )
            }}
            item={item}
        />
    )
}

// ====================
// === SecretsTable ===
// ====================

/** State passed through from a {@link SecretsTable} to every cell. */
export interface SecretsTableState {
    secretEvent: secretEventModule.SecretEvent | null
    dispatchSecretListEvent: (event: secretListEventModule.SecretListEvent) => void
    markItemAsHidden: (key: string) => void
    markItemAsVisible: (key: string) => void
}

/** Data associated with a {@link SecretRow}, used for rendering. */
export interface SecretRowState {
    isEditingName: boolean
}

/** The default {@link SecretRowState} associated with a {@link SecretRow}. */
export const INITIAL_ROW_STATE: SecretRowState = Object.freeze({
    isEditingName: false,
})

/** Props for a {@link SecretsTable}. */
export interface SecretsTableProps {
    directoryId: backendModule.DirectoryId | null
    items: backendModule.SecretAsset[]
    filter: ((item: backendModule.SecretAsset) => boolean) | null
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
}

/** The table of secret assets. */
function SecretsTable(props: SecretsTableProps) {
    const { directoryId, items: rawItems, filter, isLoading, columnDisplayMode } = props
    const logger = loggerProvider.useLogger()
    const { organization } = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [items, setItems] = React.useState(rawItems)
    const [secretEvent, dispatchSecretEvent] = hooks.useEvent<secretEventModule.SecretEvent>()
    const [secretListEvent, dispatchSecretListEvent] =
        hooks.useEvent<secretListEventModule.SecretListEvent>()

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

    hooks.useEventHandler(secretListEvent, event => {
        switch (event.type) {
            case secretListEventModule.SecretListEventType.create: {
                if (backend.type !== backendModule.BackendType.remote) {
                    const message = 'Secrets cannot be created on the local backend.'
                    toast.error(message)
                    logger.error(message)
                } else {
                    const placeholderItem: backendModule.SecretAsset = {
                        id: backendModule.SecretId(uniqueString.uniqueString()),
                        title: event.name,
                        modifiedAt: dateTime.toRfc3339(new Date()),
                        parentId: directoryId ?? backendModule.DirectoryId(''),
                        permissions: permissions.tryGetSingletonOwnerPermission(organization),
                        projectState: null,
                        type: backendModule.AssetType.secret,
                    }
                    setItems(oldItems => [placeholderItem, ...oldItems])
                    dispatchSecretEvent({
                        type: secretEventModule.SecretEventType.create,
                        placeholderId: placeholderItem.id,
                        value: event.value,
                    })
                }
                break
            }
            case secretListEventModule.SecretListEventType.delete: {
                setItems(oldItems => oldItems.filter(item => item.id !== event.secretId))
                break
            }
        }
    })

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): SecretsTableState => ({
            secretEvent,
            dispatchSecretListEvent,
            markItemAsHidden,
            markItemAsVisible,
        }),
        [secretEvent, dispatchSecretListEvent, markItemAsHidden, markItemAsVisible]
    )

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<
                backendModule.SecretAsset,
                backendModule.SecretId,
                SecretsTableState,
                SecretRowState
            >
                rowComponent={SecretRow}
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
                                  <SecretNameHeading
                                      dispatchSecretListEvent={dispatchSecretListEvent}
                                  />
                              ),
                              render: SecretName,
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
                                    dispatchSecretEvent({
                                        type: secretEventModule.SecretEventType.deleteMultiple,
                                        secretIds: selectedKeys,
                                    })
                                    setSelectedKeys(new Set())
                                }}
                            />
                        )
                    }
                    const pluralized = pluralize(selectedKeys.size)
                    setModal(
                        <ContextMenu key={uniqueString.uniqueString()} event={event}>
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

export default SecretsTable
