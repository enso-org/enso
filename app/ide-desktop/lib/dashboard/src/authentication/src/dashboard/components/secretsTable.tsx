/** @file Table displaying a list of secrets. */
import * as React from 'react'
import toast from 'react-hot-toast'

import PlusIcon from 'enso-assets/plus.svg'
import SecretIcon from 'enso-assets/secret.svg'

import * as backendModule from '../backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as permissions from '../permissions'
import * as presence from '../presence'
import * as secretEventModule from '../events/secretEvent'
import * as secretListEventModule from '../events/secretListEvent'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
import * as uniqueString from '../../uniqueString'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import * as tableColumn from './tableColumn'
import CreateForm, * as createForm from './createForm'
import TableRow, * as tableRow from './tableRow'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'
import Table from './table'

// =================
// === Constants ===
// =================

/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'secret'
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

// ========================
// === SecretCreateForm ===
// ========================

/** Props for a {@link SecretCreateForm}. */
interface InternalSecretCreateFormProps extends createForm.CreateFormPassthroughProps {
    dispatchSecretListEvent: (event: secretListEventModule.SecretListEvent) => void
}

/** A form to create a new secret asset. */
function SecretCreateForm(props: InternalSecretCreateFormProps) {
    const { dispatchSecretListEvent, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = React.useState<string | null>(null)
    const [value, setValue] = React.useState<string | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = (event: React.FormEvent) => {
            event.preventDefault()
            if (name == null) {
                toast.error('Please provide a secret name.')
            } else if (value == null) {
                // Secret value explicitly can be empty.
                toast.error('Please provide a secret value.')
            } else {
                unsetModal()
                dispatchSecretListEvent({
                    type: secretListEventModule.SecretListEventType.create,
                    name: name,
                    value: value,
                })
            }
        }

        return (
            <CreateForm title="New Secret" onSubmit={onSubmit} {...passThrough}>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="project_name">
                        Name
                    </label>
                    <input
                        id="project_name"
                        type="text"
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        onChange={event => {
                            setName(event.target.value)
                        }}
                    />
                </div>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="secret_value">
                        Value
                    </label>
                    <input
                        id="secret_value"
                        type="text"
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        onChange={event => {
                            setValue(event.target.value)
                        }}
                    />
                </div>
            </CreateForm>
        )
    }
}

// =========================
// === SecretNameHeading ===
// =========================

/** Props for a {@link SecretNameHeading}. */
interface InternalSecretNameHeadingProps {
    dispatchSecretListEvent: (event: secretListEventModule.SecretListEvent) => void
}

/** The column header for the "name" column for the table of secret assets. */
function SecretNameHeading(props: InternalSecretNameHeadingProps) {
    const { dispatchSecretListEvent } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div className="inline-flex">
            {string.capitalizeFirst(ASSET_TYPE_NAME_PLURAL)}
            <button
                className="mx-1"
                onClick={event => {
                    event.stopPropagation()
                    const buttonPosition = event.currentTarget.getBoundingClientRect()
                    setModal(
                        <SecretCreateForm
                            left={buttonPosition.left + window.scrollX}
                            top={buttonPosition.top + window.scrollY}
                            dispatchSecretListEvent={dispatchSecretListEvent}
                        />
                    )
                }}
            >
                <img src={PlusIcon} />
            </button>
        </div>
    )
}

// ==================
// === SecretName ===
// ==================

/** Props for a {@link SecretName}. */
interface InternalSecretNameProps
    extends tableColumn.TableColumnProps<
        backendModule.SecretAsset,
        SecretsTableState,
        SecretRowState
    > {}

/** The icon and name of a specific secret asset. */
function SecretName(props: InternalSecretNameProps) {
    const { item, setItem, selected, setRowState } = props

    // TODO[sb]: Wait for backend implementation. `editable` should also be re-enabled, and the
    // context menu entry should be re-added.
    // Backend implementation is tracked here: https://github.com/enso-org/cloud-v2/issues/505.
    const doRename = async (/* _newName: string */) => {
        await Promise.resolve(null)
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
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: true,
                    }))
                }
            }}
        >
            <img src={SecretIcon} />{' '}
            <EditableSpan
                editable={false}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
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
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                }}
                className="bg-transparent grow px-2"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}

// ============================
// === SecretRowContextMenu ===
// ============================

/** Props for a {@link SecretRowContextMenu}. */
interface InternalDirectoryRowContextMenuProps {
    innerProps: tableRow.TableRowInnerProps<
        backendModule.SecretAsset,
        backendModule.SecretId,
        SecretRowState
    >
    event: React.MouseEvent
    doDelete: () => Promise<void>
}

/** The context menu for a row of a {@link SecretsTable}. */
function SecretRowContextMenu(props: InternalDirectoryRowContextMenuProps) {
    const {
        innerProps: { item },
        event,
        doDelete,
    } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <ContextMenu key={item.id} event={event}>
            <ContextMenuEntry
                onClick={() => {
                    setModal(
                        <ConfirmDeleteModal
                            description={`the ${ASSET_TYPE_NAME} '${item.title}'`}
                            doDelete={doDelete}
                        />
                    )
                }}
            >
                <span className="text-red-700">Delete</span>
            </ContextMenuEntry>
        </ContextMenu>
    )
}

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
interface SecretsTableState {
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
