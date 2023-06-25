/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import PlusIcon from 'enso-assets/plus.svg'
import SecretIcon from 'enso-assets/secret.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
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
/** Messages to be passed to {@link toastPromiseMultiple.toastPromiseMultiple}. */
const TOAST_PROMISE_MULTIPLE_MESSAGES: toastPromiseMultiple.ToastPromiseMultipleMessages<backendModule.SecretAsset> =
    {
        begin: total => `Deleting ${total} ${pluralize(total)}...`,
        inProgress: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        end: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        error: asset => `Could not delete ${ASSET_TYPE_NAME} '${asset.title}'.`,
    }

// ========================
// === SecretCreateForm ===
// ========================

/** Props for a {@link SecretCreateForm}. */
interface InternalSecretCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId | null
    onSuccess: () => void
}

/** A form to create a new secret asset. */
function SecretCreateForm(props: InternalSecretCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = React.useState<string | null>(null)
    const [value, setValue] = React.useState<string | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = async (event: React.FormEvent) => {
            event.preventDefault()
            if (name == null) {
                toast.error('Please provide a secret name.')
            } else if (value == null) {
                // Secret value explicitly can be empty.
                toast.error('Please provide a secret value.')
            } else {
                unsetModal()
                await toast
                    .promise(
                        backend.createSecret({
                            parentDirectoryId: directoryId,
                            secretName: name,
                            secretValue: value,
                        }),
                        {
                            loading: 'Creating secret...',
                            success: 'Sucessfully created secret.',
                            error: errorModule.tryGetMessage,
                        }
                    )
                    .then(onSuccess)
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
    directoryId: backendModule.DirectoryId | null
    onCreate: () => void
}

/** The column header for the "name" column for the table of secret assets. */
function SecretNameHeading(props: InternalSecretNameHeadingProps) {
    const { directoryId, onCreate } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div className="inline-flex">
            Secret
            <button
                className="mx-1"
                onClick={event => {
                    event.stopPropagation()
                    const buttonPosition = event.currentTarget.getBoundingClientRect()
                    setModal(
                        <SecretCreateForm
                            left={buttonPosition.left + window.scrollX}
                            top={buttonPosition.top + window.scrollY}
                            directoryId={directoryId}
                            onSuccess={onCreate}
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
interface InternalSecretNameProps extends table.ColumnProps<backendModule.SecretAsset> {}

/** The icon and name of a specific secret asset. */
function SecretName(props: InternalSecretNameProps) {
    const { item, selected } = props
    const [, doRefresh] = hooks.useRefresh()
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    // TODO[sb]: Wait for backend implementation.
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
                    setIsNameEditable(true)
                }
            }}
        >
            <img src={SecretIcon} />{' '}
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

// ==================
// === SecretRows ===
// ==================

/** Props for a {@link SecretsTable}. */
export interface SecretsTableProps {
    directoryId: backendModule.DirectoryId | null
    items: backendModule.SecretAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    onCreate: () => void
    onDelete: () => void
}

/** The table of secret assets. */
function SecretsTable(props: SecretsTableProps) {
    const { directoryId, items, isLoading, columnDisplayMode, query, onCreate, onDelete } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        return (
            <Table<backendModule.SecretAsset>
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
                                  <SecretNameHeading
                                      directoryId={directoryId}
                                      onCreate={onCreate}
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
                onContextMenu={(secrets, event, setSelectedItems) => {
                    event.preventDefault()
                    event.stopPropagation()
                    const doDeleteAll = () => {
                        setModal(
                            <ConfirmDeleteModal
                                description={`${secrets.size} selected secrets`}
                                assetType="secrets"
                                doDelete={async () => {
                                    setSelectedItems(new Set())
                                    await toastPromiseMultiple.toastPromiseMultiple(
                                        logger,
                                        [...secrets],
                                        secret => backend.deleteSecret(secret.id, secret.title),
                                        TOAST_PROMISE_MULTIPLE_MESSAGES
                                    )
                                }}
                                onSuccess={onDelete}
                            />
                        )
                    }
                    const secretsText = secrets.size === 1 ? 'secret' : 'secrets'
                    setModal(
                        <ContextMenu key={uniqueString.uniqueString()} event={event}>
                            <ContextMenuEntry onClick={doDeleteAll}>
                                <span className="text-red-700">
                                    Delete {secrets.size} {secretsText}
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
                                description={item.title}
                                assetType={item.type}
                                doDelete={() => backend.deleteSecret(item.id, item.title)}
                                onSuccess={onDelete}
                            />
                        )
                    }
                    setModal(
                        <ContextMenu key={item.id} event={event}>
                            <ContextMenuEntry onClick={doDelete}>
                                <span className="text-red-700">Delete</span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    )
                }}
            />
        )
    }
}

export default SecretsTable
