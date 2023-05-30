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
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'

// ========================
// === SecretCreateForm ===
// ========================

/** Props for a {@link SecretCreateForm}. */
export interface SecretCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

/** A form to create a new secret asset. */
function SecretCreateForm(props: SecretCreateFormProps) {
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
            if (!name) {
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
                            error: error.unsafeIntoErrorMessage,
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
export interface SecretNameHeadingProps {
    directoryId: backendModule.DirectoryId
    onCreate: () => void
}

/** The column header for the "name" column for the table of secret assets. */
function SecretNameHeading(props: SecretNameHeadingProps) {
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
                    setModal(() => (
                        <SecretCreateForm
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

// ==================
// === SecretName ===
// ==================

/** State passed through from a {@link SecretsTable} to every cell. */
export interface SecretNamePropsState {
    onRename: () => void
}

/** Props for a {@link SecretName}. */
export interface SecretNameProps
    extends table.ColumnProps<backendModule.SecretAsset, SecretNamePropsState> {}

/** The icon and name of a specific secret asset. */
function SecretName(props: SecretNameProps) {
    const {
        item,
        selected,
        state: { onRename },
    } = props
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    // TODO: Wait for backend implementation.
    const doRename = async (_newName: string) => {
        onRename()
        await Promise.resolve(null)
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
            {svg.SECRET_ICON}{' '}
            <EditableSpan
                editable={isNameEditable}
                onBlur={async event => {
                    setIsNameEditable(false)
                    if (event.target.value === item.title) {
                        toast.success('The secret name is unchanged.')
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

// ==================
// === SecretRows ===
// ==================

/** Props for a {@link SecretsTable}. */
export interface SecretsTableProps {
    directoryId: backendModule.DirectoryId
    items: backendModule.SecretAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    onCreate: () => void
    onRename: () => void
    onDelete: () => void
    onAssetClick: (
        asset: backendModule.SecretAsset,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

/** The table of secret assets. */
function SecretsTable(props: SecretsTableProps) {
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
            <Table<backendModule.SecretAsset, SecretNamePropsState>
                items={items}
                isLoading={isLoading}
                state={{ onRename }}
                getKey={backendModule.getAssetId}
                placeholder={
                    <span className="opacity-75">
                        This directory does not contain any secrets
                        {query ? ' matching your query' : ''}.
                    </span>
                }
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
                onClick={onAssetClick}
                onContextMenu={(secret, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    // This is not a React component even though it contains JSX.
                    // eslint-disable-next-line no-restricted-syntax
                    const doDelete = () => {
                        setModal(() => (
                            <ConfirmDeleteModal
                                name={secret.title}
                                assetType={secret.type}
                                doDelete={() => backend.deleteSecret(secret.id)}
                                onSuccess={onDelete}
                            />
                        ))
                    }
                    setModal(() => (
                        <ContextMenu event={event}>
                            <ContextMenuEntry onClick={doDelete}>
                                <span className="text-red-700">Delete</span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    ))
                }}
            />
        )
    }
}

export default SecretsTable
