/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import * as platform from '../../platform'
import * as svg from '../../components/svg'

import CreateForm, * as createForm from './createForm'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import RenameModal from './renameModal'
import Rows from './rows'

// ========================
// === SecretCreateForm ===
// ========================

/** Props for a {@link SecretCreateForm}. */
export interface SecretCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

function SecretCreateForm(props: SecretCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = React.useState<string | null>(null)
    const [value, setValue] = React.useState<string | null>(null)

    if (backend.platform === platform.Platform.desktop) {
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
interface SecretNameHeadingProps {
    directoryId: backendModule.DirectoryId
    onCreate: () => void
}

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

/** Props for a {@link SecretName}. */
export interface SecretNameProps {
    item: backendModule.SecretAsset
    onRename: () => void
}

function SecretName(props: SecretNameProps) {
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
                            // FIXME[sb]: Wait for backend implementation.
                            doRename={() => Promise.resolve()}
                            onSuccess={onRename}
                        />
                    ))
                }
            }}
        >
            {svg.SECRET_ICON} <span className="px-2">{item.title}</span>
        </div>
    )
}

// ==================
// === SecretRows ===
// ==================

/** Props for a {@link SecretRows}. */
export interface SecretRowsProps {
    directoryId: backendModule.DirectoryId
    items: backendModule.SecretAsset[]
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

function SecretRows(props: SecretRowsProps) {
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
                <Rows<backendModule.SecretAsset>
                    items={items}
                    getKey={secret => secret.id}
                    placeholder={
                        <span className="opacity-75">
                            This directory does not contain any secrets
                            {query ? ' matching your query' : ''}.
                        </span>
                    }
                    columns={columnModule.COLUMNS_FOR[columnDisplayMode].map(column =>
                        column === columnModule.Column.name
                            ? {
                                  id: column,
                                  heading: (
                                      <SecretNameHeading
                                          directoryId={directoryId}
                                          onCreate={onCreate}
                                      />
                                  ),
                                  render: innerProps => (
                                      <SecretName item={innerProps.item} onRename={onRename} />
                                  ),
                              }
                            : {
                                  id: column,
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
            </>
        )
    }
}

export default SecretRows
