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
import ContextMenu from './contextMenu'
import RenameModal from './renameModal'
import Rows from './rows'

// ===========================
// === DirectoryCreateForm ===
// ===========================

/** Props for a {@link DirectoryCreateForm}. */
export interface DirectoryCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

function DirectoryCreateForm(props: DirectoryCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const [name, setName] = React.useState<string | null>(null)

    if (backend.platform === platform.Platform.desktop) {
        return <></>
    } else {
        const onSubmit = async (event: React.FormEvent) => {
            event.preventDefault()
            if (name == null) {
                toast.error('Please provide a directory name.')
            } else {
                unsetModal()
                await toast
                    .promise(
                        backend.createDirectory({
                            parentId: directoryId,
                            title: name,
                        }),
                        {
                            loading: 'Creating directory...',
                            success: 'Sucessfully created directory.',
                            error: error.unsafeIntoErrorMessage,
                        }
                    )
                    .then(onSuccess)
            }
        }

        return (
            <CreateForm title="New Directory" onSubmit={onSubmit} {...passThrough}>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="directory_name">
                        Name
                    </label>
                    <input
                        id="directory_name"
                        type="text"
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        onChange={event => {
                            setName(event.target.value)
                        }}
                    />
                </div>
            </CreateForm>
        )
    }
}

// ============================
// === DirectoryNameHeading ===
// ============================

/** Props for a {@link DirectoryNameHeading}. */
export interface ProjectNameHeadingProps {
    directoryId: backendModule.DirectoryId
    onCreate: () => void
}

function DirectoryNameHeading(props: ProjectNameHeadingProps) {
    const { directoryId, onCreate } = props
    const { setModal } = modalProvider.useSetModal()

    return (
        <div className="inline-flex">
            Project
            <button
                className="mx-1"
                onClick={event => {
                    event.stopPropagation()
                    const buttonPosition = event.currentTarget.getBoundingClientRect()
                    setModal(() => (
                        <DirectoryCreateForm
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

// =====================
// === DirectoryName ===
// =====================

/** Props for a {@link DirectoryName}. */
export interface DirectoryNameProps {
    item: backendModule.DirectoryAsset
    onRename: () => void
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
}

function DirectoryName(props: DirectoryNameProps) {
    const { item, onRename, enterDirectory } = props
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
            onDoubleClick={() => {
                enterDirectory(item)
            }}
        >
            {svg.DIRECTORY_ICON} <span className="px-2">{item.title}</span>
        </div>
    )
}

// =====================
// === DirectoryRows ===
// =====================

/** Props for a {@link DirectoryRows}. */
interface DirectoryRowsProps {
    directoryId: backendModule.DirectoryId
    items: backendModule.DirectoryAsset[]
    columnDisplayMode: columnModule.ColumnDisplayMode
    query: string
    onCreate: () => void
    onRename: () => void
    onAssetClick: (
        asset: backendModule.DirectoryAsset,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
    enterDirectory: (directory: backendModule.DirectoryAsset) => void
}

function DirectoryRows(props: DirectoryRowsProps) {
    const {
        directoryId,
        items,
        columnDisplayMode,
        query,
        onCreate,
        onRename,
        onAssetClick,
        enterDirectory,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    if (backend.platform === platform.Platform.desktop) {
        return <></>
    } else {
        return (
            <>
                <tr className="h-10" />
                <Rows<backendModule.DirectoryAsset>
                    items={items}
                    getKey={dir => dir.id}
                    placeholder={
                        <span className="opacity-75">
                            This directory does not contain any subdirectories
                            {query ? ' matching your query' : ''}.
                        </span>
                    }
                    columns={columnModule.COLUMNS_FOR[columnDisplayMode].map(column =>
                        column === columnModule.Column.name
                            ? {
                                  id: column,
                                  heading: (
                                      <DirectoryNameHeading
                                          directoryId={directoryId}
                                          onCreate={onCreate}
                                      />
                                  ),
                                  render: innerProps => (
                                      <DirectoryName
                                          item={innerProps.item}
                                          enterDirectory={enterDirectory}
                                          onRename={onRename}
                                      />
                                  ),
                              }
                            : {
                                  id: column,
                                  heading: <>{columnModule.COLUMN_NAME[column]}</>,
                                  render: columnModule.COLUMN_RENDERER[column],
                              }
                    )}
                    onClick={onAssetClick}
                    onContextMenu={(_directory, event) => {
                        event.preventDefault()
                        event.stopPropagation()
                        setModal(() => <ContextMenu event={event}></ContextMenu>)
                    }}
                />
            </>
        )
    }
}

export default DirectoryRows
