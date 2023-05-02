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
import ProjectActionButton from './projectActionButton'
import RenameModal from './renameModal'
import Rows from './rows'

export interface ProjectCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

function ProjectCreateForm(props: ProjectCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = React.useState<string | null>(null)
    const [template, setTemplate] = React.useState<string | null>(null)

    if (backend.platform === platform.Platform.desktop) {
        return <></>
    } else {
        const onSubmit = async (event: React.FormEvent) => {
            event.preventDefault()
            if (name == null) {
                toast.error('Please provide a project name.')
            } else {
                unsetModal()
                await toast
                    .promise(
                        backend.createProject({
                            parentDirectoryId: directoryId,
                            projectName: name,
                            projectTemplateName: template,
                        }),
                        {
                            loading: 'Creating project...',
                            success: 'Sucessfully created project.',
                            error: error.unsafeIntoErrorMessage,
                        }
                    )
                    .then(onSuccess)
            }
        }

        return (
            <CreateForm title="New Project" onSubmit={onSubmit} {...passThrough}>
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
                    {/* FIXME[sb]: Use the array of templates in a dropdown when it becomes available. */}
                    <label className="inline-block flex-1 grow m-1" htmlFor="project_template_name">
                        Template
                    </label>
                    <input
                        id="project_template_name"
                        type="text"
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        onChange={event => {
                            setTemplate(event.target.value)
                        }}
                    />
                </div>
            </CreateForm>
        )
    }
}

export interface ProjectNameHeadingProps {
    directoryId: backendModule.DirectoryId
    onCreate: () => void
}

function ProjectNameHeading(props: ProjectNameHeadingProps) {
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
                        <ProjectCreateForm
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

export interface ProjectNameProps {
    item: backendModule.ProjectAsset
    appRunner: AppRunner | null
    onRename: () => void
    onOpenIde: (project: backendModule.ProjectAsset) => void
    onCloseIde: () => void
}

function ProjectName(props: ProjectNameProps) {
    const { item, appRunner, onRename, onOpenIde, onCloseIde } = props
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
            <ProjectActionButton
                project={item}
                appRunner={appRunner}
                openIde={() => {
                    onOpenIde(item)
                }}
                onClose={onCloseIde}
            />
            <span className="px-2">{item.title}</span>
        </div>
    )
}

interface ProjectRowsProps {
    appRunner: AppRunner | null
    directoryId: backendModule.DirectoryId
    items: backendModule.ProjectAsset[]
    columnDisplayMode: columnModule.ColumnDisplayMode
    onCreate: () => void
    onRename: () => void
    onDelete: () => void
    onOpenIde: (project: backendModule.ProjectAsset) => void
    onCloseIde: () => void
    onAssetClick: (
        asset: backendModule.ProjectAsset,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
}

function ProjectRows(props: ProjectRowsProps) {
    const {
        appRunner,
        directoryId,
        items,
        columnDisplayMode,
        onCreate,
        onRename,
        onDelete,
        onOpenIde,
        onCloseIde,
        onAssetClick,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()

    return (
        <>
            <tr className="h-10" />
            <Rows<backendModule.ProjectAsset>
                items={items}
                getKey={proj => proj.id}
                placeholder={
                    <span className="opacity-75">
                        You have no project yet. Go ahead and create one using the form above.
                    </span>
                }
                columns={columnModule.COLUMNS_FOR[columnDisplayMode].map(column =>
                    column === columnModule.Column.name
                        ? {
                              id: column,
                              heading: (
                                  <ProjectNameHeading
                                      directoryId={directoryId}
                                      onCreate={onCreate}
                                  />
                              ),
                              render: innerProps => (
                                  <ProjectName
                                      item={innerProps.item}
                                      appRunner={appRunner}
                                      onRename={onRename}
                                      onOpenIde={onOpenIde}
                                      onCloseIde={onCloseIde}
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
                onContextMenu={(projectAsset, event) => {
                    event.preventDefault()
                    event.stopPropagation()
                    const doOpenForEditing = () => {
                        // FIXME[sb]: Switch to IDE tab once it is fully loaded.
                    }
                    const doOpenAsFolder = () => {
                        // FIXME[sb]: Uncomment once backend support is in place.
                        // The following code does not typecheck
                        // since `ProjectId`s are not `DirectoryId`s.
                        // enterDirectory(projectAsset)
                    }
                    // This is not a React component even though it contains JSX.
                    // eslint-disable-next-line no-restricted-syntax
                    const doRename = () => {
                        setModal(() => (
                            <RenameModal
                                name={projectAsset.title}
                                assetType={projectAsset.type}
                                // FIXME[sb]: Replace with API call when implemented in backend.
                                doRename={() => Promise.resolve()}
                                onSuccess={onRename}
                            />
                        ))
                    }
                    // This is not a React component even though it contains JSX.
                    // eslint-disable-next-line no-restricted-syntax
                    const doDelete = () => {
                        // The button is disabled when using the desktop backend,
                        // so this condition should never be `false`.
                        if (backend.platform === platform.Platform.cloud) {
                            setModal(() => (
                                <ConfirmDeleteModal
                                    name={projectAsset.title}
                                    assetType={projectAsset.type}
                                    doDelete={() => backend.deleteProject(projectAsset.id)}
                                    onSuccess={onDelete}
                                />
                            ))
                        }
                    }
                    setModal(() => (
                        <ContextMenu event={event}>
                            <ContextMenuEntry disabled onClick={doOpenForEditing}>
                                Open for editing
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doOpenAsFolder}>
                                Open as folder
                            </ContextMenuEntry>
                            <ContextMenuEntry disabled onClick={doRename}>
                                Rename
                            </ContextMenuEntry>
                            <ContextMenuEntry
                                disabled={backend.platform === platform.Platform.desktop}
                                onClick={doDelete}
                            >
                                <span className="text-red-700">Delete</span>
                            </ContextMenuEntry>
                        </ContextMenu>
                    ))
                }}
            />
        </>
    )
}

export default ProjectRows
