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
import Table from './table'

// =================
// === Constants ===
// =================

/** HTML validation attributes for project names when using the local backend.
 * There are no such limitations on the remote backend. */
const DESKTOP_PROJECT_NAME_VALIDATION = {
    namePattern: '[A-Z][a-z]*(?:_\\d+|_[A-Z][a-z]*)*',
    title: 'Names must be in Upper_Snake_Case. (Numbers (_0, _1) are also allowed.)',
} as const

/** Props for a {@link ProjectCreateForm}. */
export interface ProjectCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

/** A form to create a new project asset. */
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

// ==========================
// === ProjectNameHeading ===
// ==========================

/** Props for a {@link ProjectNameHeading}. */
export interface ProjectNameHeadingProps {
    directoryId: backendModule.DirectoryId
    onCreate: () => void
}

/** The column header for the "name" column for the table of project assets. */
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

// ===================
// === ProjectName ===
// ===================

/** State passed through from a {@link ProjectsTable} to every cell. */
export interface ProjectNamePropsState {
    appRunner: AppRunner | null
    onRename: () => void
    onOpenIde: (project: backendModule.ProjectAsset) => void
    onCloseIde: () => void
}

/** Props for a {@link ProjectName}. */
export interface ProjectNameProps {
    item: backendModule.ProjectAsset
    state: ProjectNamePropsState
}

/** The icon and name of a specific project asset. */
function ProjectName(props: ProjectNameProps) {
    const {
        item,
        state: { appRunner, onRename, onOpenIde, onCloseIde },
    } = props
    const { backend } = backendProvider.useBackend()
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
                            doRename={async newName => {
                                await backend.projectUpdate(item.id, {
                                    ami: null,
                                    ideVersion: null,
                                    projectName: newName,
                                })
                            }}
                            onSuccess={onRename}
                            {...(backend.platform === platform.Platform.desktop
                                ? DESKTOP_PROJECT_NAME_VALIDATION
                                : {})}
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

interface ProjectsTableProps {
    appRunner: AppRunner | null
    directoryId: backendModule.DirectoryId
    items: backendModule.ProjectAsset[]
    isLoading: boolean
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

/** The table of project assets. */
function ProjectsTable(props: ProjectsTableProps) {
    const {
        appRunner,
        directoryId,
        items,
        isLoading,
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
        <Table<backendModule.ProjectAsset, ProjectNamePropsState>
            items={items}
            isLoading={isLoading}
            state={{
                appRunner,
                onRename,
                onOpenIde,
                onCloseIde,
            }}
            getKey={backendModule.getAssetId}
            placeholder={
                <span className="opacity-75">
                    You have no project yet. Go ahead and create one using the form above.
                </span>
            }
            columns={columnModule.columnsFor(columnDisplayMode, backend.platform).map(column =>
                column === columnModule.Column.name
                    ? {
                          id: column,
                          className: columnModule.COLUMN_CSS_CLASS[column],
                          heading: (
                              <ProjectNameHeading directoryId={directoryId} onCreate={onCreate} />
                          ),
                          render: ProjectName,
                      }
                    : {
                          id: column,
                          className: columnModule.COLUMN_CSS_CLASS[column],
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
                            doRename={async newName => {
                                await backend.projectUpdate(projectAsset.id, {
                                    ami: null,
                                    ideVersion: null,
                                    projectName: newName,
                                })
                            }}
                            onSuccess={onRename}
                            {...(backend.platform === platform.Platform.desktop
                                ? DESKTOP_PROJECT_NAME_VALIDATION
                                : {})}
                        />
                    ))
                }
                // This is not a React component even though it contains JSX.
                // eslint-disable-next-line no-restricted-syntax
                const doDelete = () => {
                    setModal(() => (
                        <ConfirmDeleteModal
                            name={projectAsset.title}
                            assetType={projectAsset.type}
                            doDelete={() => backend.deleteProject(projectAsset.id)}
                            onSuccess={onDelete}
                        />
                    ))
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
                        <ContextMenuEntry onClick={doDelete}>
                            <span className="text-red-700">Delete</span>
                        </ContextMenuEntry>
                    </ContextMenu>
                ))
            }}
        />
    )
}

export default ProjectsTable
