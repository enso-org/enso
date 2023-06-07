/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import * as reactiveEvents from '../reactiveEvents'
import * as svg from '../../components/svg'
import * as templates from './templates'
import * as validation from '../validation'

import CreateForm, * as createForm from './createForm'
import Table, * as table from './table'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import Dropdown from './dropdown'
import EditableSpan from './editableSpan'
import ProjectActionButton from './projectActionButton'
import RenameModal from './renameModal'

// =========================
// === ProjectCreateForm ===
// =========================

/** Props for a {@link ProjectCreateForm}. */
export interface ProjectCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    getNewProjectName: (templateId: string | null) => string
    onSuccess: () => void
}

/** A form to create a new project asset. */
// While this is unused, it is being kept in case it will be needed in the future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
function ProjectCreateForm(props: ProjectCreateFormProps) {
    const { directoryId, getNewProjectName, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()

    const [defaultName, setDefaultName] = React.useState(() => getNewProjectName(null))
    const [name, setName] = React.useState<string | null>(null)
    const [templateId, setTemplateId] = React.useState<string | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = async (event: React.FormEvent) => {
            event.preventDefault()
            const finalName = name ?? defaultName
            const templateText = templateId == null ? '' : `from template '${templateId}'`
            await toast.promise(
                backend.createProject({
                    parentDirectoryId: directoryId,
                    projectName: name ?? defaultName,
                    projectTemplateName: templateId,
                }),
                {
                    loading: `Creating project '${finalName}'${templateText}...`,
                    success: `Sucessfully created project '${finalName}'${templateText}.`,
                    // This is UNSAFE, as the original function's parameter is of type `any`.
                    error: (promiseError: Error) =>
                        `Error creating project '${finalName}'${templateText}: ${promiseError.message}`,
                }
            )
            onSuccess()
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
                        value={name ?? defaultName}
                        onChange={event => {
                            setName(event.target.value)
                        }}
                    />
                </div>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="project_template_name">
                        Template
                    </label>
                    <Dropdown
                        className="flex-1 grow-2 px-2 m-1"
                        optionsClassName="-mx-2"
                        items={['None', ...templates.TEMPLATES.map(item => item.title)]}
                        onChange={newTemplateTitle => {
                            const newTemplateId =
                                templates.TEMPLATES.find(
                                    template => template.title === newTemplateTitle
                                )?.id ?? null
                            setTemplateId(newTemplateId)
                            if (name == null) {
                                setDefaultName(getNewProjectName(newTemplateId))
                            }
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
    doCreateProject: () => Promise<void>
    onCreate: () => void
}

/** The column header for the "name" column for the table of project assets. */
function ProjectNameHeading(props: ProjectNameHeadingProps) {
    const { doCreateProject, onCreate } = props

    return (
        <div className="inline-flex">
            Project
            <button
                className="mx-1"
                onClick={event => {
                    event.stopPropagation()
                    void toast.promise(doCreateProject(), {
                        loading: 'Creating new empty project...',
                        success: 'Created new empty project.',
                        // This is UNSAFE, as the original function's parameter is of type
                        // `any`.
                        error: (promiseError: Error) =>
                            `Error creating new empty project: ${promiseError.message}`,
                    })
                    onCreate()
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
    projectEvent: reactiveEvents.ProjectEvent | null
    setProjectEvent: (projectEvent: reactiveEvents.ProjectEvent | null) => void
    getExtraData: (projectId: backendModule.ProjectId) => ProjectExtraData
    setExtraData: (projectId: backendModule.ProjectId, newExtraData: ProjectExtraData) => void
    deleteExtraData: (projectId: backendModule.ProjectId) => void
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
    onRename: () => void
    doRefresh: () => void
}

/** Props for a {@link ProjectName}. */
export interface ProjectNameProps
    extends table.ColumnProps<backendModule.ProjectAsset, ProjectNamePropsState> {}

/** The icon and name of a specific project asset. */
function ProjectName(props: ProjectNameProps) {
    const {
        item,
        selected,
        state: {
            appRunner,
            getExtraData,
            setExtraData,
            deleteExtraData,
            projectEvent,
            setProjectEvent,
            doOpenManually,
            onRename,
            doOpenIde,
            doCloseIde,
            doRefresh,
        },
    } = props
    const { backend } = backendProvider.useBackend()
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    const doRename = async (newName: string) => {
        await toast.promise(
            backend.projectUpdate(item.id, {
                ami: null,
                ideVersion: null,
                projectName: newName,
            }),
            {
                loading: 'Renaming project...',
                success: 'Renamed project',
                error: reason => `Error renaming project: ${error.unsafeIntoErrorMessage(reason)}`,
            }
        )
        onRename()
    }

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
            onClick={event => {
                if (event.detail === 2 && event.target === event.currentTarget) {
                    // It is a double click; open the project.
                    setProjectEvent({
                        type: reactiveEvents.ProjectEventType.open,
                        projectId: item.id,
                    })
                } else if (
                    event.detail === 1 &&
                    (selected ||
                        (event.ctrlKey && !event.altKey && !event.shiftKey && !event.metaKey))
                ) {
                    setIsNameEditable(true)
                }
            }}
        >
            <ProjectActionButton
                project={item}
                getExtraData={getExtraData}
                setExtraData={setExtraData}
                deleteExtraData={deleteExtraData}
                event={projectEvent}
                doOpenManually={doOpenManually}
                appRunner={appRunner}
                openIde={() => {
                    doOpenIde(item)
                }}
                onClose={doCloseIde}
                doRefresh={doRefresh}
            />
            <EditableSpan
                editable={isNameEditable}
                onBlur={async event => {
                    setIsNameEditable(false)
                    if (event.target.value === item.title) {
                        toast.success('The project name is unchanged.')
                    } else {
                        await doRename(event.target.value)
                    }
                }}
                onCancel={() => {
                    setIsNameEditable(false)
                }}
                {...(backend.type === backendModule.BackendType.local
                    ? {
                          inputPattern: validation.LOCAL_PROJECT_NAME_PATTERN,
                          inputTitle: validation.LOCAL_PROJECT_NAME_TITLE,
                      }
                    : {})}
                className="px-2 bg-transparent"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}

// =====================
// === ProjectsTable ===
// =====================

/** Data associated with a project, used for rendering. */
export interface ProjectExtraData {
    isRunning: boolean
}

/** The default {@link ProjectExtraData} associated with a {@link backendModule.Project}. */
export const DEFAULT_PROJECT_EXTRA_DATA: ProjectExtraData = Object.freeze({
    isRunning: false,
})

/** Props for a {@link ProjectsTable}. */
export interface ProjectsTableProps {
    appRunner: AppRunner | null
    items: backendModule.ProjectAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    projectEvent: reactiveEvents.ProjectEvent | null
    setProjectEvent: (projectEvent: reactiveEvents.ProjectEvent | null) => void
    doCreateBlankProject: () => Promise<void>
    onCreate: () => void
    onRename: () => void
    onDelete: () => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
    onAssetClick: (
        asset: backendModule.ProjectAsset,
        event: React.MouseEvent<HTMLTableRowElement>
    ) => void
    doRefresh: () => void
}

/** The table of project assets. */
function ProjectsTable(props: ProjectsTableProps) {
    const {
        appRunner,
        items,
        isLoading,
        columnDisplayMode,
        projectEvent,
        setProjectEvent,
        doCreateBlankProject,
        onCreate,
        onRename,
        onDelete,
        doOpenIde,
        doCloseIde: rawDoCloseIde,
        onAssetClick,
        doRefresh,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    // const extraDatas = useMap<backendModule.ProjectId, ProjectExtraData>()
    const [extraDatas, setExtraDatas] = React.useState<
        Record<backendModule.ProjectId, ProjectExtraData>
    >({})

    const getExtraData = (projectId: backendModule.ProjectId) =>
        extraDatas[projectId] ?? DEFAULT_PROJECT_EXTRA_DATA

    const setExtraData = (projectId: backendModule.ProjectId, extraData: ProjectExtraData) => {
        setExtraDatas(oldExtraDatas => ({ ...oldExtraDatas, [projectId]: extraData }))
    }

    /** This must never change as it needs to be called exactly once when
     * the {@link ProjectActionButton} is being cleaned up. */
    const deleteExtraData = React.useCallback((projectId: backendModule.ProjectId) => {
        setExtraDatas(oldExtraDatas => {
            // The unused variable is intentional as it needs to be removed from the map.
            // eslint-disable-next-line @typescript-eslint/no-unused-vars
            const { [projectId]: ignored, ...newExtraDatas } = oldExtraDatas
            return newExtraDatas
        })
    }, [])

    React.useEffect(() => {
        if (projectEvent != null) {
            setProjectEvent(null)
        }
    }, [projectEvent, setProjectEvent])

    const doOpenManually = (projectId: backendModule.ProjectId) => {
        setProjectEvent({
            type: reactiveEvents.ProjectEventType.open,
            projectId,
        })
    }

    const doCloseIde = () => {
        setProjectEvent({
            type: reactiveEvents.ProjectEventType.cancelOpeningAll,
        })
        rawDoCloseIde()
    }

    return (
        <Table<backendModule.ProjectAsset, ProjectNamePropsState>
            items={items}
            isLoading={isLoading}
            state={{
                appRunner,
                projectEvent,
                setProjectEvent,
                doOpenManually,
                onRename,
                doOpenIde,
                doCloseIde,
                doRefresh,
                getExtraData,
                setExtraData,
                deleteExtraData,
            }}
            getKey={backendModule.getAssetId}
            placeholder={
                <span className="opacity-75">
                    You have no project yet. Go ahead and create one using the form above.
                </span>
            }
            columns={columnModule.columnsFor(columnDisplayMode, backend.type).map(column =>
                column === columnModule.Column.name
                    ? {
                          id: column,
                          className: columnModule.COLUMN_CSS_CLASS[column],
                          heading: (
                              <ProjectNameHeading
                                  doCreateProject={doCreateBlankProject}
                                  onCreate={onCreate}
                              />
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
                const isDeleteDisabled =
                    backend.type === backendModule.BackendType.local &&
                    getExtraData(projectAsset.id).isRunning
                const doOpenForEditing = () => {
                    unsetModal()
                    setProjectEvent({
                        type: reactiveEvents.ProjectEventType.open,
                        projectId: projectAsset.id,
                    })
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
                    const innerDoRename = async (newName: string) => {
                        await backend.projectUpdate(projectAsset.id, {
                            ami: null,
                            ideVersion: null,
                            projectName: newName,
                        })
                    }
                    setModal(() => (
                        <RenameModal
                            name={projectAsset.title}
                            assetType={projectAsset.type}
                            doRename={innerDoRename}
                            onSuccess={onRename}
                            {...(backend.type === backendModule.BackendType.local
                                ? {
                                      namePattern: validation.LOCAL_PROJECT_NAME_PATTERN,
                                      title: validation.LOCAL_PROJECT_NAME_TITLE,
                                  }
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
                    <ContextMenu key={projectAsset.id} event={event}>
                        <ContextMenuEntry onClick={doOpenForEditing}>
                            Open for editing
                        </ContextMenuEntry>
                        {backend.type !== backendModule.BackendType.local && (
                            <ContextMenuEntry disabled onClick={doOpenAsFolder}>
                                Open as folder
                            </ContextMenuEntry>
                        )}
                        <ContextMenuEntry onClick={doRename}>Rename</ContextMenuEntry>
                        <ContextMenuEntry
                            disabled={isDeleteDisabled}
                            {...(isDeleteDisabled
                                ? {
                                      title: 'A running local project cannot be removed.',
                                  }
                                : {})}
                            onClick={doDelete}
                        >
                            <span className="text-red-700">Delete</span>
                        </ContextMenuEntry>
                    </ContextMenu>
                ))
            }}
        />
    )
}

export default ProjectsTable
