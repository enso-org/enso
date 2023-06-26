/** @file Form to create a project. */
import * as React from 'react'

import PlusIcon from 'enso-assets/plus.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as modalProvider from '../../providers/modal'
import * as projectEventModule from '../events/projectEvent'
import * as projectListEventModule from '../events/projectListEvent'
import * as projectRowState from '../projectRowState'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
import * as toastPromise from '../toastPromise'
import * as uniqueString from '../../uniqueString'
import * as validation from '../validation'

import Table, * as table from './table'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'
import ProjectActionButton from './projectActionButton'
import RenameModal from './renameModal'

// =================
// === Constants ===
// =================

/** The user-facing name of this asset type. */
const ASSET_TYPE_NAME = 'project'
/** The user-facing plural name of this asset type. */
const ASSET_TYPE_NAME_PLURAL = 'projects'
// This is a function, even though it is not syntactically a function.
// eslint-disable-next-line no-restricted-syntax
const pluralize = string.makePluralize(ASSET_TYPE_NAME, ASSET_TYPE_NAME_PLURAL)
/** Placeholder row. */
const PLACEHOLDER = (
    <span className="opacity-75">
        You have no project yet. Go ahead and create one using the form above.
    </span>
)

// ==========================
// === ProjectNameHeading ===
// ==========================

/** Props for a {@link ProjectNameHeading}. */
interface InternalProjectNameHeadingProps {
    doCreateProject: () => void
}

/** The column header for the "name" column for the table of project assets. */
function ProjectNameHeading(props: InternalProjectNameHeadingProps) {
    const { doCreateProject } = props

    const onClick = (event: React.MouseEvent) => {
        event.stopPropagation()
        doCreateProject()
    }

    return (
        <div className="inline-flex">
            Project
            <button className="mx-1" onClick={onClick}>
                <img src={PlusIcon} />
            </button>
        </div>
    )
}

// ===================
// === ProjectName ===
// ===================

/** State passed through from a {@link ProjectsTable} to every cell. */
interface ProjectNamePropsState {
    appRunner: AppRunner | null
    projectEvent: projectEventModule.ProjectEvent | null
    dispatchProjectEvent: (projectEvent: projectEventModule.ProjectEvent) => void
    dispatchProjectListEvent: (projectListEvent: projectListEventModule.ProjectListEvent) => void
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** Props for a {@link ProjectName}. */
interface InternalProjectNameProps
    extends table.ColumnProps<
        backendModule.ProjectAsset,
        ProjectNamePropsState,
        projectRowState.ProjectRowState
    > {}

/** The icon and name of a specific project asset. */
function ProjectName(props: InternalProjectNameProps) {
    const {
        item,
        setItem,
        selected,
        rowState,
        setRowState,
        state: {
            appRunner,
            projectEvent,
            dispatchProjectEvent,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        },
    } = props
    const { backend } = backendProvider.useBackend()
    const [isNameEditable, setIsNameEditable] = React.useState(false)

    const doRename = async (newName: string) => {
        await toastPromise.toastPromise(
            backend.projectUpdate(
                item.id,
                {
                    ami: null,
                    ideVersion: null,
                    projectName: newName,
                },
                item.title
            ),
            {
                loading: 'Renaming project...',
                success: 'Renamed project',
                error: error =>
                    `Error renaming project: ${
                        errorModule.tryGetMessage(error) ?? 'unknown error'
                    }`,
            }
        )
    }

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
            onClick={event => {
                if (eventModule.isDoubleClick(event)) {
                    // It is a double click; open the project.
                    dispatchProjectEvent({
                        type: projectEventModule.ProjectEventType.open,
                        projectId: item.id,
                    })
                } else if (
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
            <ProjectActionButton
                project={item}
                rowState={rowState}
                setRowState={setRowState}
                event={projectEvent}
                doOpenManually={doOpenManually}
                appRunner={appRunner}
                openIde={() => {
                    doOpenIde(item)
                }}
                onClose={doCloseIde}
            />
            <EditableSpan
                editable={isNameEditable}
                onSubmit={async newTitle => {
                    setIsNameEditable(false)
                    if (newTitle !== item.title) {
                        const oldTitle = item.title
                        setItem(oldItem => ({ ...oldItem, title: newTitle }))
                        try {
                            await doRename(newTitle)
                        } catch {
                            setItem(oldItem => ({ ...oldItem, title: oldTitle }))
                        }
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
                className="cursor-pointer bg-transparent grow px-2"
            >
                {item.title}
            </EditableSpan>
        </div>
    )
}

// =============================
// === ProjectRowContextMenu ===
// =============================

/** Props for a {@link ProjectRowContextMenu}. */
interface InternalProjectRowContextMenuProps {
    innerProps: table.RowInnerProps<backendModule.ProjectAsset, projectRowState.ProjectRowState>
    event: React.MouseEvent
    dispatchProjectEvent: (projectListEvent: projectEventModule.ProjectEvent) => void
}

/** The context menu for a row of a {@link ProjectsTable}. */
function ProjectRowContextMenu(props: InternalProjectRowContextMenuProps) {
    const {
        innerProps: { item, rowState },
        event,
        dispatchProjectEvent,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const isDeleteDisabled = backend.type === backendModule.BackendType.local && rowState.isRunning
    const doOpenForEditing = () => {
        unsetModal()
        dispatchProjectEvent({
            type: projectEventModule.ProjectEventType.open,
            projectId: item.id,
        })
    }
    const doRename = () => {
        const innerDoRename = async (newName: string) => {
            await backend.projectUpdate(
                item.id,
                {
                    ami: null,
                    ideVersion: null,
                    projectName: newName,
                },
                item.title
            )
        }
        setModal(
            <RenameModal
                name={item.title}
                assetType={item.type}
                doRename={innerDoRename}
                {...(backend.type === backendModule.BackendType.local
                    ? {
                          namePattern: validation.LOCAL_PROJECT_NAME_PATTERN,
                          title: validation.LOCAL_PROJECT_NAME_TITLE,
                      }
                    : {})}
            />
        )
    }
    const doDelete = () => {
        setModal(
            <ConfirmDeleteModal
                description={item.title}
                assetType={item.type}
                doDelete={() => backend.deleteProject(item.id, item.title)}
            />
        )
    }
    return (
        <ContextMenu key={item.id} event={event}>
            <ContextMenuEntry onClick={doOpenForEditing}>Open for editing</ContextMenuEntry>
            {/*backend.type !== backendModule.BackendType.local && (
                            <ContextMenuEntry disabled onClick={doOpenAsFolder}>
                                Open as folder
                            </ContextMenuEntry>
                        )*/}
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
    )
}

// =====================
// === ProjectsTable ===
// =====================

/** Props for a {@link ProjectsTable}. */
export interface ProjectsTableProps {
    appRunner: AppRunner | null
    directoryId: backendModule.DirectoryId | null
    items: backendModule.ProjectAsset[]
    filter: ((item: backendModule.ProjectAsset) => boolean) | null
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    projectEvent: projectEventModule.ProjectEvent | null
    dispatchProjectEvent: (projectEvent: projectEventModule.ProjectEvent) => void
    projectListEvent: projectListEventModule.ProjectListEvent | null
    dispatchProjectListEvent: (projectListEvent: projectListEventModule.ProjectListEvent) => void
    doCreateProject: () => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** The table of project assets. */
function ProjectsTable(props: ProjectsTableProps) {
    const {
        appRunner,
        directoryId,
        items: rawItems,
        filter,
        isLoading,
        columnDisplayMode,
        projectEvent,
        dispatchProjectEvent,
        projectListEvent,
        dispatchProjectListEvent,
        doCreateProject,
        doOpenIde,
        doCloseIde: rawDoCloseIde,
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [items, setItems] = React.useState(rawItems)

    React.useEffect(() => {
        setItems(rawItems)
    }, [rawItems])

    const visibleItems = React.useMemo(
        () => (filter != null ? items.filter(filter) : items),
        [items, filter]
    )

    const getNewProjectName = React.useCallback(
        (templateId?: string | null) => {
            const prefix = `${templateId ?? 'New_Project'}_`
            const projectNameTemplate = new RegExp(`^${prefix}(?<projectIndex>\\d+)$`)
            const projectIndices = items
                .map(project => projectNameTemplate.exec(project.title)?.groups?.projectIndex)
                .map(maybeIndex => (maybeIndex != null ? parseInt(maybeIndex, 10) : 0))
            return `${prefix}${Math.max(0, ...projectIndices) + 1}`
        },
        [items]
    )

    hooks.useEvent(projectListEvent, async event => {
        switch (event.type) {
            case projectListEventModule.ProjectListEventType.create: {
                const projectName = getNewProjectName(event.templateId)
                // Although this is a dummy value, it MUST be unique as it is used
                // as the React key for lists.
                const dummyId = backendModule.ProjectId(uniqueString.uniqueString())
                const placeholderItem: backendModule.ProjectAsset = {
                    type: backendModule.AssetType.project,
                    title: projectName,
                    id: dummyId,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: directoryId ?? backendModule.DirectoryId(''),
                    permissions: [],
                    projectState: { type: backendModule.ProjectState.new },
                }
                setItems(oldProjectAssets => [placeholderItem, ...oldProjectAssets])
                dispatchProjectEvent({
                    type: projectEventModule.ProjectEventType.showAsOpening,
                    projectId: dummyId,
                })
                // FIXME: individual rows should handle this?
                const createProjectPromise = backend.createProject({
                    projectName,
                    projectTemplateName: event.templateId ?? null,
                    parentDirectoryId: directoryId,
                })
                const createdProject = await toastPromise.toastPromise(createProjectPromise, {
                    loading: 'Creating new empty project...',
                    success: 'Created new empty project.',
                    error: error =>
                        `Could not create new empty project: ${
                            errorModule.tryGetMessage(error) ?? 'unknown error.'
                        }`,
                })
                const newItem: backendModule.ProjectAsset = {
                    ...placeholderItem,
                    type: backendModule.AssetType.project,
                    title: createdProject.name,
                    id: createdProject.projectId,
                    projectState: createdProject.state,
                }
                setItems(oldItems =>
                    oldItems.map(item => (item !== placeholderItem ? item : newItem))
                )
                dispatchProjectEvent({
                    type: projectEventModule.ProjectEventType.open,
                    projectId: createdProject.projectId,
                })
                break
            }
            case projectListEventModule.ProjectListEventType.delete: {
                setItems(oldItems => oldItems.filter(item => item.id !== event.projectId))
                break
            }
        }
    })

    const doOpenManually = React.useCallback(
        (projectId: backendModule.ProjectId) => {
            dispatchProjectEvent({
                type: projectEventModule.ProjectEventType.open,
                projectId,
            })
        },
        [/* should never change */ dispatchProjectEvent]
    )

    const doCloseIde = React.useCallback(() => {
        dispatchProjectEvent({
            type: projectEventModule.ProjectEventType.cancelOpeningAll,
        })
        rawDoCloseIde()
    }, [rawDoCloseIde, /* should never change */ dispatchProjectEvent])

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): ProjectNamePropsState => ({
            appRunner,
            projectEvent,
            dispatchProjectEvent,
            dispatchProjectListEvent,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        }),
        [
            appRunner,
            projectEvent,
            doOpenManually,
            doOpenIde,
            doCloseIde,
            /* should never change */ dispatchProjectEvent,
            /* should never change */ dispatchProjectListEvent,
        ]
    )

    return (
        <Table<backendModule.ProjectAsset, ProjectNamePropsState, projectRowState.ProjectRowState>
            items={visibleItems}
            isLoading={isLoading}
            state={state}
            initialRowState={projectRowState.INITIAL_ROW_STATE}
            getKey={backendModule.getAssetId}
            placeholder={PLACEHOLDER}
            columns={columnModule.columnsFor(columnDisplayMode, backend.type).map(column =>
                column === columnModule.Column.name
                    ? {
                          id: column,
                          className: columnModule.COLUMN_CSS_CLASS[column],
                          heading: <ProjectNameHeading doCreateProject={doCreateProject} />,
                          render: ProjectName,
                      }
                    : {
                          id: column,
                          className: columnModule.COLUMN_CSS_CLASS[column],
                          heading: <>{columnModule.COLUMN_NAME[column]}</>,
                          render: columnModule.COLUMN_RENDERER[column],
                      }
            )}
            onContextMenu={(projects, event, setSelectedItems) => {
                event.preventDefault()
                event.stopPropagation()
                // This is not a React component even though it contains JSX.
                // eslint-disable-next-line no-restricted-syntax
                const doDeleteAll = () => {
                    setModal(
                        <ConfirmDeleteModal
                            description={`${projects.size} selected projects`}
                            assetType="projects"
                            doDelete={() => {
                                setSelectedItems(new Set())
                                dispatchProjectEvent({
                                    type: projectEventModule.ProjectEventType.deleteMultiple,
                                    projectIds: new Set([...projects].map(project => project.id)),
                                })
                                return Promise.resolve()
                            }}
                        />
                    )
                }
                const pluralized = pluralize(projects.size)
                setModal(
                    <ContextMenu key={uniqueString.uniqueString()} event={event}>
                        <ContextMenuEntry onClick={doDeleteAll}>
                            <span className="text-red-700">
                                Delete {projects.size} {pluralized}
                            </span>
                        </ContextMenuEntry>
                    </ContextMenu>
                )
            }}
            onRowContextMenu={(innerProps, event) => {
                event.preventDefault()
                event.stopPropagation()
                setModal(
                    <ProjectRowContextMenu
                        innerProps={innerProps}
                        event={event}
                        dispatchProjectEvent={dispatchProjectEvent}
                    />
                )
            }}
        />
    )
}

export default ProjectsTable
