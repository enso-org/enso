/** @file Form to create a project. */
import * as React from 'react'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as projectEventModule from '../events/projectEvent'
import * as projectRowState from '../projectRowState'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
import * as svg from '../../components/svg'
import * as toastPromise from '../toastPromise'
import * as toastPromiseMultiple from '../../toastPromiseMultiple'
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
/** Messages to be passed to {@link toastPromiseMultiple.toastPromiseMultiple}. */
const TOAST_PROMISE_MULTIPLE_MESSAGES: toastPromiseMultiple.ToastPromiseMultipleMessages<backendModule.ProjectAsset> =
    {
        begin: total => `Deleting ${total} ${pluralize(total)}...`,
        inProgress: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        end: (successful, total) => `Deleted ${successful}/${total} ${pluralize(total)}.`,
        error: asset => `Could not delete ${ASSET_TYPE_NAME} '${asset.title}'.`,
    }

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
                {svg.ADD_ICON}
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
    setProjectEvent: (projectEvent: projectEventModule.ProjectEvent | null) => void
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
        selected,
        rowState,
        setRowState,
        state: { appRunner, projectEvent, setProjectEvent, doOpenManually, doOpenIde, doCloseIde },
    } = props
    const { backend } = backendProvider.useBackend()
    const [, doRefresh] = hooks.useRefresh()
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
                    setProjectEvent({
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
                        // Mutation is bad practice as it does not cause a re-render. However, a
                        // `useState` is not an option because a new value may come from the props.
                        // `doRefresh()` ensures that a re-render always happens.
                        item.title = newTitle
                        doRefresh()
                        try {
                            await doRename(newTitle)
                        } catch {
                            item.title = oldTitle
                            doRefresh()
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

// =====================
// === ProjectsTable ===
// =====================

/** Props for a {@link ProjectsTable}. */
export interface ProjectsTableProps {
    appRunner: AppRunner | null
    items: backendModule.ProjectAsset[]
    isLoading: boolean
    columnDisplayMode: columnModule.ColumnDisplayMode
    projectEvent: projectEventModule.ProjectEvent | null
    setProjectEvent: (projectEvent: projectEventModule.ProjectEvent | null) => void
    doCreateProject: () => void
    onRename: () => void
    onDelete: () => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
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
        doCreateProject,
        onRename,
        onDelete,
        doOpenIde,
        doCloseIde: rawDoCloseIde,
    } = props
    const logger = loggerProvider.useLogger()
    const { backend } = backendProvider.useBackend()
    const { setModal, unsetModal } = modalProvider.useSetModal()

    const doOpenManually = React.useCallback(
        (projectId: backendModule.ProjectId) => {
            setProjectEvent({
                type: projectEventModule.ProjectEventType.open,
                projectId,
            })
        },
        [setProjectEvent]
    )

    const doCloseIde = React.useCallback(() => {
        setProjectEvent({
            type: projectEventModule.ProjectEventType.cancelOpeningAll,
        })
        rawDoCloseIde()
    }, [rawDoCloseIde, setProjectEvent])

    const state = React.useMemo(
        // The type MUST be here to trigger excess property errors at typecheck time.
        (): ProjectNamePropsState => ({
            appRunner,
            projectEvent,
            setProjectEvent,
            doOpenManually,
            doOpenIde,
            doCloseIde,
        }),
        [appRunner, projectEvent, setProjectEvent, doOpenManually, doOpenIde, doCloseIde]
    )

    return (
        <Table<backendModule.ProjectAsset, ProjectNamePropsState, projectRowState.ProjectRowState>
            items={items}
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
                            shouldShowToast={false}
                            doDelete={async () => {
                                setSelectedItems(new Set())
                                await toastPromiseMultiple.toastPromiseMultiple(
                                    logger,
                                    [...projects],
                                    project => backend.deleteProject(project.id, project.title),
                                    TOAST_PROMISE_MULTIPLE_MESSAGES
                                )
                            }}
                            onSuccess={onDelete}
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
            onRowContextMenu={(project, event, rowState) => {
                event.preventDefault()
                event.stopPropagation()
                const isDeleteDisabled =
                    backend.type === backendModule.BackendType.local && rowState.isRunning
                const doOpenForEditing = () => {
                    unsetModal()
                    setProjectEvent({
                        type: projectEventModule.ProjectEventType.open,
                        projectId: project.id,
                    })
                }
                const doRename = () => {
                    const innerDoRename = async (newName: string) => {
                        await backend.projectUpdate(
                            project.id,
                            {
                                ami: null,
                                ideVersion: null,
                                projectName: newName,
                            },
                            project.title
                        )
                    }
                    setModal(
                        <RenameModal
                            name={project.title}
                            assetType={project.type}
                            doRename={innerDoRename}
                            onSuccess={onRename}
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
                            description={project.title}
                            assetType={project.type}
                            doDelete={() => backend.deleteProject(project.id, project.title)}
                            onSuccess={onDelete}
                        />
                    )
                }
                setModal(
                    <ContextMenu key={project.id} event={event}>
                        <ContextMenuEntry onClick={doOpenForEditing}>
                            Open for editing
                        </ContextMenuEntry>
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
            }}
        />
    )
}

export default ProjectsTable
