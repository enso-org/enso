/** @file Table displaying a list of projects. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'
import PlusIcon from 'enso-assets/plus.svg'

import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as columnModule from '../column'
import * as dateTime from '../dateTime'
import * as errorModule from '../../error'
import * as eventModule from '../event'
import * as hooks from '../../hooks'
import * as modalProvider from '../../providers/modal'
import * as permissions from '../permissions'
import * as presence from '../presence'
import * as projectEventModule from '../events/projectEvent'
import * as projectListEventModule from '../events/projectListEvent'
import * as shortcuts from '../shortcuts'
import * as string from '../../string'
import * as svg from '../../components/svg'
import * as uniqueString from '../../uniqueString'
import * as validation from '../validation'

import * as spinner from './spinner'
import * as tableColumn from './tableColumn'
import TableRow, * as tableRow from './tableRow'
import ConfirmDeleteModal from './confirmDeleteModal'
import ContextMenu from './contextMenu'
import ContextMenuEntry from './contextMenuEntry'
import EditableSpan from './editableSpan'
import Table from './table'

// =============
// === Types ===
// =============

/** The state of the spinner. It should go from initial, to loading, to done. */
enum SpinnerState {
    initial = 'initial',
    loading = 'loading',
    done = 'done',
}

/** The state of checking whether a project is ready. It should go from not checking, to checking
 * status, to checking resources, to done. */
enum CheckState {
    /** The project is not open. */
    notChecking = 'not-checking',
    /** A local project is being opened. There are no status and resource checks; the state is
     * set to done when the RPC call finishes. */
    localProject = 'local-project',
    /** Status is not yet `ProjectState.opened`. */
    checkingStatus = 'checking-status',
    /** `backend.checkResources` calls are still failing. */
    checkingResources = 'checking-resources',
    /** The project is open. */
    done = 'done',
}

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
        You have no projects yet. Go ahead and create one using the form above.
    </span>
)

// === ProjectActionButton constants ===

const LOADING_MESSAGE =
    'Your environment is being created. It will take some time, please be patient.'
/** The interval between requests checking whether the IDE is ready. */
const CHECK_STATUS_INTERVAL_MS = 5000
/** The interval between requests checking whether the VM is ready. */
const CHECK_RESOURCES_INTERVAL_MS = 1000
/** The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState},
 * when using the remote backend. */
const REMOTE_SPINNER_STATE: Record<backendModule.ProjectState, spinner.SpinnerState> = {
    [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingSlow,
    [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}
/** The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState},
 * when using the local backend. */
const LOCAL_SPINNER_STATE: Record<backendModule.ProjectState, spinner.SpinnerState> = {
    [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingMedium,
    [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}

// ===========================
// === ProjectActionButton ===
// ===========================

/** Props for a {@link ProjectActionButton}. */
export interface ProjectActionButtonProps {
    project: backendModule.ProjectAsset
    rowState: ProjectRowState
    setRowState: React.Dispatch<React.SetStateAction<ProjectRowState>>
    projectEvent: projectEventModule.ProjectEvent | null
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    onClose: () => void
    appRunner: AppRunner | null
    openIde: () => void
}

/** An interactive button displaying the status of a project. */
function ProjectActionButton(props: ProjectActionButtonProps) {
    const {
        project,
        rowState,
        setRowState,
        projectEvent,
        appRunner,
        doOpenManually,
        onClose,
        openIde,
    } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const shouldCheckIfActuallyOpen =
        backend.type === backendModule.BackendType.remote &&
        (project.projectState.type === backendModule.ProjectState.opened ||
            project.projectState.type === backendModule.ProjectState.openInProgress)

    const [state, setState] = React.useState(() => {
        if (shouldCheckIfActuallyOpen) {
            return backendModule.ProjectState.created
        } else {
            return project.projectState.type
        }
    })
    const [checkState, setCheckState] = React.useState(CheckState.notChecking)
    const [spinnerState, setSpinnerState] = React.useState(REMOTE_SPINNER_STATE[state])
    const [onSpinnerStateChange, setOnSpinnerStateChange] = React.useState<
        ((state: spinner.SpinnerState | null) => void) | null
    >(null)
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
    const [toastId, setToastId] = React.useState<toastify.Id | null>(null)

    const openProject = React.useCallback(async () => {
        setState(backendModule.ProjectState.openInProgress)
        try {
            switch (backend.type) {
                case backendModule.BackendType.remote:
                    setToastId(toastify.toast.loading(LOADING_MESSAGE))
                    setRowState({ ...rowState, isRunning: true })
                    await backend.openProject(project.id, null, project.title)
                    setCheckState(CheckState.checkingStatus)
                    break
                case backendModule.BackendType.local:
                    setCheckState(CheckState.localProject)
                    setRowState({ ...rowState, isRunning: true })
                    await backend.openProject(project.id, null, project.title)
                    setCheckState(oldCheckState => {
                        if (oldCheckState === CheckState.localProject) {
                            setState(backendModule.ProjectState.opened)
                        }
                        return oldCheckState
                    })
                    break
            }
        } catch (error) {
            setCheckState(CheckState.notChecking)
            toastify.toast.error(
                `Error opening project '${project.title}': ${
                    errorModule.tryGetMessage(error) ?? 'unknown error'
                }.`
            )
            setState(backendModule.ProjectState.closed)
        }
    }, [backend, project.id, project.title, rowState, /* should never change */ setRowState])

    React.useEffect(() => {
        if (toastId != null) {
            return () => {
                toastify.toast.dismiss(toastId)
            }
        } else {
            return
        }
    }, [toastId])

    React.useEffect(() => {
        // Ensure that the previous spinner state is visible for at least one frame.
        requestAnimationFrame(() => {
            const newSpinnerState =
                backend.type === backendModule.BackendType.remote
                    ? REMOTE_SPINNER_STATE[state]
                    : LOCAL_SPINNER_STATE[state]
            setSpinnerState(newSpinnerState)
            onSpinnerStateChange?.(
                state === backendModule.ProjectState.closed ? null : newSpinnerState
            )
        })
    }, [state, onSpinnerStateChange, backend.type])

    React.useEffect(() => {
        onSpinnerStateChange?.(spinner.SpinnerState.initial)
    }, [onSpinnerStateChange])

    React.useEffect(() => {
        if (toastId != null && state !== backendModule.ProjectState.openInProgress) {
            toastify.toast.dismiss(toastId)
        }
    }, [state, toastId])

    React.useEffect(() => {
        if (shouldCheckIfActuallyOpen) {
            setState(backendModule.ProjectState.openInProgress)
            setCheckState(CheckState.checkingResources)
        }
    }, [shouldCheckIfActuallyOpen])

    hooks.useEventHandler(projectEvent, event => {
        switch (event.type) {
            default: {
                // Ignore; all other events are handled by `ProjectRow`.
                break
            }
            case projectEventModule.ProjectEventType.open: {
                if (event.projectId !== project.id) {
                    setShouldOpenWhenReady(false)
                } else {
                    setShouldOpenWhenReady(true)
                    void openProject()
                }
                break
            }
            case projectEventModule.ProjectEventType.cancelOpeningAll: {
                // `CheckState` should not be set to `notChecking`, as there is currently no way
                // to actually cancel an open action. Instead, the project should not be opened
                // automatically.
                setShouldOpenWhenReady(false)
                onSpinnerStateChange?.(null)
                setOnSpinnerStateChange(null)
                break
            }
            case projectEventModule.ProjectEventType.create: {
                if (event.placeholderId === project.id) {
                    setState(backendModule.ProjectState.openInProgress)
                    setOnSpinnerStateChange(() => event.onSpinnerStateChange)
                } else if (event.onSpinnerStateChange === onSpinnerStateChange) {
                    setOnSpinnerStateChange(null)
                }
                break
            }
            case projectEventModule.ProjectEventType.showAsOpening: {
                if (event.projectId === project.id) {
                    setState(backendModule.ProjectState.openInProgress)
                }
                break
            }
        }
    })

    React.useEffect(() => {
        if (shouldOpenWhenReady && state === backendModule.ProjectState.opened) {
            openIde()
            setShouldOpenWhenReady(false)
        }
    }, [shouldOpenWhenReady, state, openIde])

    React.useEffect(() => {
        switch (checkState) {
            case CheckState.notChecking:
            case CheckState.localProject:
            case CheckState.done: {
                return
            }
            case CheckState.checkingStatus: {
                let handle: number | null = null
                let continuePolling = true
                let previousTimestamp = 0
                const checkProjectStatus = async () => {
                    try {
                        const response = await backend.getProjectDetails(project.id, project.title)
                        handle = null
                        if (
                            continuePolling &&
                            response.state.type === backendModule.ProjectState.opened
                        ) {
                            continuePolling = false
                            setCheckState(CheckState.checkingResources)
                        }
                    } finally {
                        if (continuePolling) {
                            const nowTimestamp = Number(new Date())
                            const delay =
                                CHECK_STATUS_INTERVAL_MS - (nowTimestamp - previousTimestamp)
                            previousTimestamp = nowTimestamp
                            handle = window.setTimeout(
                                () => void checkProjectStatus(),
                                Math.max(0, delay)
                            )
                        }
                    }
                }
                void checkProjectStatus()
                return () => {
                    continuePolling = false
                    if (handle != null) {
                        window.clearTimeout(handle)
                    }
                }
            }
            case CheckState.checkingResources: {
                let handle: number | null = null
                let continuePolling = true
                let previousTimestamp = 0
                const checkProjectResources = async () => {
                    if (backend.type === backendModule.BackendType.local) {
                        // This should never happen, but still should be handled.
                        setState(backendModule.ProjectState.opened)
                        setCheckState(CheckState.done)
                    } else {
                        try {
                            // This call will error if the VM is not ready yet.
                            await backend.checkResources(project.id, project.title)
                            handle = null
                            if (continuePolling) {
                                continuePolling = false
                                setState(backendModule.ProjectState.opened)
                                setCheckState(CheckState.done)
                            }
                        } catch {
                            if (continuePolling) {
                                const nowTimestamp = Number(new Date())
                                const delay =
                                    CHECK_RESOURCES_INTERVAL_MS - (nowTimestamp - previousTimestamp)
                                previousTimestamp = nowTimestamp
                                handle = window.setTimeout(
                                    () => void checkProjectResources(),
                                    Math.max(0, delay)
                                )
                            }
                        }
                    }
                }
                void checkProjectResources()
                return () => {
                    continuePolling = false
                    if (handle != null) {
                        window.clearTimeout(handle)
                    }
                }
            }
        }
    }, [checkState, project.id, project.title, backend])

    const closeProject = async () => {
        onClose()
        setShouldOpenWhenReady(false)
        setState(backendModule.ProjectState.closed)
        onSpinnerStateChange?.(null)
        setOnSpinnerStateChange(null)
        appRunner?.stopApp()
        setCheckState(CheckState.notChecking)
        try {
            await backend.closeProject(project.id, project.title)
        } finally {
            // This is not 100% correct, but it is better than never setting `isRunning` to `false`,
            // which would prevent the project from ever being deleted.
            setRowState({ ...rowState, isRunning: false })
        }
    }

    switch (state) {
        case null:
        case backendModule.ProjectState.created:
        case backendModule.ProjectState.new:
        case backendModule.ProjectState.closed:
            return (
                <button
                    className="w-6"
                    onClick={clickEvent => {
                        clickEvent.stopPropagation()
                        unsetModal()
                        doOpenManually(project.id)
                    }}
                >
                    <img src={PlayIcon} />
                </button>
            )
        case backendModule.ProjectState.openInProgress:
            return (
                <button
                    className="w-6"
                    onClick={async clickEvent => {
                        clickEvent.stopPropagation()
                        unsetModal()
                        await closeProject()
                    }}
                >
                    <svg.StopIcon className={spinner.SPINNER_CSS_CLASSES[spinnerState]} />
                </button>
            )
        case backendModule.ProjectState.opened:
            return (
                <>
                    <button
                        className="w-6"
                        onClick={async clickEvent => {
                            clickEvent.stopPropagation()
                            unsetModal()
                            await closeProject()
                        }}
                    >
                        <svg.StopIcon className={spinner.SPINNER_CSS_CLASSES[spinnerState]} />
                    </button>
                    <button
                        className="w-6"
                        onClick={clickEvent => {
                            clickEvent.stopPropagation()
                            unsetModal()
                            openIde()
                        }}
                    >
                        <img src={ArrowUpIcon} />
                    </button>
                </>
            )
    }
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
            {string.capitalizeFirst(ASSET_TYPE_NAME_PLURAL)}
            <button className="mx-1" onClick={onClick}>
                <img src={PlusIcon} />
            </button>
        </div>
    )
}

// ===================
// === ProjectName ===
// ===================

/** Props for a {@link ProjectName}. */
interface InternalProjectNameProps
    extends tableColumn.TableColumnProps<
        backendModule.ProjectAsset,
        ProjectsTableState,
        ProjectRowState
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

    const doRename = async (newName: string) => {
        try {
            await backend.projectUpdate(
                item.id,
                {
                    ami: null,
                    ideVersion: null,
                    projectName: newName,
                },
                item.title
            )
            return
        } catch (error) {
            errorModule.toastAndLog('Unable to rename project', error)
            throw error
        }
    }

    return (
        <div
            className="flex text-left items-center align-middle whitespace-nowrap"
            onClick={event => {
                if (!rowState.isEditingName && eventModule.isDoubleClick(event)) {
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
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: true,
                    }))
                }
            }}
        >
            <ProjectActionButton
                project={item}
                rowState={rowState}
                setRowState={setRowState}
                projectEvent={projectEvent}
                doOpenManually={doOpenManually}
                appRunner={appRunner}
                openIde={() => {
                    doOpenIde(item)
                }}
                onClose={doCloseIde}
            />
            <EditableSpan
                editable={rowState.isEditingName}
                onSubmit={async newTitle => {
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
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
                    setRowState(oldRowState => ({
                        ...oldRowState,
                        isEditingName: false,
                    }))
                }}
                {...(backend.type === backendModule.BackendType.local
                    ? {
                          inputPattern: validation.LOCAL_PROJECT_NAME_PATTERN,
                          inputTitle: validation.LOCAL_PROJECT_NAME_TITLE,
                      }
                    : {})}
                className={`bg-transparent grow px-2 ${
                    rowState.isEditingName ? 'cursor-text' : 'cursor-pointer'
                }`}
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
    innerProps: tableRow.TableRowInnerProps<
        backendModule.ProjectAsset,
        backendModule.ProjectId,
        ProjectRowState
    >
    event: React.MouseEvent
    dispatchProjectEvent: (projectListEvent: projectEventModule.ProjectEvent) => void
    doDelete: () => Promise<void>
}

/** The context menu for a row of a {@link ProjectsTable}. */
function ProjectRowContextMenu(props: InternalProjectRowContextMenuProps) {
    const {
        innerProps: { item, rowState, setRowState },
        event,
        dispatchProjectEvent,
        doDelete,
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
        setRowState(oldRowState => ({
            ...oldRowState,
            isEditingName: true,
        }))
        unsetModal()
    }
    return (
        <ContextMenu key={item.id} event={event}>
            <ContextMenuEntry onClick={doOpenForEditing}>Open for editing</ContextMenuEntry>
            {/* TODO[sb]: Implement once backend support is in place.
              * https://github.com/enso-org/cloud-v2/issues/506
            backend.type !== backendModule.BackendType.local && (
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

// ==================
// === ProjectRow ===
// ==================

/** A row in a {@link ProjectsTable}. */
function ProjectRow(
    props: tableRow.TableRowProps<
        backendModule.ProjectAsset,
        backendModule.ProjectId,
        ProjectsTableState,
        ProjectRowState
    >
) {
    const {
        keyProp: key,
        item: rawItem,
        state: {
            projectEvent,
            dispatchProjectEvent,
            dispatchProjectListEvent,
            markItemAsHidden,
            markItemAsVisible,
        },
    } = props
    const { backend } = backendProvider.useBackend()
    const { setModal } = modalProvider.useSetModal()
    const [item, setItem] = React.useState(rawItem)
    const [status, setStatus] = React.useState(presence.Presence.present)

    React.useEffect(() => {
        setItem(rawItem)
    }, [rawItem])

    const doDelete = async () => {
        setStatus(presence.Presence.deleting)
        markItemAsHidden(key)
        try {
            await backend.deleteProject(item.id, item.title)
            dispatchProjectListEvent({
                type: projectListEventModule.ProjectListEventType.delete,
                projectId: key,
            })
        } catch (error) {
            setStatus(presence.Presence.present)
            markItemAsVisible(key)
            errorModule.toastAndLog('Unable to delete project', error)
        }
    }

    hooks.useEventHandler(projectEvent, async event => {
        switch (event.type) {
            default: {
                // Ignore; all other events are handled by `ProjectActionButton`.
                break
            }
            case projectEventModule.ProjectEventType.create: {
                if (key === event.placeholderId) {
                    setStatus(presence.Presence.inserting)
                    try {
                        const createdProject = await backend.createProject({
                            parentDirectoryId: item.parentId,
                            projectName: item.title,
                            projectTemplateName: event.templateId,
                        })
                        setStatus(presence.Presence.present)
                        const newItem: backendModule.ProjectAsset = {
                            ...item,
                            id: createdProject.projectId,
                            projectState: createdProject.state,
                        }
                        setItem(newItem)
                        // This MUST be delayed, otherwise the `ProjectActionButton` does not yet
                        // have the correct `Project`.
                        setTimeout(() => {
                            dispatchProjectEvent({
                                type: projectEventModule.ProjectEventType.open,
                                projectId: createdProject.projectId,
                            })
                        }, 0)
                    } catch (error) {
                        dispatchProjectListEvent({
                            type: projectListEventModule.ProjectListEventType.delete,
                            projectId: key,
                        })
                        errorModule.toastAndLog('Error creating new project', error)
                    }
                }
                break
            }
            case projectEventModule.ProjectEventType.deleteMultiple: {
                if (event.projectIds.has(key)) {
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
                    <ProjectRowContextMenu
                        innerProps={innerProps}
                        event={event}
                        dispatchProjectEvent={dispatchProjectEvent}
                        doDelete={doDelete}
                    />
                )
            }}
            item={item}
        />
    )
}

// =====================
// === ProjectsTable ===
// =====================

/** State passed through from a {@link ProjectsTable} to every cell. */
interface ProjectsTableState {
    appRunner: AppRunner | null
    projectEvent: projectEventModule.ProjectEvent | null
    dispatchProjectEvent: (event: projectEventModule.ProjectEvent) => void
    dispatchProjectListEvent: (event: projectListEventModule.ProjectListEvent) => void
    markItemAsHidden: (key: string) => void
    markItemAsVisible: (key: string) => void
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    doOpenIde: (project: backendModule.ProjectAsset) => void
    doCloseIde: () => void
}

/** Data associated with a {@link ProjectRow}, used for rendering. */
export interface ProjectRowState {
    isRunning: boolean
    isEditingName: boolean
}

/** The default {@link ProjectRowState} associated with a {@link ProjectRow}. */
export const INITIAL_ROW_STATE: ProjectRowState = Object.freeze({
    isRunning: false,
    isEditingName: false,
})

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
    const { organization } = authProvider.useNonPartialUserSession()
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

    hooks.useEventHandler(projectListEvent, event => {
        switch (event.type) {
            case projectListEventModule.ProjectListEventType.create: {
                const projectName = getNewProjectName(event.templateId)
                const dummyId = backendModule.ProjectId(uniqueString.uniqueString())
                const placeholderItem: backendModule.ProjectAsset = {
                    id: dummyId,
                    title: projectName,
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId: directoryId ?? backendModule.DirectoryId(''),
                    permissions: permissions.tryGetSingletonOwnerPermission(organization),
                    projectState: { type: backendModule.ProjectState.new },
                    type: backendModule.AssetType.project,
                }
                setItems(oldProjectAssets => [placeholderItem, ...oldProjectAssets])
                dispatchProjectEvent({
                    type: projectEventModule.ProjectEventType.create,
                    placeholderId: dummyId,
                    templateId: event.templateId,
                    onSpinnerStateChange: event.onSpinnerStateChange,
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
        (): ProjectsTableState => ({
            appRunner,
            projectEvent,
            dispatchProjectEvent,
            dispatchProjectListEvent,
            markItemAsHidden,
            markItemAsVisible,
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
            markItemAsHidden,
            markItemAsVisible,
            /* should never change */ dispatchProjectEvent,
            /* should never change */ dispatchProjectListEvent,
        ]
    )

    return (
        <Table<
            backendModule.ProjectAsset,
            backendModule.ProjectId,
            ProjectsTableState,
            ProjectRowState
        >
            rowComponent={ProjectRow}
            items={visibleItems}
            isLoading={isLoading}
            state={state}
            initialRowState={INITIAL_ROW_STATE}
            getKey={backendModule.getAssetId}
            placeholder={PLACEHOLDER}
            forceShowPlaceholder={shouldForceShowPlaceholder}
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
            onContextMenu={(selectedKeys, event, setSelectedKeys) => {
                event.preventDefault()
                event.stopPropagation()
                // This is not a React component even though it contains JSX.
                // eslint-disable-next-line no-restricted-syntax
                const doDeleteAll = () => {
                    setModal(
                        <ConfirmDeleteModal
                            description={`${selectedKeys.size} selected projects`}
                            doDelete={() => {
                                setSelectedKeys(new Set())
                                dispatchProjectEvent({
                                    type: projectEventModule.ProjectEventType.deleteMultiple,
                                    projectIds: selectedKeys,
                                })
                                return Promise.resolve()
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

export default ProjectsTable
