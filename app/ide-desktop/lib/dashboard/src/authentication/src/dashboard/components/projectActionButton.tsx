/** @file An interactive button displaying the status of a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as errorModule from '../../error'
import * as modalProvider from '../../providers/modal'
import * as projectEvent from '../events/projectEvent'
// Warning: This is a circular import.
import * as projectsTable from './projectsTable'
import * as svg from '../../components/svg'

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

const LOADING_MESSAGE =
    'Your environment is being created. It will take some time, please be patient.'
/** The interval between requests checking whether the IDE is ready. */
const CHECK_STATUS_INTERVAL_MS = 5000
/** The interval between requests checking whether the VM is ready. */
const CHECK_RESOURCES_INTERVAL_MS = 1000
/** The fallback project state, when it is set to `null` before it is first set. */
const DEFAULT_PROJECT_STATE = backendModule.ProjectState.created
/** The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState}. */
const SPINNER_STATE: Record<backendModule.ProjectState, SpinnerState> = {
    [backendModule.ProjectState.closed]: SpinnerState.initial,
    [backendModule.ProjectState.created]: SpinnerState.initial,
    [backendModule.ProjectState.new]: SpinnerState.initial,
    [backendModule.ProjectState.openInProgress]: SpinnerState.loading,
    [backendModule.ProjectState.opened]: SpinnerState.done,
}

const SPINNER_CSS_CLASSES: Record<SpinnerState, string> = {
    [SpinnerState.initial]: 'dasharray-5 ease-linear',
    [SpinnerState.loading]: 'dasharray-75 duration-90000 ease-linear',
    [SpinnerState.done]: 'dasharray-100 duration-1000 ease-in',
} as const

// =================
// === Component ===
// =================

/** Props for a {@link ProjectActionButton}. */
export interface ProjectActionButtonProps {
    project: backendModule.ProjectAsset
    rowState: projectsTable.ProjectRowState
    setRowState: (newRowState: projectsTable.ProjectRowState) => void
    event: projectEvent.ProjectEvent | null
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    onClose: () => void
    appRunner: AppRunner | null
    openIde: () => void
}

/** An interactive button displaying the status of a project. */
function ProjectActionButton(props: ProjectActionButtonProps) {
    const { project, rowState, setRowState, event, appRunner, doOpenManually, onClose, openIde } =
        props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [state, setState] = React.useState<backendModule.ProjectState | null>(null)
    const [checkState, setCheckState] = React.useState(CheckState.notChecking)
    const [spinnerState, setSpinnerState] = React.useState(SpinnerState.initial)
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
    const [toastId, setToastId] = React.useState<string | null>(null)

    const openProject = React.useCallback(async () => {
        setState(backendModule.ProjectState.openInProgress)
        try {
            switch (backend.type) {
                case backendModule.BackendType.remote:
                    setToastId(toast.loading(LOADING_MESSAGE))
                    await backend.openProject(project.id, null, project.title)
                    setRowState({ ...rowState, isRunning: true })
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
            toast.error(
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
                toast.dismiss(toastId)
            }
        } else {
            return
        }
    }, [toastId])

    React.useEffect(() => {
        // Ensure that the previous spinner state is visible for at least one frame.
        requestAnimationFrame(() => {
            setSpinnerState(SPINNER_STATE[state ?? DEFAULT_PROJECT_STATE])
        })
    }, [state])

    React.useEffect(() => {
        if (toastId != null && state !== backendModule.ProjectState.openInProgress) {
            toast.dismiss(toastId)
        }
    }, [state, toastId])

    React.useEffect(() => {
        switch (project.projectState.type) {
            case backendModule.ProjectState.opened:
                setState(backendModule.ProjectState.openInProgress)
                setCheckState(CheckState.checkingResources)
                setRowState({ ...rowState, isRunning: true })
                break
            case backendModule.ProjectState.openInProgress:
                setState(backendModule.ProjectState.openInProgress)
                setCheckState(CheckState.checkingStatus)
                setRowState({ ...rowState, isRunning: true })
                break
            default:
                // Some functions below set the state to something different to
                // the backend state. In that case, the state should not be overridden.
                setState(oldState => oldState ?? project.projectState.type)
                break
        }
        // `getExtraData` is NOT a dependency.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [project.projectState.type])

    React.useEffect(() => {
        if (event != null) {
            switch (event.type) {
                case projectEvent.ProjectEventType.open: {
                    if (event.projectId !== project.id) {
                        if (backend.type === backendModule.BackendType.local) {
                            setState(backendModule.ProjectState.closed)
                            setCheckState(CheckState.notChecking)
                        }
                        setShouldOpenWhenReady(false)
                    } else {
                        setShouldOpenWhenReady(true)
                        void openProject()
                    }
                    break
                }
                case projectEvent.ProjectEventType.cancelOpeningAll: {
                    if (backend.type === backendModule.BackendType.local) {
                        setState(backendModule.ProjectState.closed)
                        setCheckState(CheckState.notChecking)
                    }
                    setShouldOpenWhenReady(false)
                }
            }
        }
    }, [event, openProject, project.id, backend.type])

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
                        clearTimeout(handle)
                    }
                }
            }
            case CheckState.checkingResources: {
                let handle: number | null = null
                let continuePolling = true
                let previousTimestamp = 0
                const checkProjectResources = async () => {
                    if (backend.type === backendModule.BackendType.local) {
                        // This should never happen, but still should be handled
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
                        clearTimeout(handle)
                    }
                }
            }
        }
    }, [checkState, project.id, project.title, backend])

    const closeProject = async () => {
        onClose()
        setShouldOpenWhenReady(false)
        setState(backendModule.ProjectState.closed)
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
                    onClick={clickEvent => {
                        clickEvent.stopPropagation()
                        unsetModal()
                        doOpenManually(project.id)
                    }}
                >
                    {svg.PLAY_ICON}
                </button>
            )
        case backendModule.ProjectState.openInProgress:
            return (
                <button
                    onClick={async clickEvent => {
                        clickEvent.stopPropagation()
                        unsetModal()
                        await closeProject()
                    }}
                >
                    <svg.StopIcon className={SPINNER_CSS_CLASSES[spinnerState]} />
                </button>
            )
        case backendModule.ProjectState.opened:
            return (
                <>
                    <button
                        onClick={async clickEvent => {
                            clickEvent.stopPropagation()
                            unsetModal()
                            await closeProject()
                        }}
                    >
                        <svg.StopIcon className={SPINNER_CSS_CLASSES[spinnerState]} />
                    </button>
                    <button
                        onClick={clickEvent => {
                            clickEvent.stopPropagation()
                            unsetModal()
                            openIde()
                        }}
                    >
                        {svg.ARROW_UP_ICON}
                    </button>
                </>
            )
    }
}

export default ProjectActionButton
