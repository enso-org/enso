/** @file An interactive button indicating the status of a project. */
import * as React from 'react'
import * as toast from 'react-toastify'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'
import StopIcon from 'enso-assets/stop.svg'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as errorModule from '../../error'
import * as hooks from '../../hooks'
import * as modalProvider from '../../providers/modal'

import * as assetsTable from './assetsTable'
import Spinner, * as spinner from './spinner'

// =================
// === Constants ===
// =================

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

// =============
// === Types ===
// =============

/** The state of checking whether a project is ready. It should go from not checking, to checking
 * status, to checking resources, to done. */
export enum CheckState {
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

// ===================
// === ProjectIcon ===
// ===================

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
    project: backendModule.ProjectAsset
    rowState: assetsTable.AssetRowState
    setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
    assetEvent: assetEventModule.AssetEvent | null
    /** Called when the project is opened via the {@link ProjectIcon}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    onClose: () => void
    appRunner: AppRunner | null
    openIde: () => void
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
    const {
        project,
        rowState,
        setRowState,
        assetEvent,
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
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
    const [toastId, setToastId] = React.useState<toast.Id | null>(null)

    const openProject = React.useCallback(async () => {
        setState(backendModule.ProjectState.openInProgress)
        try {
            switch (backend.type) {
                case backendModule.BackendType.remote:
                    setToastId(toast.toast.loading(LOADING_MESSAGE))
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
            toast.toast.error(
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
                toast.toast.dismiss(toastId)
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
        })
    }, [state, backend.type])

    React.useEffect(() => {
        if (toastId != null && state !== backendModule.ProjectState.openInProgress) {
            toast.toast.dismiss(toastId)
        }
    }, [state, toastId])

    React.useEffect(() => {
        if (shouldCheckIfActuallyOpen) {
            setState(backendModule.ProjectState.openInProgress)
            setCheckState(CheckState.checkingResources)
        }
    }, [shouldCheckIfActuallyOpen])

    hooks.useEventHandler(assetEvent, event => {
        switch (event.type) {
            default: {
                // Ignore; all other events are handled by `ProjectRow`.
                break
            }
            case assetEventModule.AssetEventType.openProject: {
                if (event.id !== project.id) {
                    setShouldOpenWhenReady(false)
                } else {
                    setShouldOpenWhenReady(true)
                    void openProject()
                }
                break
            }
            case assetEventModule.AssetEventType.cancelOpeningAllProjects: {
                // `CheckState` should not be set to `notChecking`, as there is currently no way
                // to actually cancel an open action. Instead, the project should not be opened
                // automatically.
                setShouldOpenWhenReady(false)
                break
            }
            case assetEventModule.AssetEventType.createProject: {
                if (event.placeholderId === project.id) {
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
                    <div className="relative h-0">
                        <Spinner size={24} state={spinnerState} />
                    </div>
                    <img src={StopIcon} />
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
                        <div className="relative h-0">
                            <Spinner size={24} state={spinnerState} />
                        </div>
                        <img src={StopIcon} />
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
