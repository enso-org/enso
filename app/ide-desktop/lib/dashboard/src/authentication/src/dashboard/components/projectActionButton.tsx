/** @file An interactive button displaying the status of a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as localBackend from '../localBackend'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import * as spinner from './spinner'

// =============
// === Types ===
// =============

/** Data associated with a project, used for rendering.
 * FIXME[sb]: This is a hack that is required because each row does not carry its own extra state.
 * It will be obsoleted by the implementation in https://github.com/enso-org/enso/pull/6546. */
export interface ProjectData {
    isRunning: boolean
}

/** Possible types of project state change. */
export enum ProjectEventType {
    open = 'open',
    cancelOpeningAll = 'cancelOpeningAll',
}

/** Properties common to all project state change events. */
interface ProjectBaseEvent<Type extends ProjectEventType> {
    type: Type
}

/** Requests the specified project to be opened. */
export interface ProjectOpenEvent extends ProjectBaseEvent<ProjectEventType.open> {
    /** This must be a name because it may be specified by name on the command line.
     * Note that this will not work properly with the cloud backend if there are multiple projects
     * with the same name. */
    projectId: backendModule.ProjectId
    onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null
}

/** Requests the specified project to be opened. */
export interface ProjectCancelOpeningAllEvent
    extends ProjectBaseEvent<ProjectEventType.cancelOpeningAll> {}

/** Every possible type of project event. */
export type ProjectEvent = ProjectCancelOpeningAllEvent | ProjectOpenEvent

// =================
// === Constants ===
// =================

/** The default {@link ProjectData} associated with a {@link backendModule.Project}. */
export const DEFAULT_PROJECT_DATA: ProjectData = Object.freeze({
    isRunning: false,
})
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

// =================
// === Component ===
// =================

/** Props for a {@link ProjectActionButton}. */
export interface ProjectActionButtonProps {
    project: backendModule.Asset<backendModule.AssetType.project>
    projectData: ProjectData
    setProjectData: React.Dispatch<React.SetStateAction<ProjectData>>
    appRunner: AppRunner | null
    event: ProjectEvent | null
    /** Called when the project is opened via the {@link ProjectActionButton}. */
    doOpenManually: () => void
    onClose: () => void
    openIde: () => void
    doRefresh: () => void
}

/** An interactive button displaying the status of a project. */
function ProjectActionButton(props: ProjectActionButtonProps) {
    const {
        project,
        setProjectData,
        event,
        appRunner,
        doOpenManually,
        onClose,
        openIde,
        doRefresh,
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
    const [isCheckingStatus, setIsCheckingStatus] = React.useState(false)
    const [isCheckingResources, setIsCheckingResources] = React.useState(false)
    const [spinnerState, setSpinnerState] = React.useState(REMOTE_SPINNER_STATE[state])
    const [onSpinnerStateChange, setOnSpinnerStateChange] = React.useState<
        ((state: spinner.SpinnerState | null) => void) | null
    >(null)
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
    const [toastId, setToastId] = React.useState<string | null>(null)

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
            toast.dismiss(toastId)
        }
    }, [state, toastId])

    React.useEffect(() => {
        if (shouldCheckIfActuallyOpen) {
            setState(backendModule.ProjectState.openInProgress)
            setIsCheckingResources(true)
        }
    }, [shouldCheckIfActuallyOpen])

    const openProject = React.useCallback(async () => {
        setState(backendModule.ProjectState.openInProgress)
        try {
            switch (backend.type) {
                case backendModule.BackendType.remote:
                    setToastId(toast.loading(LOADING_MESSAGE))
                    await backend.openProject(project.id)
                    setProjectData(oldProjectData => ({ ...oldProjectData, isRunning: true }))
                    doRefresh()
                    setIsCheckingStatus(true)
                    break
                case backendModule.BackendType.local:
                    await backend.openProject(project.id)
                    setProjectData(oldProjectData => ({ ...oldProjectData, isRunning: true }))
                    setState(oldState => {
                        if (oldState === backendModule.ProjectState.openInProgress) {
                            setTimeout(() => {
                                doRefresh()
                            }, 0)
                            return backendModule.ProjectState.opened
                        } else {
                            return oldState
                        }
                    })
                    break
            }
        } catch {
            setIsCheckingStatus(false)
            setIsCheckingResources(false)
            toast.error(`Error opening project '${project.title}'.`)
            setState(backendModule.ProjectState.closed)
        }
    }, [backend, doRefresh, project.id, project.title, setProjectData])

    React.useEffect(() => {
        if (event != null) {
            switch (event.type) {
                case ProjectEventType.open: {
                    if (event.projectId !== project.id) {
                        setShouldOpenWhenReady(false)
                        if (onSpinnerStateChange === event.onSpinnerStateChange) {
                            setOnSpinnerStateChange(null)
                        }
                    } else {
                        setShouldOpenWhenReady(true)
                        setOnSpinnerStateChange(() => event.onSpinnerStateChange)
                        void openProject()
                    }
                    break
                }
                case ProjectEventType.cancelOpeningAll: {
                    setShouldOpenWhenReady(false)
                    onSpinnerStateChange?.(null)
                    setOnSpinnerStateChange(null)
                    break
                }
            }
        }
        // This effect MUST run if and only if `event` changes.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [event])

    React.useEffect(() => {
        if (shouldOpenWhenReady && state === backendModule.ProjectState.opened) {
            openIde()
            setShouldOpenWhenReady(false)
        }
    }, [openIde, shouldOpenWhenReady, state])

    React.useEffect(() => {
        if (
            backend.type === backendModule.BackendType.local &&
            project.id !== localBackend.LocalBackend.currentlyOpeningProjectId
        ) {
            setState(backendModule.ProjectState.closed)
        }
        // `localBackend.LocalBackend.currentlyOpeningProjectId` is a mutable outer scope value.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [project, state, backend.type, localBackend.LocalBackend.currentlyOpeningProjectId])

    React.useEffect(() => {
        if (!isCheckingStatus) {
            return
        } else {
            let handle: number | null = null
            let continuePolling = true
            let previousTimestamp = 0
            const checkProjectStatus = async () => {
                try {
                    const response = await backend.getProjectDetails(project.id)
                    handle = null
                    if (
                        continuePolling &&
                        response.state.type === backendModule.ProjectState.opened
                    ) {
                        continuePolling = false
                        setIsCheckingStatus(false)
                        setIsCheckingResources(true)
                    }
                } finally {
                    if (continuePolling) {
                        const nowTimestamp = Number(new Date())
                        const delay = CHECK_STATUS_INTERVAL_MS - (nowTimestamp - previousTimestamp)
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
    }, [backend, isCheckingStatus, project.id])

    React.useEffect(() => {
        if (!isCheckingResources) {
            return
        } else {
            let handle: number | null = null
            let continuePolling = true
            let previousTimestamp = 0
            const checkProjectResources = async () => {
                if (backend.type === backendModule.BackendType.local) {
                    setState(backendModule.ProjectState.opened)
                    setIsCheckingResources(false)
                } else {
                    try {
                        // This call will error if the VM is not ready yet.
                        await backend.checkResources(project.id)
                        handle = null
                        if (continuePolling) {
                            continuePolling = false
                            setState(backendModule.ProjectState.opened)
                            setIsCheckingResources(false)
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
    }, [backend, isCheckingResources, project.id])

    const closeProject = async () => {
        onClose()
        setShouldOpenWhenReady(false)
        setState(backendModule.ProjectState.closed)
        onSpinnerStateChange?.(null)
        setOnSpinnerStateChange(null)
        appRunner?.stopApp()
        setIsCheckingStatus(false)
        setIsCheckingResources(false)
        try {
            await backend.closeProject(project.id)
        } finally {
            // This is not 100% correct, but it is better than never setting `isRunning` to `false`,
            // which would prevent the project from ever being deleted.
            setProjectData(oldProjectData => ({ ...oldProjectData, isRunning: false }))
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
                        doOpenManually()
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

export default ProjectActionButton
