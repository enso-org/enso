/** @file An interactive button displaying the status of a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as localBackend from '../localBackend'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

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
    // FIXME: provide projectId instead
    /** This must be a name because it may be specified by name on the command line.
     * Note that this will not work properly with the cloud backend if there are multiple projects
     * with the same name. */
    projectId: backendModule.ProjectId
}

/** Requests the specified project to be opened. */
export interface ProjectCancelOpeningAllEvent
    extends ProjectBaseEvent<ProjectEventType.cancelOpeningAll> {}

/** Every possible type of project event. */
export type ProjectEvent = ProjectCancelOpeningAllEvent | ProjectOpenEvent

/** The state of the spinner. It should go from initial, to loading, to done. */
enum SpinnerState {
    initial = 'initial',
    loading = 'loading',
    done = 'done',
}

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
    project: backendModule.Asset<backendModule.AssetType.project>
    projectData: ProjectData
    setProjectData: react.Dispatch<react.SetStateAction<ProjectData>>
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

    const [state, setState] = react.useState(() => {
        if (shouldCheckIfActuallyOpen) {
            return backendModule.ProjectState.created
        } else {
            return project.projectState.type
        }
    })
    const [isCheckingStatus, setIsCheckingStatus] = react.useState(false)
    const [isCheckingResources, setIsCheckingResources] = react.useState(false)
    const [spinnerState, setSpinnerState] = react.useState(SPINNER_STATE[state])
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = react.useState(false)
    const [toastId, setToastId] = react.useState<string | null>(null)

    react.useEffect(() => {
        if (toastId != null) {
            return () => {
                toast.dismiss(toastId)
            }
        } else {
            return
        }
    }, [toastId])

    react.useEffect(() => {
        // Ensure that the previous spinner state is visible for at least one frame.
        requestAnimationFrame(() => {
            setSpinnerState(SPINNER_STATE[state])
        })
    }, [state])

    react.useEffect(() => {
        if (toastId != null && state !== backendModule.ProjectState.openInProgress) {
            toast.dismiss(toastId)
        }
    }, [state])

    react.useEffect(() => {
        if (shouldCheckIfActuallyOpen) {
            setState(backendModule.ProjectState.openInProgress)
            setIsCheckingResources(true)
        }
    }, [])

    react.useEffect(() => {
        if (event != null) {
            switch (event.type) {
                case ProjectEventType.open: {
                    if (event.projectId !== project.id) {
                        setShouldOpenWhenReady(false)
                    } else {
                        setShouldOpenWhenReady(true)
                        void openProject()
                    }
                    break
                }
                case ProjectEventType.cancelOpeningAll: {
                    setShouldOpenWhenReady(false)
                }
            }
        }
    }, [event])

    react.useEffect(() => {
        if (shouldOpenWhenReady && state === backendModule.ProjectState.opened) {
            openIde()
            setShouldOpenWhenReady(false)
        }
    }, [shouldOpenWhenReady, state])

    react.useEffect(() => {
        if (
            backend.type === backendModule.BackendType.local &&
            project.id !== localBackend.LocalBackend.currentlyOpeningProjectId
        ) {
            setState(backendModule.ProjectState.closed)
        }
    }, [project, state, localBackend.LocalBackend.currentlyOpeningProjectId])

    react.useEffect(() => {
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
    }, [isCheckingStatus])

    react.useEffect(() => {
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
    }, [isCheckingResources])

    const closeProject = async () => {
        onClose()
        setShouldOpenWhenReady(false)
        setState(backendModule.ProjectState.closed)
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

    const openProject = async () => {
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
                            doRefresh()
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
                        doOpenManually()
                    }}
                >
                    <img src={PlayIcon} />
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
                        <img src={ArrowUpIcon} />
                    </button>
                </>
            )
    }
}

export default ProjectActionButton
