/** @file An interactive button displaying the status of a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as localBackend from '../localBackend'
import * as platform from '../../platform'
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

export interface ProjectActionButtonProps {
    project: backendModule.Asset<backendModule.AssetType.project>
    appRunner: AppRunner | null
    /** Whether this Project should open immediately. */
    shouldOpenImmediately: boolean
    /** Whether this Project should cancel opening immediately.
     * This would happen if another project is being opened immediately instead. */
    shouldCancelOpeningImmediately: boolean
    onClose: () => void
    openIde: () => void
}

/** An interactive button displaying the status of a project. */
function ProjectActionButton(props: ProjectActionButtonProps) {
    const {
        project,
        onClose,
        shouldOpenImmediately,
        shouldCancelOpeningImmediately,
        appRunner,
        openIde,
    } = props
    const { backend } = backendProvider.useBackend()

    const [state, setState] = react.useState<backendModule.ProjectState | null>(null)
    const [isCheckingStatus, setIsCheckingStatus] = react.useState(false)
    const [isCheckingResources, setIsCheckingResources] = react.useState(false)
    const [spinnerState, setSpinnerState] = react.useState(SpinnerState.done)
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = react.useState(false)
    const [toastId, setToastId] = react.useState<string | null>(null)

    react.useEffect(() => {
        // Ensure that the previous spinner state is visible for at least one frame.
        requestAnimationFrame(() => {
            setSpinnerState(SPINNER_STATE[state ?? DEFAULT_PROJECT_STATE])
        })
    }, [state])

    react.useEffect(() => {
        if (toastId != null && state !== backendModule.ProjectState.openInProgress) {
            toast.dismiss(toastId)
        }
    }, [state])

    react.useEffect(() => {
        void (async () => {
            const projectDetails = await backend.getProjectDetails(project.id)
            switch (projectDetails.state.type) {
                case backendModule.ProjectState.openInProgress:
                    setState(projectDetails.state.type)
                    setIsCheckingStatus(true)
                    break
                case backendModule.ProjectState.opened:
                    setState(backendModule.ProjectState.openInProgress)
                    setIsCheckingResources(true)
                    break
                default:
                    // Some functions below set the state to something different to
                    // the backend state. In that case, the state should not be overridden.
                    setState(previousState => previousState ?? projectDetails.state.type)
                    break
            }
        })()
    }, [])

    react.useEffect(() => {
        // `shouldOpenImmediately` (set to `true` for the relevant project) takes precedence over
        // `shouldCancelOpeningImmediately` (set to `true` for all projects, including the relevant
        // project).
        if (shouldOpenImmediately) {
            setShouldOpenWhenReady(true)
            switch (state) {
                case backendModule.ProjectState.opened: {
                    setIsCheckingResources(true)
                    break
                }
                case backendModule.ProjectState.openInProgress: {
                    setIsCheckingStatus(true)
                    break
                }
                default: {
                    void openProject()
                    break
                }
            }
        } else if (shouldCancelOpeningImmediately) {
            setShouldOpenWhenReady(false)
        }
    }, [shouldOpenImmediately, shouldCancelOpeningImmediately])

    react.useEffect(() => {
        if (shouldOpenWhenReady && state === backendModule.ProjectState.opened) {
            openIde()
            setShouldOpenWhenReady(false)
        }
    }, [shouldOpenWhenReady, state])

    react.useEffect(() => {
        if (
            backend.platform === platform.Platform.desktop &&
            project.id !== localBackend.LocalBackend.currentlyOpeningProjectId
        ) {
            setIsCheckingResources(false)
            setIsCheckingStatus(false)
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
                if (backend.platform === platform.Platform.desktop) {
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

    function closeProject() {
        setState(backendModule.ProjectState.closed)
        appRunner?.stopApp()
        void backend.closeProject(project.id)
        setIsCheckingStatus(false)
        setIsCheckingResources(false)
        onClose()
    }

    async function openProject() {
        setState(backendModule.ProjectState.openInProgress)
        switch (backend.platform) {
            case platform.Platform.cloud:
                await backend.openProject(project.id)
                setToastId(toast.loading(LOADING_MESSAGE))
                setIsCheckingStatus(true)
                break
            case platform.Platform.desktop:
                await backend.openProject(project.id)
                setState(backendModule.ProjectState.opened)
                break
        }
    }

    switch (state) {
        case null:
        case backendModule.ProjectState.created:
        case backendModule.ProjectState.new:
        case backendModule.ProjectState.closed:
            return (
                <button
                    onClick={async () => {
                        setShouldOpenWhenReady(true)
                        await openProject()
                    }}
                >
                    {svg.PLAY_ICON}
                </button>
            )
        case backendModule.ProjectState.openInProgress:
            return (
                <button onClick={closeProject}>
                    <svg.StopIcon className={SPINNER_CSS_CLASSES[spinnerState]} />
                </button>
            )
        case backendModule.ProjectState.opened:
            return (
                <>
                    <button onClick={closeProject}>
                        <svg.StopIcon className={SPINNER_CSS_CLASSES[spinnerState]} />
                    </button>
                    <button onClick={openIde}>{svg.ARROW_UP_ICON}</button>
                </>
            )
    }
}

export default ProjectActionButton
