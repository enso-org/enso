/** @file An interactive button displaying the status of a project. */
import * as react from 'react'

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

/** The interval between requests checking whether the IDE is ready. */
const CHECK_STATUS_INTERVAL_MS = 5000
/** The interval between requests checking whether the VM is ready. */
const CHECK_RESOURCES_INTERVAL_MS = 1000

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
    appRunner: AppRunner | null
    onClose: () => void
    openIde: () => void
}

/** An interactive button displaying the status of a project. */
function ProjectActionButton(props: ProjectActionButtonProps) {
    const { project, onClose, appRunner, openIde } = props
    const { backend } = backendProvider.useBackend()

    const [state, setState] = react.useState(backendModule.ProjectState.created)
    const [isCheckingStatus, setIsCheckingStatus] = react.useState(false)
    const [isCheckingResources, setIsCheckingResources] = react.useState(false)
    const [spinnerState, setSpinnerState] = react.useState(SpinnerState.done)

    react.useEffect(() => {
        void (async () => {
            const projectDetails = await backend.getProjectDetails(project.id)
            setState(projectDetails.state.type)
            if (projectDetails.state.type === backendModule.ProjectState.openInProgress) {
                setSpinnerState(SpinnerState.initial)
                setIsCheckingStatus(true)
            }
        })()
    }, [])

    react.useEffect(() => {
        if (backend.platform === platform.Platform.desktop) {
            if (project.id !== localBackend.LocalBackend.currentlyOpeningProjectId) {
                setIsCheckingResources(false)
                setIsCheckingStatus(false)
                setState(backendModule.ProjectState.closed)
                setSpinnerState(SpinnerState.done)
            }
        }
    }, [project, state, localBackend.LocalBackend.currentlyOpeningProjectId])

    react.useEffect(() => {
        if (!isCheckingStatus) {
            return
        } else {
            const checkProjectStatus = async () => {
                const response = await backend.getProjectDetails(project.id)
                if (response.state.type === backendModule.ProjectState.opened) {
                    setIsCheckingStatus(false)
                    setIsCheckingResources(true)
                } else {
                    setState(response.state.type)
                }
            }
            const handle = window.setInterval(
                () => void checkProjectStatus(),
                CHECK_STATUS_INTERVAL_MS
            )
            return () => {
                clearInterval(handle)
            }
        }
    }, [isCheckingStatus])

    react.useEffect(() => {
        if (!isCheckingResources) {
            return
        } else {
            const checkProjectResources = async () => {
                if (!('checkResources' in backend)) {
                    setState(backendModule.ProjectState.opened)
                    setIsCheckingResources(false)
                    setSpinnerState(SpinnerState.done)
                } else {
                    try {
                        // This call will error if the VM is not ready yet.
                        await backend.checkResources(project.id)
                        setState(backendModule.ProjectState.opened)
                        setIsCheckingResources(false)
                        setSpinnerState(SpinnerState.done)
                    } catch {
                        // Ignored.
                    }
                }
            }
            const handle = window.setInterval(
                () => void checkProjectResources(),
                CHECK_RESOURCES_INTERVAL_MS
            )
            return () => {
                clearInterval(handle)
            }
        }
    }, [isCheckingResources])

    const closeProject = () => {
        setState(backendModule.ProjectState.closed)
        appRunner?.stopApp()
        void backend.closeProject(project.id)
        setIsCheckingStatus(false)
        onClose()
    }

    const openProject = async () => {
        setState(backendModule.ProjectState.openInProgress)
        setSpinnerState(SpinnerState.initial)
        // The `setTimeout` is required so that the completion percentage goes from
        // the `initial` fraction to the `loading` fraction,
        // rather than starting at the `loading` fraction.
        setTimeout(() => {
            setSpinnerState(SpinnerState.loading)
        }, 0)
        switch (backend.platform) {
            case platform.Platform.cloud:
                await backend.openProject(project.id)
                setIsCheckingStatus(true)
                break
            case platform.Platform.desktop:
                await backend.openProject(project.id)
                setState(backendModule.ProjectState.opened)
                setSpinnerState(SpinnerState.done)
                break
        }
    }

    switch (state) {
        case backendModule.ProjectState.created:
        case backendModule.ProjectState.new:
        case backendModule.ProjectState.closed:
            return <button onClick={openProject}>{svg.PLAY_ICON}</button>
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
