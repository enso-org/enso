/** @file An interactive button displaying the status of a project. */
import * as react from 'react'

import * as backendProvider from '../../providers/backend'
import * as cloudService from '../cloudService'
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
const CHECK_STATUS_INTERVAL = 10000
/** The interval between requests checking whether the VM is ready. */
const CHECK_RESOURCES_INTERVAL = 2500

const SPINNER_CSS_CLASSES: Record<SpinnerState, string> = {
    [SpinnerState.initial]: 'dasharray-5 ease-linear',
    [SpinnerState.loading]: 'dasharray-75 duration-90000 ease-linear',
    [SpinnerState.done]: 'dasharray-100 duration-1000 ease-in',
} as const

// =================
// === Component ===
// =================

export interface ProjectActionButtonProps {
    project: cloudService.Asset<cloudService.AssetType.project>
    openIde: () => void
}

/** An interactive button displaying the status of a project. */
function ProjectActionButton(props: ProjectActionButtonProps) {
    const { project, openIde } = props
    const { backend } = backendProvider.useBackend()

    const [state, setState] = react.useState(cloudService.ProjectState.created)
    const [isCheckingStatus, setIsCheckingStatus] = react.useState(false)
    const [isCheckingResources, setIsCheckingResources] = react.useState(false)
    const [spinnerState, setSpinnerState] = react.useState(SpinnerState.done)

    react.useEffect(() => {
        async function checkProjectStatus() {
            const response = await backend.getProjectDetails(project.id)
            if (response.state.type === cloudService.ProjectState.opened) {
                setIsCheckingStatus(false)
                setIsCheckingResources(true)
            } else {
                setState(response.state.type)
            }
        }
        if (!isCheckingStatus) {
            return
        } else {
            const handle = window.setInterval(
                () => void checkProjectStatus(),
                CHECK_STATUS_INTERVAL
            )
            return () => {
                clearInterval(handle)
            }
        }
    }, [isCheckingStatus])

    react.useEffect(() => {
        async function checkProjectResources() {
            if (!('checkResources' in backend)) {
                setIsCheckingResources(false)
            } else {
                try {
                    // This call will error if the VM is not ready yet.
                    await backend.checkResources(project.id)
                    setState(cloudService.ProjectState.opened)
                    setIsCheckingResources(false)
                    setSpinnerState(SpinnerState.done)
                } catch {
                    // Ignored.
                }
            }
        }
        if (!isCheckingResources) {
            return
        } else {
            const handle = window.setInterval(
                () => void checkProjectResources(),
                CHECK_RESOURCES_INTERVAL
            )
            return () => {
                clearInterval(handle)
            }
        }
    }, [isCheckingResources])

    react.useEffect(() => {
        void (async () => {
            const projectDetails = await backend.getProjectDetails(project.id)
            setState(projectDetails.state.type)
            if (projectDetails.state.type === cloudService.ProjectState.openInProgress) {
                setSpinnerState(SpinnerState.initial)
                setIsCheckingStatus(true)
            }
        })()
    }, [])

    function closeProject() {
        setState(cloudService.ProjectState.closed)
        window.tryStopApp()
        void backend.closeProject(project.id)
        setIsCheckingStatus(false)
    }

    function openProject() {
        setState(cloudService.ProjectState.openInProgress)
        setSpinnerState(SpinnerState.initial)
        // The `setTimeout` is required so that the completion percentage goes from
        // the `initial` fraction to the `loading` fraction,
        // rather than starting at the `loading` fraction.
        setTimeout(() => {
            setSpinnerState(SpinnerState.loading)
        }, 0)
        void backend.openProject(project.id)
        setIsCheckingStatus(true)
    }

    switch (state) {
        case cloudService.ProjectState.created:
        case cloudService.ProjectState.new:
        case cloudService.ProjectState.closed:
            return <button onClick={openProject}>{svg.PLAY_ICON}</button>
        case cloudService.ProjectState.openInProgress:
            return (
                <button onClick={closeProject}>
                    <svg.StopIcon className={SPINNER_CSS_CLASSES[spinnerState]} />
                </button>
            )
        case cloudService.ProjectState.opened:
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
