/** @file An interactive button displaying the status of a project. */
import * as react from 'react'
import * as reactDom from 'react-dom'

import * as auth from '../../authentication/providers/auth'
import * as backend from '../service'
import * as loggerProvider from '../../providers/logger'
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
const STATUS_CHECK_INTERVAL = 10000

const SPINNER_CSS_CLASSES: Record<SpinnerState, string> = {
    [SpinnerState.initial]: 'dasharray-5 ease-linear',
    [SpinnerState.loading]: 'dasharray-75 duration-90000 ease-linear',
    [SpinnerState.done]: 'dasharray-100 duration-1000 ease-in',
} as const

/** Displayed when a project is ready to stop. */
function StopIcon(spinnerState: SpinnerState) {
    return (
        <svg
            width={24}
            height={24}
            viewBox="0 0 24 24"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
        >
            <path
                d="m9 8L15 8a1 1 0 0 1 1 1L16 15a1 1 0 0 1 -1 1L9 16a1 1 0 0 1 -1 -1L8 9a1 1 0 0 1 1 -1"
                fill="currentColor"
            />
            <rect
                x={1.5}
                y={1.5}
                width={21}
                height={21}
                rx={10.5}
                stroke="currentColor"
                strokeOpacity={0.1}
                strokeWidth={3}
            />
            <rect
                x={1.5}
                y={1.5}
                width={21}
                height={21}
                rx={10.5}
                stroke="currentColor"
                strokeLinecap="round"
                strokeWidth={3}
                className={`animate-spin-ease origin-center transition-stroke-dasharray ${SPINNER_CSS_CLASSES[spinnerState]}`}
            />
        </svg>
    )
}

// =================
// === Component ===
// =================

export interface ProjectActionButtonProps {
    project: backend.Asset<backend.AssetType.project>
    openIde: () => void
}

/** An interactive button displaying the status of a project. */
function ProjectActionButton(props: ProjectActionButtonProps) {
    const { project, openIde } = props
    const { accessToken } = auth.useFullUserSession()
    const logger = loggerProvider.useLogger()
    const backendService = backend.createBackend(accessToken, logger)

    const [state, setState] = react.useState(backend.ProjectState.created)
    const [checkStatusInterval, setCheckStatusInterval] = react.useState<number | null>(null)
    const [spinnerState, setSpinnerState] = react.useState(SpinnerState.done)

    react.useEffect(() => {
        void (async () => {
            const projectDetails = await backendService.getProjectDetails(project.id)
            setState(projectDetails.state.type)
        })()
    }, [])

    function closeProject() {
        setState(backend.ProjectState.closed)
        void backendService.closeProject(project.id)

        reactDom.unstable_batchedUpdates(() => {
            setCheckStatusInterval(null)
            if (checkStatusInterval != null) {
                clearInterval(checkStatusInterval)
            }
        })
    }

    function openProject() {
        setState(backend.ProjectState.openInProgress)
        setSpinnerState(SpinnerState.initial)
        // The `setTimeout` is required so that the completion percentage goes from
        // the `initial` fraction to the `loading` fraction,
        // rather than starting at the `loading` fraction.
        setTimeout(() => {
            setSpinnerState(SpinnerState.loading)
        }, 0)

        void backendService.openProject(project.id)

        const checkProjectStatus = async () => {
            const response = await backendService.getProjectDetails(project.id)

            setState(response.state.type)

            if (response.state.type === backend.ProjectState.opened) {
                setCheckStatusInterval(null)
                if (checkStatusInterval != null) {
                    clearInterval(checkStatusInterval)
                }
                setSpinnerState(SpinnerState.done)
            }
        }

        reactDom.unstable_batchedUpdates(() => {
            setCheckStatusInterval(
                window.setInterval(() => void checkProjectStatus(), STATUS_CHECK_INTERVAL)
            )
        })
    }

    switch (state) {
        case backend.ProjectState.created:
        case backend.ProjectState.new:
        case backend.ProjectState.closed:
            return <button onClick={openProject}>{svg.PLAY_ICON}</button>
        case backend.ProjectState.openInProgress:
            return <button onClick={closeProject}>{StopIcon(spinnerState)}</button>
        case backend.ProjectState.opened:
            return (
                <>
                    <button onClick={closeProject}>{StopIcon(spinnerState)}</button>
                    <button onClick={openIde}>{svg.ARROW_UP_ICON}</button>
                </>
            )
    }
}

export default ProjectActionButton
