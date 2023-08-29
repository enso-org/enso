/** @file An interactive button indicating the status of a project. */
import * as React from 'react'
import * as toast from 'react-toastify'

import PlayIcon from 'enso-assets/play.svg'
import StopIcon from 'enso-assets/stop.svg'

import * as assetEventModule from '../events/assetEvent'
import * as authProvider from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as errorModule from '../../error'
import * as hooks from '../../hooks'
import * as localStorageModule from '../localStorage'
import * as localStorageProvider from '../../providers/localStorage'
import * as modalProvider from '../../providers/modal'
import * as remoteBackend from '../remoteBackend'

import Spinner, * as spinner from './spinner'
import SvgMask from '../../authentication/components/svgMask'

// =================
// === Constants ===
// =================

/** The size of the icon, in pixels. */
const ICON_SIZE_PX = 24
/** The styles of the icons. */
const ICON_STYLE = { width: ICON_SIZE_PX, height: ICON_SIZE_PX } satisfies React.CSSProperties
const LOADING_MESSAGE =
    'Your environment is being created. It will take some time, please be patient.'
/** The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState},
 * when using the remote backend. */
const REMOTE_SPINNER_STATE: Record<backendModule.ProjectState, spinner.SpinnerState> = {
    [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingSlow,
    [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingSlow,
    [backendModule.ProjectState.provisioned]: spinner.SpinnerState.loadingSlow,
    [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}
/** The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState},
 * when using the local backend. */
const LOCAL_SPINNER_STATE: Record<backendModule.ProjectState, spinner.SpinnerState> = {
    [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingMedium,
    [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingMedium,
    [backendModule.ProjectState.provisioned]: spinner.SpinnerState.loadingMedium,
    [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}

// ===================
// === ProjectIcon ===
// ===================

/** Props for a {@link ProjectIcon}. */
export interface ProjectIconProps {
    keyProp: string
    item: backendModule.ProjectAsset
    setItem: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>
    assetEvents: assetEventModule.AssetEvent[]
    /** Called when the project is opened via the {@link ProjectIcon}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    onClose: () => void
    openIde: (switchPage: boolean) => void
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
    const { keyProp: key, item, setItem, assetEvents, doOpenManually, onClose, openIde } = props
    const { backend } = backendProvider.useBackend()
    const { organization } = authProvider.useNonPartialUserSession()
    const { unsetModal } = modalProvider.useSetModal()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const toastAndLog = hooks.useToastAndLog()
    const state = item.projectState.type
    const setState = React.useCallback(
        (stateOrUpdater: React.SetStateAction<backendModule.ProjectState>) => {
            setItem(oldItem => {
                let newState: backendModule.ProjectState
                if (typeof stateOrUpdater === 'function') {
                    newState = stateOrUpdater(oldItem.projectState.type)
                } else {
                    newState = stateOrUpdater
                }
                let newProjectState: backendModule.ProjectStateType = {
                    ...oldItem.projectState,
                    type: newState,
                }
                if (!backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[newState]) {
                    // eslint-disable-next-line @typescript-eslint/naming-convention, @typescript-eslint/no-unused-vars
                    const { opened_by, ...newProjectState2 } = newProjectState
                    newProjectState = newProjectState2
                } else if (organization != null) {
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    newProjectState = { ...newProjectState, opened_by: organization.email }
                }
                return {
                    ...oldItem,
                    projectState: newProjectState,
                }
            })
        },
        [organization, /* should never change */ setItem]
    )
    const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
    const [onSpinnerStateChange, setOnSpinnerStateChange] = React.useState<
        ((state: spinner.SpinnerState | null) => void) | null
    >(null)
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
    const [shouldSwitchPage, setShouldSwitchPage] = React.useState(false)
    const [toastId, setToastId] = React.useState<toast.Id | null>(null)
    const [openProjectAbortController, setOpenProjectAbortController] =
        React.useState<AbortController | null>(null)
    const [closeProjectAbortController, setCloseProjectAbortController] =
        React.useState<AbortController | null>(null)
    const isOtherUserUsingProject = item.projectState.opened_by !== organization?.email

    const openProject = React.useCallback(async () => {
        closeProjectAbortController?.abort()
        setCloseProjectAbortController(null)
        setState(backendModule.ProjectState.openInProgress)
        try {
            switch (backend.type) {
                case backendModule.BackendType.remote: {
                    if (!backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[state]) {
                        setToastId(toast.toast.loading(LOADING_MESSAGE))
                        await backend.openProject(item.id, null, item.title)
                    }
                    const abortController = new AbortController()
                    setOpenProjectAbortController(abortController)
                    await remoteBackend.waitUntilProjectIsReady(backend, item, abortController)
                    setToastId(null)
                    if (!abortController.signal.aborted) {
                        setState(oldState =>
                            oldState === backendModule.ProjectState.openInProgress
                                ? backendModule.ProjectState.opened
                                : oldState
                        )
                    }
                    break
                }
                case backendModule.BackendType.local: {
                    await backend.openProject(item.id, null, item.title)
                    setState(oldState =>
                        oldState === backendModule.ProjectState.openInProgress
                            ? backendModule.ProjectState.opened
                            : oldState
                    )
                    break
                }
            }
        } catch (error) {
            const project = await backend.getProjectDetails(item.id, item.title)
            setItem(oldItem => ({
                ...oldItem,
                projectState: project.state,
            }))
            toastAndLog(
                errorModule.tryGetMessage(error)?.slice(0, -1) ??
                    `Could not open project '${item.title}'`
            )
            setState(backendModule.ProjectState.closed)
        }
    }, [
        state,
        backend,
        item,
        closeProjectAbortController,
        /* should never change */ toastAndLog,
        /* should never change */ setState,
        /* should never change */ setItem,
    ])

    React.useEffect(() => {
        setItem(oldItem => ({ ...oldItem, projectState: { ...oldItem.projectState, type: state } }))
    }, [state, /* should never change */ setItem])

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
            onSpinnerStateChange?.(
                state === backendModule.ProjectState.closed ? null : newSpinnerState
            )
        })
    }, [state, backend.type, onSpinnerStateChange])

    React.useEffect(() => {
        onSpinnerStateChange?.(spinner.SpinnerState.initial)
        return () => {
            onSpinnerStateChange?.(null)
        }
    }, [onSpinnerStateChange])

    hooks.useEventHandler(assetEvents, event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.newFolder:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.newSecret:
            case assetEventModule.AssetEventType.deleteMultiple:
            case assetEventModule.AssetEventType.downloadSelected:
            case assetEventModule.AssetEventType.removeSelf: {
                // Ignored. Any missing project-related events should be handled by
                // `ProjectNameColumn`. `deleteMultiple` and `downloadSelected` are handled by
                // `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.openProject: {
                if (event.id !== item.id) {
                    setShouldOpenWhenReady(false)
                    if (!isOtherUserUsingProject) {
                        void closeProject(false)
                    }
                } else {
                    setShouldOpenWhenReady(true)
                    setShouldSwitchPage(event.shouldAutomaticallySwitchPage)
                    void openProject()
                }
                break
            }
            case assetEventModule.AssetEventType.closeProject: {
                if (event.id === item.id) {
                    setShouldOpenWhenReady(false)
                    void closeProject(false)
                }
                break
            }
            case assetEventModule.AssetEventType.cancelOpeningAllProjects: {
                setShouldOpenWhenReady(false)
                onSpinnerStateChange?.(null)
                setOnSpinnerStateChange(null)
                openProjectAbortController?.abort()
                setOpenProjectAbortController(null)
                if (!isOtherUserUsingProject) {
                    void closeProject(false)
                }
                break
            }
            case assetEventModule.AssetEventType.newProject: {
                if (event.placeholderId === key) {
                    setOnSpinnerStateChange(() => event.onSpinnerStateChange)
                } else if (event.onSpinnerStateChange === onSpinnerStateChange) {
                    setOnSpinnerStateChange(null)
                }
                break
            }
        }
    })

    React.useEffect(() => {
        if (shouldOpenWhenReady && state === backendModule.ProjectState.opened) {
            openIde(shouldSwitchPage)
            setShouldOpenWhenReady(false)
        }
        // `openIde` is a callback, not a dependency.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [shouldOpenWhenReady, shouldSwitchPage, state])

    const closeProject = async (triggerOnClose = true) => {
        if (triggerOnClose) {
            onClose()
            localStorage.delete(localStorageModule.LocalStorageKey.projectStartupInfo)
        }
        setToastId(null)
        setShouldOpenWhenReady(false)
        setState(backendModule.ProjectState.closing)
        onSpinnerStateChange?.(null)
        setOnSpinnerStateChange(null)
        openProjectAbortController?.abort()
        setOpenProjectAbortController(null)
        const abortController = new AbortController()
        setCloseProjectAbortController(abortController)
        if (backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[state]) {
            try {
                if (
                    backend.type === backendModule.BackendType.local &&
                    state === backendModule.ProjectState.openInProgress
                ) {
                    // Projects that are not opened cannot be closed.
                    // This is the only way to wait until the project is open.
                    await backend.openProject(item.id, null, item.title)
                }
                try {
                    await backend.closeProject(item.id, item.title)
                } catch {
                    // Ignored. The project is already closed.
                }
            } finally {
                if (!abortController.signal.aborted) {
                    setState(backendModule.ProjectState.closed)
                }
            }
        }
    }

    switch (state) {
        case null:
        case backendModule.ProjectState.created:
        case backendModule.ProjectState.new:
        case backendModule.ProjectState.closing:
        case backendModule.ProjectState.closed:
            return (
                <button
                    className="w-6 h-6 disabled:opacity-50"
                    onClick={clickEvent => {
                        clickEvent.stopPropagation()
                        unsetModal()
                        doOpenManually(item.id)
                    }}
                >
                    <SvgMask style={ICON_STYLE} src={PlayIcon} />
                </button>
            )
        case backendModule.ProjectState.openInProgress:
        case backendModule.ProjectState.provisioned:
        case backendModule.ProjectState.placeholder:
            return (
                <button
                    disabled={isOtherUserUsingProject}
                    {...(isOtherUserUsingProject
                        ? { title: 'Someone else is using this project.' }
                        : {})}
                    className="w-6 h-6 disabled:opacity-50"
                    onClick={async clickEvent => {
                        clickEvent.stopPropagation()
                        unsetModal()
                        await closeProject()
                    }}
                >
                    <div className="relative h-0">
                        <Spinner size={ICON_SIZE_PX} state={spinnerState} />
                    </div>
                    <SvgMask style={ICON_STYLE} src={StopIcon} />
                </button>
            )
        case backendModule.ProjectState.opened:
            return (
                <>
                    <button
                        disabled={isOtherUserUsingProject}
                        {...(isOtherUserUsingProject
                            ? { title: 'Someone else has this project open.' }
                            : {})}
                        className="w-6 h-6 disabled:opacity-50"
                        onClick={async clickEvent => {
                            clickEvent.stopPropagation()
                            unsetModal()
                            await closeProject()
                        }}
                    >
                        <div className="relative h-0">
                            <Spinner size={24} state={spinnerState} />
                        </div>
                        <SvgMask src={StopIcon} />
                    </button>
                </>
            )
    }
}
