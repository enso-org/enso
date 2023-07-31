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
    [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.created]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.new]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.openInProgress]: spinner.SpinnerState.loadingSlow,
    [backendModule.ProjectState.opened]: spinner.SpinnerState.done,
}
/** The corresponding {@link SpinnerState} for each {@link backendModule.ProjectState},
 * when using the local backend. */
const LOCAL_SPINNER_STATE: Record<backendModule.ProjectState, spinner.SpinnerState> = {
    [backendModule.ProjectState.closed]: spinner.SpinnerState.initial,
    [backendModule.ProjectState.closing]: spinner.SpinnerState.initial,
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
    keyProp: string
    item: backendModule.ProjectAsset
    setItem: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>
    assetEvents: assetEventModule.AssetEvent[]
    /** Called when the project is opened via the {@link ProjectIcon}. */
    doOpenManually: (projectId: backendModule.ProjectId) => void
    onClose: () => void
    appRunner: AppRunner | null
    openIde: () => void
}

/** An interactive icon indicating the status of a project. */
export default function ProjectIcon(props: ProjectIconProps) {
    const {
        keyProp: key,
        item,
        setItem,
        assetEvents,
        appRunner,
        doOpenManually,
        onClose,
        openIde,
    } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const shouldCheckIfActuallyOpen =
        item.projectState.type === backendModule.ProjectState.openInProgress ||
        (backend.type === backendModule.BackendType.remote &&
            item.projectState.type === backendModule.ProjectState.opened)

    const [state, setState] = React.useState(() => {
        if (shouldCheckIfActuallyOpen) {
            return backendModule.ProjectState.created
        } else {
            return item.projectState.type
        }
    })
    const [checkState, setCheckState] = React.useState(CheckState.notChecking)
    const [spinnerState, setSpinnerState] = React.useState(REMOTE_SPINNER_STATE[state])
    const [onSpinnerStateChange, setOnSpinnerStateChange] = React.useState<
        ((state: spinner.SpinnerState | null) => void) | null
    >(null)
    const [shouldOpenWhenReady, setShouldOpenWhenReady] = React.useState(false)
    const [toastId, setToastId] = React.useState<toast.Id | null>(null)

    const openProject = React.useCallback(async () => {
        setState(backendModule.ProjectState.openInProgress)
        try {
            switch (backend.type) {
                case backendModule.BackendType.remote:
                    setToastId(toast.toast.loading(LOADING_MESSAGE))
                    await backend.openProject(item.id, null, item.title)
                    setCheckState(CheckState.checkingStatus)
                    break
                case backendModule.BackendType.local:
                    setCheckState(CheckState.localProject)
                    await backend.openProject(item.id, null, item.title)
                    setCheckState(oldCheckState => {
                        if (oldCheckState === CheckState.localProject) {
                            setState(oldState =>
                                oldState === backendModule.ProjectState.openInProgress
                                    ? backendModule.ProjectState.opened
                                    : oldState
                            )
                        }
                        return oldCheckState
                    })
                    break
            }
        } catch (error) {
            setCheckState(CheckState.notChecking)
            toast.toast.error(
                `Error opening project '${item.title}': ${
                    errorModule.tryGetMessage(error) ?? 'unknown error'
                }.`
            )
            setState(backendModule.ProjectState.closed)
        }
    }, [backend, item.id, item.title])

    React.useEffect(() => {
        setItem(oldItem => ({ ...oldItem, projectState: { type: state } }))
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

    hooks.useEventHandler(assetEvents, event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.createDirectory:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.createSecret:
            case assetEventModule.AssetEventType.deleteMultiple: {
                // Ignored. Any missing project-related events should be handled by
                // `ProjectNameColumn`. `deleteMultiple` is handled by `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.openProject: {
                if (event.id !== item.id) {
                    setShouldOpenWhenReady(false)
                    if (state === backendModule.ProjectState.opened) {
                        void closeProject(false)
                    }
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
                onSpinnerStateChange?.(null)
                setOnSpinnerStateChange(null)
                break
            }
            case assetEventModule.AssetEventType.createProject: {
                if (event.placeholderId === key) {
                    setState(backendModule.ProjectState.openInProgress)
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
                        const response = await backend.getProjectDetails(item.id, item.title)
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
                        await backend.openProject(item.id, null, item.title)
                        setState(oldState =>
                            oldState === backendModule.ProjectState.openInProgress
                                ? backendModule.ProjectState.opened
                                : oldState
                        )
                    } else {
                        try {
                            // This call will error if the VM is not ready yet.
                            await backend.checkResources(item.id, item.title)
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
    }, [checkState, item.id, item.title, backend])

    const closeProject = async (triggerOnClose = true) => {
        if (triggerOnClose) {
            onClose()
        }
        setShouldOpenWhenReady(false)
        setState(backendModule.ProjectState.closing)
        onSpinnerStateChange?.(null)
        setOnSpinnerStateChange(null)
        appRunner?.stopApp()
        setCheckState(CheckState.notChecking)
        try {
            if (
                backend.type === backendModule.BackendType.local &&
                state === backendModule.ProjectState.openInProgress
            ) {
                // Projects that are not opened cannot be closed.
                // This is the only way to wait until the project is open.
                await backend.openProject(item.id, null, item.title)
            }
            await backend.closeProject(item.id, item.title)
        } finally {
            setState(backendModule.ProjectState.closed)
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
                    className="w-6"
                    onClick={clickEvent => {
                        clickEvent.stopPropagation()
                        unsetModal()
                        doOpenManually(item.id)
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
