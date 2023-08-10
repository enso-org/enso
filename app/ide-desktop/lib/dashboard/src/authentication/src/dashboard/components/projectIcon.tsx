/** @file An interactive button indicating the status of a project. */
import * as React from 'react'
import * as toast from 'react-toastify'

import ArrowUpIcon from 'enso-assets/arrow_up.svg'
import PlayIcon from 'enso-assets/play.svg'
import StopIcon from 'enso-assets/stop.svg'

import * as assetEventModule from '../events/assetEvent'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
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
    [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingSlow,
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
    [backendModule.ProjectState.placeholder]: spinner.SpinnerState.loadingMedium,
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
    const toastAndLog = hooks.useToastAndLog()
    const state = item.projectState.type
    const setState = React.useCallback(
        (stateOrUpdater: React.SetStateAction<backendModule.ProjectState>) => {
            if (typeof stateOrUpdater === 'function') {
                setItem(oldItem => ({
                    ...oldItem,
                    projectState: { type: stateOrUpdater(oldItem.projectState.type) },
                }))
            } else {
                setItem(oldItem => ({ ...oldItem, projectState: { type: stateOrUpdater } }))
            }
        },
        [/* should never change */ setItem]
    )
    const [checkState, setCheckState] = React.useState(CheckState.notChecking)
    const [spinnerState, setSpinnerState] = React.useState(spinner.SpinnerState.initial)
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
                    if (
                        state !== backendModule.ProjectState.openInProgress &&
                        state !== backendModule.ProjectState.opened
                    ) {
                        setToastId(toast.toast.loading(LOADING_MESSAGE))
                        await backend.openProject(item.id, null, item.title)
                    }
                    setCheckState(CheckState.checkingStatus)
                    break
                case backendModule.BackendType.local:
                    setCheckState(CheckState.localProject)
                    await backend.openProject(item.id, null, item.title)
                    setState(oldState =>
                        oldState === backendModule.ProjectState.openInProgress
                            ? backendModule.ProjectState.opened
                            : oldState
                    )
                    break
            }
        } catch (error) {
            setCheckState(CheckState.notChecking)
            toastAndLog(`Could not open project '${item.title}'`, error)
            setState(backendModule.ProjectState.closed)
        }
    }, [
        state,
        backend,
        item.id,
        item.title,
        /* should never change */ toastAndLog,
        /* should never change */ setState,
    ])

    React.useEffect(() => {
        if (item.projectState.type === backendModule.ProjectState.openInProgress) {
            void openProject()
        }
        // This MUST only run once, when the component is initially mounted.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

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

    hooks.useEventHandler(assetEvents, event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.createDirectory:
            case assetEventModule.AssetEventType.uploadFiles:
            case assetEventModule.AssetEventType.createSecret:
            case assetEventModule.AssetEventType.deleteMultiple:
            case assetEventModule.AssetEventType.downloadSelected: {
                // Ignored. Any missing project-related events should be handled by
                // `ProjectNameColumn`. `deleteMultiple` and `downloadSelected` are handled by
                // `AssetRow`.
                break
            }
            case assetEventModule.AssetEventType.openProject: {
                if (event.id !== item.id) {
                    setShouldOpenWhenReady(false)
                    void closeProject(false)
                } else {
                    setShouldOpenWhenReady(true)
                    void openProject()
                }
                break
            }
            case assetEventModule.AssetEventType.cancelOpeningAllProjects: {
                setShouldOpenWhenReady(false)
                onSpinnerStateChange?.(null)
                setOnSpinnerStateChange(null)
                setCheckState(CheckState.notChecking)
                void closeProject(false)
                break
            }
            case assetEventModule.AssetEventType.createProject: {
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
                    try {
                        // This call will error if the VM is not ready yet.
                        await backend.checkResources(item.id, item.title)
                        setToastId(null)
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
                void checkProjectResources()
                return () => {
                    continuePolling = false
                    if (handle != null) {
                        window.clearTimeout(handle)
                    }
                }
            }
        }
        // `backend` is NOT a dependency as an asset belongs to a specific backend.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [checkState, item.id, item.title, /* should never change */ setState])

    const closeProject = async (triggerOnClose = true) => {
        setToastId(null)
        setShouldOpenWhenReady(false)
        setState(backendModule.ProjectState.closing)
        onSpinnerStateChange?.(null)
        setOnSpinnerStateChange(null)
        appRunner?.stopApp()
        setCheckState(CheckState.notChecking)
        if (
            state !== backendModule.ProjectState.closing &&
            state !== backendModule.ProjectState.closed
        ) {
            if (triggerOnClose) {
                onClose()
            }
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
                setState(backendModule.ProjectState.closed)
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
        case backendModule.ProjectState.placeholder:
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
