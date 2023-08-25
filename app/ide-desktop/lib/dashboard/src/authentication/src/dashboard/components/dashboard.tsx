/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as common from 'enso-common'

import * as assetEventModule from '../events/assetEvent'
import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as hooks from '../../hooks'
import * as http from '../../http'
import * as localBackend from '../localBackend'
import * as localStorageModule from '../localStorage'
import * as projectManager from '../projectManager'
import * as remoteBackendModule from '../remoteBackend'
import * as shortcutsModule from '../shortcuts'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as localStorageProvider from '../../providers/localStorage'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as shortcutsProvider from '../../providers/shortcuts'

import * as app from '../../components/app'
import * as pageSwitcher from './pageSwitcher'
import * as spinner from './spinner'
import Chat, * as chat from './chat'
import DriveView from './driveView'
import Editor from './editor'
import Templates from './templates'
import TheModal from './theModal'
import TopBar from './topBar'

// =================
// === Dashboard ===
// =================

/** Props for {@link Dashboard}s that are common to all platforms. */
export interface DashboardProps {
    /** Whether the application may have the local backend running. */
    supportsLocalBackend: boolean
    appRunner: AppRunner
    initialProjectName: string | null
    projectManagerUrl: string | null
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
    const { supportsLocalBackend, appRunner, initialProjectName, projectManagerUrl } = props
    const navigate = hooks.useNavigate()
    const logger = loggerProvider.useLogger()
    const session = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setBackend } = backendProvider.useSetBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const [initialized, setInitialized] = React.useState(false)
    const [query, setQuery] = React.useState('')
    const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)
    const [isHelpChatVisible, setIsHelpChatVisible] = React.useState(false)
    const [loadingProjectManagerDidFail, setLoadingProjectManagerDidFail] = React.useState(false)
    const [page, setPage] = React.useState(
        () => localStorage.get(localStorageModule.LocalStorageKey.page) ?? pageSwitcher.Page.drive
    )
    const [queuedAssetEvents, setQueuedAssetEvents] = React.useState<assetEventModule.AssetEvent[]>(
        []
    )
    const [projectStartupInfo, setProjectStartupInfo] =
        React.useState<backendModule.ProjectStartupInfo | null>(null)
    const [openProjectAbortController, setOpenProjectAbortController] =
        React.useState<AbortController | null>(null)
    const [assetListEvents, dispatchAssetListEvent] =
        hooks.useEvent<assetListEventModule.AssetListEvent>()
    const [assetEvents, dispatchAssetEvent] = hooks.useEvent<assetEventModule.AssetEvent>()

    const isListingLocalDirectoryAndWillFail =
        backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail
    const isListingRemoteDirectoryAndWillFail =
        backend.type === backendModule.BackendType.remote &&
        session.organization?.isEnabled !== true
    const isListingRemoteDirectoryWhileOffline =
        session.type === authProvider.UserSessionType.offline &&
        backend.type === backendModule.BackendType.remote

    React.useEffect(() => {
        setInitialized(true)
    }, [])

    React.useEffect(() => {
        unsetModal()
        if (page === pageSwitcher.Page.editor) {
            document.body.style.cursor = 'none'
        } else {
            document.body.style.cursor = 'auto'
        }
    }, [page, /* should never change */ unsetModal])

    React.useEffect(() => {
        let currentBackend = backend
        if (
            supportsLocalBackend &&
            session.type !== authProvider.UserSessionType.offline &&
            localStorage.get(localStorageModule.LocalStorageKey.backendType) ===
                backendModule.BackendType.local
        ) {
            currentBackend = new localBackend.LocalBackend(
                projectManagerUrl,
                localStorage.get(localStorageModule.LocalStorageKey.projectStartupInfo) ?? null
            )
            setBackend(currentBackend)
        }
        const savedProjectStartupInfo = localStorage.get(
            localStorageModule.LocalStorageKey.projectStartupInfo
        )
        if (savedProjectStartupInfo != null) {
            if (savedProjectStartupInfo.backendType === backendModule.BackendType.remote) {
                if (session.accessToken != null) {
                    if (
                        currentBackend.type === backendModule.BackendType.remote &&
                        savedProjectStartupInfo.projectAsset.parentId ===
                            backend.rootDirectoryId(session.organization)
                    ) {
                        // `projectStartupInfo` is still `null`, so the `editor` page will be empty.
                        setPage(pageSwitcher.Page.drive)
                        setQueuedAssetEvents([
                            {
                                type: assetEventModule.AssetEventType.openProject,
                                id: savedProjectStartupInfo.project.projectId,
                                shouldAutomaticallySwitchPage: page === pageSwitcher.Page.editor,
                            },
                        ])
                    } else {
                        setPage(pageSwitcher.Page.drive)
                        const httpClient = new http.Client(
                            new Headers([['Authorization', `Bearer ${session.accessToken}`]])
                        )
                        const remoteBackend = new remoteBackendModule.RemoteBackend(
                            httpClient,
                            logger
                        )
                        void (async () => {
                            const abortController = new AbortController()
                            setOpenProjectAbortController(abortController)
                            await remoteBackendModule.waitUntilProjectIsReady(
                                remoteBackend,
                                savedProjectStartupInfo.projectAsset,
                                abortController
                            )
                            if (!abortController.signal.aborted) {
                                const project = await remoteBackend.getProjectDetails(
                                    savedProjectStartupInfo.projectAsset.id,
                                    savedProjectStartupInfo.projectAsset.title
                                )
                                setProjectStartupInfo({ ...savedProjectStartupInfo, project })
                                if (page === pageSwitcher.Page.editor) {
                                    setPage(page)
                                }
                            }
                        })()
                    }
                }
            } else {
                setProjectStartupInfo(savedProjectStartupInfo)
            }
        }
        // This MUST only run when the component is mounted.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    hooks.useEventHandler(assetEvents, event => {
        switch (event.type) {
            case assetEventModule.AssetEventType.openProject: {
                openProjectAbortController?.abort()
                setOpenProjectAbortController(null)
                break
            }
            default: {
                // Ignored.
                break
            }
        }
    })

    React.useEffect(() => {
        if (initialized) {
            if (projectStartupInfo != null) {
                localStorage.set(
                    localStorageModule.LocalStorageKey.projectStartupInfo,
                    projectStartupInfo
                )
            } else {
                localStorage.delete(localStorageModule.LocalStorageKey.projectStartupInfo)
            }
        }
        // `initialized` is NOT a dependency.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [projectStartupInfo, /* should never change */ localStorage])

    React.useEffect(() => {
        localStorage.set(localStorageModule.LocalStorageKey.page, page)
    }, [page, /* should never change */ localStorage])

    React.useEffect(() => {
        const onClick = () => {
            if (getSelection()?.type !== 'Range') {
                unsetModal()
            }
        }
        document.addEventListener('click', onClick)
        return () => {
            document.removeEventListener('click', onClick)
        }
    }, [/* should never change */ unsetModal])

    React.useEffect(() => {
        // The types come from a third-party API and cannot be changed.
        // eslint-disable-next-line no-restricted-syntax
        let handle: number | undefined
        if (isHelpChatOpen) {
            setIsHelpChatVisible(true)
        } else {
            handle = window.setTimeout(() => {
                setIsHelpChatVisible(false)
            }, chat.ANIMATION_DURATION_MS)
        }
        return () => {
            clearTimeout(handle)
        }
    }, [isHelpChatOpen])

    React.useEffect(() => {
        const onProjectManagerLoadingFailed = () => {
            setLoadingProjectManagerDidFail(true)
        }
        document.addEventListener(
            projectManager.ProjectManagerEvents.loadingFailed,
            onProjectManagerLoadingFailed
        )
        return () => {
            document.removeEventListener(
                projectManager.ProjectManagerEvents.loadingFailed,
                onProjectManagerLoadingFailed
            )
        }
    }, [])

    React.useEffect(() => {
        return shortcuts.registerKeyboardHandlers({
            [shortcutsModule.KeyboardAction.closeModal]: unsetModal,
        })
    }, [shortcuts, unsetModal])

    const setBackendType = React.useCallback(
        (newBackendType: backendModule.BackendType) => {
            if (newBackendType !== backend.type) {
                switch (newBackendType) {
                    case backendModule.BackendType.local:
                        setBackend(new localBackend.LocalBackend(projectManagerUrl, null))
                        break
                    case backendModule.BackendType.remote: {
                        const headers = new Headers()
                        headers.append('Authorization', `Bearer ${session.accessToken ?? ''}`)
                        const client = new http.Client(headers)
                        setBackend(new remoteBackendModule.RemoteBackend(client, logger))
                        break
                    }
                }
            }
        },
        [
            backend.type,
            session.accessToken,
            logger,
            /* should never change */ projectManagerUrl,
            /* should never change */ setBackend,
        ]
    )

    const doCreateProject = React.useCallback(
        (
            templateId: string | null,
            onSpinnerStateChange?: (state: spinner.SpinnerState) => void
        ) => {
            dispatchAssetListEvent({
                type: assetListEventModule.AssetListEventType.newProject,
                parentKey: null,
                parentId: null,
                templateId: templateId ?? null,
                onSpinnerStateChange: onSpinnerStateChange ?? null,
            })
        },
        [/* should never change */ dispatchAssetListEvent]
    )

    const openEditor = React.useCallback(
        async (newProject: backendModule.ProjectAsset, switchPage: boolean) => {
            if (switchPage) {
                setPage(pageSwitcher.Page.editor)
            }
            if (projectStartupInfo?.project.projectId !== newProject.id) {
                setProjectStartupInfo({
                    project: await backend.getProjectDetails(newProject.id, newProject.title),
                    projectAsset: newProject,
                    backendType: backend.type,
                    accessToken: session.accessToken,
                })
            }
        },
        [backend, projectStartupInfo?.project.projectId, session.accessToken]
    )

    const closeEditor = React.useCallback(() => {
        setProjectStartupInfo(null)
    }, [])

    const driveHiddenClass = page === pageSwitcher.Page.drive ? '' : 'hidden'
    return (
        <>
            <div
                className={`flex flex-col gap-2 relative select-none text-primary text-xs h-screen pb-2 ${
                    page === pageSwitcher.Page.editor ? 'cursor-none pointer-events-none' : ''
                }`}
                onContextMenu={event => {
                    event.preventDefault()
                    unsetModal()
                }}
            >
                <TopBar
                    supportsLocalBackend={supportsLocalBackend}
                    page={page}
                    setPage={setPage}
                    asset={null}
                    isEditorDisabled={projectStartupInfo == null}
                    isHelpChatOpen={isHelpChatOpen}
                    setIsHelpChatOpen={setIsHelpChatOpen}
                    setBackendType={setBackendType}
                    query={query}
                    setQuery={setQuery}
                    onSignOut={() => {
                        if (page === pageSwitcher.Page.editor) {
                            setPage(pageSwitcher.Page.drive)
                        }
                        setProjectStartupInfo(null)
                    }}
                />
                {isListingRemoteDirectoryWhileOffline ? (
                    <div className={`grow grid place-items-center mx-2 ${driveHiddenClass}`}>
                        <div className="flex flex-col gap-4">
                            <div className="text-base text-center">You are not signed in.</div>
                            <button
                                className="text-base text-white bg-help rounded-full self-center leading-170 h-8 py-px w-16"
                                onClick={() => {
                                    navigate(app.LOGIN_PATH)
                                }}
                            >
                                Login
                            </button>
                        </div>
                    </div>
                ) : isListingLocalDirectoryAndWillFail ? (
                    <div className={`grow grid place-items-center mx-2 ${driveHiddenClass}`}>
                        <div className="text-base text-center">
                            Could not connect to the Project Manager. Please try restarting{' '}
                            {common.PRODUCT_NAME}, or manually launching the Project Manager.
                        </div>
                    </div>
                ) : isListingRemoteDirectoryAndWillFail ? (
                    <div className={`grow grid place-items-center mx-2 ${driveHiddenClass}`}>
                        <div className="text-base text-center">
                            We will review your user details and enable the cloud experience for you
                            shortly.
                        </div>
                    </div>
                ) : (
                    <>
                        <Templates
                            hidden={page !== pageSwitcher.Page.drive}
                            onTemplateClick={doCreateProject}
                        />
                        <DriveView
                            hidden={page !== pageSwitcher.Page.drive}
                            page={page}
                            initialProjectName={initialProjectName}
                            queuedAssetEvents={queuedAssetEvents}
                            assetListEvents={assetListEvents}
                            dispatchAssetListEvent={dispatchAssetListEvent}
                            assetEvents={assetEvents}
                            dispatchAssetEvent={dispatchAssetEvent}
                            query={query}
                            doCreateProject={doCreateProject}
                            doOpenEditor={openEditor}
                            doCloseEditor={closeEditor}
                            appRunner={appRunner}
                            loadingProjectManagerDidFail={loadingProjectManagerDidFail}
                            isListingRemoteDirectoryWhileOffline={
                                isListingRemoteDirectoryWhileOffline
                            }
                            isListingLocalDirectoryAndWillFail={isListingLocalDirectoryAndWillFail}
                            isListingRemoteDirectoryAndWillFail={
                                isListingRemoteDirectoryAndWillFail
                            }
                        />
                    </>
                )}
                <Editor
                    visible={page === pageSwitcher.Page.editor}
                    supportsLocalBackend={supportsLocalBackend}
                    projectStartupInfo={projectStartupInfo}
                    appRunner={appRunner}
                />
                {/* `session.accessToken` MUST be present in order for the `Chat` component to work. */}
                {isHelpChatVisible && session.accessToken != null && (
                    <Chat
                        isOpen={isHelpChatOpen}
                        doClose={() => {
                            setIsHelpChatOpen(false)
                        }}
                    />
                )}
            </div>
            <div className="text-xs text-primary select-none">
                <TheModal />
            </div>
        </>
    )
}
