/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'
import * as eventHooks from '#/hooks/eventHooks'
import type * as assetSearchBar from '#/layouts/dashboard/assetSearchBar'
import type * as assetSettingsPanel from '#/layouts/dashboard/AssetSettingsPanel'
import AssetSettingsPanel from '#/layouts/dashboard/AssetSettingsPanel'
import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import Chat, * as chat from '#/layouts/dashboard/Chat'
import ChatPlaceholder from '#/layouts/dashboard/ChatPlaceholder'
import Drive from '#/layouts/dashboard/Drive'
import Editor from '#/layouts/dashboard/Editor'
import Home from '#/layouts/dashboard/Home'
import * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import TopBar from '#/layouts/dashboard/TopBar'
import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as loggerProvider from '#/providers/LoggerProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as shortcutsProvider from '#/providers/ShortcutsProvider'
import * as backendModule from '#/services/backend'
import * as localBackendModule from '#/services/localBackend'
import * as remoteBackendModule from '#/services/remoteBackend'
import * as assetQuery from '#/utilities/assetQuery'
import * as http from '#/utilities/http'
import * as localStorageModule from '#/utilities/localStorage'
import * as object from '#/utilities/object'
import * as shortcutsModule from '#/utilities/shortcuts'

import TheModal from '#/components/dashboard/TheModal'
import type * as spinner from '#/components/Spinner'

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
    const { supportsLocalBackend, appRunner, initialProjectName: rawInitialProjectName } = props
    const { projectManagerUrl } = props
    const logger = loggerProvider.useLogger()
    const session = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setBackend } = backendProvider.useSetBackend()
    const { modalRef } = modalProvider.useModalRef()
    const { unsetModal } = modalProvider.useSetModal()
    const { localStorage } = localStorageProvider.useLocalStorage()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const [initialized, setInitialized] = React.useState(false)
    const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)
    const [isHelpChatVisible, setIsHelpChatVisible] = React.useState(false)
    const [page, setPage] = React.useState(
        () => localStorage.get(localStorageModule.LocalStorageKey.page) ?? pageSwitcher.Page.drive
    )
    const [queuedAssetEvents, setQueuedAssetEvents] = React.useState<assetEvent.AssetEvent[]>([])
    const [query, setQuery] = React.useState(() => assetQuery.AssetQuery.fromString(''))
    const [labels, setLabels] = React.useState<backendModule.Label[]>([])
    const [suggestions, setSuggestions] = React.useState<assetSearchBar.Suggestion[]>([])
    const [projectStartupInfo, setProjectStartupInfo] =
        React.useState<backendModule.ProjectStartupInfo | null>(null)
    const [openProjectAbortController, setOpenProjectAbortController] =
        React.useState<AbortController | null>(null)
    const [assetListEvents, dispatchAssetListEvent] =
        eventHooks.useEvent<assetListEvent.AssetListEvent>()
    const [assetEvents, dispatchAssetEvent] = eventHooks.useEvent<assetEvent.AssetEvent>()
    const [assetSettingsPanelProps, setAssetSettingsPanelProps] =
        React.useState<assetSettingsPanel.AssetSettingsPanelRequiredProps | null>(null)
    const [isAssetSettingsPanelVisible, setIsAssetSettingsPanelVisible] = React.useState(
        () =>
            localStorage.get(localStorageModule.LocalStorageKey.isAssetSettingsPanelVisible) ??
            false
    )
    const [initialProjectName, setInitialProjectName] = React.useState(rawInitialProjectName)
    const rootDirectoryId = React.useMemo(
        () => session.organization?.rootDirectoryId ?? backendModule.DirectoryId(''),
        [session.organization]
    )

    React.useEffect(() => {
        setInitialized(true)
    }, [])

    React.useEffect(() => {
        unsetModal()
        // FIXME [sb]: https://github.com/enso-org/cloud-v2/issues/777
        // Workarounds for GUI1 should be removed when they are no longer necessary.
        document.body.style.cursor = page === pageSwitcher.Page.editor ? 'none' : 'auto'
    }, [page, /* should never change */ unsetModal])

    React.useEffect(() => {
        if (query.query !== '') {
            setPage(pageSwitcher.Page.drive)
        }
    }, [query])

    React.useEffect(() => {
        let currentBackend = backend
        if (
            supportsLocalBackend &&
            localStorage.get(localStorageModule.LocalStorageKey.backendType) ===
                backendModule.BackendType.local
        ) {
            currentBackend = new localBackendModule.LocalBackend(projectManagerUrl)
            setBackend(currentBackend)
        }
        const savedProjectStartupInfo = localStorage.get(
            localStorageModule.LocalStorageKey.projectStartupInfo
        )
        if (rawInitialProjectName != null) {
            if (page === pageSwitcher.Page.editor) {
                setPage(pageSwitcher.Page.drive)
            }
        } else if (savedProjectStartupInfo != null) {
            if (savedProjectStartupInfo.backendType === backendModule.BackendType.remote) {
                if (session.accessToken != null) {
                    if (
                        currentBackend.type === backendModule.BackendType.remote &&
                        savedProjectStartupInfo.projectAsset.parentId ===
                            session.organization.rootDirectoryId
                    ) {
                        // `projectStartupInfo` is still `null`, so the `editor` page will be empty.
                        setPage(pageSwitcher.Page.drive)
                        setQueuedAssetEvents([
                            {
                                type: AssetEventType.openProject,
                                id: savedProjectStartupInfo.project.projectId,
                                shouldAutomaticallySwitchPage: page === pageSwitcher.Page.editor,
                                runInBackground: false,
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
                            const oldProject = await backend.getProjectDetails(
                                savedProjectStartupInfo.projectAsset.id,
                                savedProjectStartupInfo.projectAsset.title
                            )
                            if (
                                backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[
                                    oldProject.state.type
                                ]
                            ) {
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
                                    setProjectStartupInfo(
                                        object.merge(savedProjectStartupInfo, { project })
                                    )
                                    if (page === pageSwitcher.Page.editor) {
                                        setPage(page)
                                    }
                                }
                            }
                        })()
                    }
                }
            } else {
                if (currentBackend.type === backendModule.BackendType.local) {
                    setInitialProjectName(savedProjectStartupInfo.projectAsset.id)
                } else {
                    const localBackend = new localBackendModule.LocalBackend(projectManagerUrl)
                    void (async () => {
                        await localBackend.openProject(
                            savedProjectStartupInfo.projectAsset.id,
                            null,
                            savedProjectStartupInfo.projectAsset.title
                        )
                        const project = await localBackend.getProjectDetails(
                            savedProjectStartupInfo.projectAsset.id,
                            savedProjectStartupInfo.projectAsset.title
                        )
                        setProjectStartupInfo(object.merge(savedProjectStartupInfo, { project }))
                    })()
                }
            }
        }
        // This MUST only run when the component is mounted.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    eventHooks.useEventHandler(assetEvents, event => {
        switch (event.type) {
            case AssetEventType.openProject: {
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
        localStorage.set(
            localStorageModule.LocalStorageKey.isAssetSettingsPanelVisible,
            isAssetSettingsPanelVisible
        )
    }, [isAssetSettingsPanelVisible, /* should never change */ localStorage])

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
        return shortcuts.registerKeyboardHandlers({
            [shortcutsModule.KeyboardAction.closeModal]: () => {
                unsetModal()
                if (modalRef.current == null) {
                    // eslint-disable-next-line no-restricted-syntax
                    return false
                }
            },
        })
    }, [shortcuts, /* should never change */ modalRef, /* should never change */ unsetModal])

    const setBackendType = React.useCallback(
        (newBackendType: backendModule.BackendType) => {
            if (newBackendType !== backend.type) {
                switch (newBackendType) {
                    case backendModule.BackendType.local:
                        setBackend(new localBackendModule.LocalBackend(projectManagerUrl))
                        break
                    case backendModule.BackendType.remote: {
                        const client = new http.Client([
                            ['Authorization', `Bearer ${session.accessToken ?? ''}`],
                        ])
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
            templateId: string | null = null,
            templateName: string | null = null,
            onSpinnerStateChange: ((state: spinner.SpinnerState) => void) | null = null
        ) => {
            dispatchAssetListEvent({
                type: AssetListEventType.newProject,
                parentKey: rootDirectoryId,
                parentId: rootDirectoryId,
                templateId: templateId,
                templateName: templateName,
                onSpinnerStateChange: onSpinnerStateChange,
            })
        },
        [rootDirectoryId, /* should never change */ dispatchAssetListEvent]
    )

    const openEditor = React.useCallback(
        async (
            newProject: backendModule.ProjectAsset,
            setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
            switchPage: boolean
        ) => {
            if (switchPage) {
                setPage(pageSwitcher.Page.editor)
            }
            if (projectStartupInfo?.project.projectId !== newProject.id) {
                setProjectStartupInfo({
                    project: await backend.getProjectDetails(newProject.id, newProject.title),
                    projectAsset: newProject,
                    setProjectAsset: setProjectAsset,
                    backendType: backend.type,
                    accessToken: session.accessToken,
                })
            }
        },
        [backend, projectStartupInfo?.project.projectId, session.accessToken]
    )

    const closeEditor = React.useCallback((closingProject: backendModule.ProjectAsset) => {
        setProjectStartupInfo(oldInfo =>
            oldInfo?.projectAsset.id === closingProject.id ? null : oldInfo
        )
    }, [])

    const doRemoveSelf = React.useCallback(() => {
        if (projectStartupInfo?.projectAsset != null) {
            dispatchAssetListEvent({
                type: AssetListEventType.removeSelf,
                id: projectStartupInfo.projectAsset.id,
            })
            setProjectStartupInfo(null)
        }
    }, [projectStartupInfo?.projectAsset, /* should never change */ dispatchAssetListEvent])

    const onSignOut = React.useCallback(() => {
        if (page === pageSwitcher.Page.editor) {
            setPage(pageSwitcher.Page.drive)
        }
        setProjectStartupInfo(null)
    }, [page])

    return (
        <>
            <div
                className={`flex text-primary text-xs ${
                    page === pageSwitcher.Page.editor ? 'cursor-none pointer-events-none' : ''
                }`}
            >
                <div
                    className="flex flex-col grow container-size gap-2 overflow-hidden relative select-none h-screen pb-2"
                    onContextMenu={event => {
                        event.preventDefault()
                        unsetModal()
                    }}
                >
                    <TopBar
                        supportsLocalBackend={supportsLocalBackend}
                        projectAsset={projectStartupInfo?.projectAsset ?? null}
                        setProjectAsset={projectStartupInfo?.setProjectAsset ?? null}
                        page={page}
                        setPage={setPage}
                        isEditorDisabled={projectStartupInfo == null}
                        isHelpChatOpen={isHelpChatOpen}
                        setIsHelpChatOpen={setIsHelpChatOpen}
                        setBackendType={setBackendType}
                        query={query}
                        setQuery={setQuery}
                        labels={labels}
                        suggestions={suggestions}
                        canToggleSettingsPanel={assetSettingsPanelProps != null}
                        isSettingsPanelVisible={
                            isAssetSettingsPanelVisible && assetSettingsPanelProps != null
                        }
                        setIsSettingsPanelVisible={setIsAssetSettingsPanelVisible}
                        doRemoveSelf={doRemoveSelf}
                        onSignOut={() => {
                            if (page === pageSwitcher.Page.editor) {
                                setPage(pageSwitcher.Page.drive)
                            }
                            setProjectStartupInfo(null)
                        }}
                    />
                    <Home
                        hidden={page !== pageSwitcher.Page.home}
                        createProject={doCreateProject}
                    />
                    <Drive
                        supportsLocalBackend={supportsLocalBackend}
                        hidden={page !== pageSwitcher.Page.drive}
                        page={page}
                        initialProjectName={initialProjectName}
                        query={query}
                        setQuery={setQuery}
                        labels={labels}
                        setLabels={setLabels}
                        setSuggestions={setSuggestions}
                        projectStartupInfo={projectStartupInfo}
                        queuedAssetEvents={queuedAssetEvents}
                        assetListEvents={assetListEvents}
                        dispatchAssetListEvent={dispatchAssetListEvent}
                        assetEvents={assetEvents}
                        dispatchAssetEvent={dispatchAssetEvent}
                        setAssetSettingsPanelProps={setAssetSettingsPanelProps}
                        doCreateProject={doCreateProject}
                        doOpenEditor={openEditor}
                        doCloseEditor={closeEditor}
                    />
                    <Editor
                        hidden={page !== pageSwitcher.Page.editor}
                        supportsLocalBackend={supportsLocalBackend}
                        projectStartupInfo={projectStartupInfo}
                        appRunner={appRunner}
                    />
                    {/* `session.accessToken` MUST be present in order for the `Chat` component to work. */}
                    {isHelpChatVisible && session.accessToken != null ? (
                        <Chat
                            page={page}
                            isOpen={isHelpChatOpen}
                            doClose={() => {
                                setIsHelpChatOpen(false)
                            }}
                        />
                    ) : (
                        <ChatPlaceholder
                            page={page}
                            isOpen={isHelpChatOpen}
                            doClose={() => {
                                setIsHelpChatOpen(false)
                            }}
                        />
                    )}
                </div>
                <div
                    className={`flex flex-col duration-500 transition-min-width ease-in-out overflow-hidden ${
                        isAssetSettingsPanelVisible && assetSettingsPanelProps != null
                            ? 'min-w-120'
                            : 'min-w-0'
                    }`}
                >
                    {assetSettingsPanelProps && (
                        <AssetSettingsPanel
                            supportsLocalBackend={supportsLocalBackend}
                            key={assetSettingsPanelProps.item.item.id}
                            {...assetSettingsPanelProps}
                            page={page}
                            category={Category.home}
                            isHelpChatOpen={isHelpChatOpen}
                            setIsHelpChatOpen={setIsHelpChatOpen}
                            setIsSettingsPanelVisible={setIsAssetSettingsPanelVisible}
                            dispatchAssetEvent={dispatchAssetEvent}
                            projectAsset={projectStartupInfo?.projectAsset ?? null}
                            setProjectAsset={projectStartupInfo?.setProjectAsset ?? null}
                            doRemoveSelf={doRemoveSelf}
                            onSignOut={onSignOut}
                        />
                    )}
                </div>
            </div>
            <div className="text-xs text-primary select-none">
                <TheModal />
            </div>
        </>
    )
}
