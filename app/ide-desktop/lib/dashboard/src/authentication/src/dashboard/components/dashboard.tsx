/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as assetListEventModule from '../events/assetListEvent'
import * as backendModule from '../backend'
import * as hooks from '../../hooks'
import * as http from '../../http'
import * as localBackend from '../localBackend'
import * as projectManager from '../projectManager'
import * as remoteBackendModule from '../remoteBackend'
import * as shortcutsModule from '../shortcuts'

import * as authProvider from '../../authentication/providers/auth'
import * as backendProvider from '../../providers/backend'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import * as shortcutsProvider from '../../providers/shortcuts'

import * as pageSwitcher from './pageSwitcher'
import * as spinner from './spinner'
import Chat, * as chat from './chat'
import Drive from './drive'
import Editor from './editor'
import Home from './home'
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
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
    const { supportsLocalBackend, appRunner, initialProjectName } = props
    const logger = loggerProvider.useLogger()
    const session = authProvider.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const { setBackend } = backendProvider.useSetBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const { shortcuts } = shortcutsProvider.useShortcuts()
    const [query, setQuery] = React.useState('')
    const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)
    const [isHelpChatVisible, setIsHelpChatVisible] = React.useState(false)
    const [loadingProjectManagerDidFail, setLoadingProjectManagerDidFail] = React.useState(false)
    const [page, setPage] = React.useState(pageSwitcher.Page.drive)
    const [project, setProject] = React.useState<backendModule.Project | null>(null)
    const [assetListEvents, dispatchAssetListEvent] =
        hooks.useEvent<assetListEventModule.AssetListEvent>()

    const isListingLocalDirectoryAndWillFail =
        backend.type === backendModule.BackendType.local && loadingProjectManagerDidFail
    const isListingRemoteDirectoryAndWillFail =
        backend.type === backendModule.BackendType.remote &&
        session.organization?.isEnabled !== true
    const isListingRemoteDirectoryWhileOffline =
        session.type === authProvider.UserSessionType.offline &&
        backend.type === backendModule.BackendType.remote

    React.useEffect(() => {
        unsetModal()
    }, [page, /* should never change */ unsetModal])

    React.useEffect(() => {
        if (query !== '') {
            setPage(pageSwitcher.Page.drive)
        }
    }, [query])

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
        if (
            supportsLocalBackend &&
            session.type !== authProvider.UserSessionType.offline &&
            localStorage.getItem(backendProvider.BACKEND_TYPE_KEY) !==
                backendModule.BackendType.remote
        ) {
            setBackend(new localBackend.LocalBackend())
        }
        // This hook MUST only run once, on mount.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

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
                        setBackend(new localBackend.LocalBackend())
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
        [backend.type, logger, session.accessToken, setBackend]
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
        async (newProject: backendModule.ProjectAsset) => {
            setPage(pageSwitcher.Page.editor)
            if (project?.projectId !== newProject.id) {
                setProject(await backend.getProjectDetails(newProject.id, newProject.title))
            }
        },
        [backend, project?.projectId, setPage]
    )

    const closeEditor = React.useCallback(() => {
        setProject(null)
    }, [])

    return (
        <>
            <div
                className={`flex flex-col relative select-none text-primary text-xs h-screen pb-2 ${
                    page === pageSwitcher.Page.editor ? 'cursor-none pointer-events-none' : ''
                }`}
                onContextMenu={event => {
                    event.preventDefault()
                    unsetModal()
                }}
            >
                <TopBar
                    supportsLocalBackend={supportsLocalBackend}
                    projectName={project?.name ?? null}
                    page={page}
                    setPage={setPage}
                    asset={null}
                    isEditorDisabled={project == null}
                    isHelpChatOpen={isHelpChatOpen}
                    setIsHelpChatOpen={setIsHelpChatOpen}
                    setBackendType={setBackendType}
                    query={query}
                    setQuery={setQuery}
                    onSignOut={() => {
                        if (page === pageSwitcher.Page.editor) {
                            setPage(pageSwitcher.Page.drive)
                        }
                        setProject(null)
                    }}
                />
                <Home hidden={page !== pageSwitcher.Page.home} onTemplateClick={doCreateProject} />
                <Drive
                    hidden={page !== pageSwitcher.Page.drive}
                    page={page}
                    initialProjectName={initialProjectName}
                    assetListEvents={assetListEvents}
                    dispatchAssetListEvent={dispatchAssetListEvent}
                    query={query}
                    doCreateProject={doCreateProject}
                    doOpenEditor={openEditor}
                    doCloseEditor={closeEditor}
                    appRunner={appRunner}
                    loadingProjectManagerDidFail={loadingProjectManagerDidFail}
                    isListingRemoteDirectoryWhileOffline={isListingRemoteDirectoryWhileOffline}
                    isListingLocalDirectoryAndWillFail={isListingLocalDirectoryAndWillFail}
                    isListingRemoteDirectoryAndWillFail={isListingRemoteDirectoryAndWillFail}
                />
                <TheModal />
                <Editor
                    visible={page === pageSwitcher.Page.editor}
                    project={project}
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
