/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as eventHooks from '#/hooks/eventHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as loggerProvider from '#/providers/LoggerProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/dashboard/AssetPanel'
import AssetPanel from '#/layouts/dashboard/AssetPanel'
import type * as assetSearchBar from '#/layouts/dashboard/AssetSearchBar'
import Category from '#/layouts/dashboard/CategorySwitcher/Category'
import Chat from '#/layouts/dashboard/Chat'
import ChatPlaceholder from '#/layouts/dashboard/ChatPlaceholder'
import Drive from '#/layouts/dashboard/Drive'
import Editor from '#/layouts/dashboard/Editor'
import Home from '#/layouts/dashboard/Home'
import * as pageSwitcher from '#/layouts/dashboard/PageSwitcher'
import Settings from '#/layouts/dashboard/Settings'
import TopBar from '#/layouts/dashboard/TopBar'

import TheModal from '#/components/dashboard/TheModal'
import type * as spinner from '#/components/Spinner'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import RemoteBackend, * as remoteBackendModule from '#/services/RemoteBackend'

import * as array from '#/utilities/array'
import AssetQuery from '#/utilities/AssetQuery'
import HttpClient from '#/utilities/HttpClient'
import LocalStorage from '#/utilities/LocalStorage'
import * as object from '#/utilities/object'
import * as shortcutManagerModule from '#/utilities/ShortcutManager'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    isAssetPanelVisible: boolean
    page: pageSwitcher.Page
    projectStartupInfo: backendModule.ProjectStartupInfo
  }
}

LocalStorage.registerKey('isAssetPanelVisible', {
  tryParse: value => (value === true ? value : null),
})

const PAGES = Object.values(pageSwitcher.Page)
LocalStorage.registerKey('page', {
  tryParse: value => (array.includes(PAGES, value) ? value : null),
})

const BACKEND_TYPES = Object.values(backendModule.BackendType)
LocalStorage.registerKey('projectStartupInfo', {
  isUserSpecific: true,
  tryParse: value => {
    if (typeof value !== 'object' || value == null) {
      return null
    } else if (!('accessToken' in value) || typeof value.accessToken !== 'string') {
      return null
    } else if (!('backendType' in value) || !array.includes(BACKEND_TYPES, value.backendType)) {
      return null
    } else if (!('project' in value) || !('projectAsset' in value)) {
      return null
    } else {
      return {
        // These type assertions are UNSAFE, however correctly type-checking these
        // would be very complicated.
        // eslint-disable-next-line no-restricted-syntax
        project: value.project as backendModule.Project,
        // eslint-disable-next-line no-restricted-syntax
        projectAsset: value.projectAsset as backendModule.SmartProject,
        backendType: value.backendType,
        accessToken: value.accessToken,
      }
    }
  },
})

// =================
// === Dashboard ===
// =================

/** Props for {@link Dashboard}s that are common to all platforms. */
export interface DashboardProps {
  /** Whether the application may have the local backend running. */
  supportsLocalBackend: boolean
  backend: Backend
  setBackend: (backend: Backend) => void
  appRunner: AppRunner
  initialProjectName: string | null
  projectManagerUrl: string | null
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
  const { supportsLocalBackend, backend, setBackend, appRunner } = props
  const { initialProjectName: rawInitialProjectName, projectManagerUrl } = props
  const logger = loggerProvider.useLogger()
  const { organization, accessToken } = authProvider.useNonPartialUserSession()
  const { modalRef } = modalProvider.useModalRef()
  const { updateModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { shortcutManager } = shortcutManagerProvider.useShortcutManager()
  const [initialized, setInitialized] = React.useState(false)
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)
  const [page, setPage] = React.useState(() => localStorage.get('page') ?? pageSwitcher.Page.drive)
  const [query, setQuery] = React.useState(() => AssetQuery.fromString(''))
  const [labels, setLabels] = React.useState<backendModule.Label[]>([])
  const [suggestions, setSuggestions] = React.useState<assetSearchBar.Suggestion[]>([])
  const [projectStartupInfo, setProjectStartupInfo] =
    React.useState<backendModule.ProjectStartupInfo | null>(null)
  const [openProjectAbortController, setOpenProjectAbortController] =
    React.useState<AbortController | null>(null)
  const [assetListEvents, dispatchAssetListEvent] =
    eventHooks.useEvent<assetListEvent.AssetListEvent>()
  const [assetEvents, dispatchAssetEvent] = eventHooks.useEvent<assetEvent.AssetEvent>()
  const [assetPanelProps, setAssetPanelProps] =
    React.useState<assetPanel.AssetPanelRequiredProps | null>(null)
  const [isAssetPanelVisible, setIsAssetPanelVisible] = React.useState(
    () => localStorage.get('isAssetPanelVisible') ?? false
  )
  const [initialProjectName, setInitialProjectName] = React.useState(rawInitialProjectName)
  const isCloud = backend.type === backendModule.BackendType.remote
  const rootDirectory = React.useMemo(() => organization?.rootDirectory(), [organization])

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
      localStorage.get('backendType') === backendModule.BackendType.local
    ) {
      currentBackend = new LocalBackend(projectManagerUrl)
      setBackend(currentBackend)
    }
    const savedProjectStartupInfo = localStorage.get('projectStartupInfo')
    if (rawInitialProjectName != null) {
      if (page === pageSwitcher.Page.editor) {
        setPage(pageSwitcher.Page.drive)
      }
    } else if (savedProjectStartupInfo != null) {
      if (savedProjectStartupInfo.backendType === backendModule.BackendType.remote) {
        if (accessToken != null) {
          setPage(pageSwitcher.Page.drive)
          const httpClient = new HttpClient(
            new Headers([['Authorization', `Bearer ${accessToken}`]])
          )
          const remoteBackend = new RemoteBackend(httpClient, logger)
          void (async () => {
            const abortController = new AbortController()
            setOpenProjectAbortController(abortController)
            const projectAsset = savedProjectStartupInfo.projectAsset
            try {
              const oldProject = await remoteBackend.getProjectDetails(
                projectAsset.value.id,
                projectAsset.value.title
              )
              if (backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[oldProject.state.type]) {
                await remoteBackendModule.waitUntilProjectIsReady(
                  remoteBackend,
                  projectAsset.value.id,
                  projectAsset.value.title,
                  abortController
                )
                if (!abortController.signal.aborted) {
                  const project = await remoteBackend.getProjectDetails(
                    projectAsset.value.id,
                    projectAsset.value.title
                  )
                  setProjectStartupInfo(object.merge(savedProjectStartupInfo, { project }))
                  if (page === pageSwitcher.Page.editor) {
                    setPage(page)
                  }
                }
              }
            } catch {
              setProjectStartupInfo(null)
            }
          })()
        }
      } else {
        const projectId = savedProjectStartupInfo.projectAsset.value.id
        const projectTitle = savedProjectStartupInfo.projectAsset.value.title
        if (currentBackend.type === backendModule.BackendType.local) {
          setInitialProjectName(projectId)
        } else {
          const localBackend = new LocalBackend(projectManagerUrl)
          void (async () => {
            await localBackend.openProject(projectId, null, projectTitle)
            const project = await localBackend.getProjectDetails(projectId, projectTitle)
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
        localStorage.set('projectStartupInfo', projectStartupInfo)
      } else {
        localStorage.delete('projectStartupInfo')
      }
    }
    // `initialized` is NOT a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [projectStartupInfo, /* should never change */ localStorage])

  React.useEffect(() => {
    localStorage.set('isAssetPanelVisible', isAssetPanelVisible)
  }, [isAssetPanelVisible, /* should never change */ localStorage])

  React.useEffect(() => {
    if (page !== pageSwitcher.Page.settings) {
      localStorage.set('page', page)
    }
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
    return shortcutManager.registerKeyboardHandlers({
      [shortcutManagerModule.KeyboardAction.closeModal]: () => {
        updateModal(oldModal => {
          if (oldModal == null) {
            queueMicrotask(() => {
              setPage(oldPage => {
                if (oldPage !== pageSwitcher.Page.settings) {
                  return oldPage
                } else {
                  return localStorage.get('page') ?? pageSwitcher.Page.drive
                }
              })
            })
            return oldModal
          } else {
            return null
          }
        })
        if (modalRef.current == null) {
          // eslint-disable-next-line no-restricted-syntax
          return false
        }
      },
    })
  }, [
    shortcutManager,
    /* should never change */ modalRef,
    /* should never change */ localStorage,
    /* should never change */ updateModal,
  ])

  const setBackendType = React.useCallback(
    (newBackendType: backendModule.BackendType) => {
      if (newBackendType !== backend.type) {
        switch (newBackendType) {
          case backendModule.BackendType.local:
            setBackend(new LocalBackend(projectManagerUrl))
            break
          case backendModule.BackendType.remote: {
            const client = new HttpClient([['Authorization', `Bearer ${accessToken ?? ''}`]])
            setBackend(new RemoteBackend(client, logger))
            break
          }
        }
      }
    },
    [
      backend.type,
      accessToken,
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
      if (rootDirectory != null) {
        dispatchAssetListEvent({
          type: AssetListEventType.newProject,
          parentKey: rootDirectory.value.id,
          parent: rootDirectory,
          templateId,
          templateName,
          onSpinnerStateChange,
        })
      }
    },
    [rootDirectory, /* should never change */ dispatchAssetListEvent]
  )

  const doOpenEditor = React.useCallback(
    async (
      projectAsset: backendModule.SmartProject,
      setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
      switchPage: boolean
    ) => {
      if (switchPage) {
        setPage(pageSwitcher.Page.editor)
      }
      if (projectStartupInfo?.project.projectId !== projectAsset.value.id) {
        setProjectStartupInfo({
          project: await projectAsset.getDetails(),
          projectAsset,
          setProjectAsset,
          backendType: backend.type,
          accessToken,
        })
      }
    },
    [backend, projectStartupInfo?.project.projectId, accessToken]
  )

  const doCloseEditor = React.useCallback((closingProject: backendModule.ProjectAsset) => {
    setProjectStartupInfo(oldInfo =>
      oldInfo?.projectAsset.value.id === closingProject.id ? null : oldInfo
    )
  }, [])

  const doRemoveSelf = React.useCallback(() => {
    if (projectStartupInfo?.projectAsset != null) {
      dispatchAssetListEvent({
        type: AssetListEventType.removeSelf,
        id: projectStartupInfo.projectAsset.value.id,
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
            isCloud={isCloud}
            isEditorDisabled={projectStartupInfo == null}
            isHelpChatOpen={isHelpChatOpen}
            setIsHelpChatOpen={setIsHelpChatOpen}
            backendType={backend.type}
            setBackendType={setBackendType}
            query={query}
            setQuery={setQuery}
            labels={labels}
            suggestions={suggestions}
            canToggleAssetPanel={assetPanelProps != null}
            isAssetPanelVisible={isAssetPanelVisible && assetPanelProps != null}
            setIsAssetPanelVisible={setIsAssetPanelVisible}
            doRemoveSelf={doRemoveSelf}
            onSignOut={() => {
              if (page === pageSwitcher.Page.editor) {
                setPage(pageSwitcher.Page.drive)
              }
              setProjectStartupInfo(null)
            }}
          />
          <Home hidden={page !== pageSwitcher.Page.home} createProject={doCreateProject} />
          <Drive
            supportsLocalBackend={supportsLocalBackend}
            hidden={page !== pageSwitcher.Page.drive}
            backend={backend}
            initialProjectName={initialProjectName}
            query={query}
            setQuery={setQuery}
            labels={labels}
            setLabels={setLabels}
            setSuggestions={setSuggestions}
            projectStartupInfo={projectStartupInfo}
            assetListEvents={assetListEvents}
            dispatchAssetListEvent={dispatchAssetListEvent}
            assetEvents={assetEvents}
            dispatchAssetEvent={dispatchAssetEvent}
            setAssetPanelProps={setAssetPanelProps}
            doCreateProject={doCreateProject}
            doOpenEditor={doOpenEditor}
            doCloseEditor={doCloseEditor}
          />
          <Editor
            hidden={page !== pageSwitcher.Page.editor}
            supportsLocalBackend={supportsLocalBackend}
            projectStartupInfo={projectStartupInfo}
            appRunner={appRunner}
          />
          {page === pageSwitcher.Page.settings && <Settings />}
          {/* `accessToken` MUST be present in order for the `Chat` component to work. */}
          {accessToken != null ? (
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
            isAssetPanelVisible && assetPanelProps != null ? 'min-w-120' : 'min-w-0 invisible'
          }`}
        >
          {assetPanelProps && isAssetPanelVisible && (
            <AssetPanel
              key={assetPanelProps.item.item.value.id}
              {...assetPanelProps}
              isCloud={isCloud}
              supportsLocalBackend={supportsLocalBackend}
              page={page}
              setPage={setPage}
              category={Category.home}
              isHelpChatOpen={isHelpChatOpen}
              setIsHelpChatOpen={setIsHelpChatOpen}
              setVisibility={setIsAssetPanelVisible}
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
