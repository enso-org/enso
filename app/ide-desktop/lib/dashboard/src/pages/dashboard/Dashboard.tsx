/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as eventHooks from '#/hooks/eventHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as loggerProvider from '#/providers/LoggerProvider'
import * as modalProvider from '#/providers/ModalProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetPanel from '#/layouts/AssetPanel'
import AssetPanel from '#/layouts/AssetPanel'
import type * as assetSearchBar from '#/layouts/AssetSearchBar'
import Category from '#/layouts/CategorySwitcher/Category'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import Drive from '#/layouts/Drive'
import Editor from '#/layouts/Editor'
import Home from '#/layouts/Home'
import * as pageSwitcher from '#/layouts/PageSwitcher'
import Settings from '#/layouts/Settings'
import TopBar from '#/layouts/TopBar'

import TheModal from '#/components/dashboard/TheModal'
import type * as spinner from '#/components/Spinner'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import RemoteBackend from '#/services/RemoteBackend'

import * as array from '#/utilities/array'
import AssetQuery from '#/utilities/AssetQuery'
import HttpClient from '#/utilities/HttpClient'
import LocalStorage from '#/utilities/LocalStorage'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
    readonly page: pageSwitcher.Page
    readonly project: backendModule.SavedProjectStartupInfo
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
LocalStorage.registerKey('project', {
  isUserSpecific: true,
  tryParse: value => {
    if (typeof value !== 'object' || value == null) {
      return null
    } else if (!('backendType' in value) || !array.includes(BACKEND_TYPES, value.backendType)) {
      return null
    } else if (!('parentId' in value) || typeof value.parentId !== 'string') {
      return null
    } else if (!('id' in value) || typeof value.id !== 'string') {
      return null
    } else if (!('title' in value) || typeof value.title !== 'string') {
      return null
    } else if (
      !('accessToken' in value) ||
      (typeof value.accessToken !== 'string' && value.accessToken != null)
    ) {
      return null
    } else {
      const { backendType, parentId, id, title, accessToken = null } = value
      return {
        backendType,
        parentId: backendModule.DirectoryId(parentId),
        id: backendModule.ProjectId(id),
        title,
        accessToken,
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
  readonly supportsLocalBackend: boolean
  readonly backend: Backend
  readonly setBackend: (backend: Backend) => void
  readonly appRunner: AppRunner
  readonly initialProjectName: string | null
  readonly projectManagerUrl: string | null
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
  const { supportsLocalBackend, backend, setBackend, appRunner } = props
  const { initialProjectName: rawInitialProjectName, projectManagerUrl } = props
  const logger = loggerProvider.useLogger()
  const { user, accessToken } = authProvider.useNonPartialUserSession()
  const { modalRef } = modalProvider.useModalRef()
  const { unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const inputBindings = inputBindingsProvider.useInputBindings()
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
  const [isAssetPanelEnabled, setIsAssetPanelEnabled] = React.useState(
    () => localStorage.get('isAssetPanelVisible') ?? false
  )
  const [isAssetPanelTemporarilyVisible, setIsAssetPanelTemporarilyVisible] = React.useState(false)
  const [initialProjectName, setInitialProjectName] = React.useState(rawInitialProjectName)
  const isCloud = backend.type === backendModule.BackendType.remote
  const self = asyncEffectHooks.useAsyncEffect(user, () => backend.self(), [backend])
  const rootDirectory = React.useMemo(() => self?.rootDirectory() ?? null, [self])
  const isAssetPanelVisible =
    page === pageSwitcher.Page.drive && (isAssetPanelEnabled || isAssetPanelTemporarilyVisible)

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
    const lastOpenedProject = localStorage.get('project')
    if (rawInitialProjectName != null) {
      if (page === pageSwitcher.Page.editor) {
        setPage(pageSwitcher.Page.drive)
      }
    } else if (lastOpenedProject != null) {
      const { parentId, id, title, backendType } = lastOpenedProject
      if (backendType === backendModule.BackendType.remote) {
        if (accessToken != null) {
          setPage(pageSwitcher.Page.drive)
          const httpClient = new HttpClient(
            new Headers([['Authorization', `Bearer ${accessToken}`]])
          )
          const remoteBackend = new RemoteBackend(httpClient, logger)
          void (async () => {
            const abortController = new AbortController()
            setOpenProjectAbortController(abortController)
            try {
              const projectAsset = await remoteBackend.getProject(parentId, id, title)
              const projectState = projectAsset.value.projectState.type
              if (backendModule.IS_OPENING_OR_OPENED[projectState]) {
                const details = await projectAsset.waitUntilReady()
                if (!abortController.signal.aborted) {
                  setProjectStartupInfo({ details, projectAsset, backendType, accessToken })
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
        if (currentBackend.type === backendModule.BackendType.local) {
          setInitialProjectName(id)
        } else {
          const localBackend = new LocalBackend(projectManagerUrl)
          void (async () => {
            await localBackend.openProject(id, null, title)
            const projectAsset = await localBackend.getProject(id, title)
            const details = await projectAsset.waitUntilReady()
            setProjectStartupInfo({ details, projectAsset, backendType, accessToken: null })
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
        localStorage.set('project', backendModule.serializeProjectStartupInfo(projectStartupInfo))
      } else {
        localStorage.delete('project')
      }
    }
    // `initialized` is NOT a dependency.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [projectStartupInfo, /* should never change */ localStorage])

  React.useEffect(() => {
    localStorage.set('isAssetPanelVisible', isAssetPanelEnabled)
  }, [isAssetPanelEnabled, /* should never change */ localStorage])

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

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeModal: () => {
          if (modalRef.current == null) {
            setPage(oldPage =>
              oldPage !== pageSwitcher.Page.settings
                ? oldPage
                : localStorage.get('page') ?? pageSwitcher.Page.drive
            )
            return false
          } else {
            unsetModal()
            return
          }
        },
      }),
    [
      inputBindings,
      /* should never change */ modalRef,
      /* should never change */ localStorage,
      /* should never change */ unsetModal,
    ]
  )

  const setBackendType = React.useCallback(
    (newBackendType: backendModule.BackendType) => {
      if (newBackendType !== backend.type) {
        switch (newBackendType) {
          case backendModule.BackendType.local: {
            setBackend(new LocalBackend(projectManagerUrl))
            break
          }
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
      if (projectStartupInfo?.details.projectId !== projectAsset.value.id) {
        setProjectStartupInfo({
          details: await projectAsset.getDetails(),
          projectAsset,
          setProjectAsset,
          backendType: backend.type,
          accessToken,
        })
      }
    },
    [backend, projectStartupInfo?.details.projectId, accessToken]
  )

  const doCloseEditor = React.useCallback((closingProject: backendModule.ProjectAsset) => {
    setProjectStartupInfo(oldInfo =>
      oldInfo?.projectAsset.value.id === closingProject.id ? null : oldInfo
    )
  }, [])

  const doRemoveSelf = React.useCallback(() => {
    if (projectStartupInfo?.projectAsset != null) {
      const id = projectStartupInfo.projectAsset.value.id
      dispatchAssetListEvent({ type: AssetListEventType.removeSelf, id })
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
        className={`flex text-xs text-primary ${
          page === pageSwitcher.Page.editor ? 'pointer-events-none cursor-none' : ''
        }`}
      >
        <div
          className={`relative flex h-screen grow select-none flex-col overflow-hidden container-size ${
            page === pageSwitcher.Page.home ? 'pb-home-page-b' : 'gap-top-level'
          }`}
          onContextMenu={event => {
            event.preventDefault()
            unsetModal()
          }}
        >
          <TopBar
            supportsLocalBackend={supportsLocalBackend}
            isCloud={isCloud}
            projectAsset={projectStartupInfo?.projectAsset ?? null}
            setProjectAsset={projectStartupInfo?.setProjectAsset ?? null}
            page={page}
            setPage={setPage}
            isEditorDisabled={projectStartupInfo == null}
            isHelpChatOpen={isHelpChatOpen}
            setIsHelpChatOpen={setIsHelpChatOpen}
            backendType={backend.type}
            setBackendType={setBackendType}
            query={query}
            setQuery={setQuery}
            labels={labels}
            suggestions={suggestions}
            isAssetPanelVisible={isAssetPanelVisible}
            isAssetPanelEnabled={isAssetPanelEnabled}
            setIsAssetPanelEnabled={setIsAssetPanelEnabled}
            doRemoveSelf={doRemoveSelf}
            onSignOut={onSignOut}
          />
          <Home hidden={page !== pageSwitcher.Page.home} createProject={doCreateProject} />
          <Drive
            supportsLocalBackend={supportsLocalBackend}
            hidden={page !== pageSwitcher.Page.drive}
            hideRows={page !== pageSwitcher.Page.drive && page !== pageSwitcher.Page.home}
            backend={backend}
            rootDirectory={rootDirectory}
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
            setIsAssetPanelTemporarilyVisible={setIsAssetPanelTemporarilyVisible}
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
          {accessToken != null && process.env.ENSO_CLOUD_CHAT_URL != null ? (
            <Chat
              isOpen={isHelpChatOpen}
              doClose={() => {
                setIsHelpChatOpen(false)
              }}
              endpoint={process.env.ENSO_CLOUD_CHAT_URL}
            />
          ) : (
            <ChatPlaceholder
              isOpen={isHelpChatOpen}
              doClose={() => {
                setIsHelpChatOpen(false)
              }}
            />
          )}
        </div>
        <div
          className={`flex flex-col overflow-hidden transition-min-width duration-side-panel ease-in-out ${
            isAssetPanelVisible ? 'min-w-side-panel' : 'invisible min-w'
          }`}
        >
          {isAssetPanelVisible && (
            <AssetPanel
              key={assetPanelProps?.item?.item.value.id}
              item={assetPanelProps?.item ?? null}
              setItem={assetPanelProps?.setItem ?? null}
              setQuery={setQuery}
              category={Category.home}
              labels={labels}
              dispatchAssetEvent={dispatchAssetEvent}
            />
          )}
        </div>
      </div>
      <div className="select-none text-xs text-primary">
        <TheModal />
      </div>
    </>
  )
}
