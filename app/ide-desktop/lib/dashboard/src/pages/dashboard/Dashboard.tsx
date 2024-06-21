/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import DriveIcon from 'enso-assets/drive.svg'
import WorkspaceIcon from 'enso-assets/workspace.svg'
import * as detect from 'enso-common/src/detect'

import * as eventHooks from '#/hooks/eventHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import Drive from '#/layouts/Drive'
import Editor from '#/layouts/Editor'
import * as tabBar from '#/layouts/TabBar'
import TabBar from '#/layouts/TabBar'
import UserBar from '#/layouts/UserBar'

import Page from '#/components/Page'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'
import type * as projectManager from '#/services/ProjectManager'

import * as array from '#/utilities/array'
import LocalStorage from '#/utilities/LocalStorage'
import * as object from '#/utilities/object'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

import type * as types from '../../../../types/types'

// ============================
// === Global configuration ===
// ============================

/** Main content of the screen. Only one should be visible at a time. */
enum TabType {
  drive = 'drive',
  editor = 'editor',
}

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
    readonly page: TabType
    readonly projectStartupInfo: backendModule.ProjectStartupInfo
  }
}

LocalStorage.registerKey('isAssetPanelVisible', {
  tryParse: value => (value === true ? value : null),
})

const PAGES = Object.values(TabType)
LocalStorage.registerKey('page', {
  tryParse: value => (array.includes(PAGES, value) ? value : null),
})

const BACKEND_TYPES = Object.values(backendModule.BackendType)
LocalStorage.registerKey('projectStartupInfo', {
  isUserSpecific: true,
  tryParse: value => {
    if (typeof value !== 'object' || value == null) {
      return null
    } else if (
      !('accessToken' in value) ||
      (typeof value.accessToken !== 'string' && value.accessToken != null)
    ) {
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
        projectAsset: value.projectAsset as backendModule.ProjectAsset,
        backendType: value.backendType,
        accessToken: value.accessToken ?? null,
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
  readonly appRunner: types.EditorRunner | null
  readonly initialProjectName: string | null
  readonly projectManagerUrl: string | null
  readonly ydocUrl: string | null
  readonly projectManagerRootDirectory: projectManager.Path | null
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
  const { appRunner, initialProjectName } = props
  const { ydocUrl, projectManagerUrl, projectManagerRootDirectory } = props
  const session = authProvider.useNonPartialUserSession()
  const remoteBackend = backendProvider.useRemoteBackend()
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()
  const { modalRef } = modalProvider.useModalRef()
  const { updateModal, unsetModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [initialized, setInitialized] = React.useState(false)
  const initializedRef = React.useRef(initialized)
  initializedRef.current = initialized
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)

  // These pages MUST be ROUTER PAGES.
  const [page, setPage] = searchParamsState.useSearchParamsState(
    'page',
    () => localStorage.get('page') ?? TabType.drive,
    (value: unknown): value is TabType => array.includes(Object.values(TabType), value)
  )
  const [projectStartupInfo, setProjectStartupInfo] =
    React.useState<backendModule.ProjectStartupInfo | null>(null)
  const [openProjectAbortController, setOpenProjectAbortController] =
    React.useState<AbortController | null>(null)
  const [assetListEvents, dispatchAssetListEvent] =
    eventHooks.useEvent<assetListEvent.AssetListEvent>()
  const [assetEvents, dispatchAssetEvent] = eventHooks.useEvent<assetEvent.AssetEvent>()
  const defaultCategory = remoteBackend != null ? Category.cloud : Category.local
  const [category, setCategory] = searchParamsState.useSearchParamsState(
    'driveCategory',
    () => defaultCategory,
    (value): value is Category => {
      if (array.includes(Object.values(Category), value)) {
        return categoryModule.isLocal(value) ? localBackend != null : true
      } else {
        return false
      }
    }
  )

  const isCloud = categoryModule.isCloud(category)
  const isUserEnabled = session.user.isEnabled

  if (isCloud && !isUserEnabled && localBackend != null) {
    setTimeout(() => {
      // This sets `BrowserRouter`, so it must not be set synchronously.
      setCategory(Category.local)
    })
  }

  React.useEffect(() => {
    setInitialized(true)
  }, [])

  React.useEffect(() => {
    const savedProjectStartupInfo = localStorage.get('projectStartupInfo')
    if (initialProjectName != null) {
      if (page === TabType.editor) {
        setPage(TabType.drive)
      }
    } else if (savedProjectStartupInfo != null) {
      if (savedProjectStartupInfo.backendType === backendModule.BackendType.remote) {
        if (remoteBackend != null) {
          setPage(TabType.drive)
          void (async () => {
            const abortController = new AbortController()
            setOpenProjectAbortController(abortController)
            try {
              const oldProject = await remoteBackend.getProjectDetails(
                savedProjectStartupInfo.projectAsset.id,
                savedProjectStartupInfo.projectAsset.parentId,
                savedProjectStartupInfo.projectAsset.title
              )
              if (backendModule.IS_OPENING_OR_OPENED[oldProject.state.type]) {
                const project = await remoteBackend.waitUntilProjectIsReady(
                  savedProjectStartupInfo.projectAsset.id,
                  savedProjectStartupInfo.projectAsset.parentId,
                  savedProjectStartupInfo.projectAsset.title,
                  abortController
                )
                if (!abortController.signal.aborted) {
                  setProjectStartupInfo(object.merge(savedProjectStartupInfo, { project }))
                  if (page === TabType.editor) {
                    setPage(page)
                  }
                }
              }
            } catch {
              setProjectStartupInfo(null)
            }
          })()
        }
      } else if (projectManagerUrl != null && projectManagerRootDirectory != null) {
        if (localBackend != null) {
          void (async () => {
            await localBackend.openProject(
              savedProjectStartupInfo.projectAsset.id,
              {
                executeAsync: false,
                cognitoCredentials: null,
                parentId: savedProjectStartupInfo.projectAsset.parentId,
              },
              savedProjectStartupInfo.projectAsset.title
            )
            const project = await localBackend.getProjectDetails(
              savedProjectStartupInfo.projectAsset.id,
              savedProjectStartupInfo.projectAsset.parentId,
              savedProjectStartupInfo.projectAsset.title
            )
            setProjectStartupInfo(object.merge(savedProjectStartupInfo, { project }))
            if (page === TabType.editor) {
              setPage(page)
            }
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
    if (initializedRef.current) {
      if (projectStartupInfo != null) {
        localStorage.set('projectStartupInfo', projectStartupInfo)
      } else {
        localStorage.delete('projectStartupInfo')
      }
    }
  }, [projectStartupInfo, localStorage])

  React.useEffect(() => {
    localStorage.set('page', page)
  }, [page, localStorage])

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeModal: () => {
          updateModal(oldModal => {
            if (oldModal == null) {
              queueMicrotask(() => {
                setPage(localStorage.get('page') ?? TabType.drive)
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
      }),
    [inputBindings, modalRef, localStorage, updateModal, setPage]
  )

  React.useEffect(() => {
    if (detect.isOnElectron()) {
      // We want to handle the back and forward buttons in electron the same way as in the browser.
      // eslint-disable-next-line no-restricted-syntax
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        goBack: () => {
          window.navigationApi.goBack()
        },
        goForward: () => {
          window.navigationApi.goForward()
        },
      })
    }
  }, [inputBindings])

  const doOpenEditor = React.useCallback(
    async (
      backend: Backend,
      newProject: backendModule.ProjectAsset,
      setProjectAsset: React.Dispatch<React.SetStateAction<backendModule.ProjectAsset>>,
      switchPage: boolean
    ) => {
      if (switchPage) {
        setPage(TabType.editor)
      }
      if (projectStartupInfo?.project.projectId !== newProject.id) {
        setProjectStartupInfo({
          project: await backend.getProjectDetails(
            newProject.id,
            newProject.parentId,
            newProject.title
          ),
          projectAsset: newProject,
          setProjectAsset: setProjectAsset,
          backendType: backend.type,
          accessToken: session.accessToken,
        })
      }
    },
    [projectStartupInfo?.project.projectId, session.accessToken, setPage]
  )

  const doCloseEditor = React.useCallback((closingProject: backendModule.ProjectAsset) => {
    setProjectStartupInfo(oldInfo =>
      oldInfo?.projectAsset.id === closingProject.id ? null : oldInfo
    )
  }, [])

  const doRemoveSelf = React.useCallback(() => {
    if (projectStartupInfo?.projectAsset != null) {
      const id = projectStartupInfo.projectAsset.id
      dispatchAssetListEvent({ type: AssetListEventType.removeSelf, id })
      setProjectStartupInfo(null)
    }
  }, [projectStartupInfo?.projectAsset, dispatchAssetListEvent])

  const onSignOut = React.useCallback(() => {
    if (page === TabType.editor) {
      setPage(TabType.drive)
    }
    setProjectStartupInfo(null)
  }, [page, setPage])

  return (
    <Page hideInfoBar hideChat>
      <div className="flex text-xs text-primary">
        <div
          className="relative flex h-screen grow select-none flex-col container-size"
          onContextMenu={event => {
            event.preventDefault()
            unsetModal()
          }}
        >
          <div className="flex">
            <TabBar>
              <tabBar.Tab
                isActive={page === TabType.drive}
                icon={DriveIcon}
                onPress={() => {
                  setPage(TabType.drive)
                }}
              >
                {getText('drivePageName')}
              </tabBar.Tab>
              {projectStartupInfo != null && (
                <tabBar.Tab
                  isActive={page === TabType.editor}
                  icon={WorkspaceIcon}
                  onPress={() => {
                    setPage(TabType.editor)
                  }}
                  onClose={() => {
                    setProjectStartupInfo(null)
                    setPage(TabType.drive)
                  }}
                >
                  {projectStartupInfo.projectAsset.title}
                </tabBar.Tab>
              )}
            </TabBar>
            <UserBar
              backend={remoteBackend}
              isOnEditorPage={page === TabType.editor}
              setIsHelpChatOpen={setIsHelpChatOpen}
              projectAsset={projectStartupInfo?.projectAsset ?? null}
              setProjectAsset={projectStartupInfo?.setProjectAsset ?? null}
              doRemoveSelf={doRemoveSelf}
              onSignOut={onSignOut}
            />
          </div>
          <Drive
            category={category}
            setCategory={setCategory}
            hidden={page !== TabType.drive}
            initialProjectName={initialProjectName}
            projectStartupInfo={projectStartupInfo}
            setProjectStartupInfo={setProjectStartupInfo}
            assetListEvents={assetListEvents}
            dispatchAssetListEvent={dispatchAssetListEvent}
            assetEvents={assetEvents}
            dispatchAssetEvent={dispatchAssetEvent}
            doOpenEditor={doOpenEditor}
            doCloseEditor={doCloseEditor}
          />
          <Editor
            hidden={page !== TabType.editor}
            ydocUrl={ydocUrl}
            projectStartupInfo={projectStartupInfo}
            appRunner={appRunner}
          />
          {process.env.ENSO_CLOUD_CHAT_URL != null ? (
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
      </div>
    </Page>
  )
}
