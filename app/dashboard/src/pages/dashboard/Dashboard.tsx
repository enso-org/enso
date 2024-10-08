/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as z from 'zod'

import * as detect from 'enso-common/src/detect'

import DriveIcon from '#/assets/drive.svg'
import EditorIcon from '#/assets/network.svg'
import SettingsIcon from '#/assets/settings.svg'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import ProjectsProvider, {
  PAGES_SCHEMA,
  useClearLaunchedProjects,
  useLaunchedProjects,
  usePage,
  useProjectsStore,
  useSetPage,
  type LaunchedProject,
} from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetTable from '#/layouts/AssetsTable'
import EventListProvider, * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import * as categoryModule from '#/layouts/CategorySwitcher/Category'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import Drive from '#/layouts/Drive'
import type * as editor from '#/layouts/Editor'
import Editor from '#/layouts/Editor'
import Settings from '#/layouts/Settings'
import { TabBar } from '#/layouts/TabBar'
import UserBar from '#/layouts/UserBar'

import * as aria from '#/components/aria'
import Page from '#/components/Page'

import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'

import { baseName } from '#/utilities/fileInfo'
import LocalStorage from '#/utilities/LocalStorage'
import * as object from '#/utilities/object'
import { tryFindSelfPermission } from '#/utilities/permissions'
import { STATIC_QUERY_OPTIONS } from '#/utilities/reactQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { usePrefetchQuery } from '@tanstack/react-query'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
  }
}

LocalStorage.registerKey('isAssetPanelVisible', { schema: z.boolean() })

// =================
// === Dashboard ===
// =================

/** Props for {@link Dashboard}s that are common to all platforms. */
export interface DashboardProps {
  /** Whether the application may have the local backend running. */
  readonly supportsLocalBackend: boolean
  readonly appRunner: editor.GraphEditorRunner | null
  readonly initialProjectName: string | null
  readonly ydocUrl: string | null
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
  return (
    <EventListProvider>
      <ProjectsProvider>
        <DashboardInner {...props} />
      </ProjectsProvider>
    </EventListProvider>
  )
}

/**
 * Extract proper path from `file://` URL.
 */
function fileURLToPath(url: string): string | null {
  if (URL.canParse(url)) {
    const parsed = new URL(url)
    if (parsed.protocol === 'file:') {
      return decodeURIComponent(
        detect.platform() === detect.Platform.windows ?
          // On Windows, we must remove leading `/` from URL.
          parsed.pathname.slice(1)
        : parsed.pathname,
      )
    } else {
      return null
    }
  } else {
    return null
  }
}

/** The component that contains the entire UI. */
function DashboardInner(props: DashboardProps) {
  const { appRunner, initialProjectName: initialProjectNameRaw, ydocUrl } = props
  const { user } = authProvider.useFullUserSession()
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()
  const { modalRef } = modalProvider.useModalRef()
  const { updateModal, unsetModal, setModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)

  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const assetManagementApiRef = React.useRef<assetTable.AssetManagementApi | null>(null)

  const initialLocalProjectPath =
    initialProjectNameRaw != null ? fileURLToPath(initialProjectNameRaw) : null
  const initialProjectName = initialLocalProjectPath != null ? null : initialProjectNameRaw

  const [category, setCategory] = searchParamsState.useSearchParamsState<categoryModule.Category>(
    'driveCategory',
    () => (localBackend != null ? { type: 'local' } : { type: 'cloud' }),
    (value): value is categoryModule.Category =>
      categoryModule.CATEGORY_SCHEMA.safeParse(value).success,
  )

  const projectsStore = useProjectsStore()
  const page = usePage()
  const launchedProjects = useLaunchedProjects()
  const selectedProject = launchedProjects.find((p) => p.id === page) ?? null

  const setPage = useSetPage()
  const openEditor = projectHooks.useOpenEditor()
  const openProject = projectHooks.useOpenProject()
  const closeProject = projectHooks.useCloseProject()
  const closeAllProjects = projectHooks.useCloseAllProjects()
  const clearLaunchedProjects = useClearLaunchedProjects()
  const openProjectMutation = projectHooks.useOpenProjectMutation()
  const renameProjectMutation = projectHooks.useRenameProjectMutation()

  usePrefetchQuery({
    queryKey: ['loadInitialLocalProject'],
    networkMode: 'always',
    ...STATIC_QUERY_OPTIONS,
    queryFn: async () => {
      if (initialLocalProjectPath != null && window.backendApi && localBackend) {
        const projectName = baseName(initialLocalProjectPath)
        const { id } = await window.backendApi.importProjectFromPath(
          initialLocalProjectPath,
          localBackend.rootPath(),
          projectName,
        )
        openProject({
          type: backendModule.BackendType.local,
          id: localBackendModule.newProjectId(projectManager.UUID(id)),
          title: projectName,
          parentId: localBackendModule.newDirectoryId(localBackend.rootPath()),
        })
      }
      return null
    },
    staleTime: Infinity,
  })

  React.useEffect(() => {
    window.projectManagementApi?.setOpenProjectHandler((project) => {
      setCategory({ type: 'local' })
      const projectId = localBackendModule.newProjectId(projectManager.UUID(project.id))
      openProject({
        type: backendModule.BackendType.local,
        id: projectId,
        title: project.name,
        parentId: localBackendModule.newDirectoryId(backendModule.Path(project.parentDirectory)),
      })
    })
    return () => {
      window.projectManagementApi?.setOpenProjectHandler(() => {})
    }
  }, [dispatchAssetListEvent, openEditor, openProject, setCategory])

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeModal: () => {
          updateModal((oldModal) => {
            if (oldModal == null) {
              const currentPage = projectsStore.getState().page
              if (currentPage === 'settings') {
                setPage('drive')
              }
            }
            return null
          })
          if (modalRef.current == null) {
            // eslint-disable-next-line no-restricted-syntax
            return false
          }
        },
      }),
    [inputBindings, modalRef, localStorage, updateModal, setPage, projectsStore],
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

  const doRemoveSelf = eventCallbacks.useEventCallback((project: LaunchedProject) => {
    dispatchAssetListEvent({ type: AssetListEventType.removeSelf, id: project.id })
    closeProject(project)
  })

  const onSignOut = eventCallbacks.useEventCallback(() => {
    setPage('drive')
    closeAllProjects()
    clearLaunchedProjects()
  })

  const doOpenShareModal = eventCallbacks.useEventCallback(() => {
    if (assetManagementApiRef.current != null && selectedProject != null) {
      const asset = assetManagementApiRef.current.getAsset(selectedProject.id)
      const self = tryFindSelfPermission(user, asset?.permissions)

      if (asset != null && self != null) {
        setModal(
          <ManagePermissionsModal
            item={asset}
            setItem={(updater) => {
              const nextAsset = updater instanceof Function ? updater(asset) : updater
              assetManagementApiRef.current?.setAsset(asset.id, nextAsset)
            }}
            self={self}
            doRemoveSelf={() => {
              doRemoveSelf(selectedProject)
            }}
            eventTarget={null}
          />,
        )
      }
    }
  })

  return (
    <Page hideInfoBar hideChat>
      <div
        className="flex min-h-full flex-col text-xs text-primary"
        onContextMenu={(event) => {
          event.preventDefault()
          unsetModal()
        }}
      >
        <aria.Tabs
          className="relative flex min-h-full grow select-none flex-col container-size"
          selectedKey={page}
          onSelectionChange={(newPage) => {
            const validated = PAGES_SCHEMA.safeParse(newPage)
            if (validated.success) {
              setPage(validated.data)
            }
          }}
        >
          <div className="flex">
            <TabBar>
              <TabBar.Tab
                id="drive"
                isActive={page === 'drive'}
                icon={DriveIcon}
                labelId="drivePageName"
              >
                {getText('drivePageName')}
              </TabBar.Tab>

              {launchedProjects.map((project) => (
                <TabBar.Tab
                  data-testid="editor-tab-button"
                  id={project.id}
                  project={project}
                  key={project.id}
                  isActive={page === project.id}
                  icon={EditorIcon}
                  labelId="editorPageName"
                  onClose={() => {
                    closeProject(project)
                  }}
                  onLoadEnd={() => {
                    openEditor(project.id)
                  }}
                >
                  {project.title}
                </TabBar.Tab>
              ))}

              <TabBar.Tab
                isActive
                id="settings"
                isHidden={page !== 'settings'}
                icon={SettingsIcon}
                labelId="settingsPageName"
                onClose={() => {
                  setPage('drive')
                }}
              >
                {getText('settingsPageName')}
              </TabBar.Tab>
            </TabBar>

            <UserBar
              onShareClick={selectedProject ? doOpenShareModal : undefined}
              setIsHelpChatOpen={setIsHelpChatOpen}
              goToSettingsPage={() => {
                setPage('settings')
              }}
              onSignOut={onSignOut}
            />
          </div>
          <aria.TabPanel
            shouldForceMount
            id="drive"
            className="flex min-h-0 grow [&[data-inert]]:hidden"
          >
            <Drive
              assetsManagementApiRef={assetManagementApiRef}
              category={category}
              setCategory={setCategory}
              hidden={page !== 'drive'}
              initialProjectName={initialProjectName}
            />
          </aria.TabPanel>
          {appRunner != null &&
            launchedProjects.map((project) => (
              <aria.TabPanel
                key={project.id}
                shouldForceMount
                id={project.id}
                className="flex min-h-0 grow [&[data-inert]]:hidden"
              >
                <Editor
                  hidden={page !== project.id}
                  ydocUrl={ydocUrl}
                  project={project}
                  projectId={project.id}
                  appRunner={appRunner}
                  isOpening={openProjectMutation.isPending}
                  isOpeningFailed={openProjectMutation.isError}
                  openingError={openProjectMutation.error}
                  startProject={openProjectMutation.mutate}
                  renameProject={async (newName) => {
                    try {
                      await renameProjectMutation.mutateAsync({ newName, project })
                      dispatchAssetEvent({
                        type: AssetEventType.setItem,
                        id: project.id,
                        valueOrUpdater: object.merger({ title: newName }),
                      })
                    } catch {
                      dispatchAssetEvent({
                        type: AssetEventType.setItem,
                        id: project.id,
                        valueOrUpdater: object.merger({ title: project.title }),
                      })
                    }
                  }}
                />
              </aria.TabPanel>
            ))}
          <aria.TabPanel id="settings" className="flex min-h-0 grow">
            <Settings />
          </aria.TabPanel>
        </aria.Tabs>
        {process.env.ENSO_CLOUD_CHAT_URL != null ?
          <Chat
            isOpen={isHelpChatOpen}
            doClose={() => {
              setIsHelpChatOpen(false)
            }}
            endpoint={process.env.ENSO_CLOUD_CHAT_URL}
          />
        : <ChatPlaceholder
            isOpen={isHelpChatOpen}
            doClose={() => {
              setIsHelpChatOpen(false)
            }}
          />
        }
      </div>
    </Page>
  )
}
