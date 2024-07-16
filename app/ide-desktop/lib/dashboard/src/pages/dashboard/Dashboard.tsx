/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as validator from 'validator'
import * as z from 'zod'

import DriveIcon from 'enso-assets/drive.svg'
import EditorIcon from 'enso-assets/network.svg'
import SettingsIcon from 'enso-assets/settings.svg'
import * as detect from 'enso-common/src/detect'
import type * as types from 'enso-common/src/types'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'
import * as projectHooks from '#/hooks/projectHooks'
import * as searchParamsState from '#/hooks/searchParamsStateHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import ProjectsProvider, * as projectsProvider from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetTable from '#/layouts/AssetsTable'
import EventListProvider, * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import Drive from '#/layouts/Drive'
import Editor from '#/layouts/Editor'
import Settings from '#/layouts/Settings'
import * as tabBar from '#/layouts/TabBar'
import TabBar from '#/layouts/TabBar'
import UserBar from '#/layouts/UserBar'

import * as aria from '#/components/aria'
import Page from '#/components/Page'

import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'

import * as array from '#/utilities/array'
import LocalStorage from '#/utilities/LocalStorage'
import * as object from '#/utilities/object'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

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
  readonly appRunner: types.EditorRunner | null
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

  const initialLocalProjectId =
    initialProjectNameRaw != null && validator.isUUID(initialProjectNameRaw)
      ? localBackendModule.newProjectId(projectManager.UUID(initialProjectNameRaw))
      : null
  const initialProjectName = initialLocalProjectId ?? initialProjectNameRaw

  const [category, setCategory] = searchParamsState.useSearchParamsState(
    'driveCategory',
    () => {
      const shouldDefaultToCloud =
        initialLocalProjectId == null && (user.isEnabled || localBackend == null)
      return shouldDefaultToCloud ? Category.cloud : Category.local
    },
    (value): value is Category => {
      if (array.includes(Object.values(Category), value)) {
        return categoryModule.isLocal(value) ? localBackend != null : true
      } else {
        return false
      }
    }
  )

  const page = projectsProvider.usePage()
  const launchedProjects = projectsProvider.useLaunchedProjects()
  const selectedProject = launchedProjects.find(p => p.id === page) ?? null

  const setPage = projectsProvider.useSetPage()
  const openEditor = projectHooks.useOpenEditor()
  const openProject = projectHooks.useOpenProject()
  const closeProject = projectHooks.useCloseProject()
  const closeAllProjects = projectHooks.useCloseAllProjects()
  const clearLaunchedProjects = projectsProvider.useClearLaunchedProjects()
  const openProjectMutation = projectHooks.useOpenProjectMutation()
  const renameProjectMutation = projectHooks.useRenameProjectMutation()

  eventListProvider.useAssetEventListener(event => {
    switch (event.type) {
      case AssetEventType.openProject: {
        const { title, parentId, backendType, id, runInBackground } = event
        openProject(
          { title, parentId, type: backendType, id },
          { openInBackground: runInBackground }
        )
        break
      }
      case AssetEventType.closeProject: {
        const { title, parentId, backendType, id } = event
        closeProject({ title, parentId, type: backendType, id })
        break
      }
      default: {
        // Ignored. Any missing project-related events should be handled by `ProjectNameColumn`.
        // `delete`, `deleteForever`, `restore`, `download`, and `downloadSelected`
        // are handled by`AssetRow`.
        break
      }
    }
  })

  React.useEffect(() => {
    window.projectManagementApi?.setOpenProjectHandler(project => {
      setCategory(Category.local)
      dispatchAssetListEvent({
        type: AssetListEventType.openProject,
        backendType: backendModule.BackendType.local,
        id: localBackendModule.newProjectId(projectManager.UUID(project.id)),
        title: project.name,
        parentId: localBackendModule.newDirectoryId(backendModule.Path(project.parentDirectory)),
      })
    })
    return () => {
      window.projectManagementApi?.setOpenProjectHandler(() => {})
    }
  }, [dispatchAssetListEvent, setCategory])

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeModal: () => {
          updateModal(oldModal => {
            if (oldModal == null) {
              queueMicrotask(() => {
                setPage(localStorage.get('page') ?? projectsProvider.TabType.drive)
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

  const doRemoveSelf = eventCallbacks.useEventCallback((project: projectHooks.Project) => {
    dispatchAssetListEvent({ type: AssetListEventType.removeSelf, id: project.id })
    closeProject(project)
  })

  const onSignOut = eventCallbacks.useEventCallback(() => {
    setPage(projectsProvider.TabType.drive)
    closeAllProjects()
    clearLaunchedProjects()
  })

  const doOpenShareModal = eventCallbacks.useEventCallback(() => {
    if (assetManagementApiRef.current != null && selectedProject != null) {
      const asset = assetManagementApiRef.current.getAsset(selectedProject.id)
      const self =
        asset?.permissions?.find(
          backendModule.isUserPermissionAnd(permissions => permissions.user.userId === user.userId)
        ) ?? null

      if (asset != null && self != null) {
        setModal(
          <ManagePermissionsModal
            item={asset}
            setItem={updater => {
              const nextAsset = updater instanceof Function ? updater(asset) : updater
              assetManagementApiRef.current?.setAsset(asset.id, nextAsset)
            }}
            self={self}
            doRemoveSelf={() => {
              doRemoveSelf(selectedProject)
            }}
            eventTarget={null}
          />
        )
      }
    }
  })

  return (
    <Page hideInfoBar hideChat>
      <div
        className="flex text-xs text-primary"
        onContextMenu={event => {
          event.preventDefault()
          unsetModal()
        }}
      >
        <aria.Tabs
          className="relative flex h-screen grow select-none flex-col container-size"
          selectedKey={page}
          onSelectionChange={newPage => {
            const validated = projectsProvider.PAGES_SCHEMA.safeParse(newPage)
            if (validated.success) {
              setPage(validated.data)
            }
          }}
        >
          <div className="flex">
            <TabBar>
              <tabBar.Tab
                id={projectsProvider.TabType.drive}
                isActive={page === projectsProvider.TabType.drive}
                icon={DriveIcon}
                labelId="drivePageName"
              >
                {getText('drivePageName')}
              </tabBar.Tab>

              {launchedProjects.map(project => (
                <tabBar.Tab
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
                </tabBar.Tab>
              ))}

              <tabBar.Tab
                isActive
                id={projectsProvider.TabType.settings}
                isHidden={page !== projectsProvider.TabType.settings}
                icon={SettingsIcon}
                labelId="settingsPageName"
                onClose={() => {
                  setPage(projectsProvider.TabType.drive)
                }}
              >
                {getText('settingsPageName')}
              </tabBar.Tab>
            </TabBar>

            <UserBar
              onShareClick={selectedProject ? doOpenShareModal : undefined}
              setIsHelpChatOpen={setIsHelpChatOpen}
              goToSettingsPage={() => {
                setPage(projectsProvider.TabType.settings)
              }}
              onSignOut={onSignOut}
            />
          </div>
          <aria.TabPanel
            shouldForceMount
            id={projectsProvider.TabType.drive}
            className="flex grow [&[data-inert]]:hidden"
          >
            <Drive
              assetsManagementApiRef={assetManagementApiRef}
              category={category}
              setCategory={setCategory}
              hidden={page !== projectsProvider.TabType.drive}
              initialProjectName={initialProjectName}
            />
          </aria.TabPanel>
          {appRunner != null &&
            launchedProjects.map(project => (
              <aria.TabPanel
                shouldForceMount
                id={project.id}
                className="flex grow [&[data-inert]]:hidden"
              >
                <Editor
                  key={project.id}
                  hidden={page !== project.id}
                  ydocUrl={ydocUrl}
                  project={project}
                  projectId={project.id}
                  appRunner={appRunner}
                  isOpening={openProjectMutation.isPending}
                  isOpeningFailed={openProjectMutation.isError}
                  openingError={openProjectMutation.error}
                  startProject={openProjectMutation.mutate}
                  renameProject={async newName => {
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
          <aria.TabPanel id={projectsProvider.TabType.settings} className="flex grow">
            <Settings />
          </aria.TabPanel>
        </aria.Tabs>
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
    </Page>
  )
}
