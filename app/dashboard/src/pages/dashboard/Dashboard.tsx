/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import { useLocation } from 'react-router'
import * as validator from 'validator'
import * as z from 'zod'

import * as detect from 'enso-common/src/detect'

import DriveIcon from '#/assets/drive.svg'
import EditorIcon from '#/assets/network.svg'
import SettingsIcon from '#/assets/settings.svg'

import { isAppFullPath } from '#/appUtils'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as projectHooks from '#/hooks/projectHooks'
import { useNavigate } from '#/hooks/routerHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import {
  useClearLaunchedProjects,
  useLaunchedProjects,
  useProjectsStore,
  type LaunchedProject,
} from '#/providers/ProjectsProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetEventType from '#/events/AssetEventType'
import AssetListEventType from '#/events/AssetListEventType'

import type * as assetTable from '#/layouts/AssetsTable'
import * as eventListProvider from '#/layouts/AssetsTable/EventListProvider'
import { isDriveCategory, type DriveCategory } from '#/layouts/CategorySwitcher/Category'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import TabBar, * as tabBar from '#/layouts/TabBar'
import UserBar from '#/layouts/UserBar'
import Drive from '#/pages/dashboard/Drive'
import type * as editor from '#/pages/dashboard/Editor'
import Editor from '#/pages/dashboard/Editor'
import Settings from '#/pages/dashboard/Settings'

import * as aria from '#/components/aria'
import Page from '#/components/Page'

import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'

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
  readonly appRunner: editor.GraphEditorRunner | null
  readonly initialProjectName: string | null
  readonly ydocUrl: string | null
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
  const { appRunner, initialProjectName: initialProjectNameRaw, ydocUrl } = props
  const { user } = authProvider.useFullUserSession()
  const { getText } = textProvider.useText()
  const { modalRef } = modalProvider.useModalRef()
  const { updateModal, unsetModal, setModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)
  const navigate = useNavigate()
  const { pathname } = useLocation()
  const pathnameRef = useSyncRef(pathname)
  const tab =
    pathname.startsWith('/settings/') ? '/settings'
    : pathname.startsWith('/drive/') || pathname === '/' ? '/drive'
    : pathname
  const maybeCategory = pathname.startsWith('/drive/') ? pathname.replace(/^[/]drive[/]/, '') : null
  const category =
    isDriveCategory(maybeCategory) ? maybeCategory : localStorage.get('driveCategory') ?? 'cloud'
  const setCategory = useEventCallback((newCategory: DriveCategory) => {
    navigate(`/drive/${newCategory}`, { replace: true })
    localStorage.set('driveCategory', newCategory)
  })

  const dispatchAssetEvent = eventListProvider.useDispatchAssetEvent()
  const dispatchAssetListEvent = eventListProvider.useDispatchAssetListEvent()
  const assetManagementApiRef = React.useRef<assetTable.AssetManagementApi | null>(null)

  const initialLocalProjectId =
    initialProjectNameRaw != null && validator.isUUID(initialProjectNameRaw) ?
      localBackendModule.newProjectId(projectManager.UUID(initialProjectNameRaw))
    : null
  const initialProjectName = initialLocalProjectId ?? initialProjectNameRaw

  const projectsStore = useProjectsStore()
  const launchedProjects = useLaunchedProjects()
  const selectedProject = launchedProjects.find((p) => `/editor/${p.id}` === tab) ?? null

  const openEditor = projectHooks.useOpenEditor()
  const openProject = projectHooks.useOpenProject()
  const closeProject = projectHooks.useCloseProject()
  const closeAllProjects = projectHooks.useCloseAllProjects()
  const clearLaunchedProjects = useClearLaunchedProjects()
  const openProjectMutation = projectHooks.useOpenProjectMutation()
  const renameProjectMutation = projectHooks.useRenameProjectMutation()

  React.useEffect(() => {
    window.projectManagementApi?.setOpenProjectHandler((project) => {
      setCategory('local')
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
              const currentPathname = pathnameRef.current
              if (!currentPathname.startsWith('/editor')) {
                navigate('/drive')
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
    [inputBindings, modalRef, localStorage, updateModal, projectsStore, pathnameRef, navigate],
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

  const doRemoveSelf = useEventCallback((project: LaunchedProject) => {
    dispatchAssetListEvent({ type: AssetListEventType.removeSelf, id: project.id })
    closeProject(project)
  })

  const onSignOut = useEventCallback(() => {
    navigate('/drive')
    closeAllProjects()
    clearLaunchedProjects()
  })

  const doOpenShareModal = useEventCallback(() => {
    if (assetManagementApiRef.current != null && selectedProject != null) {
      const asset = assetManagementApiRef.current.getAsset(selectedProject.id)
      const self =
        asset?.permissions?.find(
          backendModule.isUserPermissionAnd(
            (permissions) => permissions.user.userId === user.userId,
          ),
        ) ?? null

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
          selectedKey={tab}
          onSelectionChange={(newPage) => {
            if (typeof newPage === 'string' && isAppFullPath(newPage)) {
              navigate(newPage)
            }
          }}
        >
          <div className="flex">
            <TabBar>
              <tabBar.Tab path="/drive" icon={DriveIcon} labelId="drivePageName">
                {getText('drivePageName')}
              </tabBar.Tab>

              {launchedProjects.map((project) => (
                <tabBar.Tab
                  data-testid="editor-tab-button"
                  path={`/editor/${project.id}`}
                  project={project}
                  key={project.id}
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
                path="/settings"
                isHidden={tab !== '/settings'}
                icon={SettingsIcon}
                labelId="settingsPageName"
                onClose={() => {
                  navigate('/drive')
                }}
              >
                {getText('settingsPageName')}
              </tabBar.Tab>
            </TabBar>

            <UserBar
              onShareClick={selectedProject ? doOpenShareModal : undefined}
              setIsHelpChatOpen={setIsHelpChatOpen}
              goToSettingsPage={() => {
                navigate('/settings/account')
              }}
              onSignOut={onSignOut}
            />
          </div>
          <aria.TabPanel
            shouldForceMount
            id="/drive"
            className="flex min-h-0 grow [&[data-inert]]:hidden"
          >
            <Drive
              assetsManagementApiRef={assetManagementApiRef}
              category={category}
              setCategory={setCategory}
              hidden={tab !== '/drive'}
              initialProjectName={initialProjectName}
            />
          </aria.TabPanel>
          {appRunner != null &&
            launchedProjects.map((project) => (
              <aria.TabPanel
                shouldForceMount
                id={`/editor/${project.id}`}
                className="flex min-h-0 grow [&[data-inert]]:hidden"
              >
                <Editor
                  key={project.id}
                  hidden={tab !== `/editor/${project.id}`}
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
          <aria.TabPanel id="/settings" className="flex min-h-0 grow">
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
