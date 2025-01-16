/**
 * @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components.
 */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import { DashboardTabBar } from './DashboardTabBar'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'
import * as projectHooks from '#/hooks/projectHooks'
import { CategoriesProvider } from '#/layouts/Drive/Categories/categoriesHooks'
import DriveProvider from '#/providers/DriveProvider'

import * as backendProvider from '#/providers/BackendProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import ProjectsProvider, {
  TabType,
  useClearLaunchedProjects,
  usePage,
  useProjectsStore,
  useSetPage,
} from '#/providers/ProjectsProvider'

import type * as assetTable from '#/layouts/AssetsTable'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import UserBar from '#/layouts/UserBar'

import * as aria from '#/components/aria'
import Page from '#/components/Page'

import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'

import { useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import { baseName } from '#/utilities/fileInfo'
import { STATIC_QUERY_OPTIONS } from '#/utilities/reactQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { usePrefetchQuery } from '@tanstack/react-query'
import { DashboardTabPanels } from './DashboardTabPanels'

/** Props for {@link Dashboard}s that are common to all platforms. */
export interface DashboardProps {
  /** Whether the application may have the local backend running. */
  readonly supportsLocalBackend: boolean
  readonly initialProjectName: string | null
  readonly ydocUrl: string | null
}

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
  return (
    /* Ideally this would be in `Drive.tsx`, but it currently must be all the way out here
     * due to modals being in `TheModal`. */
    <DriveProvider>
      {({ resetAssetTableState }) => (
        <CategoriesProvider onCategoryChange={resetAssetTableState}>
          <ProjectsProvider>
            <DashboardInner {...props} />
          </ProjectsProvider>
        </CategoriesProvider>
      )}
    </DriveProvider>
  )
}

/** Extract proper path from `file://` URL. */
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
  const { initialProjectName: initialProjectNameRaw, ydocUrl } = props
  const localBackend = backendProvider.useLocalBackend()
  const { modalRef } = modalProvider.useModalRef()
  const { updateModal, unsetModal } = modalProvider.useSetModal()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)

  const assetManagementApiRef = React.useRef<assetTable.AssetManagementApi | null>(null)

  const initialLocalProjectPath =
    initialProjectNameRaw != null ? fileURLToPath(initialProjectNameRaw) : null
  const initialProjectName = initialLocalProjectPath != null ? null : initialProjectNameRaw

  const categoriesAPI = useCategoriesAPI()

  const projectsStore = useProjectsStore()
  const page = usePage()

  const setPage = useSetPage()
  const openEditor = projectHooks.useOpenEditor()
  const openProject = projectHooks.useOpenProject()
  const closeProject = projectHooks.useCloseProject()
  const closeAllProjects = projectHooks.useCloseAllProjects()
  const clearLaunchedProjects = useClearLaunchedProjects()

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
          id: localBackendModule.newProjectId(projectManager.UUID(id), localBackend.rootPath()),
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
      categoriesAPI.setCategory('local')

      const projectId = localBackendModule.newProjectId(
        projectManager.UUID(project.id),
        projectManager.Path(project.parentDirectory),
      )

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
  }, [openEditor, openProject, categoriesAPI])

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeModal: () => {
          updateModal((oldModal) => {
            if (oldModal == null) {
              const currentPage = projectsStore.getState().page
              if (currentPage === TabType.settings) {
                setPage(TabType.drive)
              }
            }
            return null
          })
          if (modalRef.current == null) {
            return false
          }
        },
      }),
    [inputBindings, modalRef, updateModal, setPage, projectsStore],
  )

  React.useEffect(() => {
    if (detect.isOnElectron()) {
      // We want to handle the back and forward buttons in electron the same way as in the browser.
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

  const onSignOut = eventCallbacks.useEventCallback(() => {
    setPage(TabType.drive)
    closeAllProjects()
    clearLaunchedProjects()
  })

  const goToSettings = eventCallbacks.useEventCallback(() => {
    setPage(TabType.settings)
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
            // This is safe as we render only valid pages.
            // eslint-disable-next-line no-restricted-syntax
            setPage(newPage as TabType)
          }}
        >
          <div className="flex">
            <DashboardTabBar onCloseProject={closeProject} onOpenEditor={openEditor} />

            <UserBar
              setIsHelpChatOpen={setIsHelpChatOpen}
              goToSettingsPage={goToSettings}
              onSignOut={onSignOut}
            />
          </div>

          <DashboardTabPanels
            initialProjectName={initialProjectName}
            ydocUrl={ydocUrl}
            assetManagementApiRef={assetManagementApiRef}
          />
        </aria.Tabs>
        {$config.CHAT_URL != null ?
          <Chat
            isOpen={isHelpChatOpen}
            doClose={() => {
              setIsHelpChatOpen(false)
            }}
            endpoint={$config.CHAT_URL}
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
