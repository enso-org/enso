/**
 * @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components.
 */
import Page from '#/components/Page'
import { useCloseAllProjects, useCloseProject, useOpenProjectLocally } from '#/hooks/projectHooks'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import { CategoriesProvider, useCategoriesAPI } from '#/layouts/Drive/CategorySwitcher'
import { useLocalBackend } from '#/providers/BackendProvider'
import { DriveProvider } from '#/providers/DriveProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { unsetModal } from '#/providers/ModalProvider'
import {
  ProjectsProvider,
  useClearLaunchedProjects,
  useLaunchedProjects,
  usePage,
  useSetPage,
} from '#/providers/ProjectsProvider'
import * as backendModule from '#/services/Backend'
import { Path } from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'
import { newProjectId } from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'
import { UUID } from '#/services/ProjectManager'
import { baseName } from '#/utilities/fileInfo'
import { STATIC_QUERY_OPTIONS } from '#/utilities/reactQuery'
import { document } from '#/utilities/sanitizedEventTargets'
import { vueComponent } from '#/utilities/vue'
import { useConfigInReact } from '$/providers/react'
import { usePrefetchQuery } from '@tanstack/react-query'
import { isOnElectron, platform, Platform } from 'enso-common/src/detect'
import * as React from 'react'

const TabView = React.lazy(() =>
  import('$/components/TabView.vue').then(({ default: vue }) => vueComponent(vue)),
)

/** The component that contains the entire UI. */
export default function Dashboard() {
  return (
    /* Ideally this would be in `Drive.tsx`, but it currently must be all the way out here
     * due to modals being in `TheModal`. */
    <DriveProvider>
      {({ resetAssetTableState }) => (
        <CategoriesProvider onCategoryChange={resetAssetTableState}>
          <ProjectsProvider>
            <DashboardInner />
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
        platform() === Platform.windows ?
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
function DashboardInner() {
  const localBackend = useLocalBackend()
  const inputBindings = useInputBindings()
  const config = useConfigInReact()

  const initialProjectNameRaw = config.params.startup.project
  const initialLocalProjectPath = fileURLToPath(initialProjectNameRaw)
  const initialProjectName = initialLocalProjectPath != null ? null : initialProjectNameRaw

  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)

  const categoriesAPI = useCategoriesAPI()

  const openProjectLocally = useOpenProjectLocally()

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
        await openProjectLocally(
          {
            id: localBackendModule.newProjectId(projectManager.UUID(id), localBackend.rootPath()),
            title: projectName,
            parentId: localBackendModule.newDirectoryId(localBackend.rootPath()),
          },
          backendModule.BackendType.local,
        )
      }
      return null
    },
    staleTime: Infinity,
  })

  React.useEffect(() => {
    window.projectManagementApi?.setOpenProjectHandler((project) => {
      categoriesAPI.setCategory('local')

      const projectId = newProjectId(UUID(project.id), Path(project.parentDirectory))

      void openProjectLocally(
        {
          id: projectId,
          title: project.name,
          parentId: localBackendModule.newDirectoryId(backendModule.Path(project.parentDirectory)),
        },
        backendModule.BackendType.local,
      )
    })

    return () => {
      window.projectManagementApi?.setOpenProjectHandler(() => {})
    }
  }, [openProjectLocally, categoriesAPI])

  React.useEffect(() => {
    if (isOnElectron()) {
      // We want to handle the back and forward buttons in electron the same way as in the browser.
      return inputBindings.attach(document.body, 'keydown', {
        goBack: () => {
          window.navigationApi.goBack()
        },
        goForward: () => {
          window.navigationApi.goForward()
        },
      })
    }
  }, [inputBindings])

  React.useEffect(
    () => inputBindings.attach(document.body, 'keydown', { closeModal: unsetModal }),
    [inputBindings],
  )

  const page = usePage()
  const setPage = useSetPage()
  const launchedProjects = useLaunchedProjects()
  const closeProject = useCloseProject()
  const closeAllProjects = useCloseAllProjects()
  const clearLaunchedProjects = useClearLaunchedProjects()

  return (
    <Page hideInfoBar hideChat>
      <div
        className="flex min-h-full flex-col text-xs text-primary"
        onContextMenu={(event) => {
          event.preventDefault()
          unsetModal()
        }}
      >
        <TabView
          initialProjectName={initialProjectName}
          setIsChatOpen={setIsHelpChatOpen}
          page={page}
          setPage={setPage}
          launchedProjects={launchedProjects}
          closeProject={closeProject}
          closeAllProjects={closeAllProjects}
          clearLaunchedProjects={clearLaunchedProjects}
        />
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
