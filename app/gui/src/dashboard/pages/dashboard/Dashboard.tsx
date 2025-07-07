/**
 * @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components.
 */
import Page from '#/components/Page'
import { usePaywall } from '#/hooks/billing'
import * as projectHooks from '#/hooks/projectHooks'
import { CategoriesProvider } from '#/layouts/Drive/Categories'
import { useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import DriveProvider from '#/providers/DriveProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import ProjectsProvider, { useLaunchedProjects } from '#/providers/ProjectsProvider'
import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'
import * as projectManager from '#/services/ProjectManager'
import { baseName } from '#/utilities/fileInfo'
import { STATIC_QUERY_OPTIONS } from '#/utilities/reactQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { vueComponent } from '#/utilities/vue'
import AppContainerVue from '$/components/AppContainer.vue'
import { useBackends, useConfig, useFullUserSession } from '$/providers/react'
import { useVueValue } from '$/providers/react/common'
import { usePrefetchQuery } from '@tanstack/react-query'
import * as detect from 'enso-common/src/detect'
import * as React from 'react'

// This is a component, not a mere constant
// eslint-disable-next-line no-restricted-syntax
const AppContainer = vueComponent(AppContainerVue).default

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
function DashboardInner() {
  const { localBackend } = useBackends()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const config = useConfig()

  const initialProjectNameRaw = useVueValue(
    React.useCallback(() => config.params.startup.project, [config]),
  )
  const initialLocalProjectPath = fileURLToPath(initialProjectNameRaw)
  const initialProjectName = initialLocalProjectPath != null ? null : initialProjectNameRaw

  const categoriesAPI = useCategoriesAPI()

  const openProjectLocally = projectHooks.useOpenProjectLocally()

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

      const projectId = localBackendModule.newProjectId(
        projectManager.UUID(project.id),
        projectManager.Path(project.parentDirectory),
      )

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

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        closeModal: () => modalProvider.unsetModal(),
      }),
    [inputBindings],
  )

  const launchedProjects = useLaunchedProjects()
  const closeProject = projectHooks.useCloseProject()
  const closeAllProjects = projectHooks.useCloseAllProjects()
  const { user } = useFullUserSession()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })

  return (
    <Page hideInfoBar>
      <div
        className="flex min-h-full flex-col text-xs text-primary"
        onContextMenu={(event) => {
          event.preventDefault()
          modalProvider.unsetModal()
        }}
      >
        <AppContainer
          initialProjectName={initialProjectName}
          launchedProjects={launchedProjects}
          closeProject={closeProject}
          closeAllProjects={closeAllProjects}
          isFeatureUnderPaywall={isFeatureUnderPaywall}
        />
      </div>
    </Page>
  )
}
