/**
 * @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components.
 */
import Page from '#/components/Page'
import { backendQueryOptions } from '#/hooks/backendHooks'
import { usePaywall } from '#/hooks/billing'
import { useBindGlobalActions } from '#/hooks/menuHooks'
import * as projectHooks from '#/hooks/projectHooks'
import { CategoriesProvider } from '#/layouts/Drive/Categories'
import SettingsTabType from '#/layouts/Settings/TabType'
import { setDriveLocation } from '#/providers/DriveProvider'
import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/Backend'
import * as localBackendModule from '#/services/LocalBackend'
import { baseName } from '#/utilities/fileInfo'
import { STATIC_QUERY_OPTIONS } from '#/utilities/reactQuery'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import { vueComponent } from '#/utilities/vue'
import { SEARCH_PARAMS_PREFIX } from '$/appUtils'
import AppContainerInnerVue from '$/components/AppContainer/AppContainerInner.vue'
import { useBackends, useConfig, useFullUserSession, useRouter } from '$/providers/react'
import { useVueValue } from '$/providers/react/common'
import { useLaunchedProjects } from '$/providers/react/container'
import { usePrefetchQuery, useQuery } from '@tanstack/react-query'
import * as detect from 'enso-common/src/detect'
import * as React from 'react'
import type { Router } from 'vue-router'

/** Dashboard properties */
export interface DashboardProps {
  readonly projectToOpen?:
    | { readonly asset: backendModule.ProjectAsset; readonly backend: backendModule.BackendType }
    | undefined
}

// This is a component, not a mere constant
// eslint-disable-next-line no-restricted-syntax
const AppContainerInner = vueComponent(AppContainerInnerVue).default

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

/** Navigate to a specific settings tab. */
function goToSettingsTab(router: Router, tab: SettingsTabType) {
  void router.push({
    path: '/settings',
    query: { [`${SEARCH_PARAMS_PREFIX}SettingsTab`]: JSON.stringify(tab) },
  })
}

/** The component that contains the entire UI. */
export function Dashboard(props: DashboardProps) {
  const { remoteBackend, localBackend } = useBackends()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const config = useConfig()
  const { router } = useRouter()
  const initialProjectNameRaw = useVueValue(
    React.useCallback(() => config.params.startup.project, [config]),
  )
  const { data: organization = null } = useQuery(
    backendQueryOptions(remoteBackend, 'getOrganization', []),
  )
  const initialLocalProjectPath = fileURLToPath(initialProjectNameRaw)
  const launchedProjects = useLaunchedProjects()
  const openProjectLocally = projectHooks.useOpenProjectLocally()
  const initialAlreadyLaunchedProject = launchedProjects.find(
    (lp) => lp.id === props.projectToOpen?.asset.id,
  )
  const initialAlreadyLaunchedHybridProject = launchedProjects.find(
    (lp) => lp.state === 'launched' && lp.hybrid?.cloudProjectId === props.projectToOpen?.asset.id,
  )

  const closeProject = projectHooks.useCloseProject()
  const closeAllProjects = projectHooks.useCloseAllProjects()
  const { user } = useFullUserSession()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })

  usePrefetchQuery({
    queryKey: ['loadInitialProject'],
    networkMode: 'always',
    ...STATIC_QUERY_OPTIONS,
    queryFn: async () => {
      if (props.projectToOpen) {
        if (
          // If project is already on launched list, then the Editor.tsx will handle opening it.
          !initialAlreadyLaunchedProject &&
          !initialAlreadyLaunchedHybridProject &&
          !projectHooks.BUSY_PROJECT_STATES.has(props.projectToOpen.asset.projectState.type)
        ) {
          await openProjectLocally(props.projectToOpen.asset, props.projectToOpen.backend)
        }
      } else if (initialLocalProjectPath != null && localBackend) {
        const projectName = baseName(initialLocalProjectPath)
        const parentDirectoryId = localBackendModule.newDirectoryId(localBackend.rootPath())
        const metadata = await localBackend.uploadFileStart(
          {
            parentDirectoryId,
            fileName: projectName,
            fileId: null,
            filePath: backendModule.Path(initialLocalProjectPath),
          },
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          null!,
        )
        const endMetadata = await localBackend.uploadFileEnd({
          parentDirectoryId,
          fileName: projectName,
          assetId: null,
          parts: [],
          ...metadata,
        })
        if (endMetadata.project == null) {
          return
        }
        await openProjectLocally(
          {
            id: endMetadata.id,
            title: projectName,
            parentId: localBackendModule.newDirectoryId(localBackend.rootPath()),
            ensoPath: backendModule.EnsoPath(
              String(backendModule.extractTypeAndPath(endMetadata.id).path),
            ),
          },
          backendModule.BackendType.local,
        )
      }
      return null
    },
  })

  React.useEffect(() => {
    window.projectManagementApi?.setOpenProjectHandler((project) => {
      setDriveLocation(null, 'local')

      const projectId = localBackendModule.newProjectId(project.projectRoot)

      void openProjectLocally(
        {
          id: projectId,
          title: project.name,
          parentId: localBackendModule.newDirectoryId(backendModule.Path(project.parentDirectory)),
          ensoPath: backendModule.EnsoPath(String(project.projectRoot)),
        },
        backendModule.BackendType.local,
      )
    })

    return () => {
      window.projectManagementApi?.setOpenProjectHandler(() => {})
    }
  }, [openProjectLocally])

  const inputBindingHandlers = React.useMemo(() => {
    const hasOrganization = backendModule.isUserOnPlanWithMultipleSeats(user)

    return inputBindings.defineHandlers({
      // We want to handle the back and forward buttons in electron the same way as in the browser.
      ...(detect.isOnElectron() && {
        goBack: () => {
          window.navigationApi.goBack()
        },
        goForward: () => {
          window.navigationApi.goForward()
        },
        goToAccountSettings: () => {
          goToSettingsTab(router, SettingsTabType.account)
        },
        ...(hasOrganization && {
          goToOrganizationSettings: () => {
            goToSettingsTab(router, SettingsTabType.organization)
          },
        }),
        ...(localBackend && {
          goToLocalSettings: () => {
            goToSettingsTab(router, SettingsTabType.local)
          },
        }),
        ...(user.isOrganizationAdmin &&
          organization?.subscription != null && {
            goToBillingAndPlansSettings: () => {
              goToSettingsTab(router, SettingsTabType.billingAndPlans)
            },
          }),
        ...(hasOrganization && {
          goToMembersSettings: () => {
            goToSettingsTab(router, SettingsTabType.members)
          },
        }),
        ...(hasOrganization && {
          goToUserGroupsSettings: () => {
            goToSettingsTab(router, SettingsTabType.userGroups)
          },
        }),
        goToKeyboardShortcutsSettings: () => {
          goToSettingsTab(router, SettingsTabType.keyboardShortcuts)
        },
        ...(hasOrganization && {
          goToActivityLogSettings: () => {
            goToSettingsTab(router, SettingsTabType.activityLog)
          },
        }),
      }),
      closeModal: () => modalProvider.unsetModal(),
    })
  }, [inputBindings, localBackend, organization?.subscription, router, user])

  useBindGlobalActions(inputBindingHandlers)

  React.useEffect(
    () =>
      inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', inputBindingHandlers),
    [inputBindings, inputBindingHandlers],
  )

  return (
    <CategoriesProvider>
      <Page hideInfoBar>
        <div
          className="flex min-h-full flex-col text-xs text-primary"
          onContextMenu={(event) => {
            event.preventDefault()
            modalProvider.unsetModal()
          }}
        >
          <AppContainerInner
            onCloseProject={closeProject}
            onCloseAllProjects={closeAllProjects}
            isFeatureUnderPaywall={isFeatureUnderPaywall}
          />
        </div>
      </Page>
    </CategoriesProvider>
  )
}
