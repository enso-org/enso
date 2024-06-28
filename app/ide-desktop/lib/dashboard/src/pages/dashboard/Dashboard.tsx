/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'
import invariant from 'tiny-invariant'
import * as z from 'zod'

import DriveIcon from 'enso-assets/drive.svg'
import EditorIcon from 'enso-assets/network.svg'
import SettingsIcon from 'enso-assets/settings.svg'
import * as detect from 'enso-common/src/detect'

import * as eventCallbacks from '#/hooks/eventCallbackHooks'
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

import type * as assetTable from '#/layouts/AssetsTable'
import Category, * as categoryModule from '#/layouts/CategorySwitcher/Category'
import Chat from '#/layouts/Chat'
import ChatPlaceholder from '#/layouts/ChatPlaceholder'
import Drive from '#/layouts/Drive'
import Editor from '#/layouts/Editor'
import Settings from '#/layouts/Settings'
import * as tabBar from '#/layouts/TabBar'
import TabBar from '#/layouts/TabBar'
import UserBar from '#/layouts/UserBar'

import Page from '#/components/Page'

import ManagePermissionsModal from '#/modals/ManagePermissionsModal'

import * as backendModule from '#/services/Backend'
import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as array from '#/utilities/array'
import LocalStorage from '#/utilities/LocalStorage'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

import type * as types from '../../../../types/types'

// ============================
// === Global configuration ===
// ============================

/** Main content of the screen. Only one should be visible at a time. */
enum TabType {
  drive = 'drive',
  settings = 'settings',
}

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
    readonly page: z.infer<typeof PAGES_SCHEMA>
    readonly launchedProjects: z.infer<typeof LAUNCHED_PROJECT_SCHEMA>
  }
}

LocalStorage.registerKey('isAssetPanelVisible', { schema: z.boolean() })

const PROJECT_SCHEMA = z.object({
  id: z.custom<backendModule.ProjectId>(),
  parentId: z.custom<backendModule.DirectoryId>(),
  title: z.string(),
  type: z.nativeEnum(backendModule.BackendType),
})
const LAUNCHED_PROJECT_SCHEMA = z.array(PROJECT_SCHEMA)

/**
 * Launched project information.
 */
export type Project = z.infer<typeof PROJECT_SCHEMA>
/**
 * Launched project ID.
 */
export type ProjectId = Project['id']

LocalStorage.registerKey('launchedProjects', {
  isUserSpecific: true,
  schema: LAUNCHED_PROJECT_SCHEMA,
})

const PAGES_SCHEMA = z.nativeEnum(TabType).or(z.custom<backendModule.ProjectId>())

LocalStorage.registerKey('page', {
  schema: PAGES_SCHEMA,
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
  readonly ydocUrl: string | null
}

/**
 *
 */
export interface CreateOpenedProjectQueryOptions {
  readonly type: backendModule.BackendType
  readonly assetId: backendModule.Asset<backendModule.AssetType.project>['id']
  readonly parentId: backendModule.Asset<backendModule.AssetType.project>['parentId']
  readonly title: backendModule.Asset<backendModule.AssetType.project>['title']
  readonly remoteBackend: RemoteBackend
  readonly localBackend: LocalBackend | null
}

/**
 * Project status query.
 */
export function createGetProjectDetailsQuery(options: CreateOpenedProjectQueryOptions) {
  const { assetId, parentId, title, remoteBackend, localBackend, type } = options

  return reactQuery.queryOptions({
    queryKey: createGetProjectDetailsQuery.getQueryKey(assetId),
    meta: { persist: false },
    refetchInterval: ({ state }) => {
      const states = [backendModule.ProjectState.opened, backendModule.ProjectState.closed]

      if (state.data == null) {
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        return 30_000
      } else if (states.includes(state.data.state.type)) {
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        return 30_000
      } else {
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        return 5_000
      }
    },
    refetchIntervalInBackground: true,
    refetchOnWindowFocus: true,
    refetchOnMount: true,
    queryFn: async () => {
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      return backend.getProjectDetails(assetId, parentId, title)
    },
  })
}
createGetProjectDetailsQuery.getQueryKey = (id: Project['id']) => ['project', id] as const
createGetProjectDetailsQuery.createPassiveListener = (id: Project['id']) =>
  reactQuery.queryOptions<backendModule.Project>({
    queryKey: createGetProjectDetailsQuery.getQueryKey(id),
  })

/** The component that contains the entire UI. */
export default function Dashboard(props: DashboardProps) {
  const { appRunner, initialProjectName, ydocUrl } = props

  const { user, ...session } = authProvider.useFullUserSession()

  const remoteBackend = backendProvider.useRemoteBackendStrict()
  const localBackend = backendProvider.useLocalBackend()
  const { getText } = textProvider.useText()
  const { modalRef } = modalProvider.useModalRef()
  const { updateModal, unsetModal, setModal } = modalProvider.useSetModal()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [isHelpChatOpen, setIsHelpChatOpen] = React.useState(false)

  const assetManagementApiRef = React.useRef<assetTable.AssetManagementApi | null>(null)

  const defaultCategory = Category.cloud
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

  const [launchedProjects, privateSetLaunchedProjects] = React.useState<Project[]>(
    () => localStorage.get('launchedProjects') ?? []
  )

  // These pages MUST be ROUTER PAGES.
  const [page, privateSetPage] = searchParamsState.useSearchParamsState(
    'page',
    () => localStorage.get('page') ?? TabType.drive,
    (value: unknown): value is Project['id'] | TabType => {
      return (
        array.includes(Object.values(TabType), value) || launchedProjects.some(p => p.id === value)
      )
    }
  )

  const setLaunchedProjects = eventCallbacks.useEventCallback(
    (fn: (currentState: Project[]) => Project[]) => {
      privateSetLaunchedProjects(currentState => {
        const nextState = fn(currentState)
        localStorage.set('launchedProjects', nextState)
        return nextState
      })
    }
  )

  const addLaunchedProject = eventCallbacks.useEventCallback((project: Project) => {
    setLaunchedProjects(currentState => [...currentState, project])
  })

  const removeLaunchedProject = eventCallbacks.useEventCallback((projectId: Project['id']) => {
    setLaunchedProjects(currentState => currentState.filter(({ id }) => id !== projectId))
  })

  const clearLaunchedProjects = eventCallbacks.useEventCallback(() => {
    setLaunchedProjects(() => [])
  })

  const setPage = eventCallbacks.useEventCallback((nextPage: Project['id'] | TabType) => {
    privateSetPage(nextPage)
    localStorage.set('page', nextPage)
  })

  const [assetListEvents, dispatchAssetListEvent] =
    eventHooks.useEvent<assetListEvent.AssetListEvent>()
  const [assetEvents, dispatchAssetEvent] = eventHooks.useEvent<assetEvent.AssetEvent>()

  const isCloud = categoryModule.isCloud(category)
  const isUserEnabled = user.isEnabled

  const selectedProject = launchedProjects.find(p => p.id === page) ?? null

  if (isCloud && !isUserEnabled && localBackend != null) {
    setTimeout(() => {
      // This sets `BrowserRouter`, so it must not be set synchronously.
      setCategory(Category.local)
    })
  }

  const openProjectMutation = reactQuery.useMutation({
    networkMode: 'always',
    mutationFn: ({ title, id, type, parentId }: Project) => {
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      return backend.openProject(
        id,
        {
          executeAsync: false,
          cognitoCredentials: {
            accessToken: session.accessToken,
            refreshToken: session.accessToken,
            clientId: session.clientId,
            expireAt: session.expireAt,
            refreshUrl: session.refreshUrl,
          },
          parentId,
        },
        title
      )
    },
    onMutate: async ({ id }) => {
      const queryKey = createGetProjectDetailsQuery.getQueryKey(id)

      client.setQueryData(queryKey, {
        state: { type: backendModule.ProjectState.openInProgress },
      })

      await client.cancelQueries({ queryKey: queryKey })
    },
    onError: async (_, { id }) => {
      await client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) })
    },
  })

  const closeProjectMutation = reactQuery.useMutation({
    mutationFn: async ({ type, id, title }: Project) => {
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      if (backend == null) {
        throw new Error('Backend is null')
      } else {
        return backend.closeProject(id, title)
      }
    },
    onSuccess: (_, { id }) =>
      client.invalidateQueries({ queryKey: createGetProjectDetailsQuery.getQueryKey(id) }),
  })

  const client = reactQuery.useQueryClient()

  const renameProjectMutation = reactQuery.useMutation({
    mutationFn: ({ newName, project }: { newName: string; project: Project }) => {
      const { parentId, type, id, title } = project
      const backend = type === backendModule.BackendType.remote ? remoteBackend : localBackend

      invariant(backend != null, 'Backend is null')

      return backend.updateProject(
        id,
        { projectName: newName, ami: null, ideVersion: null, parentId },
        title
      )
    },
    onSuccess: (_, { project }) =>
      client.invalidateQueries({
        queryKey: createGetProjectDetailsQuery.getQueryKey(project.id),
      }),
  })

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

  const doCloseAllProjects = eventCallbacks.useEventCallback(() => {
    for (const launchedProject of launchedProjects) {
      doCloseProject(launchedProject)
    }
  })

  const doOpenProject = eventCallbacks.useEventCallback((project: Project) => {
    // since we don't support multitabs, we need to close opened project first
    if (launchedProjects.length > 0) {
      doCloseAllProjects()
    }

    openProjectMutation.mutate(project)
    addLaunchedProject(project)
  })

  const doOpenEditor = eventCallbacks.useEventCallback((projectId: Project['id']) => {
    setPage(projectId)
  })

  const doCloseProject = eventCallbacks.useEventCallback((project: Project) => {
    const { id } = project

    closeProjectMutation.mutate(project)
    dispatchAssetEvent({ type: AssetEventType.closeProject, id })
    removeLaunchedProject(project.id)

    setPage(TabType.drive)
  })

  const doRemoveSelf = eventCallbacks.useEventCallback((project: Project) => {
    dispatchAssetListEvent({ type: AssetListEventType.removeSelf, id: project.id })
    doCloseProject(project)
  })

  const onSignOut = eventCallbacks.useEventCallback(() => {
    setPage(TabType.drive)
    doCloseAllProjects()
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
                labelId="drivePageName"
                onPress={() => {
                  setPage(TabType.drive)
                }}
              >
                {getText('drivePageName')}
              </tabBar.Tab>

              {launchedProjects.map(project => (
                <tabBar.Tab
                  project={project}
                  key={project.id}
                  isActive={page === project.id}
                  icon={EditorIcon}
                  labelId="editorPageName"
                  onPress={() => {
                    setPage(project.id)
                  }}
                  onClose={() => {
                    doCloseProject(project)
                  }}
                >
                  {project.title}
                </tabBar.Tab>
              ))}

              {page === TabType.settings && (
                <tabBar.Tab
                  isActive
                  icon={SettingsIcon}
                  labelId="settingsPageName"
                  onPress={() => {
                    setPage(TabType.settings)
                  }}
                  onClose={() => {
                    setPage(TabType.drive)
                  }}
                >
                  {getText('settingsPageName')}
                </tabBar.Tab>
              )}
            </TabBar>

            <UserBar
              onShareClick={selectedProject ? doOpenShareModal : undefined}
              setIsHelpChatOpen={setIsHelpChatOpen}
              goToSettingsPage={() => {
                setPage(TabType.settings)
              }}
              onSignOut={onSignOut}
            />
          </div>

          <Drive
            assetsManagementApiRef={assetManagementApiRef}
            openedProjects={launchedProjects}
            category={category}
            setCategory={setCategory}
            hidden={page !== TabType.drive}
            initialProjectName={initialProjectName}
            assetListEvents={assetListEvents}
            dispatchAssetListEvent={dispatchAssetListEvent}
            assetEvents={assetEvents}
            dispatchAssetEvent={dispatchAssetEvent}
            doOpenProject={doOpenProject}
            doOpenEditor={doOpenEditor}
            doCloseProject={doCloseProject}
          />

          {launchedProjects.map(project => (
            <Editor
              key={project.id}
              hidden={page !== project.id}
              ydocUrl={ydocUrl}
              project={project}
              projectId={project.id}
              appRunner={appRunner}
              isOpening={openProjectMutation.isPending}
              startProject={openProjectMutation.mutate}
              renameProject={newName => {
                renameProjectMutation.mutate({ newName, project })
              }}
            />
          ))}

          {page === TabType.settings && <Settings />}
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
