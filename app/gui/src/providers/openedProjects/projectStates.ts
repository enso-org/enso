import Backend, {
  AssetType,
  ProjectState as BackendProjectState,
  BackendType,
  EnsoPath,
  NetworkError,
  type DirectoryId,
  type ProjectAsset,
  type Project as ProjectDetails,
  type ProjectId,
  type ProjectSessionId,
} from '#/services/Backend'
import type LocalBackend from '#/services/LocalBackend'
import type RemoteBackend from '#/services/RemoteBackend'
import LocalStorage from '#/utilities/LocalStorage'
import { backendMutationOptions } from '@/composables/backend'
import { injectGuiConfig } from '@/providers/guiConfig'
import { assert, assertDefined } from '@/util/assert'
import { Err, Ok, rejectionToResult, type Result } from '@/util/data/result'
import * as vueQuery from '@tanstack/vue-query'
import { computed, effectScope, markRaw, onScopeDispose, type EffectScope, type Ref } from 'vue'
import * as z from 'zod'
import { useBackends } from '../backends'
import { useSession } from '../session'
import { useText } from '../text'
import { useUploadsToCloudStore } from '../upload'
import { createGraphStore, type GraphStore } from './graph'
import { createModuleStore, type ModuleStore } from './module'
import { createProjectStore, type ProjectStore } from './project'
import { createProjectNameStore, type ProjectNameStore } from './projectNames'
import { createSuggestionDbStore, type SuggestionDbStore } from './suggestionDatabase'
import { WidgetRegistry } from './widgetRegistry'

declare module '#/utilities/LocalStorage' {
  interface LocalStorageData {
    readonly openedTabs: RunningProjectInfo[]
    readonly unuploadedProjects: RunningProjectInfo[]
  }
}

const PROJECT_ID_SCHEMA = z.custom<ProjectId>(
  (x) => typeof x === 'string' && x.startsWith('project-'),
)
const PROJECT_SESSION_ID_SCHEMA = z.custom<ProjectSessionId>(
  (x) => typeof x === 'string' && x.startsWith('projectsession-'),
)
const DIRECTORY_ID_SCHEMA = z.custom<DirectoryId>(
  (x) => typeof x === 'string' && x.startsWith('directory-'),
)
const ENSO_PATH_SCHEMA = z.custom<EnsoPath>((x) => typeof x === 'string')
const PROJECT_INFO_SCHEMA = z.object({
  id: PROJECT_ID_SCHEMA,
  parentId: DIRECTORY_ID_SCHEMA,
  title: z.string(),
  ensoPath: ENSO_PATH_SCHEMA,
  mode: z.enum(['local', 'cloud', 'hybrid']),
})

const RUNNING_NATIVE_PROJECT_INFO_SCHEMA = PROJECT_INFO_SCHEMA.extend({
  mode: z.enum(['local', 'cloud']),
})

const RUNNING_HYBRID_PROJECT_INFO_SCHEMA = PROJECT_INFO_SCHEMA.extend({
  mode: z.literal('hybrid'),
  runningId: PROJECT_ID_SCHEMA,
  hybridSessionId: PROJECT_SESSION_ID_SCHEMA,
  localParentId: DIRECTORY_ID_SCHEMA,
})
const RUNNING_PROJECT_INFO_SCHEMA = z.discriminatedUnion('mode', [
  RUNNING_NATIVE_PROJECT_INFO_SCHEMA,
  RUNNING_HYBRID_PROJECT_INFO_SCHEMA,
])

LocalStorage.registerKey('openedTabs', { schema: z.array(RUNNING_PROJECT_INFO_SCHEMA) })
LocalStorage.registerKey('unuploadedProjects', { schema: z.array(RUNNING_PROJECT_INFO_SCHEMA) })

export type ProjectInfo = z.infer<typeof PROJECT_INFO_SCHEMA>
export type RunningProjectInfo = z.infer<typeof RUNNING_PROJECT_INFO_SCHEMA>
export type RunMode = ProjectInfo['mode']

export interface LsUrls {
  rpcUrl: string
  dataUrl: string
  ydocUrl: string
}
export interface NotOpened {
  status: 'not-opened'
  info: ProjectInfo
}

export interface HybridOpened {
  status: 'hybrid-opened'
  info: ProjectInfo & { mode: 'hybrid'; hybridSessionId: ProjectSessionId }
}

export interface HybridDownloaded {
  status: 'hybrid-downloaded'
  localProjectRootId: DirectoryId
  localProjectParentId: DirectoryId
  info: ProjectInfo & { mode: 'hybrid'; hybridSessionId: ProjectSessionId }
}

export interface Opened {
  status: 'opened'
  info: RunningProjectInfo
  runningId: ProjectId
}

export interface Initialized {
  status: 'initialized'
  info: RunningProjectInfo
  runningId: ProjectId
  runDetails: Ref<ProjectDetails>
  name: Ref<string>
  scope: EffectScope
  store: ProjectStore
  projectNames: ProjectNameStore
  suggestionDb: SuggestionDbStore
  module: ModuleStore
  graph: GraphStore
  widgetRegistry: WidgetRegistry
}

export interface HybridLocallyClosed {
  status: 'hybrid-closed'
  info: RunningProjectInfo & { mode: 'hybrid' }
}

export interface HybridUploaded {
  status: 'hybrid-uploaded'
  info: ProjectInfo & { mode: 'hybrid' }
}

export type ProjectState =
  | NotOpened
  | HybridOpened
  | HybridDownloaded
  | Opened
  | Initialized
  | HybridLocallyClosed
  | HybridUploaded

export type ProjectStatus = ProjectState['status']

export function useProjectStates() {
  const backends = useBackends()
  const session = useSession()
  const text = useText()
  const config = injectGuiConfig()
  const uploads = useUploadsToCloudStore()
  const queryClient = vueQuery.useQueryClient()

  const openLocalProject = vueQuery.useMutation(
    backendMutationOptions('openProject', backends.localBackend),
  )
  const openRemoteProject = vueQuery.useMutation(
    backendMutationOptions('openProject', backends.remoteBackend),
  )
  const closeLocalProject = vueQuery.useMutation(
    backendMutationOptions('closeProject', backends.localBackend),
  )
  const closeRemoteProject = vueQuery.useMutation(
    backendMutationOptions('closeProject', backends.remoteBackend),
  )

  const catchNetworkError = rejectionToResult(NetworkError)

  async function openProject(project: NotOpened): Promise<Result<HybridOpened | Opened>> {
    if (session.session == null) return Err('No user session')
    const cognitoCredentials = {
      accessToken: session.session.accessToken,
      refreshToken: session.session.refreshToken,
      clientId: session.session.clientId,
      expireAt: session.session.expireAt,
      refreshUrl: session.session.refreshUrl,
    }
    switch (project.info.mode) {
      case 'local': {
        if (!backends.localBackend) return Err('Cannot open local project: Local Backend missing.')
        const result = await catchNetworkError(
          openLocalProject.mutateAsync([
            project.info.id,
            {
              executeAsync: false,
              cognitoCredentials: null,
              openHybridProjectParameters: null,
            },
            project.info.title,
          ]),
        )
        if (!result.ok) return result
        return Ok({
          status: 'opened',
          info: { ...project.info, mode: project.info.mode },
          runningId: project.info.id,
        })
      }
      case 'cloud': {
        const result = await catchNetworkError(
          openRemoteProject.mutateAsync([
            project.info.id,
            {
              executeAsync: false,
              cognitoCredentials,
              openHybridProjectParameters: null,
            },
            project.info.title,
          ]),
        )
        if (!result.ok) return result
        return Ok({
          status: 'opened',
          info: { ...project.info, mode: project.info.mode },
          runningId: project.info.id,
        })
      }
      case 'hybrid': {
        if (!backends.localBackend) return Err('Cannot open hybrid project: Local Backend missing.')
        const hybridSessionId = await catchNetworkError(
          backends.remoteBackend.setHybridOpenInProgress(project.info.id, project.info.title),
        )
        if (!hybridSessionId.ok) return hybridSessionId
        // This strange spread is needed for TS for some reason
        return Ok({
          status: 'hybrid-opened',
          info: {
            ...project.info,
            mode: project.info.mode,
            hybridSessionId: hybridSessionId.value,
          },
        })
      }
    }
  }

  async function downloadHybridProject(project: HybridOpened): Promise<Result<HybridDownloaded>> {
    if (!backends.localBackend) return Err('Cannot open hybrid project: Local Backend missing.')
    const localProject = await catchNetworkError(
      backends.remoteBackend.downloadProject(project.info.id),
    )
    if (!localProject.ok) return localProject
    return Ok({
      status: 'hybrid-downloaded',
      localProjectParentId: localProject.value.parentId,
      localProjectRootId: localProject.value.projectRootId,
      info: project.info,
    })
  }

  async function openLocalVersionOfHybridProject(
    project: HybridDownloaded,
    abort: AbortSignal,
  ): Promise<Result<Opened>> {
    if (!backends.localBackend) return Err('Cannot open local project: Local Backend missing.')
    let localProjectAsset: ProjectAsset | undefined
    // TODO[ao]: Apparently, the only way to get local project id is to list directory, because
    // "downloadProject" does not return it. To discuss.
    for (const parentId of [project.localProjectParentId, project.localProjectRootId]) {
      const listing = await catchNetworkError(
        backends.localBackend.listDirectory({
          parentId: parentId,
          filterBy: null,
          labels: null,
          sortExpression: null,
          sortDirection: null,
          from: null,
          pageSize: null,
          recentProjects: false,
        }),
      )
      if (!listing.ok) continue
      abort.throwIfAborted()
      localProjectAsset = listing.value.assets
        .filter((item) => item.type === AssetType.project)
        .at(0)
      if (localProjectAsset) {
        break
      }
    }
    if (!localProjectAsset) return Err('Cannot find downloaded local project.')

    const cloudParentPath = EnsoPath(
      project.info.ensoPath.slice(0, project.info.ensoPath.lastIndexOf('/')),
    )
    const result = await catchNetworkError(
      backends.localBackend.openProject(
        localProjectAsset.id,
        {
          executeAsync: false,
          cognitoCredentials: null,
          openHybridProjectParameters: {
            cloudProjectDirectoryPath: cloudParentPath,
            cloudProjectId: project.info.id,
            cloudProjectSessionId: project.info.hybridSessionId,
          },
        },
        project.info.title,
      ),
    )
    if (!result.ok) return result
    return Ok({
      status: 'opened',
      info: {
        ...project.info,
        runningId: localProjectAsset.id,
        localParentId: localProjectAsset.parentId,
      },
      runningId: localProjectAsset.id,
    })
  }

  async function initializeProject(project: Opened): Promise<Result<Initialized>> {
    const scope = effectScope()

    let details: Awaited<ReturnType<typeof getProjectDetails>>
    switch (project.info.mode) {
      case 'local':
        if (backends.localBackend == null)
          return Err('Cannot get details of local project: no local backend.')
        details = await getProjectDetails(
          backends.localBackend,
          project.info.id,
          scope,
          queryClient,
        )
        break
      case 'cloud':
        details = await getProjectDetails(
          backends.remoteBackend,
          project.info.id,
          scope,
          queryClient,
        )
        break
      case 'hybrid':
        if (backends.localBackend == null)
          return Err('Cannot get details of hybrid project: no local backend.')
        details = await getHybridProjectDetails(project.info, scope, {
          localBackend: backends.localBackend,
          remoteBackend: backends.remoteBackend,
        })
    }

    return scope.run(() => {
      const runningId = project.info.mode === 'hybrid' ? project.info.runningId : project.info.id
      const projectNames = createProjectNameStore({
        projectNamespace: undefined, // TODO[ao]: we should get project's namespace from cloud. This never worked in old Editor.tsx
        projectDisplayedName: details.name,
        projectInitialName: details.runDetails.value.packageName,
      })
      const rpcUrl = details.runDetails.value.jsonAddress
      const dataUrl = details.runDetails.value.binaryAddress
      const ydocUrl = details.runDetails.value.ydocAddress ?? config.ydocUrl ?? ''
      assert(rpcUrl != null, text.getText('noJSONEndpointError'))
      assert(dataUrl != null, text.getText('noBinaryEndpointError'))
      const store = createProjectStore(
        {
          projectId: runningId,
          //TODO[ao]: fix before merge.
          renameProject: () => Promise.resolve(),
          engine: {
            rpcUrl,
            dataUrl,
            ydocUrl,
          },
        },
        projectNames,
      )
      const suggestionDb = createSuggestionDbStore(store, projectNames)
      const module = createModuleStore(store, projectNames, suggestionDb)
      const graph = createGraphStore(store, suggestionDb, projectNames, module)
      const widgetRegistry = new WidgetRegistry(graph.db)
      const logger = eventLogger(project.info.id)

      logger.send('ide_project_opened')
      onScopeDispose(() => logger.send('ide_project_closed'))

      return Ok(
        markRaw({
          status: 'initialized',
          info: project.info,
          runningId,
          ...details,
          store,
          projectNames,
          suggestionDb,
          module,
          graph,
          widgetRegistry,
          scope,
        }),
      )
    })!
  }

  async function getHybridProjectDetails(
    project: RunningProjectInfo & { mode: 'hybrid' },
    scope: EffectScope,
    backends: { remoteBackend: RemoteBackend; localBackend: LocalBackend },
  ) {
    const [localDetails, cloudDetails] = await Promise.all([
      getProjectDetails(backends.localBackend, project.runningId, scope, queryClient),
      getProjectDetails(backends.remoteBackend, project.id, scope, queryClient),
    ])
    return {
      runDetails: localDetails.runDetails,
      name: cloudDetails.name,
    }
  }

  function eventLogger(projectId: ProjectId) {
    const logProjectId = computed(() => {
      const prefix = 'project-'
      const projectUuid =
        projectId.startsWith(prefix) ? projectId.substring(prefix.length) : projectId
      return `${prefix}${projectUuid.replace(/-/g, '')}`
    })

    return {
      async send(message: string) {
        backends.remoteBackend.logEvent(message, logProjectId.value)
      },
    }
  }

  async function closeProject(
    project: Opened | Initialized,
  ): Promise<Result<NotOpened | HybridLocallyClosed>> {
    if (project.status === 'initialized') {
      project.scope.stop()
    }
    switch (project.info.mode) {
      case 'local':
        if (backends.localBackend == null)
          return Err('Cannot close local project: no local backend')
        await closeLocalProject.mutateAsync([project.info.id, project.info.title])
        return Ok({
          status: 'not-opened',
          info: project.info,
        })
      case 'cloud':
        await closeRemoteProject.mutateAsync([project.info.id, project.info.title])
        return Ok({
          status: 'not-opened',
          info: project.info,
        })
      case 'hybrid':
        if (backends.localBackend == null)
          return Err('Cannot close hybrid project: no local backend')
        await backends.localBackend.closeProject(project.runningId, project.info.title)
        return Ok({
          status: 'hybrid-closed',
          info: { ...project.info, mode: project.info.mode },
        })
    }
  }

  async function uploadHybridProject(
    project: HybridLocallyClosed,
  ): Promise<Result<HybridUploaded>> {
    if (backends.localBackend == null) {
      return Err('Cannot close Hybrid Project without local backend')
    }
    const fileName = 'project_root.enso-project'
    const file = await backends.remoteBackend.getProjectArchive(
      project.info.localParentId,
      fileName,
    )
    await uploads.uploadFile(
      file,
      {
        fileId: project.info.id,
        fileName,
        parentDirectoryId: project.info.parentId,
      },
      'hybridSync',
    )
    backends.localBackend
      .deleteAsset(project.info.localParentId, { force: true }, null)
      .catch((err) => console.error('Failed to delete local version of hybrid project', err))
    return Ok({
      status: 'hybrid-uploaded',
      info: project.info,
    })
  }

  async function closeHybridProject(
    project: HybridUploaded | HybridOpened | HybridDownloaded,
  ): Promise<Result<NotOpened>> {
    closeRemoteProject.mutateAsync([project.info.id, project.info.title])
    return Ok({ status: 'not-opened', info: project.info })
  }

  function closeProjectInBackend(project: { id: ProjectId; title: string }, backend: BackendType) {
    if (backend === BackendType.local) {
      closeLocalProject.mutate([project.id, project.title])
    } else {
      closeRemoteProject.mutate([project.id, project.title])
    }
  }

  return {
    openProject,
    downloadHybridProject,
    openLocalVersionOfHybridProject,
    initializeProject,
    closeProject,
    uploadHybridProject,
    closeHybridProject,
    closeProjectInBackend,
  }
}

/** Default interval for refetching project status when the project is opened. */
const OPENED_INTERVAL_MS = 30_000
/**
 * Interval when we open a cloud project.
 * Since opening a cloud project is a long operation, we want to check the status less often.
 */
const CLOUD_OPENING_INTERVAL_MS = 2_500
/**
 * Interval when we open a local project or when we want to sync the project status as soon as
 * possible.
 */
const LOCAL_OPENING_INTERVAL_MS = 100

const DEFAULT_INTERVAL_MS = 120_000

const OPENING_PROJECT_STATES = new Set([
  BackendProjectState.provisioned,
  BackendProjectState.scheduled,
  BackendProjectState.openInProgress,
])
const STATIC_PROJECT_STATES = new Set([BackendProjectState.opened, BackendProjectState.closed])
const CREATED_PROJECT_STATES = new Set([BackendProjectState.created, BackendProjectState.new])
export const BUSY_PROJECT_STATES = new Set([
  ...Array.from(OPENING_PROJECT_STATES),
  BackendProjectState.opened,
  BackendProjectState.hybridOpened,
])

async function getProjectDetails(
  backend: Backend,
  id: ProjectId,
  scope: EffectScope,
  queryClient: vueQuery.QueryClient,
) {
  const isLocal = backend.type === BackendType.local

  const detailsQuery = scope.run(() =>
    vueQuery.useQuery(
      {
        queryKey: ['project', id] as const,
        queryFn: () => backend.getProjectDetails(id),
        refetchIntervalInBackground: true,
        refetchOnWindowFocus: true,
        refetchOnMount: true,
        networkMode: backend.type === BackendType.remote ? 'online' : 'always',
        meta: { persist: false },
        refetchInterval: (query): number | false => {
          const { state } = query

          const staticStates = STATIC_PROJECT_STATES

          const openingStates = OPENING_PROJECT_STATES

          const createdStates = CREATED_PROJECT_STATES

          if (state.status === 'error') {
            return false
          }

          if (state.data == null) {
            return false
          }

          const currentState = state.data.state.type

          if (isLocal) {
            if (createdStates.has(currentState)) {
              return LOCAL_OPENING_INTERVAL_MS
            }

            if (staticStates.has(state.data.state.type)) {
              return OPENED_INTERVAL_MS
            }

            if (openingStates.has(state.data.state.type)) {
              return LOCAL_OPENING_INTERVAL_MS
            }
          }

          if (createdStates.has(currentState)) {
            return CLOUD_OPENING_INTERVAL_MS
          }

          // Cloud project
          if (staticStates.has(state.data.state.type)) {
            return OPENED_INTERVAL_MS
          }
          if (openingStates.has(state.data.state.type)) {
            return CLOUD_OPENING_INTERVAL_MS
          }

          return DEFAULT_INTERVAL_MS
        },
      },
      queryClient,
    ),
  )
  assertDefined(detailsQuery)
  await detailsQuery.suspense()

  const runDetails = computed<ProjectDetails>((old) => {
    const data = detailsQuery.data.value ?? old
    assertDefined(data)
    return data
  })

  return {
    name: computed(() => runDetails.value.name),
    runDetails,
  }
}
