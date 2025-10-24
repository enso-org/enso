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
import { backendMutationOptions } from '@/composables/backend'
import { injectGuiConfig } from '@/providers/guiConfig'
import { assert, assertDefined } from '@/util/assert'
import { Err, Ok, rejectionToResult, type Result } from '@/util/data/result'
import * as vueQuery from '@tanstack/vue-query'
import { computed, effectScope, markRaw, onScopeDispose, type EffectScope, type Ref } from 'vue'
import { useBackends } from '../backends'
import { useSession } from '../session'
import { useText } from '../text'
import { useUploadsToCloudStore } from '../upload'
import { createGraphStore, type GraphStore } from './graph'
import { createModuleStore, type ModuleStore } from './module'
import { createProjectStore, type ProjectStore } from './project'
import type { ProjectInfo, RunningProjectInfo } from './projectInfo'
import { createProjectNameStore, type ProjectNameStore } from './projectNames'
import { createSuggestionDbStore, type SuggestionDbStore } from './suggestionDatabase'
import { WidgetRegistry } from './widgetRegistry'

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
  scope: EffectScope
  details?: Ref<ProjectDetails>
}

export interface Opened {
  status: 'opened'
  info: RunningProjectInfo
  runningId: ProjectId
  scope: EffectScope
  details?: Ref<ProjectDetails>
}

export interface Initialized {
  status: 'initialized'
  info: RunningProjectInfo
  runningId: ProjectId
  details: Ref<ProjectDetails>
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

export interface ToRestore {
  status: 'to-restore'
  info: RunningProjectInfo
}

export interface ClosedByBackend {
  status: 'closed-by-backend'
  info: RunningProjectInfo
  scope: EffectScope
  details: Ref<ProjectDetails>
}

export type ProjectState =
  | NotOpened
  | HybridOpened
  | HybridDownloaded
  | Opened
  | Initialized
  | HybridLocallyClosed
  | HybridUploaded
  | ToRestore
  | ClosedByBackend

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
  const renameProjectMut = renameProjectMutation(queryClient)

  const catchNetworkError = rejectionToResult(NetworkError)

  async function openProject(
    project: NotOpened & { info: ProjectInfo & { mode: 'hybrid' } },
  ): Promise<Result<HybridOpened>>
  async function openProject(
    project: NotOpened & { info: ProjectInfo & { mode: 'local' | 'cloud' } },
  ): Promise<Result<Opened>>
  async function openProject(project: NotOpened): Promise<Result<HybridOpened | Opened>>
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
          scope: effectScope(),
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
          scope: effectScope(),
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
      scope: effectScope(),
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
    return openLocalVersionOfHybridProjectByRunningInfo({
      ...project.info,
      runningId: localProjectAsset.id,
      localParentId: localProjectAsset.parentId,
    })
  }

  async function openLocalVersionOfHybridProjectByRunningInfo(
    info: RunningProjectInfo & { mode: 'hybrid' },
    scope: EffectScope = effectScope(),
    details?: Ref<ProjectDetails>,
  ) {
    if (!backends.localBackend) return Err('Cannot open local project: Local Backend missing.')
    const cloudParentPath = EnsoPath(info.ensoPath.slice(0, info.ensoPath.lastIndexOf('/')))
    const result = await catchNetworkError(
      backends.localBackend.openProject(
        info.runningId,
        {
          executeAsync: false,
          cognitoCredentials: null,
          openHybridProjectParameters: {
            cloudProjectDirectoryPath: cloudParentPath,
            cloudProjectId: info.id,
            cloudProjectSessionId: info.hybridSessionId,
          },
        },
        info.title,
      ),
    )
    if (!result.ok) return result
    return Ok({
      status: 'opened',
      info,
      runningId: info.runningId,
      scope,
      ...(details != null ? { details } : {}),
    })
  }

  async function initializeProject(project: Opened): Promise<Result<Initialized>> {
    const scope = project.scope
    const detailsResult =
      project.details ? Ok(project.details) : await getProjectDetails(project.info, scope)
    if (!detailsResult.ok) return detailsResult
    const details = detailsResult.value

    const runDetailsResult = await getRunningProjectDetails(project.info, scope, details)
    if (!runDetailsResult.ok) return runDetailsResult
    const runDetails = runDetailsResult.value

    return scope.run(() => {
      const runningId = project.info.mode === 'hybrid' ? project.info.runningId : project.info.id
      const projectNames = createProjectNameStore({
        projectNamespace: undefined, // TODO[ao]: we should get project's namespace from cloud. This never worked in old Editor.tsx
        projectDisplayedName: details.value.name,
        projectInitialName: runDetails.value.packageName,
      })
      const rpcUrl = runDetails.value.jsonAddress
      const dataUrl = runDetails.value.binaryAddress
      const ydocUrl = runDetails.value.ydocAddress ?? config.ydocUrl ?? ''
      assert(rpcUrl != null, text.getText('noJSONEndpointError'))
      assert(dataUrl != null, text.getText('noBinaryEndpointError'))
      const store = createProjectStore(
        {
          projectId: runningId,
          projectAssetId: project.info.id,
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
          status: 'initialized' as const,
          info: project.info,
          runningId,
          name: computed(() => details.value.name),
          details,
          runDetails,
          store,
          projectNames,
          suggestionDb,
          module,
          graph,
          widgetRegistry,
          scope,
        } satisfies Initialized),
      )
    })!
  }

  async function getProjectDetails(info: RunningProjectInfo, scope: EffectScope) {
    switch (info.mode) {
      case 'local': {
        if (backends.localBackend == null)
          return Err('Cannot get details of local project: no local backend.')
        return Ok(
          await getProjectDetailsFromBackend(backends.localBackend, info.id, scope, queryClient),
        )
      }
      case 'cloud':
      case 'hybrid': {
        return Ok(
          await getProjectDetailsFromBackend(backends.remoteBackend, info.id, scope, queryClient),
        )
      }
    }
  }

  async function getRunningProjectDetails(
    info: RunningProjectInfo,
    scope: EffectScope,
    details: Ref<ProjectDetails>,
  ) {
    switch (info.mode) {
      case 'local':
      case 'cloud':
        return Ok(details)
      case 'hybrid': {
        if (backends.localBackend == null)
          return Err('Cannot get details of hybrid project: no local backend.')
        return Ok(
          await getProjectDetailsFromBackend(
            backends.localBackend,
            info.runningId,
            scope,
            queryClient,
          ),
        )
      }
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

  async function uploadHybridProject(info: RunningProjectInfo & { mode: 'hybrid' }) {
    const fileName = 'project_root.enso-project'
    const file = await backends.remoteBackend.getProjectArchive(info.localParentId, fileName)
    await uploads.uploadFile(
      file,
      {
        fileId: info.id,
        fileName,
        parentDirectoryId: info.parentId,
      },
      'hybridSync',
    )
  }

  async function cleanupHybridProject(
    project: HybridLocallyClosed,
  ): Promise<Result<HybridUploaded>> {
    await uploadHybridProject(project.info)
    await deleteLocalVersionOfHybridProject(project.info)
    return Ok({
      status: 'hybrid-uploaded',
      info: project.info,
    })
  }

  async function deleteLocalVersionOfHybridProject(info: RunningProjectInfo & { mode: 'hybrid' }) {
    if (backends.localBackend == null) {
      console.error('Cannot delete Hybrid Project without local backend')
    } else {
      return backends.localBackend
        .deleteAsset(info.localParentId, { force: true }, null)
        .catch((err) => console.error('Failed to delete local version of hybrid project', err))
    }
  }

  async function closeHybridProject(
    project: HybridUploaded | HybridOpened | HybridDownloaded,
  ): Promise<Result<NotOpened>> {
    await closeRemoteProject.mutateAsync([project.info.id, project.info.title])
    return Ok({ status: 'not-opened', info: project.info })
  }

  function closeProjectInBackend(project: { id: ProjectId; title: string }, backend: BackendType) {
    if (backend === BackendType.local) {
      closeLocalProject.mutate([project.id, project.title])
    } else {
      closeRemoteProject.mutate([project.id, project.title])
    }
  }

  async function renameProject(
    project: Initialized,
    newName: string,
  ): Promise<Result<Initialized>> {
    const backend = project.info.mode === 'local' ? backends.localBackend : backends.remoteBackend
    if (backend == null) {
      return Err('Failed to rename project: no Backend available')
    }
    const result = await catchNetworkError(
      renameProjectMut.mutateAsync({ project: project.info, newName, backend }),
    )
    if (!result.ok) return result
    return Ok(markRaw({ ...project, info: { ...project.info, title: newName } }))
  }

  async function restoreProject(project: ToRestore): Promise<Result<Opened | ClosedByBackend>> {
    if (project.info.mode === 'local') {
      return openProject({ status: 'not-opened', info: project.info })
    }
    const scope = effectScope()
    const details = await getProjectDetails(project.info, scope)
    if (!details.ok) return details
    if (
      project.info.mode === 'hybrid' &&
      details.value.value.state.type === BackendProjectState.hybridOpened
    ) {
      return openLocalVersionOfHybridProjectByRunningInfo(project.info, scope, details.value)
    }
    if (
      project.info.mode === 'cloud' &&
      details.value.value.state.type === BackendProjectState.opened
    ) {
      return Ok({
        status: 'opened',
        info: project.info,
        runningId: project.info.id,
        scope,
        details: details.value,
      })
    }
    return Ok({
      status: 'closed-by-backend',
      info: project.info,
      scope,
      details: details.value,
    })
  }

  async function reopenProject(project: ClosedByBackend): Promise<Result<Opened>> {
    const opened = await openProject({ status: 'not-opened', info: project.info })
    if (!opened.ok) return opened
    if (opened.value.status === 'hybrid-opened') {
      assert(project.info.mode === 'hybrid')
      // skip the project downloading, as we already have local copy.
      return openLocalVersionOfHybridProjectByRunningInfo(
        project.info,
        project.scope,
        project.details,
      )
    } else {
      return Ok(opened.value)
    }
  }

  async function discardProject(project: ToRestore | ClosedByBackend): Promise<Result<NotOpened>> {
    if (project.info.mode === 'hybrid') {
      await deleteLocalVersionOfHybridProject(project.info)
      return closeHybridProject({ status: 'hybrid-opened', info: project.info })
    } else {
      return Ok({
        status: 'not-opened',
        info: project.info,
      })
    }
  }

  return {
    openProject,
    downloadHybridProject,
    openLocalVersionOfHybridProject,
    initializeProject,
    closeProject,
    uploadHybridProject,
    cleanupHybridProject,
    closeHybridProject,
    closeProjectInBackend,
    renameProject,
    restoreProject,
    reopenProject,
    discardProject,
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

async function getProjectDetailsFromBackend(
  backend: Backend,
  id: ProjectId,
  scope: EffectScope,
  queryClient: vueQuery.QueryClient,
) {
  const isLocal = backend.type === BackendType.local

  const detailsQuery = scope.run(() =>
    vueQuery.useQuery(
      {
        queryKey: getProjectDetailsQueryKey(id),
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

  const details = computed<ProjectDetails>((old) => {
    const data = detailsQuery.data.value ?? old
    assertDefined(data)
    return data
  })

  return details
}

function getProjectDetailsQueryKey(id: ProjectId) {
  return ['project', id] as const
}

function renameProjectMutation(queryClient: vueQuery.QueryClient) {
  return vueQuery.useMutation(
    {
      mutationKey: ['renameProject'],
      mutationFn: ({
        project,
        backend,
        newName,
      }: {
        project: ProjectInfo
        backend: Backend
        newName: string
      }) => {
        const { id, title } = project

        return backend.updateProject(id, { projectName: newName }, title)
      },
      onMutate: async ({ project, newName }) => {
        const queryKey = getProjectDetailsQueryKey(project.id)
        await queryClient.cancelQueries({ queryKey })
        // Optimistically update the project name.
        queryClient.setQueryData<ProjectDetails>(queryKey, (data) => {
          if (data == null) return undefined
          return {
            ...data,
            name: newName,
          }
        })

        return { queryKey }
      },
      onError: (_err, _variables, context) => {
        if (context?.queryKey) {
          // Invalidate the optimistic response.
          return queryClient.invalidateQueries({ queryKey: context.queryKey })
        }
      },
      meta: {
        invalidates: [['listDirectory'], ['project'], ['getAssetDetails']],
        awaitInvalidates: true,
      },
    },
    queryClient,
  )
}
