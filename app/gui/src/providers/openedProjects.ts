import Backend, {
  AssetType,
  BackendType,
  EnsoPath,
  ProjectId,
  ProjectSessionId,
  ProjectState,
  type ProjectAsset,
  type Project as ProjectDetails,
} from '#/services/Backend'
import { createGraphStore, type GraphStore } from '$/providers/openedProjects/graph'
import { createProjectStore, type ProjectStore } from '$/providers/openedProjects/project'
import {
  createProjectNameStore,
  type ProjectNameStore,
} from '$/providers/openedProjects/projectNames'
import {
  createSuggestionDbStore,
  type SuggestionDbStore,
} from '$/providers/openedProjects/suggestionDatabase'
import { WidgetRegistry } from '$/providers/openedProjects/widgetRegistry'
import { createContextStore } from '@/providers'
import { injectGuiConfig } from '@/providers/guiConfig'
import { assert, assertDefined } from '@/util/assert'
import { useToast } from '@/util/toast'
import * as vueQuery from '@tanstack/vue-query'
import { computed, EffectScope, effectScope, shallowReactive, toValue, type Ref } from 'vue'
import { useBackends } from './backends'
import { createModuleStore } from './openedProjects/module'
import { useSession } from './session'
import { useText } from './text'

type RunningMode = 'local' | 'cloud' | 'hybrid'

export interface LsUrls {
  rpcUrl: string
  dataUrl: string
  ydocUrl: string
}

export interface Task<NextState> {
  abort: AbortController
  promise: Promise<NextState>
}

export interface Requested {
  status: 'opening'
  mode: RunningMode
  id: ProjectId
  task: Task<HybridDownloaded | Opened>
}

export interface HybridDownloaded extends Omit<Requested, 'status' | 'mode' | 'task'> {
  status: 'hybrid-opening'
  mode: 'hybrid'
  runningId: ProjectId
  hybridSessionId: ProjectSessionId
  task: Task<Opened>
}

export interface Opened extends Omit<Requested | HybridDownloaded, 'status' | 'task'> {
  status: 'initializing'
  runningId: ProjectId
  runDetails: Ref<ProjectDetails>
  name: Ref<string>
  scope: EffectScope
  task: Task<Initialized>
}

export interface Initialized extends Omit<Opened, 'status' | 'task'> {
  status: 'running'
  store: ProjectStore
  projectNames: ProjectNameStore
  suggestionDb: SuggestionDbStore
  module: ModuleStore
  graph: GraphStore
  widgetRegistry: WidgetRegistry
}

export interface Dismissed extends Omit<Initialized, 'status'> {
  status: 'closing'
  closingTask: Promise<void>
}

type LaunchedProject = Requested | HybridDownloaded | Opened | Initialized | Dismissed

/**
 * A type for Opened Project Store.
 */
export type OpenedProjectsStore = ReturnType<typeof useOpenedProjects>

/**
 * Opened Projects Store
 *
 * This store maintains all "substores" of opened projects. When an opened project registers,
 * the names, project, suggestionDb, graph and widgetRegistry stores are created and available
 * through `get` method.
 *
 * See also `WithCurrentProject` component which allows setting one opened project as "default"
 * for component subtree.
 */
export const [provideOpenedProjects, useOpenedProjects] = createContextStore(
  'opened-projects',
  () => {
    const projects = shallowReactive(new Map<ProjectId, LaunchedProject>())
    const states = useProjectStates()
    const errorToast = useToast.error()

    function openProject(id: ProjectId, path: EnsoPath, mode: RunningMode, title: string) {
      const newProjectState = states.openProject(id, path, mode, title)
      projects.set(id, newProjectState)
      // Do not await updates.
      setupStateUpdates(newProjectState)
      return computed(() => projects.get(id))
    }

    async function setupStateUpdates(project: LaunchedProject, processDesc: string) {
      try {
        while ('task' in project) {
          project = await project.task.promise
          projects.set(project.id, project)
        }
      } catch (error) {
        errorToast.show(`Failed to ${processDesc}: ${error}`)
        closeProject(project.id)
      }
    }

    async function closeProject(id: ProjectId) {
      const state = projects.get(id)
      if (state == null) return
      switch (state.status) {
        case 'opening':
        case 'hybrid-opening':
        case 'initializing':
        case 'running':

        case 'closing':
          return
      }
    }

    function get(id: ProjectId): LaunchedProject | undefined {
      return projects.get(id)
    }

    function listIds() {
      return projects.keys()
    }

    return {
      openProject,
      get,
      listIds,
    }
  },
)

function useProjectStates() {
  const backends = useBackends()
  const session = useSession()
  const text = useText()
  const config = injectGuiConfig()

  function openProject(id: ProjectId, path: EnsoPath, mode: RunningMode, title: string): Requested {
    if (session.session == null) throw Error('No user session')
    const cognitoCredentials = {
      accessToken: session.session.accessToken,
      refreshToken: session.session.refreshToken,
      clientId: session.session.clientId,
      expireAt: session.session.expireAt,
      refreshUrl: session.session.refreshUrl,
    }
    return {
      status: 'opening',
      mode,
      id,
      openTask: (async () => {
        const scope = effectScope()
        switch (mode) {
          case 'local': {
            if (!backends.localBackend)
              throw Error('Cannot open local project: Local Backend missing.')
            await backends.localBackend.openProject(
              id,
              {
                executeAsync: false,
                cognitoCredentials: null,
                openHybridProjectParameters: null,
              },
              title,
            )
            const project = {
              status: 'initializing' as const,
              mode,
              id,
              runningId: id,
              ...(await getProjectDetails(backends.localBackend, id, scope)),
              scope,
            }
            return { ...project, initializeTask: initializeProject(project) }
          }
          case 'cloud': {
            backends.remoteBackend.openProject(
              id,
              {
                executeAsync: false,
                cognitoCredentials,
                openHybridProjectParameters: null,
              },
              title,
            )
            const project = {
              status: 'initializing' as const,
              mode,
              id,
              runningId: id,
              ...(await getProjectDetails(backends.remoteBackend, id, scope)),
              scope,
            }
            return { ...project, initializeTask: initializeProject(project) }
          }
          case 'hybrid': {
            if (!backends.localBackend)
              throw Error('Cannot open hybrid project: Local Backend missing.')
            const hybridSessionId = await backends.remoteBackend.setHybridOpenInProgress(id, title)
            const localProject = await backends.remoteBackend.downloadProject(id)
            let localProjectAsset: ProjectAsset | undefined
            // TODO[ao]: Apparently, the only way to get local project id is to list directory, because
            // "downloadProject" does not return it. To discuss.
            for (const parentId of [localProject.parentId, localProject.projectRootId]) {
              const { assets } = await backends.localBackend.listDirectory({
                parentId: parentId,
                filterBy: null,
                labels: null,
                sortExpression: null,
                sortDirection: null,
                from: null,
                pageSize: null,
                recentProjects: false,
              })
              localProjectAsset = assets.filter((item) => item.type === AssetType.project).at(0)
              if (localProjectAsset) {
                break
              }
            }
            if (!localProjectAsset) throw Error('Cannot find downloaded local project.')
            const project = {
              status: 'hybrid-opening' as const,
              mode,
              id,
              hybridSessionId,
              runningId: localProjectAsset.id,
            }
            const cloudParentPath = EnsoPath(path.slice(0, path.lastIndexOf('/')))
            return {
              ...project,
              openLocalTask: openLocalVersionOfHybridProject(project, cloudParentPath, title),
            }
          }
        }
      })(),
    }
  }

  async function openLocalVersionOfHybridProject(
    project: Omit<HybridDownloaded, 'openLocalTask'>,
    cloudParentPath: EnsoPath,
    title: string,
  ): Promise<Opened> {
    if (!backends.localBackend) throw Error('Cannot open local project: Local Backend missing.')
    const scope = effectScope()
    await backends.localBackend.openProject(
      project.runningId,
      {
        executeAsync: false,
        cognitoCredentials: null,
        openHybridProjectParameters: {
          cloudProjectDirectoryPath: cloudParentPath,
          cloudProjectId: project.id,
          cloudProjectSessionId: project.hybridSessionId,
        },
      },
      title,
    )
    const [localDetails, cloudDetails] = await Promise.all([
      getProjectDetails(backends.localBackend, project.runningId, scope),
      getProjectDetails(backends.remoteBackend, project.id, scope),
    ])
    const next = {
      ...project,
      status: 'initializing' as const,
      runDetails: localDetails.runDetails,
      name: cloudDetails.name,
      scope,
    }
    return { ...next, initializeTask: initializeProject(next) }
  }

  async function initializeProject(project: Omit<Opened, 'initializeTask'>): Promise<Initialized> {
    return project.scope.run(() => {
      const names = createProjectNameStore({
        projectNamespace: undefined, // TODO[ao]: we should get project's namespace from cloud. This never worked in old Editor.tsx
        projectDisplayedName: project.name,
        projectInitialName: project.runDetails.value.packageName,
      })
      const rpcUrl = project.runDetails.value.jsonAddress
      const dataUrl = project.runDetails.value.binaryAddress
      const ydocUrl = project.runDetails.value.ydocAddress ?? config.ydocUrl ?? ''
      assert(rpcUrl != null, text.getText('noJSONEndpointError'))
      assert(dataUrl != null, text.getText('noBinaryEndpointError'))
      const store = createProjectStore(
        {
          projectId: project.runningId,
          renameProject: toValue(props.renameProject),
          engine: {
            rpcUrl,
            dataUrl,
            ydocUrl,
          },
        },
        names,
      )
      const suggestionDb = createSuggestionDbStore(store, names)
      const module = createModuleStore(store, names, suggestionDb)
      const graph = createGraphStore(store, suggestionDb, names, module)
      const widgetRegistry = new WidgetRegistry(graph.db)
      return {
        status: 'running',
        store,
        names,
        suggestionDb,
        graph,
        widgetRegistry,
        mode: 'local',
        id: project.id,
        runningId: project.runningId,
        runDetails: project.runDetails,
        name: project.name,
        scope: project.scope,
      }
    })!
  }

  function closeProject(project: Initialized) {}

  return {
    openProject,
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

export const OPENING_PROJECT_STATES = new Set([
  ProjectState.provisioned,
  ProjectState.scheduled,
  ProjectState.openInProgress,
])
export const OPENED_PROJECT_STATES = new Set([ProjectState.opened])
export const CLOSED_PROJECT_STATES = new Set([ProjectState.closed])
export const STATIC_PROJECT_STATES = new Set([ProjectState.opened, ProjectState.closed])
export const CREATED_PROJECT_STATES = new Set([ProjectState.created, ProjectState.new])
export const BUSY_PROJECT_STATES = new Set([
  ...Array.from(OPENING_PROJECT_STATES),
  ProjectState.opened,
  ProjectState.hybridOpened,
])

async function getProjectDetails(backend: Backend, id: ProjectId, scope: EffectScope) {
  const isLocal = backend.type === BackendType.local

  const detailsQuery = scope.run(() =>
    vueQuery.useQuery({
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
    }),
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
