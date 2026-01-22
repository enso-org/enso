import LocalStorage from '#/utilities/LocalStorage'
import { assert } from '@/util/assert'
import { createGlobalState } from '@vueuse/core'
import {
  IS_OPENING as BACKEND_IS_OPENING,
  IS_OPENING_OR_OPENED as BACKEND_IS_OPENING_OR_OPENED,
  BackendType,
  Plan,
  ProjectId,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'
import { Err, Ok, type Result, type ResultError } from 'enso-common/src/utilities/data/result'
import { isOnElectron } from 'enso-common/src/utilities/detect'
import { computed, ref, shallowReactive, watchEffect } from 'vue'
import * as z from 'zod'
import { useAuth } from './auth'
import { useBackends } from './backends'
import { useFeatureFlag } from './featureFlags'
import {
  RUNNING_PROJECT_INFO_SCHEMA,
  type ProjectInfo,
  type RunningProjectInfo,
} from './openedProjects/projectInfo'
import {
  useProjectStates,
  type HybridLocallyClosed,
  type Initialized,
  type ProjectState,
} from './openedProjects/projectStates'

/** A reason given to AbortController when opening, restoring or closing process is being aborted. */
const PROCESS_ABORTED = 'aborted'

/** A type of process */
export type Process = 'opening' | 'closing' | 'restoring'

/** A project opened by this app instance. */
export interface Project {
  /** Project state. See {@link ProjectState} for all possible project states. */
  state: ProjectState
  /** A opening, closing or restoring step which is currently performed on given state. */
  nextTask:
    | {
        abort: AbortController
        process: Process
        promise: Promise<Result<ProjectState>>
      }
    | undefined
  /** Error of the last unsuccessful step of opening, closing or restoring. */
  error: ResultError | Error | undefined
}

LocalStorage.registerKey('openedTabs', { schema: z.array(RUNNING_PROJECT_INFO_SCHEMA) })

/**
 * A type for Opened Project Store.
 */
export type OpenedProjectsStore = ReturnType<typeof useOpenedProjects>

/**
 * Constructor of Opened Project Store.
 */
export function createOpenedProjectsStore() {
  const auth = useAuth()
  const projects = shallowReactive(new Map<ProjectId, Project>())
  const enableCloudExecution = useFeatureFlag('enableCloudExecution')
  const projectStates = useProjectStates()
  const backends = useBackends()
  const localStorage = LocalStorage.getInstance()
  const closingOnAppExit = ref(false)
  const projectReadyCallbacks: ((project: Project) => void)[] = []

  /** Whether the user can run projects. */
  const modesForBackend = computed(() => ({
    locally: {
      [BackendType.local]: backends.localBackend != null ? ('local' as const) : null,
      [BackendType.remote]: backends.localBackend != null ? ('hybrid' as const) : null,
    },
    // Local projects can be run natively; only Team plans and above have access to Cloud execution.
    // Local projects: Open normally
    // Cloud projects: Open in Cloud VM
    natively: {
      [BackendType.local]: backends.localBackend != null ? ('local' as const) : null,
      [BackendType.remote]:
        (
          enableCloudExecution.value &&
          (auth.session?.user.plan === Plan.team || auth.session?.user.plan === Plan.enterprise)
        ) ?
          ('cloud' as const)
        : null,
    },
  }))

  /** Open project. */
  function openProject(info: ProjectInfo) {
    const project =
      projects.get(info.id) ??
      shallowReactive({
        state: {
          status: 'not-opened' as const,
          info,
        },
        nextTask: undefined,
        error: undefined,
      })
    if (project.state.status === 'not-opened') {
      project.state.info = info
    }
    projects.set(info.id, project)
    performProcess(project, 'opening')
    return project
  }

  /** Checks if project with given backend type may be opened locally. */
  function canOpenProjectLocally(backend: BackendType) {
    return modesForBackend.value.locally[backend] != null
  }

  /** Open project locally, by asset data and backend type. */
  function openProjectLocally(info: Omit<ProjectInfo, 'mode'>, backend: BackendType) {
    const mode = modesForBackend.value.locally[backend]
    if (mode != null) {
      return openProject({ ...info, mode })
    }
  }

  /** Checks if project with given backend type may be opened natively. */
  function canOpenProjectNatively(backend: BackendType) {
    return modesForBackend.value.natively[backend] != null
  }

  /** Open project natively, by asset data and backend type. */
  function openProjectNatively(info: Omit<ProjectInfo, 'mode'>, backend: BackendType) {
    const mode = modesForBackend.value.natively[backend]
    if (mode != null) {
      return openProject({ ...info, mode })
    }
  }

  /**
   * Restore a project which was running on app close.
   *
   * The project may be opened or put in "closed by backend" state.
   *
   * In case of hybrid projects, the local version is assumed to be already downloaded.
   */
  function restoreProject(info: RunningProjectInfo) {
    const existing = projects.get(info.id)
    if (
      existing == null ||
      (existing?.nextTask == null && existing?.state.status === 'not-opened')
    ) {
      const project = shallowReactive({
        state: { status: 'to-restore' as const, info },
        nextTask: undefined,
        error: undefined,
      })
      projects.set(info.id, project)
      performProcess(project, 'restoring')
    } else {
      performProcess(existing, 'restoring')
    }
  }

  /**
   * Close given project. If `backendProject` is provided, the project will be closed even if not
   * running by current app instance.
   */
  function closeProject(
    id: ProjectId,
    backendProject?: { asset: ProjectAsset; backendType: BackendType },
  ) {
    const project = projects.get(id)
    if (project == null) {
      if (backendProject == null) {
        console.warn('Cannot close project: project not opened')
      } else {
        projectStates.closeProjectInBackend(backendProject.asset, backendProject.backendType)
      }

      return
    }
    performProcess(project, 'closing')
  }

  /** Close all projects run by this app instance. */
  function closeAllProjects() {
    for (const id of projects.keys()) closeProject(id)
  }

  /** Rename a running project. */
  async function renameProject(id: ProjectId, newName: string) {
    const project = projects.get(id)
    if (project?.state.status !== 'initialized') return Err('Cannot rename non-running project')
    const renamed = await projectStates.renameProject(project.state, newName)
    if (!renamed.ok) return renamed
    return Ok()
  }

  /** Perform a process across mulitple project states until finished or errored. */
  async function performProcess(project: Project, process: Process) {
    const abort = new AbortController()
    if (project.nextTask != null) {
      if (project.nextTask.process === process) {
        return
      } else {
        project.nextTask.abort.abort(PROCESS_ABORTED)
        project.nextTask = undefined
      }
    }
    DEV: assert(project.nextTask == null)
    try {
      do {
        if (project.nextTask != null) {
          const result: Result<ProjectState> = await project.nextTask.promise
          abort.signal.throwIfAborted()
          if (result.ok) {
            project.state = result.value
            if (project.state.status === 'initialized') {
              for (const cb of projectReadyCallbacks) {
                cb(project)
              }
            }
          } else {
            project.error = result.error
            project.nextTask = undefined
            break
          }
        }
        project.error = undefined
        const promise = PROCESS_STEPS[process](project.state, abort.signal)
        project.nextTask = promise ? { abort, promise, process } : undefined
      } while (project.nextTask != null)
    } catch (err) {
      if (err === PROCESS_ABORTED) {
        console.log(`${process} process aborted.`)
      } else {
        console.error(`${process} process interrupted by error.`, { cause: err })
        project.error = Error(`${process} process interrupted by error.`, { cause: err })
      }
    }
  }

  const PROCESS_STEPS: {
    [K in Process]: (
      state: ProjectState,
      abort: AbortSignal,
    ) => Promise<Result<ProjectState>> | undefined
  } = {
    opening: (state: ProjectState, abort: AbortSignal) => {
      switch (state.status) {
        case 'not-opened':
          return projectStates.openProject(state)
        case 'hybrid-opened':
          return projectStates.downloadHybridProject(state)
        case 'hybrid-downloaded':
          return projectStates.openLocalVersionOfHybridProject(state, abort)
        case 'opened':
          return projectStates.initializeProject(state)
        case 'initialized':
          break
        case 'hybrid-closed':
        case 'hybrid-uploaded':
          return projectStates.reopenLocalVersionOfHybridProject(state)
        case 'to-restore':
          return projectStates.restoreProject(state)
        case 'closed-by-backend':
          return projectStates.reopenProject(state)
      }
    },
    closing: (state: ProjectState) => {
      switch (state.status) {
        case 'not-opened':
          break
        case 'hybrid-opened':
          return projectStates.closeHybridProject(state)
        case 'hybrid-downloaded':
          return projectStates.closeHybridProject(state)
        case 'opened':
        case 'initialized':
          return projectStates.closeProject(state)
        case 'hybrid-closed':
          return projectStates.uploadHybridProjectOnClose(state)
        case 'hybrid-uploaded':
          return projectStates.closeHybridProject(state)
        case 'to-restore':
        case 'closed-by-backend':
          return projectStates.discardProject(state)
      }
    },
    restoring: (state: ProjectState, abort: AbortSignal) => {
      if (state.status !== 'closed-by-backend') return PROCESS_STEPS.opening(state, abort)
    },
  }

  /** Wait for the current project's process finish (including failure). */
  async function waitForProcess(project: Project) {
    while (project.nextTask != null) {
      await project.nextTask.promise.catch((err) =>
        console.log('Waited-for process resulted in error', err),
      )
    }
  }

  /** Get data of project with given id. */
  function get(id: ProjectId): Project | undefined {
    return projects.get(id)
  }

  /** List all projects opened by current app instance. */
  function listProjects() {
    return projects.values()
  }

  /** Check if given asset is in process of opening, either by us or in backend only. */
  function isProjectOpening(asset: ProjectAsset) {
    const openedByMe = projects.get(asset.id)
    if (openedByMe != null) {
      return openedByMe.nextTask?.process === 'opening'
    } else {
      return BACKEND_IS_OPENING[asset.projectState.type]
    }
  }

  /** Check if given asset is opened, either by us or in backend only. */
  function isProjectOpened(asset: ProjectAsset) {
    const openedByMe = projects.get(asset.id)
    if (openedByMe != null && openedByMe.state.status !== 'not-opened') {
      return openedByMe.nextTask == null && openedByMe.state.status === 'initialized'
    } else {
      return (
        !BACKEND_IS_OPENING[asset.projectState.type] &&
        BACKEND_IS_OPENING_OR_OPENED[asset.projectState.type]
      )
    }
  }

  /** Check if given asset is in process of closing, either by us or in backend only. */
  function isProjectClosing(id: ProjectId) {
    return projects.get(id)?.nextTask?.process === 'closing'
  }

  /** Register a callback fired every time a project changes it status to 'initialized'. */
  function onProjectReady(cb: (project: Project) => void) {
    projectReadyCallbacks.push(cb)
    return () => projectReadyCallbacks.splice(projectReadyCallbacks.indexOf(cb), 1)
  }

  /**
   * Read and restore projects from local storage, and then keep the storage up-to-date about
   * currently opened projects.
   */
  function syncWithLocalStorage() {
    for (const project of localStorage.get('openedTabs') ?? []) {
      restoreProject(project)
    }

    return watchEffect(() => {
      const openedTabs: RunningProjectInfo[] = []
      for (const project of projects.values()) {
        switch (project.state.status) {
          case 'opened':
          case 'initialized':
          case 'hybrid-closed':
          case 'to-restore':
          case 'closed-by-backend':
            openedTabs.push(project.state.info)
            break
        }
      }
      localStorage.set('openedTabs', openedTabs)
    })
  }

  // A handler for uploading hybrid projects before app close.
  //
  // They are not removed from local backend, but synchronized with remote in case someone else
  // would open it in the meantime.
  window.addEventListener('beforeunload', async (event) => {
    const hybridsToUpload = [...projects.values()].filter(
      (proj): proj is Project & { state: Initialized | HybridLocallyClosed } =>
        (proj.state.status === 'initialized' || proj.state.status === 'hybrid-closed') &&
        proj.state.info.mode === 'hybrid' &&
        !proj.state.info.synced,
    )
    if (hybridsToUpload.length > 0) {
      // Do not prevent default in browsers.
      // In "real" browsers users will be unable to run hybrid projects anyway, but in dev
      // servers the "data loss" messages are annoying.
      if (!isOnElectron()) return
      event.preventDefault()
      closingOnAppExit.value = true
      const errors = (
        await Promise.all(
          hybridsToUpload.map(async (project) => {
            assert(project.state.info.mode === 'hybrid')
            await projectStates.uploadHybridProject(project.state.info)
            return project
          }),
        )
      ).filter((proj) => proj.error != null)
      closingOnAppExit.value = false
      if (errors.length == 0) {
        window.close()
      }
    } else {
      // Do the project cleanup, but do not close projects entirely.
      // Local projects's PM process will be killed anyway, and
      // Cloud projects should be kept opened.
      for (const project of projects.values()) {
        if (project.state.status === 'initialized') {
          project.state.scope.stop()
        }
      }
    }
  })

  return {
    openProject,
    canOpenProjectLocally,
    openProjectLocally,
    canOpenProjectNatively,
    openProjectNatively,
    closeProject,
    closeAllProjects,
    renameProject,
    get,
    listProjects,
    isProjectOpening,
    isProjectOpened,
    isProjectClosing,
    waitForProcess,
    closingOnAppExit: closingOnAppExit,
    onProjectReady,
    syncWithLocalStorage,
  }
}

/**
 * A store containing states of all projects opened by this app instance.
 */
export const useOpenedProjects = createGlobalState(createOpenedProjectsStore)
