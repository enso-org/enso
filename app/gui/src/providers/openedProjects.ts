import {
  IS_OPENING as BACKEND_IS_OPENING,
  IS_OPENING_OR_OPENED as BACKEND_IS_OPENING_OR_OPENED,
  BackendType,
  Plan,
  ProjectId,
  type ProjectAsset,
} from '#/services/Backend'
import { assert } from '@/util/assert'
import { createGlobalState } from '@vueuse/core'
import { isOnElectron } from 'enso-common/src/detect'
import { computed, ref, shallowReactive, watchEffect } from 'vue'
import type { Result, ResultError } from 'ydoc-shared/util/data/result'
import { useAuth } from './auth'
import { useBackends } from './backends'
import { useFeatureFlag } from './featureFlags'
import {
  useProjectStates,
  type ProjectInfo,
  type ProjectState,
} from './openedProjects/projectStates'

const PROCESS_ABORTED = 'aborted'

export type Process = 'opening' | 'closing'

export interface Project {
  state: ProjectState
  nextTask:
    | {
        abort: AbortController
        process: Process
        promise: Promise<Result<ProjectState>>
      }
    | undefined
  error: ResultError | Error | undefined
}

/**
 * A type for Opened Project Store.
 */
export type OpenedProjectsStore = ReturnType<typeof useOpenedProjects>

export function createOpenedProjectsStore() {
  const auth = useAuth()
  const projects = shallowReactive(new Map<ProjectId, Project>())
  const enableCloudExecution = useFeatureFlag('enableCloudExecution')
  const projectStates = useProjectStates()
  const backends = useBackends()
  const closingOnAppExit = ref(false)
  const projectReadyCallbacks: ((project: Project) => void)[] = []

  watchEffect(
    () =>
      console.debug(
        'PROJECTS',
        [...projects.values()].map((proj) => proj.state),
      ),
    { flush: 'sync' },
  )
  watchEffect(
    () =>
      console.debug(
        'PROJECTS TASKS',
        [...projects.values()].map((proj) => proj.nextTask),
      ),
    { flush: 'sync' },
  )

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
          enableCloudExecution &&
          (auth.session?.user.plan === Plan.team || auth.session?.user.plan === Plan.enterprise)
        ) ?
          ('cloud' as const)
        : null,
    },
  }))

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
    projects.set(info.id, project)
    performProcess(project, 'opening')
    return project
  }

  function canOpenProjectLocally(backend: BackendType) {
    return modesForBackend.value.locally[backend] != null
  }

  function openProjectLocally(info: Omit<ProjectInfo, 'mode'>, backend: BackendType) {
    const mode = modesForBackend.value.locally[backend]
    if (mode != null) {
      return openProject({ ...info, mode })
    }
  }

  function canOpenProjectNatively(backend: BackendType) {
    return modesForBackend.value.natively[backend] != null
  }

  function openProjectNatively(info: Omit<ProjectInfo, 'mode'>, backend: BackendType) {
    const mode = modesForBackend.value.natively[backend]
    if (mode != null) {
      return openProject({ ...info, mode })
    }
  }

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

  function closeAllProjects() {
    for (const id of projects.keys()) closeProject(id)
  }

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
        let promise
        switch (project.state.status) {
          case 'not-opened':
            if (process === 'opening') promise = projectStates.openProject(project.state)
            break
          case 'hybrid-opened':
            promise =
              process === 'opening' ?
                projectStates.downloadHybridProject(project.state)
              : projectStates.closeHybridProject(project.state)
            break
          case 'hybrid-downloaded':
            promise =
              process === 'opening' ?
                (promise = projectStates.openLocalVersionOfHybridProject(
                  project.state,
                  abort.signal,
                ))
              : projectStates.closeHybridProject(project.state)
            break
          case 'opened':
            promise =
              process === 'opening' ?
                projectStates.initializeProject(project.state)
              : projectStates.closeProject(project.state)
            break
          case 'initialized':
            if (process === 'closing') promise = projectStates.closeProject(project.state)
            break
          case 'hybrid-closed':
            promise = projectStates.uploadHybridProject(project.state)
            break
          case 'hybrid-uploaded':
            promise = projectStates.closeHybridProject(project.state)
            break
        }
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

  async function waitForProcess(project: Project) {
    while (project.nextTask != null) {
      await project.nextTask.promise.catch((err) =>
        console.log('Waited-for process resulted in error', err),
      )
    }
  }

  window.addEventListener('beforeunload', async (event) => {
    const hybrids = [...projects.values()].filter(
      (proj) =>
        proj.state.info.mode === 'hybrid' &&
        (proj.state.status === 'initialized' ||
          proj.state.status === 'hybrid-closed' ||
          proj.state.status === 'hybrid-uploaded'),
    )
    if (hybrids.length > 0) {
      event.preventDefault()
      // Browsers have their own `beforeunload` handling.
      if (!isOnElectron()) return
      closingOnAppExit.value = true
      const errors = (
        await Promise.all(
          hybrids.map(async (project) => {
            closeProject(project.state.info.id)
            await waitForProcess(project)
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

  function get(id: ProjectId): Project | undefined {
    return projects.get(id)
  }

  function listProjects() {
    return projects.values()
  }

  function isProjectOpening(asset: ProjectAsset) {
    const openedByMe = projects.get(asset.id)
    if (openedByMe != null) {
      return openedByMe.nextTask?.process === 'opening'
    } else {
      return BACKEND_IS_OPENING[asset.projectState.type]
    }
  }

  function isProjectOpened(asset: ProjectAsset) {
    const openedByMe = projects.get(asset.id)
    console.debug(
      '>>>',
      openedByMe != null,
      openedByMe?.nextTask == null,
      openedByMe?.state.status !== 'not-opened',
      openedByMe?.state.status === 'initialized',
      asset.projectState.type,
    )
    if (openedByMe != null && openedByMe.state.status !== 'not-opened') {
      return openedByMe.nextTask == null && openedByMe.state.status === 'initialized'
    } else {
      return (
        !BACKEND_IS_OPENING[asset.projectState.type] &&
        BACKEND_IS_OPENING_OR_OPENED[asset.projectState.type]
      )
    }
  }

  function isProjectClosing(id: ProjectId) {
    return projects.get(id)?.nextTask?.process === 'closing'
  }

  function onProjectReady(cb: (project: Project) => void) {
    projectReadyCallbacks.push(cb)
    return () => projectReadyCallbacks.splice(projectReadyCallbacks.indexOf(cb), 1)
  }

  return {
    openProject,
    canOpenProjectLocally,
    openProjectLocally,
    canOpenProjectNatively,
    openProjectNatively,
    closeProject,
    closeAllProjects,
    get,
    listProjects,
    isProjectOpening,
    isProjectOpened,
    isProjectClosing,
    waitForProcess,
    closingOnAppExit: closingOnAppExit,
    onProjectReady,
  }
}

export const useOpenedProjects = createGlobalState(createOpenedProjectsStore)
