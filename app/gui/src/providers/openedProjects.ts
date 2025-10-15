import { BackendType, Plan, ProjectId } from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import { assert } from '@/util/assert'
import { createGlobalState } from '@vueuse/core'
import { computed, ref, shallowReactive } from 'vue'
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
  const localStorage = LocalStorage.getInstance()
  const projectStates = useProjectStates()
  const backends = useBackends()
  const closePrevented = ref(false)

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

  function closeProject(id: ProjectId) {
    const project = projects.get(id)
    if (project == null) {
      console.warn('Cannot close project: project not opened')
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
          const result = await project.nextTask.promise
          if (result.ok) {
            project.state = result.value
          } else {
            project.error = result.error
            project.nextTask = undefined
            break
          }
        }
        abort.signal.throwIfAborted()
        let promise
        switch (project.state.status) {
          case 'not-opened':
            if (process === 'opening') promise = projectStates.openProject(project.state)
            break
          case 'hybrid-opened':
            promise = projectStates.downloadHybridProject(project.state)
            break
          case 'hybrid-downloaded':
            promise = projectStates.openLocalVersionOfHybridProject(project.state, abort.signal)
            break
          case 'opened':
            promise = projectStates.initializeProject(project.state)
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
      closePrevented.value = true
      event.preventDefault()
      const errors = (
        await Promise.all(
          hybrids.map(async (project) => {
            closeProject(project.state.info.id)
            await waitForProcess(project)
            return project
          }),
        )
      ).filter((proj) => proj.error != null)
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

  // TODO: add saving/restoring from local storage here.

  function get(id: ProjectId): Project | undefined {
    return projects.get(id)
  }

  function listProjects() {
    return projects.values()
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
    waitForProcess,
    closePrevented,
  }
}

export const useOpenedProjects = createGlobalState(createOpenedProjectsStore)
