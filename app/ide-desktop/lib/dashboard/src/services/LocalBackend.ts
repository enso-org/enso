/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link LocalBackend} in this module corresponds to an API endpoint.
 * The functions are asynchronous and return a {@link Promise} that resolves to the response from
 * the API. */
import * as detect from 'enso-common/src/detect'

import Backend, * as backend from '#/services/Backend'

import * as errorModule from '#/utilities/error'
import * as projectManager from '#/utilities/ProjectManager'
import ProjectManager from '#/utilities/ProjectManager'

// =============================
// === ipWithSocketToAddress ===
// =============================

/** Convert a {@link projectManager.IpWithSocket} to a {@link backend.Address}. */
function ipWithSocketToAddress(ipWithSocket: projectManager.IpWithSocket) {
  return backend.Address(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

// ====================
// === LocalBackend ===
// ====================

/** Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard. */
export default class LocalBackend extends Backend {
  static currentlyOpeningProjectId: backend.ProjectId | null = null
  static currentlyOpenProjects = new Map<projectManager.ProjectId, projectManager.OpenProject>()
  readonly type = backend.BackendType.local
  private readonly projectManager: ProjectManager

  /** Create a {@link LocalBackend}. */
  constructor(projectManagerUrl: string | null) {
    super()
    this.projectManager = ProjectManager.default(projectManagerUrl)
    if (detect.IS_DEV_MODE) {
      // @ts-expect-error This exists only for debugging purposes. It does not have types
      // because it MUST NOT be used in this codebase.
      window.localBackend = this
    }
  }

  /** Close the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails. */
  async getProjectDetails(
    projectId: backend.ProjectId,
    title: string | null
  ): Promise<backend.Project> {
    const cachedProject = LocalBackend.currentlyOpenProjects.get(projectId)
    if (cachedProject == null) {
      const result = await this.projectManager.listProjects({})
      const project = result.projects.find(listedProject => listedProject.id === projectId)
      if (project == null) {
        throw new Error(
          `Could not get details of project ${
            title != null ? `'${title}'` : `with ID '${projectId}'`
          }.`
        )
      } else {
        const version =
          project.engineVersion == null
            ? null
            : {
                lifecycle: backend.detectVersionLifecycle(project.engineVersion),
                value: project.engineVersion,
              }
        return {
          name: project.name,
          engineVersion: version,
          ideVersion: version,
          jsonAddress: null,
          binaryAddress: null,
          organizationId: '',
          packageName: project.name,
          projectId,
          state: {
            type:
              projectId === LocalBackend.currentlyOpeningProjectId
                ? backend.ProjectState.openInProgress
                : project.lastOpened != null
                ? backend.ProjectState.closed
                : backend.ProjectState.created,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            volume_id: '',
          },
        }
      }
    } else {
      return {
        name: cachedProject.projectName,
        engineVersion: {
          lifecycle: backend.detectVersionLifecycle(cachedProject.engineVersion),
          value: cachedProject.engineVersion,
        },
        ideVersion: {
          lifecycle: backend.detectVersionLifecycle(cachedProject.engineVersion),
          value: cachedProject.engineVersion,
        },
        jsonAddress: ipWithSocketToAddress(cachedProject.languageServerJsonAddress),
        binaryAddress: ipWithSocketToAddress(cachedProject.languageServerBinaryAddress),
        organizationId: '',
        packageName: cachedProject.projectNormalizedName,
        projectId,
        state: {
          type: backend.ProjectState.opened,
          // eslint-disable-next-line @typescript-eslint/naming-convention
          volume_id: '',
        },
      }
    }
  }

  /** Prepare a project for execution.
   * @throws An error if the JSON-RPC call fails. */
  async openProject(
    projectId: backend.ProjectId,
    _body: backend.OpenProjectRequestBody | null,
    title: string | null
  ): Promise<void> {
    LocalBackend.currentlyOpeningProjectId = projectId
    if (!LocalBackend.currentlyOpenProjects.has(projectId)) {
      try {
        const project = await this.projectManager.openProject({
          projectId,
          missingComponentAction: projectManager.MissingComponentAction.install,
        })
        LocalBackend.currentlyOpenProjects.set(projectId, project)
        return
      } catch (error) {
        throw new Error(
          `Could not open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}: ${
            errorModule.tryGetMessage(error) ?? 'unknown error'
          }.`
        )
      } finally {
        LocalBackend.currentlyOpeningProjectId = null
      }
    }
  }

  // === Endpoints that intentionally do not work on the Local Backend ===

  /** Called for any function that does not make sense in the Local Backend.
   * @throws An error stating that the operation is intentionally unavailable on the local
   * backend. */
  invalidOperation(): never {
    throw new Error('Cannot manage users, folders, files, tags, and secrets on the local backend.')
  }

  /** Invalid operation. */
  override createUser() {
    return this.invalidOperation()
  }

  /** FIXME: This is required to get (and by extension, list) the root directory. */
  override self() {
    return Promise.resolve(null)
  }

  /** Invalid operation. */
  override createTag() {
    return this.invalidOperation()
  }

  /** Return an empty array. This function should never need to be called. */
  override listTags() {
    return Promise.resolve([])
  }

  /** Do nothing. This function should never need to be called. */
  override deleteTag() {
    return Promise.resolve()
  }
}
