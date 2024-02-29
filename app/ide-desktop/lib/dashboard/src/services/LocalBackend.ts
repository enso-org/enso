/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link LocalBackend} in this module corresponds to an API endpoint.
 * The functions are asynchronous and return a {@link Promise} that resolves to the response from
 * the API. */
import * as detect from 'enso-common/src/detect'

import Backend, * as backend from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'
import * as errorModule from '#/utilities/error'
import * as object from '#/utilities/object'
import * as projectManagerModule from '#/utilities/ProjectManager'
import ProjectManager from '#/utilities/ProjectManager'
import * as uniqueString from '#/utilities/uniqueString'

// =============================
// === ipWithSocketToAddress ===
// =============================

/** Convert a {@link projectManagerModule.IpWithSocket} to a {@link backend.Address}. */
function ipWithSocketToAddress(ipWithSocket: projectManagerModule.IpWithSocket) {
  return backend.Address(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

// ============================
// === overwriteMaterialize ===
// ============================

/** Any object that has a `materialize()` method that accepts no arguments. */
interface HasMaterialize<T> {
  readonly materialize: () => Promise<T> | T
}

/** Overwrites `materialize` so that it does not. */
function overwriteMaterialize<T>(
  smartAsset: HasMaterialize<T>,
  materialize: () => Promise<T>
): () => Promise<T> {
  return () => {
    const promise = materialize()
    // This is SAFE - `materialize` is intended to be mutated here. This is the only place where
    // it should be mutated.
    object.unsafeMutable(smartAsset).materialize = () => promise
    return promise
  }
}

// =====================
// === Smart objects ===
// =====================

/** A wrapper around a subset of the API endpoints. */
class SmartObject<T> implements backend.SmartObject<T> {
  /** Create a {@link SmartObject}. */
  constructor(
    protected readonly projectManager: ProjectManager,
    readonly value: T
  ) {}

  /** Return a copy of this object, but with a different value. */
  withValue(value: T): this {
    // This is SAFE (as long as no child classes override the constructor),
    // as it uses runtime reflection to get the constructor of the current object.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-call, no-restricted-syntax, @typescript-eslint/no-explicit-any
    return new (this.constructor as any)(this.projectManager, value)
  }

  /** Called for any function that does not make sense in the Local Backend.
   * @throws An error stating that the operation is intentionally unavailable on the local
   * backend. */
  protected invalidOperation(): never {
    throw new Error('Cannot manage users, folders, files, tags, and secrets on the local backend.')
  }
}

/** A smart wrapper around a {@link backend.User}. */
class SmartUser extends SmartObject<backend.User> implements backend.SmartUser {
  /** Invalid operation. */
  update() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  delete() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  uploadPicture() {
    return this.invalidOperation()
  }

  /** Get the root directory for this user. */
  rootDirectory(): backend.SmartDirectory {
    const rootDirectory = backend.createRootDirectoryAsset(this.value.rootDirectoryId)
    return new SmartDirectory(this.projectManager, rootDirectory)
  }

  /** Invalid operation. */
  invite() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  listUsers() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  listRecentFiles() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  listSecrets() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  getOrganization() {
    return this.invalidOperation()
  }
}

/** A smart wrapper around a {@link backend.AnyAsset}. */
class SmartAsset<T extends backend.AnyAsset = backend.AnyAsset>
  extends SmartObject<T>
  implements backend.SmartAsset
{
  /** The type of the wrapped value. */
  get type(): T['type'] {
    return this.value.type
  }

  /** If this is a placeholder asset, return its non-placeholder equivalent after creating it on
   * the backend. Otherwise, return `this`. */
  materialize(): Promise<this> | this {
    return this
  }

  /** Invalid operation. */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  update(_body: backend.UpdateAssetRequestBody): Promise<this> {
    return this.invalidOperation()
  }

  /** Permanently delete a project. */
  async delete(): Promise<void> {
    if (this.value.type !== backend.AssetType.project) {
      return this.invalidOperation()
    } else {
      try {
        await this.projectManager.deleteProject({ projectId: this.value.id })
        return
      } catch (error) {
        throw new Error(
          `Could not delete project '${this.value.title}': ${
            errorModule.tryGetMessage(error) ?? 'unknown error'
          }.`
        )
      }
    }
  }

  /** Invalid operation. */
  undoDelete() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  copy() {
    return this.invalidOperation()
  }

  /** Return a copy of this object, but with a different value. */
  // @ts-expect-error This is SAFE, under the same conditions as `SmartObject.withValue`.
  override withValue(value: T): this {
    return super.withValue(value)
  }

  /** Invalid operation. */
  listVersions() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  setPermissions() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  setTags() {
    return this.invalidOperation()
  }
}

/** A smart wrapper around a {@link backend.DirectoryAsset}. */
class SmartDirectory extends SmartAsset<backend.DirectoryAsset> implements backend.SmartDirectory {
  /** Return a list of assets in a directory. */
  async list(): Promise<backend.AnySmartAsset[]> {
    const result = await this.projectManager.listProjects({})
    return result.projects.map(
      project =>
        new SmartProject(this.projectManager, {
          type: backend.AssetType.project,
          id: project.id,
          title: project.name,
          modifiedAt: project.lastOpened ?? project.created,
          parentId: this.value.id,
          permissions: [],
          projectState: {
            type: LocalBackend.currentlyOpenProjects.has(project.id)
              ? backend.ProjectState.opened
              : project.id === LocalBackend.currentlyOpeningProjectId
              ? backend.ProjectState.openInProgress
              : backend.ProjectState.closed,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            volume_id: '',
          },
          labels: [],
          description: null,
        })
    )
  }

  /** Invalid operation. */
  override update() {
    return this.invalidOperation()
  }

  /** Create a {@link backend.SpecialLoadingAsset}. */
  createSpecialLoadingAsset(): backend.SmartSpecialLoadingAsset {
    return new SmartAsset<backend.SpecialLoadingAsset>(this.projectManager, {
      type: backend.AssetType.specialLoading,
      title: '',
      id: backend.LoadingAssetId(
        `${backend.AssetType.specialLoading}-${uniqueString.uniqueString()}`
      ),
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions: [],
      projectState: null,
      labels: [],
      description: null,
    })
  }

  /** Create a {@link backend.SpecialEmptyAsset}. */
  createSpecialEmptyAsset(): backend.SmartSpecialEmptyAsset {
    return new SmartAsset<backend.SpecialEmptyAsset>(this.projectManager, {
      type: backend.AssetType.specialEmpty,
      title: '',
      id: backend.EmptyAssetId(`${backend.AssetType.specialEmpty}-${uniqueString.uniqueString()}`),
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions: [],
      projectState: null,
      labels: [],
      description: null,
    })
  }

  /** Invalid operation. */
  createPlaceholderDirectory() {
    return this.invalidOperation()
  }

  /** Create a {@link SmartProject} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderProject(
    title: string,
    fileOrTemplateName: File | string | null,
    permissions: backend.UserPermission[]
  ): backend.SmartProject {
    const result = new SmartProject(this.projectManager, {
      type: backend.AssetType.project,
      id: backend.ProjectId(`${backend.AssetType.project}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: {
        type: backend.ProjectState.placeholder,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        volume_id: '',
      },
      labels: [],
      description: null,
    })
    if (fileOrTemplateName instanceof File) {
      return this.invalidOperation()
    } else {
      result.materialize = overwriteMaterialize(result, async () => {
        const project = await this.projectManager.createProject({
          name: projectManagerModule.ProjectName(title),
          ...(fileOrTemplateName != null ? { projectTemplate: fileOrTemplateName } : {}),
          missingComponentAction: projectManagerModule.MissingComponentAction.install,
        })
        return result.withValue(
          object.merge(result.value, {
            id: project.projectId,
            projectState: {
              type: backend.ProjectState.closed,
              // eslint-disable-next-line @typescript-eslint/naming-convention
              volume_id: '',
            },
          })
        )
      })
    }
    return result
  }

  /** Invalid operation. */
  createPlaceholderFile() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  createPlaceholderDataLink() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  createPlaceholderSecret() {
    return this.invalidOperation()
  }
}

/** A smart wrapper around a {@link backend.ProjectAsset}. */
class SmartProject extends SmartAsset<backend.ProjectAsset> implements backend.SmartProject {
  /** Set a project to an open state. */
  async open(): Promise<void> {
    LocalBackend.currentlyOpeningProjectId = this.value.id
    if (!LocalBackend.currentlyOpenProjects.has(this.value.id)) {
      try {
        const project = await this.projectManager.openProject({
          projectId: this.value.id,
          missingComponentAction: projectManagerModule.MissingComponentAction.install,
        })
        LocalBackend.currentlyOpenProjects.set(this.value.id, project)
        return
      } catch (error) {
        throw new Error(
          `Could not open project '${this.value.title}': ${
            errorModule.tryGetMessage(error) ?? 'unknown error'
          }.`
        )
      } finally {
        if (LocalBackend.currentlyOpeningProjectId === this.value.id) {
          LocalBackend.currentlyOpeningProjectId = null
        }
      }
    }
  }

  /** Return project details. */
  async getDetails(): Promise<backend.Project> {
    const cachedProject = LocalBackend.currentlyOpenProjects.get(this.value.id)
    if (cachedProject == null) {
      const result = await this.projectManager.listProjects({})
      const project = result.projects.find(listedProject => listedProject.id === this.value.id)
      if (project == null) {
        throw new Error(`Could not get details of project '${this.value.title}'.`)
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
          projectId: this.value.id,
          state: {
            type:
              this.value.id === LocalBackend.currentlyOpeningProjectId
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
        projectId: this.value.id,
        state: {
          type: backend.ProjectState.opened,
          // eslint-disable-next-line @typescript-eslint/naming-convention
          volume_id: '',
        },
      }
    }
  }

  /** Invalid operation. */
  getResourceUsage() {
    return this.invalidOperation()
  }

  /** Change the name, description, AMI, or parent of a project. */
  override async update(body: backend.UpdateAssetOrProjectRequestBody): Promise<this> {
    if (body.ami != null) {
      throw new Error('Cannot change project AMI on local backend.')
    } else if (body.ideVersion != null) {
      throw new Error('Cannot change project IDE version on local backend.')
    } else {
      if (body.projectName != null) {
        await this.projectManager.renameProject({
          projectId: this.value.id,
          name: projectManagerModule.ProjectName(body.projectName),
        })
      }
      const result = await this.projectManager.listProjects({})
      const project = result.projects.find(listedProject => listedProject.id === this.value.id)
      if (project == null) {
        throw new Error('The project that is being updated no longer exists.')
      } else {
        return this.withValue(object.merge(this.value, { title: project.name }))
      }
    }
  }

  /** Close a project. */
  async close(): Promise<void> {
    if (LocalBackend.currentlyOpeningProjectId === this.value.id) {
      LocalBackend.currentlyOpeningProjectId = null
    }
    LocalBackend.currentlyOpenProjects.delete(this.value.id)
    try {
      await this.projectManager.closeProject({ projectId: this.value.id })
      return
    } catch (error) {
      throw new Error(
        `Could not close project '${this.value.title}': ${
          errorModule.tryGetMessage(error) ?? 'unknown error'
        }.`
      )
    }
  }

  /** Resolve only when the project is ready to be opened. */
  waitUntilReady() {
    return this.getDetails()
  }
}

// ====================
// === LocalBackend ===
// ====================

/** Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard. */
export default class LocalBackend extends Backend {
  static currentlyOpeningProjectId: backend.ProjectId | null = null
  static currentlyOpenProjects = new Map<
    projectManagerModule.ProjectId,
    projectManagerModule.OpenProject
  >()
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

  /** Get the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails. */
  async getProject(projectId: backend.ProjectId, title: string): Promise<backend.SmartProject> {
    const self = await this.self()
    const rootDirectory = self.rootDirectory()
    const children = await rootDirectory.list({
      filterBy: backend.FilterBy.active,
      labels: null,
    })
    const project = children.find(child => child.value.id === projectId)
    if (project?.type !== backend.AssetType.project) {
      throw new Error(`The project '${title}' was not found.`)
    } else {
      return project
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
          missingComponentAction: projectManagerModule.MissingComponentAction.install,
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

  /** Return details for the current user. */
  override self(): Promise<SmartUser> {
    return Promise.resolve(
      new SmartUser(this.projectManager, {
        email: backend.EmailAddress(''),
        id: backend.OrganizationId('organization-local'),
        isEnabled: false,
        rootDirectoryId: backend.DirectoryId(`${backend.AssetType.directory}-local`),
        name: 'Local User',
        profilePicture: null,
      })
    )
  }

  /** Invalid operation. */
  override createTag() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override listTags() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteTag() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createCheckoutSession() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getCheckoutSession() {
    return this.invalidOperation()
  }
}
