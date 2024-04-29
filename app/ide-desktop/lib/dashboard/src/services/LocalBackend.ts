/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link LocalBackend} in this module corresponds to an API endpoint.
 * The functions are asynchronous and return a {@link Promise} that resolves to the response from
 * the API. */
import * as detect from 'enso-common/src/detect'

import Backend, * as backend from '#/services/Backend'
import * as projectManager from '#/services/ProjectManager'
import ProjectManager from '#/services/ProjectManager'

import * as dateTime from '#/utilities/dateTime'
import * as errorModule from '#/utilities/error'
import * as fileInfo from '#/utilities/fileInfo'
import * as object from '#/utilities/object'
import * as uniqueString from '#/utilities/uniqueString'

// =============================
// === ipWithSocketToAddress ===
// =============================

/** Convert a {@link projectManager.IpWithSocket} to a {@link backend.Address}. */
function ipWithSocketToAddress(ipWithSocket: projectManager.IpWithSocket) {
  return backend.Address(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

// ======================================
// === Functions for manipulating ids ===
// ======================================

/** Create a {@link backend.DirectoryId} from a path. */
export function newDirectoryId(path: projectManager.Path) {
  return backend.DirectoryId(`${backend.AssetType.directory}-${path}`)
}

/** Create a {@link backend.ProjectId} from a UUID. */
export function newProjectId(uuid: projectManager.UUID) {
  return backend.ProjectId(`${backend.AssetType.project}-${uuid}`)
}

/** Create a {@link backend.FileId} from a path. */
export function newFileId(path: projectManager.Path) {
  return backend.FileId(`${backend.AssetType.file}-${path}`)
}

/** The internal asset type and properly typed corresponding internal ID of a directory. */
interface DirectoryTypeAndId {
  readonly type: backend.AssetType.directory
  readonly id: projectManager.Path
}

/** The internal asset type and properly typed corresponding internal ID of a project. */
interface ProjectTypeAndId {
  readonly type: backend.AssetType.project
  readonly id: projectManager.UUID
}

/** The internal asset type and properly typed corresponding internal ID of a file. */
interface FileTypeAndId {
  readonly type: backend.AssetType.file
  readonly id: projectManager.Path
}

/** The internal asset type and properly typed corresponding internal ID of an arbitrary asset. */
type AssetTypeAndId<Id extends backend.AssetId = backend.AssetId> =
  | (backend.DirectoryId extends Id ? DirectoryTypeAndId : never)
  | (backend.FileId extends Id ? FileTypeAndId : never)
  | (backend.ProjectId extends Id ? ProjectTypeAndId : never)

export function extractTypeAndId<Id extends backend.AssetId>(id: Id): AssetTypeAndId<Id>
/** Extracts the asset type and its corresponding internal ID from a {@link backend.AssetId}.
 * @throws {Error} if the id has an unknown type. */
export function extractTypeAndId<Id extends backend.AssetId>(id: Id): AssetTypeAndId {
  const [, typeRaw, idRaw = ''] = id.match(/(.+?)-(.+)/) ?? []
  switch (typeRaw) {
    case backend.AssetType.directory: {
      return { type: backend.AssetType.directory, id: projectManager.Path(idRaw) }
    }
    case backend.AssetType.project: {
      return { type: backend.AssetType.project, id: projectManager.UUID(idRaw) }
    }
    case backend.AssetType.file: {
      return { type: backend.AssetType.file, id: projectManager.Path(idRaw) }
    }
    default: {
      throw new Error(`Invalid type '${typeRaw}'`)
    }
  }
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

// ======================
// === intoSmartAsset ===
// ======================

/** Converts an {@link backend.AnyAsset} into its corresponding
 * {@link backend.AnySmartAsset}. */
function intoSmartAsset(projectManagerValue: ProjectManager) {
  return (asset: backend.AnyAsset): backend.AnySmartAsset => {
    switch (asset.type) {
      case backend.AssetType.directory: {
        return new SmartDirectory(projectManagerValue, asset)
      }
      case backend.AssetType.project: {
        return new SmartProject(projectManagerValue, asset)
      }
      case backend.AssetType.file: {
        return new SmartFile(projectManagerValue, asset)
      }
      case backend.AssetType.dataLink:
      case backend.AssetType.secret: {
        throw new Error(`The Local Backend does not support '${asset.type}' assets.`)
      }
      case backend.AssetType.specialLoading:
      case backend.AssetType.specialEmpty: {
        throw new Error(
          `'${asset.type}' is a special asset type that should never be returned by the backend.`
        )
      }
    }
  }
}

// =====================
// === Smart objects ===
// =====================

/** A wrapper around a subset of the API endpoints. */
class SmartObject<T> implements backend.SmartObject<T> {
  /** Create a {@link SmartObject}. */
  constructor(
    // This is fine, as this is a property, not a local variable.
    // eslint-disable-next-line @typescript-eslint/no-shadow
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
  restore() {
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

  /** Invalid operation. */
  getLogEvents() {
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

  /** Change the name, parent directory and/or description of an asset. */
  async update(body: backend.UpdateAssetOrDirectoryRequestBody): Promise<this> {
    if (body.parentDirectoryId == null && body.title == null) {
      return this
    } else {
      const typeAndId = extractTypeAndId(this.value.id)
      const from = typeAndId.type === backend.AssetType.project ? body.projectPath : typeAndId.id
      if (from == null) {
        throw new Error('Could not move project: project has no `projectPath`.')
      } else {
        const fileName = body.title ?? fileInfo.fileName(from)
        const parentPath = extractTypeAndId(body.parentDirectoryId ?? this.value.parentId).id
        const to = projectManager.joinPath(parentPath, fileName)
        await this.projectManager.moveFile(from, to)
        const parentId = body.parentDirectoryId
        if (this instanceof SmartProject) {
          return this.withValue(
            // This is SAFE, as `backend.ProjectAsset & T` is `backend.ProjectAsset`.
            // eslint-disable-next-line no-restricted-syntax
            object.merge(this.value, {
              projectState: object.merge(this.value.projectState, { path: to }),
            } as Partial<backend.ProjectAsset & T>)
          )
        } else if (this instanceof SmartDirectory) {
          return this.withValue(
            // This is SAFE, as `backend.DirectoryAsset & T` is `backend.DirectoryAsset`.
            // eslint-disable-next-line no-restricted-syntax
            object.merge(this.value, { id: newDirectoryId(to), parentId } as Partial<
              backend.DirectoryAsset & T
            >)
          )
        } else if (this instanceof SmartFile) {
          return this.withValue(
            // This is SAFE, as `backend.FileAsset & T` is `backend.FileAsset`.
            // eslint-disable-next-line no-restricted-syntax
            object.merge(this.value, { id: newFileId(to), parentId } as Partial<
              backend.FileAsset & T
            >)
          )
        } else {
          throw new Error(`Assets of type '${this.constructor.name}' cannot be updated.`)
        }
      }
    }
  }

  /** Permanently delete a project. */
  async delete(): Promise<void> {
    const typeAndId = extractTypeAndId(this.value.id)
    switch (typeAndId.type) {
      case backend.AssetType.directory:
      case backend.AssetType.file: {
        await this.projectManager.deleteFile(typeAndId.id)
        return
      }
      case backend.AssetType.project: {
        try {
          await this.projectManager.deleteProject({
            projectId: typeAndId.id,
            projectsDirectory: extractTypeAndId(this.value.parentId).id,
          })
          return
        } catch (error) {
          throw new Error(
            `Could not delete project '${
              this.value.title
            }': ${errorModule.getMessageOrToString(error) ?? 'unknown error'}.`
          )
        }
      }
      default: {
        return this.invalidOperation()
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
    const parentId = this.value.id
    const path = extractTypeAndId(parentId).id
    const entries = await this.projectManager.listDirectory(path)
    return entries
      .map(entry => {
        switch (entry.type) {
          case projectManager.FileSystemEntryType.DirectoryEntry: {
            return {
              type: backend.AssetType.directory,
              id: newDirectoryId(entry.path),
              modifiedAt: entry.attributes.lastModifiedTime,
              parentId,
              title: fileInfo.fileName(entry.path),
              permissions: [],
              projectState: null,
              labels: [],
              description: null,
            } satisfies backend.DirectoryAsset
          }
          case projectManager.FileSystemEntryType.ProjectEntry: {
            return {
              type: backend.AssetType.project,
              id: newProjectId(entry.metadata.id),
              title: entry.metadata.name,
              modifiedAt: entry.metadata.lastOpened ?? entry.metadata.created,
              parentId,
              permissions: [],
              projectState: {
                type:
                  this.projectManager.projects.get(entry.metadata.id)?.state ??
                  backend.ProjectState.closed,
                volumeId: '',
                path: entry.path,
              },
              labels: [],
              description: null,
            } satisfies backend.ProjectAsset
          }
          case projectManager.FileSystemEntryType.FileEntry: {
            return {
              type: backend.AssetType.file,
              id: newFileId(entry.path),
              title: fileInfo.fileName(entry.path),
              modifiedAt: entry.attributes.lastModifiedTime,
              parentId,
              permissions: [],
              projectState: null,
              labels: [],
              description: null,
            } satisfies backend.FileAsset
          }
        }
      })
      .sort(backend.compareAssets)
      .map(intoSmartAsset(this.projectManager))
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

  /** Create a directory. */
  createPlaceholderDirectory(title: string, permissions: backend.UserPermission[]): SmartDirectory {
    const parentDirectoryPath = extractTypeAndId(this.value.id).id
    const path = projectManager.joinPath(parentDirectoryPath, title)
    const result = new SmartDirectory(this.projectManager, {
      type: backend.AssetType.directory,
      id: backend.DirectoryId(`${backend.AssetType.directory}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: null,
      labels: [],
      description: null,
    })
    result.materialize = overwriteMaterialize(result, async () => {
      await this.projectManager.createDirectory(path)
      return result.withValue(
        object.merge(result.value, {
          id: newDirectoryId(path),
          parentId: newDirectoryId(parentDirectoryPath),
          title: title,
        })
      )
    })
    return result
  }

  /** Create a {@link SmartProject} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderProjectFromFile(
    title: string,
    file: File,
    permissions: backend.UserPermission[]
  ): backend.SmartProject {
    const result = new SmartProject(this.projectManager, {
      type: backend.AssetType.project,
      id: backend.ProjectId(`${backend.AssetType.project}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: { type: backend.ProjectState.placeholder, volumeId: '' },
      labels: [],
      description: null,
    })
    const directory = extractTypeAndId(this.value.id).id
    result.materialize = overwriteMaterialize(result, async () => {
      let id: string
      if (
        'backendApi' in window &&
        // This non-standard property is defined in Electron.
        'path' in file &&
        typeof file.path === 'string'
      ) {
        id = await window.backendApi.importProjectFromPath(file.path, directory, title)
      } else {
        const searchParams = new URLSearchParams({ directory, name: title }).toString()
        // Ideally this would use `fileOrTemplateName.stream()`, to minimize RAM
        // requirements. for uploading large projects. Unfortunately,
        // this requires HTTP/2, which is HTTPS-only, so it will not
        // work on `http://localhost`.
        const body =
          window.location.protocol === 'https:' ? file.stream() : await file.arrayBuffer()
        const path = `./api/upload-project?${searchParams}`
        const response = await fetch(path, { method: 'POST', body })
        id = await response.text()
      }
      const path = projectManager.joinPath(directory, title)
      return result.withValue(
        object.merge(result.value, {
          id: newProjectId(projectManager.UUID(id)),
          projectState: { type: backend.ProjectState.closed, volumeId: '', path },
        })
      )
    })
    return result
  }

  /** Invalid operation. */
  createPlaceholderProjectFromDataLinkId() {
    return this.invalidOperation()
  }

  /** Create a {@link SmartProject} that is to be uploaded on the backend via `.materialize()` */
  createPlaceholderProjectFromTemplateName(
    title: string,
    templateName: string | null,
    permissions: backend.UserPermission[]
  ): backend.SmartProject {
    const result = new SmartProject(this.projectManager, {
      type: backend.AssetType.project,
      id: backend.ProjectId(`${backend.AssetType.project}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: { type: backend.ProjectState.placeholder, volumeId: '' },
      labels: [],
      description: null,
    })
    result.materialize = overwriteMaterialize(result, async () => {
      const project = await this.projectManager.createProject({
        name: projectManager.ProjectName(title),
        ...(templateName != null ? { projectTemplate: templateName } : {}),
        missingComponentAction: projectManager.MissingComponentAction.install,
      })
      const directoryPath = extractTypeAndId(this.value.id).id
      const path = projectManager.joinPath(directoryPath, title)
      return result.withValue(
        object.merge(result.value, {
          id: newProjectId(project.projectId),
          projectState: { type: backend.ProjectState.closed, volumeId: '', path },
        })
      )
    })
    return result
  }

  /** Invalid operation. */
  createPlaceholderFile(
    title: string,
    file: globalThis.File,
    permissions: backend.UserPermission[]
  ) {
    const result = new SmartFile(this.projectManager, {
      type: backend.AssetType.file,
      id: backend.FileId(`${backend.AssetType.file}-${uniqueString.uniqueString()}`),
      title,
      modifiedAt: dateTime.toRfc3339(new Date()),
      parentId: this.value.id,
      permissions,
      projectState: null,
      labels: [],
      description: null,
    })
    result.materialize = overwriteMaterialize(result, async () => {
      const parentPath = extractTypeAndId(this.value.id).id
      const path = projectManager.joinPath(parentPath, file.name)
      await this.projectManager.createFile(path, file)
      return result.withValue(object.merge(result.value, { id: newFileId(path) }))
    })
    return result
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
    const { id } = extractTypeAndId(this.value.id)
    try {
      await this.projectManager.openProject({
        projectId: id,
        missingComponentAction: projectManager.MissingComponentAction.install,
      })
      return
    } catch (error) {
      throw new Error(
        `Could not open project '${this.value.title}': ${
          errorModule.getMessageOrToString(error) ?? 'unknown error'
        }.`
      )
    }
  }

  /** Return project details. */
  async getDetails(): Promise<backend.Project> {
    const { id } = extractTypeAndId(this.value.id)
    const state = this.projectManager.projects.get(id)
    if (state == null) {
      const directoryId = extractTypeAndId(this.value.parentId).id
      const entries = await this.projectManager.listDirectory(directoryId)
      const project = entries
        .flatMap(entry =>
          entry.type === projectManager.FileSystemEntryType.ProjectEntry ? [entry.metadata] : []
        )
        .find(metadata => metadata.id === id)
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
            type: this.projectManager.projects.get(id)?.state ?? backend.ProjectState.closed,
            volumeId: '',
          },
        }
      }
    } else {
      const cachedProject = await state.data
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
          volumeId: '',
        },
      }
    }
  }

  /** Invalid operation. */
  getResourceUsage() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  getMainFile() {
    return this.invalidOperation()
  }

  /** Change the name, description, AMI, or parent of a project. */
  override async update(body: backend.UpdateAssetOrProjectRequestBody): Promise<this> {
    const updated = await super.update(body)
    if (body.ami != null) {
      throw new Error('Cannot change project AMI on local backend.')
    } else if (body.ideVersion != null) {
      throw new Error('Cannot change project IDE version on local backend.')
    } else {
      const { id } = extractTypeAndId(this.value.id)
      if (body.projectName != null && this.value.projectState.path != null) {
        const projectsDirectory = extractTypeAndId(this.value.parentId).id
        await this.projectManager.renameProject({
          projectId: id,
          name: projectManager.ProjectName(body.projectName),
          projectsDirectory,
        })
      }
      const parentPath = extractTypeAndId(this.value.parentId).id
      const result = await this.projectManager.listDirectory(parentPath)
      const project = result.flatMap(listedProject =>
        listedProject.type === projectManager.FileSystemEntryType.ProjectEntry &&
        listedProject.metadata.id === id
          ? [listedProject.metadata]
          : []
      )[0]
      if (project == null) {
        throw new Error('The project that is being updated no longer exists.')
      } else {
        return updated.withValue(object.merge(this.value, { title: project.name }))
      }
    }
  }

  /** Close a project. */
  async close(): Promise<void> {
    const { id } = extractTypeAndId(this.value.id)
    try {
      await this.projectManager.closeProject({ projectId: id })
      return
    } catch (error) {
      throw new Error(
        `Could not close project '${this.value.title}': ${
          errorModule.getMessageOrToString(error) ?? 'unknown error'
        }.`
      )
    }
  }

  /** Resolve only when the project is ready to be opened. */
  waitUntilReady() {
    return this.getDetails()
  }
}

/** A smart wrapper around a {@link backend.FileAsset}. */
class SmartFile extends SmartAsset<backend.FileAsset> implements backend.SmartFile {
  /** Invalid operation. */
  getDetails() {
    return this.invalidOperation()
  }
}

// ====================
// === LocalBackend ===
// ====================

/** Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard. */
export default class LocalBackend extends Backend {
  readonly type = backend.BackendType.local
  private readonly projectManager: ProjectManager

  /** Create a {@link LocalBackend}. */
  constructor(projectManagerUrl: string, projectManagerRootDirectory: projectManager.Path) {
    super()
    this.projectManager = new ProjectManager(projectManagerUrl, projectManagerRootDirectory)
    if (detect.IS_DEV_MODE) {
      // @ts-expect-error This exists only for debugging purposes. It does not have types
      // because it MUST NOT be used in this codebase.
      window.localBackend = this
    }
  }

  /** Return details for the current user. */
  override self(): Promise<SmartUser> {
    return Promise.resolve(
      new SmartUser(this.projectManager, {
        email: backend.EmailAddress(''),
        userId: backend.UserId('user-local'),
        organizationId: backend.OrganizationId('organization-local'),
        isEnabled: false,
        rootDirectoryId: newDirectoryId(this.projectManager.rootDirectory),
        name: 'Local User',
        profilePicture: null,
      })
    )
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
    body: backend.OpenProjectRequestBody | null,
    title: string | null
  ): Promise<void> {
    const { id } = extractTypeAndId(projectId)
    if (!this.projectManager.projects.has(id)) {
      try {
        await this.projectManager.openProject({
          projectId: id,
          missingComponentAction: projectManager.MissingComponentAction.install,
          ...(body?.parentId != null
            ? { projectsDirectory: extractTypeAndId(body.parentId).id }
            : {}),
        })
        return
      } catch (error) {
        throw new Error(
          `Could not open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}: ${
            errorModule.getMessageOrToString(error) ?? 'unknown error'
          }.`
        )
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
