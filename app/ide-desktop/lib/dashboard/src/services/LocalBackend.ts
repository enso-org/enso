/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link LocalBackend} in this module corresponds to an API endpoint.
 * The functions are asynchronous and return a {@link Promise} that resolves to the response from
 * the API. */
import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import Backend, * as backend from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'
import * as errorModule from '#/utilities/error'
import * as fileInfo from '#/utilities/fileInfo'
import * as projectManager from '#/utilities/ProjectManager'
import ProjectManager from '#/utilities/ProjectManager'

// ====================
// === ProjectState ===
// ====================

/** A project that is currently opening. */
interface OpenInProgressProjectState {
  readonly state: backend.ProjectState.openInProgress
}

/** A project that is currently opened. */
interface OpenedProjectState {
  readonly state: backend.ProjectState.opened
  readonly data: projectManager.OpenProject
}

/** Possible states and associated metadata of a project.
 * The "closed" state is omitted as it is the default state. */
type ProjectState = OpenedProjectState | OpenInProgressProjectState

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

// ====================
// === LocalBackend ===
// ====================

/** Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard. */
export default class LocalBackend extends Backend {
  static projects = new Map<projectManager.UUID, ProjectState>()
  private static readonly baseUrl = location.pathname.replace(appUtils.ALL_PATHS_REGEX, '')
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

  /** Return the ID of the root directory. */
  override rootDirectoryId(): backend.DirectoryId {
    return newDirectoryId(ProjectManager.rootDirectory)
  }

  /** Return a list of assets in a directory.
   * @throws An error if the JSON-RPC call fails. */
  override async listDirectory(
    query: backend.ListDirectoryRequestParams
  ): Promise<backend.AnyAsset[]> {
    const parentIdRaw = query.parentId == null ? null : extractTypeAndId(query.parentId).id
    const parentId = query.parentId ?? newDirectoryId(ProjectManager.rootDirectory)
    const entries = await this.projectManager.listDirectory(parentIdRaw)
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
                  LocalBackend.projects.get(entry.metadata.id)?.state ??
                  backend.ProjectState.closed,
                // eslint-disable-next-line @typescript-eslint/naming-convention
                volume_id: '',
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
  }

  /** Return a list of projects belonging to the current user.
   * @throws An error if the JSON-RPC call fails. */
  override async listProjects(): Promise<backend.ListedProject[]> {
    const result = await this.projectManager.listProjects({})
    return result.projects.map(project => ({
      name: project.name,
      organizationId: '',
      projectId: newProjectId(project.id),
      packageName: project.name,
      state: {
        type: backend.ProjectState.closed,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        volume_id: '',
      },
      jsonAddress: null,
      binaryAddress: null,
    }))
  }

  /** Create a project.
   * @throws An error if the JSON-RPC call fails. */
  override async createProject(
    body: backend.CreateProjectRequestBody
  ): Promise<backend.CreatedProject> {
    const project = await this.projectManager.createProject({
      name: projectManager.ProjectName(body.projectName),
      ...(body.projectTemplateName != null ? { projectTemplate: body.projectTemplateName } : {}),
      missingComponentAction: projectManager.MissingComponentAction.install,
      ...(body.parentDirectoryId == null
        ? {}
        : { projectsDirectory: extractTypeAndId(body.parentDirectoryId).id }),
    })
    return {
      name: body.projectName,
      organizationId: '',
      projectId: newProjectId(project.projectId),
      packageName: body.projectName,
      state: {
        type: backend.ProjectState.closed,
        // eslint-disable-next-line @typescript-eslint/naming-convention
        volume_id: '',
      },
    }
  }

  /** Close the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails. */
  override async closeProject(projectId: backend.ProjectId, title: string | null): Promise<void> {
    const { id } = extractTypeAndId(projectId)
    LocalBackend.projects.delete(id)
    try {
      await this.projectManager.closeProject({ projectId: id })
      return
    } catch (error) {
      throw new Error(
        `Could not close project ${title != null ? `'${title}'` : `with ID '${projectId}'`}: ${
          errorModule.tryGetMessage(error) ?? 'unknown error'
        }.`
      )
    }
  }

  /** Close the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails. */
  override async getProjectDetails(
    projectId: backend.ProjectId,
    title: string | null
  ): Promise<backend.Project> {
    const { id } = extractTypeAndId(projectId)
    const state = LocalBackend.projects.get(id)
    const cachedProject = state?.state === backend.ProjectState.opened ? state.data : null
    if (cachedProject == null) {
      const result = await this.projectManager.listProjects({})
      const project = result.projects.find(listedProject => listedProject.id === id)
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
            type: LocalBackend.projects.get(id)?.state ?? backend.ProjectState.closed,
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
  override async openProject(
    projectId: backend.ProjectId,
    body: backend.OpenProjectRequestBody | null,
    title: string | null
  ): Promise<void> {
    const { id } = extractTypeAndId(projectId)
    if (!LocalBackend.projects.has(id)) {
      LocalBackend.projects.set(id, { state: backend.ProjectState.openInProgress })
      try {
        const data = await this.projectManager.openProject({
          projectId: id,
          missingComponentAction: projectManager.MissingComponentAction.install,
          ...(body?.parentId != null
            ? { projectsDirectory: extractTypeAndId(body.parentId).id }
            : {}),
        })
        LocalBackend.projects.set(id, { state: backend.ProjectState.opened, data })
        return
      } catch (error) {
        LocalBackend.projects.delete(id)
        throw new Error(
          `Could not open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}: ${
            errorModule.tryGetMessage(error) ?? 'unknown error'
          }.`
        )
      }
    }
  }

  /** Change the name of a project.
   * @throws An error if the JSON-RPC call fails. */
  override async updateProject(
    projectId: backend.ProjectId,
    body: backend.UpdateProjectRequestBody
  ): Promise<backend.UpdatedProject> {
    if (body.ami != null) {
      throw new Error('Cannot change project AMI on local backend.')
    } else {
      const { id } = extractTypeAndId(projectId)
      if (body.projectName != null) {
        await this.projectManager.renameProject({
          projectId: id,
          name: projectManager.ProjectName(body.projectName),
          projectsDirectory: extractTypeAndId(body.parentId).id,
        })
      }
      const parentId = extractTypeAndId(body.parentId).id
      const result = await this.projectManager.listDirectory(parentId)
      const project = result.flatMap(listedProject =>
        listedProject.type === projectManager.FileSystemEntryType.ProjectEntry &&
        listedProject.metadata.id === id
          ? [listedProject.metadata]
          : []
      )[0]
      const version =
        project?.engineVersion == null
          ? null
          : {
              lifecycle: backend.detectVersionLifecycle(project.engineVersion),
              value: project.engineVersion,
            }
      if (project == null) {
        throw new Error(`The project ID '${projectId}' is invalid.`)
      } else {
        return {
          ami: null,
          engineVersion: version,
          ideVersion: version,
          name: project.name,
          organizationId: '',
          projectId,
        }
      }
    }
  }

  /** Delete an arbitrary asset.
   * @throws An error if the JSON-RPC call fails. */
  override async deleteAsset(
    assetId: backend.AssetId,
    body: backend.DeleteAssetRequestBody,
    title: string | null
  ): Promise<void> {
    const typeAndId = extractTypeAndId(assetId)
    switch (typeAndId.type) {
      case backend.AssetType.directory:
      case backend.AssetType.file: {
        await this.projectManager.deleteFile(typeAndId.id)
        return
      }
      case backend.AssetType.project: {
        LocalBackend.projects.delete(typeAndId.id)
        try {
          await this.projectManager.deleteProject({
            projectId: typeAndId.id,
            projectsDirectory: extractTypeAndId(body.parentId).id,
          })
          return
        } catch (error) {
          throw new Error(
            `Could not delete project ${
              title != null ? `'${title}'` : `with ID '${typeAndId.id}'`
            }: ${errorModule.tryGetMessage(error) ?? 'unknown error'}.`
          )
        }
      }
    }
  }

  /** Copy an arbitrary asset to another directory. Not yet implemented in the backend.
   * @throws {Error} Always. */
  override copyAsset(): Promise<backend.CopyAssetResponse> {
    throw new Error('Cannot copy assets in local backend yet.')
  }

  /** Return a list of engine versions. */
  override async listVersions(params: backend.ListVersionsRequestParams) {
    const engineVersions = await this.projectManager.listAvailableEngineVersions()
    const engineVersionToVersion = (version: projectManager.EngineVersion): backend.Version => ({
      ami: null,
      created: dateTime.toRfc3339(new Date()),
      number: {
        value: version.version,
        lifecycle: backend.detectVersionLifecycle(version.version),
      },
      // The names come from a third-party API and cannot be changed.
      // eslint-disable-next-line @typescript-eslint/naming-convention
      version_type: params.versionType,
    })
    return engineVersions.versions.map(engineVersionToVersion)
  }

  // === Endpoints that intentionally do not work on the Local Backend ===

  /** Called for any function that does not make sense in the Local Backend.
   * @throws An error stating that the operation is intentionally unavailable on the local
   * backend. */
  invalidOperation(): never {
    throw new Error('Cannot manage users, folders, files, tags, and secrets on the local backend.')
  }

  /** Do nothing. This function should never need to be called. */
  override undoDeleteAsset(): Promise<void> {
    return this.invalidOperation()
  }

  /** Return an empty array. This function should never need to be called. */
  override listUsers() {
    return Promise.resolve([])
  }

  /** Invalid operation. */
  override createUser() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override updateUser() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteUser() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override uploadUserPicture() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getOrganization() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override updateOrganization() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override uploadOrganizationPicture() {
    return this.invalidOperation()
  }

  /** Do nothing. This function should never need to be called. */
  override inviteUser() {
    return Promise.resolve()
  }

  /** Do nothing. This function should never need to be called. */
  override createPermission() {
    return Promise.resolve()
  }

  /** Return `null`. This function should never need to be called. */
  override usersMe() {
    return Promise.resolve(null)
  }

  /** Create a directory. */
  override async createDirectory(
    body: backend.CreateDirectoryRequestBody
  ): Promise<backend.CreatedDirectory> {
    const parentDirectoryPath =
      body.parentId == null ? ProjectManager.rootDirectory : extractTypeAndId(body.parentId).id
    const path = projectManager.joinPath(parentDirectoryPath, body.title)
    await this.projectManager.createDirectory(path)
    return {
      id: newDirectoryId(path),
      parentId: newDirectoryId(parentDirectoryPath),
      title: body.title,
    }
  }

  /** Change the parent directory of an asset.
   * Changing the description is NOT supported. */
  override async updateAsset(
    assetId: backend.AssetId,
    body: backend.UpdateAssetRequestBody
  ): Promise<void> {
    if (body.parentDirectoryId != null) {
      const typeAndId = extractTypeAndId(assetId)
      const from =
        typeAndId.type === backend.AssetType.project
          ? body.projectPath ?? projectManager.Path('')
          : typeAndId.id
      const fileName = fileInfo.fileName(from)
      const to = projectManager.joinPath(extractTypeAndId(body.parentDirectoryId).id, fileName)
      await this.projectManager.moveFile(from, to)
    }
  }

  /** Upload a file. */
  override async uploadFile(
    params: backend.UploadFileRequestParams,
    file: Blob
  ): Promise<backend.FileInfo> {
    const parentPath =
      params.parentDirectoryId == null
        ? ProjectManager.rootDirectory
        : extractTypeAndId(params.parentDirectoryId).id
    const path = projectManager.joinPath(parentPath, params.fileName)
    await this.projectManager.createFile(path, file)
    // `project` MUST BE `null` as uploading projects uses a separate endpoint.
    return { path, id: newFileId(path), project: null }
  }

  /** Invalid operation. */
  override updateDirectory() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override listAssetVersions() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override checkResources() {
    return this.invalidOperation()
  }

  /** Return an empty array. This function should never need to be called. */
  override listFiles() {
    return Promise.resolve([])
  }

  /** Invalid operation. */
  override getFileDetails() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createConnector() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getConnector() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteConnector() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createSecret() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override updateSecret() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getSecret() {
    return this.invalidOperation()
  }

  /** Return an empty array. This function should never need to be called. */
  override listSecrets() {
    return Promise.resolve([])
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
  override associateTag() {
    return Promise.resolve()
  }

  /** Do nothing. This function should never need to be called. */
  override deleteTag() {
    return Promise.resolve()
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
