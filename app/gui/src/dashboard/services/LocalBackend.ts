/**
 * @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link LocalBackend} in this module corresponds to an API endpoint.
 * The functions are asynchronous and return a {@link Promise} that resolves to the response from
 * the API.
 */
import Backend, * as backend from '#/services/Backend'
import type ProjectManager from '#/services/ProjectManager'
import * as projectManager from '#/services/ProjectManager'
import { download } from '#/utilities/download'
import { tryGetMessage } from '#/utilities/error'
import { fileExtension, getFileName, getFolderPath, normalizePath } from '#/utilities/fileInfo'
import { getDirectoryAndName, joinPath } from '#/utilities/path'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import invariant from 'tiny-invariant'
import { markRaw } from 'vue'

/** Convert a {@link projectManager.IpWithSocket} to a {@link backend.Address}. */
function ipWithSocketToAddress(ipWithSocket: projectManager.IpWithSocket) {
  return backend.Address(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

export const DIRECTORY_ID_PREFIX = `${backend.AssetType.directory}-`
export const PROJECT_ID_PREFIX = `${backend.AssetType.project}-`
export const FILE_ID_PREFIX = `${backend.AssetType.file}-`

/** Create a {@link backend.DirectoryId} from a path. */
export function newDirectoryId(path: projectManager.Path) {
  return backend.DirectoryId(`${DIRECTORY_ID_PREFIX}${path}` as const)
}

/** Create a {@link backend.ProjectId} from a UUID. */
export function newProjectId(uuid: projectManager.UUID, path: projectManager.Path) {
  return backend.ProjectId(`${PROJECT_ID_PREFIX}${uuid}-${path}`)
}

/** Create a {@link backend.FileId} from a path. */
export function newFileId(path: projectManager.Path) {
  return backend.FileId(`${FILE_ID_PREFIX}${path}`)
}

/** The internal asset type and properly typed corresponding internal ID of a directory. */
interface DirectoryTypeAndId {
  readonly type: backend.AssetType.directory
  readonly id: projectManager.Path
  readonly directory: projectManager.Path
}

/** The internal asset type and properly typed corresponding internal ID of a project. */
interface ProjectTypeAndId {
  readonly type: backend.AssetType.project
  readonly id: projectManager.UUID
  readonly directory: projectManager.Path
}

/** The internal asset type and properly typed corresponding internal ID of a file. */
interface FileTypeAndId {
  readonly type: backend.AssetType.file
  readonly id: projectManager.Path
  readonly directory: projectManager.Path
}

/** The internal asset type and properly typed corresponding internal ID of an arbitrary asset. */
type AssetTypeAndId<Id extends backend.AssetId = backend.AssetId> =
  | (backend.DirectoryId extends Id ? DirectoryTypeAndId : never)
  | (backend.FileId extends Id ? FileTypeAndId : never)
  | (backend.ProjectId extends Id ? ProjectTypeAndId : never)

export function extractTypeAndId<Id extends backend.AssetId>(id: Id): AssetTypeAndId<Id>
/**
 * Extracts the asset type and its corresponding internal ID from a {@link backend.AssetId}.
 * @throws {Error} if the id has an unknown type.
 */
export function extractTypeAndId<Id extends backend.AssetId>(id: Id): AssetTypeAndId {
  const [, typeRaw, idRaw = ''] = id.match(/(.+?)-(.+)/) ?? []
  const { directoryPath } = getDirectoryAndName(projectManager.Path(idRaw))

  switch (typeRaw) {
    case backend.AssetType.directory: {
      return {
        type: backend.AssetType.directory,
        id: projectManager.Path(idRaw),
        directory: directoryPath,
      }
    }
    case backend.AssetType.project: {
      const [, idRaw2 = '', directoryRaw = ''] = idRaw.match(/(\w+-\w+-\w+-\w+-\w+)-(.+)/) ?? []
      return {
        type: backend.AssetType.project,
        id: projectManager.UUID(idRaw2),
        directory: projectManager.Path(directoryRaw),
      }
    }
    case backend.AssetType.file: {
      return {
        type: backend.AssetType.file,
        id: projectManager.Path(idRaw),
        directory: directoryPath,
      }
    }
    case undefined:
    default: {
      throw new Error(`Invalid type '${typeRaw}'`)
    }
  }
}

/**
 * Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard.
 */
export default class LocalBackend extends Backend {
  static readonly type = backend.BackendType.local
  readonly type = LocalBackend.type
  /** All files that have been uploaded to the Project Manager. */
  uploadedFiles: Map<string, backend.UploadedLargeAsset> = new Map()
  private readonly projectManager: ProjectManager

  /** Create a {@link LocalBackend}. */
  constructor(projectManagerInstance: ProjectManager) {
    super()

    this.projectManager = projectManagerInstance
  }

  /** Get the root directory of this Backend. */
  override rootPath() {
    return this.projectManager.rootDirectory
  }

  /** Set the root directory of this Backend. */
  setRootPath(value: projectManager.Path) {
    this.projectManager.rootDirectory = value
  }

  /** Reset the root directory of this Backend. */
  resetRootPath() {
    this.projectManager.resetRootDirectory()
  }

  /** Tell the {@link ProjectManager} to reconnect. */
  async reconnectProjectManager() {
    await this.projectManager.reconnect()
  }

  /** Return the ID of the root directory. */
  override rootDirectoryId(
    _user: backend.User,
    _organization: backend.OrganizationInfo | null,
    rootDirectory: backend.Path | null | undefined,
  ): backend.DirectoryId {
    return newDirectoryId(rootDirectory ?? this.projectManager.rootDirectory)
  }

  /**
   * Return a list of assets in a directory.
   * @throws An error if the JSON-RPC call fails.
   */
  override async listDirectory(
    query: backend.ListDirectoryRequestParams,
  ): Promise<readonly backend.AnyAsset[]> {
    const { rootPath = this.rootPath() } = query
    const parentIdRaw = query.parentId == null ? null : extractTypeAndId(query.parentId).id
    const parentId = query.parentId ?? newDirectoryId(this.projectManager.rootDirectory)

    // Catch the case where the directory does not exist.
    let result: backend.AnyAsset[] = []
    try {
      const entries = await this.projectManager.listDirectory(parentIdRaw)
      result = entries
        .map((entry) => {
          const virtualParentsPath = (() => {
            let path = entry.path.replace(rootPath, '')

            if (path.startsWith('/')) {
              path = path.slice(1)
            }

            if (path.endsWith('/')) {
              path = path.slice(0, -1)
            }

            return path
          })()

          const parentsPath = (() => {
            const parentsPathArray: backend.DirectoryId[] = [newDirectoryId(rootPath)]
            const splitPath = virtualParentsPath.split('/')

            let previousPath = ''

            for (const directory of splitPath) {
              if (directory === '') {
                continue
              }

              previousPath = backend.Path(previousPath + '/' + directory)

              if (previousPath.endsWith('/')) {
                previousPath = previousPath.slice(0, -1)
              }

              parentsPathArray.push(newDirectoryId(backend.Path(rootPath + previousPath)))
            }

            return parentsPathArray.slice(0, -1).join('/')
          })()

          const ensoPathRaw = normalizePath(entry.path)
          const ensoPath = backend.EnsoPath(ensoPathRaw)
          const shared = {
            permissions: [],
            projectState: null,
            extension: null,
            parentsPath: backend.ParentsPath(parentsPath),
            virtualParentsPath: backend.VirtualParentsPath(virtualParentsPath),
            ensoPath,
            ensoPathValue: backend.EnsoPathValue(ensoPathRaw),
          } satisfies Partial<backend.DirectoryAsset>

          switch (entry.type) {
            case projectManager.FileSystemEntryType.DirectoryEntry: {
              const id = newDirectoryId(entry.path)

              return {
                ...shared,
                id,
                type: backend.AssetType.directory,
                modifiedAt: entry.attributes.lastModifiedTime,
                parentId,
                title: getFileName(entry.path),
              } satisfies backend.DirectoryAsset
            }
            case projectManager.FileSystemEntryType.ProjectEntry: {
              return {
                ...shared,
                type: backend.AssetType.project,
                id: newProjectId(entry.metadata.id, extractTypeAndId(parentId).id),
                title: entry.metadata.name,
                modifiedAt: entry.metadata.lastOpened ?? entry.metadata.created,
                parentId,
                projectState: {
                  type:
                    this.projectManager.projects.get(entry.metadata.id)?.state ??
                    backend.ProjectState.closed,
                },
              } satisfies backend.ProjectAsset
            }
            case projectManager.FileSystemEntryType.FileEntry: {
              return {
                ...shared,
                type: backend.AssetType.file,
                id: newFileId(entry.path),
                title: getFileName(entry.path),
                modifiedAt: entry.attributes.lastModifiedTime,
                parentId,
                extension: fileExtension(entry.path),
              } satisfies backend.FileAsset
            }
          }
        })
        .sort(backend.compareAssets)
    } catch {
      // Failed so check if exists
      if (!(await this.projectManager.exists(parentIdRaw))) {
        if (parentIdRaw === this.projectManager.rootDirectory) {
          // Auto create the root directory
          await this.projectManager.createDirectory(this.projectManager.rootDirectory)

          result = []
        } else {
          throw new backend.DirectoryDoesNotExistError()
        }
      }
    }

    return result
  }

  /**
   * Return a list of projects belonging to the current user.
   * @throws An error if the JSON-RPC call fails.
   */
  override async listProjects(): Promise<readonly backend.ListedProject[]> {
    const result = await this.projectManager.listProjects({})
    return result.projects.map((project) => ({
      name: project.name,
      organizationId: backend.OrganizationId('organization-'),
      projectId: newProjectId(project.id, this.projectManager.rootDirectory),
      packageName: project.name,
      state: {
        type: backend.ProjectState.closed,
        volumeId: '',
      },
      jsonAddress: null,
      binaryAddress: null,
      ydocAddress: null,
    }))
  }

  /**
   * Create a project.
   * @throws An error if the JSON-RPC call fails.
   */
  override async createProject(
    body: backend.CreateProjectRequestBody,
  ): Promise<backend.CreatedProject> {
    const projectsDirectory =
      body.parentDirectoryId == null ? null : extractTypeAndId(body.parentDirectoryId).id
    const project = await this.projectManager.createProject({
      name: projectManager.ProjectName(body.projectName),
      ...(body.projectTemplateName != null ? { projectTemplate: body.projectTemplateName } : {}),
      missingComponentAction: projectManager.MissingComponentAction.install,
      ...(projectsDirectory == null ? {} : { projectsDirectory }),
    })
    return {
      name: project.projectName,
      organizationId: backend.OrganizationId('organization-'),
      projectId: newProjectId(
        project.projectId,
        projectsDirectory ?? this.projectManager.rootDirectory,
      ),
      packageName: project.projectName,
      state: { type: backend.ProjectState.closed, volumeId: '' },
    }
  }

  /**
   * Close the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails.
   */
  override async closeProject(projectId: backend.ProjectId, title: string | null): Promise<void> {
    const { id } = extractTypeAndId(projectId)
    try {
      const state = this.projectManager.projects.get(id)
      if (state?.state === backend.ProjectState.openInProgress) {
        // Projects that are not opened cannot be closed.
        // This is the only way to wait until the project is open.
        await this.projectManager.openProject({
          projectId: id,
          missingComponentAction: projectManager.MissingComponentAction.install,
        })
      }
      await this.projectManager.closeProject({ projectId: id })
      return
    } catch (error) {
      throw new Error(
        `Could not close project ${title != null ? `'${title}'` : `with ID '${projectId}'`}: ${
          tryGetMessage(error) ?? 'unknown error'
        }.`,
      )
    }
  }

  /**
   * Return asset details.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getAssetDetails<
    Id extends backend.RealAssetId,
    Type extends backend.RealAssetTypeId<Id>,
    ReturnType extends Id extends backend.DirectoryId ?
      backend.Asset<backend.AssetType.directory> | null
    : backend.Asset<Type>,
  >(assetId: Id): Promise<ReturnType> {
    const extracted = extractTypeAndId(assetId)

    const parentPath = extracted.directory

    // Consider the root directory as a virtual directory.
    if (extracted.type === backend.AssetType.directory && extracted.id === this.rootPath()) {
      // eslint-disable-next-line no-restricted-syntax
      return null as never
    }

    const directoryContents = await this.listDirectory({
      parentId: newDirectoryId(parentPath),
      filterBy: null,
      labels: null,
      recentProjects: false,
      rootPath: this.rootPath(),
    })

    const entry = directoryContents.find((content) => content.id === assetId)

    if (entry == null) {
      if (backend.isDirectoryId(assetId)) {
        throw new backend.DirectoryDoesNotExistError()
      }

      throw new backend.AssetDoesNotExistError()
    }

    // eslint-disable-next-line no-restricted-syntax
    return entry as never
  }

  /**
   * Close the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails.
   */
  override async getProjectDetails(projectId: backend.ProjectId): Promise<backend.Project> {
    const { id, directory } = extractTypeAndId(projectId)
    const state = this.projectManager.projects.get(id)
    if (state == null) {
      const entries = await this.projectManager.listDirectory(directory)
      const project = entries
        .flatMap((entry) =>
          entry.type === projectManager.FileSystemEntryType.ProjectEntry ? [entry.metadata] : [],
        )
        .find((metadata) => metadata.id === id)
      if (project == null) {
        throw new Error(`Could not get details of project.`)
      } else {
        return {
          name: project.name,
          jsonAddress: null,
          binaryAddress: null,
          ydocAddress: null,
          organizationId: backend.OrganizationId('organization-'),
          packageName: project.name,
          projectId,
          state: { type: backend.ProjectState.closed, volumeId: '' },
        }
      }
    } else {
      const cachedProject = await state.data
      return {
        name: cachedProject.projectName,
        jsonAddress: ipWithSocketToAddress(cachedProject.languageServerJsonAddress),
        binaryAddress: ipWithSocketToAddress(cachedProject.languageServerBinaryAddress),
        ydocAddress: null,
        organizationId: backend.OrganizationId('organization-'),
        packageName: cachedProject.projectNormalizedName,
        projectId,
        state: {
          type: backend.ProjectState.opened,
          volumeId: '',
        },
      }
    }
  }

  /**
   * Prepare a project for execution.
   * @throws An error if the JSON-RPC call fails.
   */
  override async openProject(
    projectId: backend.ProjectId,
    body: backend.OpenProjectRequestBody | null,
    title: string | null,
  ): Promise<void> {
    const { id } = extractTypeAndId(projectId)
    try {
      await this.projectManager.openProject({
        projectId: id,
        missingComponentAction: projectManager.MissingComponentAction.install,
        ...(body?.cloudProjectDirectoryPath != null ?
          {
            cloudProjectDirectoryPath: body.cloudProjectDirectoryPath,
          }
        : {}),
        ...(body?.parentId != null ?
          { projectsDirectory: extractTypeAndId(body.parentId).id }
        : {}),
      })
      return
    } catch (error) {
      throw new Error(
        `Could not open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}: ${
          tryGetMessage(error) ?? 'unknown error'
        }.`,
      )
    }
  }

  /**
   * Change the name of a project.
   * @throws An error if the JSON-RPC call fails.
   */
  override async updateProject(
    projectId: backend.ProjectId,
    body: backend.UpdateProjectRequestBody,
  ): Promise<backend.UpdatedProject> {
    const { id } = extractTypeAndId(projectId)
    if (body.projectName != null) {
      await this.projectManager.renameProject({
        projectId: id,
        name: projectManager.ProjectName(body.projectName),
      })
    }
    const parentPath = getDirectoryAndName(this.projectManager.getProjectPath(id)).directoryPath
    const result = await this.projectManager.listDirectory(parentPath)
    const project = result.flatMap((listedProject) =>
      (
        listedProject.type === projectManager.FileSystemEntryType.ProjectEntry &&
        listedProject.metadata.id === id
      ) ?
        [listedProject.metadata]
      : [],
    )[0]
    if (project == null) {
      throw new Error(`The project ID '${projectId}' is invalid.`)
    } else {
      return {
        name: project.name,
        organizationId: backend.OrganizationId('organization-'),
        projectId,
        packageName: project.name,
        state: { type: backend.ProjectState.closed },
      }
    }
  }

  /** Duplicate a specific version of a project. */
  override async duplicateProject(projectId: backend.ProjectId): Promise<backend.CreatedProject> {
    const typeAndId = extractTypeAndId(projectId)
    const id = typeAndId.id
    const project = await this.projectManager.duplicateProject({ projectId: id })
    return {
      projectId: newProjectId(project.projectId, typeAndId.directory),
      name: project.projectName,
      packageName: project.projectNormalizedName,
      organizationId: backend.OrganizationId('organization-'),
      state: { type: backend.ProjectState.closed, volumeId: '' },
    }
  }

  /**
   * Delete an arbitrary asset.
   * @throws An error if the JSON-RPC call fails.
   */
  override async deleteAsset(
    assetId: backend.AssetId,
    _body: backend.DeleteAssetRequestBody,
    title: string | null,
  ): Promise<void> {
    const typeAndId = extractTypeAndId(assetId)
    switch (typeAndId.type) {
      case backend.AssetType.directory:
      case backend.AssetType.file: {
        await this.projectManager.deleteFile(typeAndId.id)
        return
      }
      case backend.AssetType.project: {
        try {
          await this.projectManager.deleteProject({ projectId: typeAndId.id })
          return
        } catch (error) {
          throw new Error(
            `Could not delete project ${
              title != null ? `'${title}'` : `with ID '${typeAndId.id}'`
            }: ${tryGetMessage(error) ?? 'unknown error'}.`,
          )
        }
      }
    }
  }

  /** Copy an arbitrary asset to another directory. */
  override async copyAsset(
    assetId: backend.AssetId,
    parentDirectoryId: backend.DirectoryId,
  ): Promise<backend.CopyAssetResponse> {
    const typeAndId = extractTypeAndId(assetId)
    if (typeAndId.type !== backend.AssetType.project) {
      throw new Error('Only projects can be copied on the Local Backend.')
    } else {
      const project = await this.projectManager.duplicateProject({ projectId: typeAndId.id })
      const projectPath = this.projectManager.projectPaths.get(typeAndId.id)
      const parentPath = projectPath == null ? null : getDirectoryAndName(projectPath).directoryPath
      if (parentPath !== extractTypeAndId(parentDirectoryId).id) {
        throw new Error('Cannot duplicate project to a different directory on the Local Backend.')
      } else {
        const asset = {
          id: newProjectId(project.projectId, parentPath),
          parentId: parentDirectoryId,
          title: project.projectName,
        }
        return { asset }
      }
    }
  }

  // === Endpoints that intentionally do not work on the Local Backend ===

  /**
   * Called for any function that does not make sense in the Local Backend.
   * @throws An error stating that the operation is intentionally unavailable on the local
   * backend.
   */
  invalidOperation(): never {
    throw new Error('Cannot manage users, folders, files, tags, and secrets on the local backend.')
  }

  /** Invalid operation. */
  override undoDeleteAsset(): Promise<void> {
    return this.invalidOperation()
  }

  /** Return an empty array. */
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
  override restoreUser() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteUser() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override removeUser() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override uploadUserPicture() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override changeUserGroup() {
    return this.invalidOperation()
  }

  /**
   * Get the current organization. Returns `null` because organizations do not exist on the
   * Local Backend. This is required for `rootDiretoryId` to work.
   */
  override async getOrganization(): Promise<backend.OrganizationInfo | null> {
    return Promise.resolve(null)
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
    return this.invalidOperation()
  }

  /** Create a directory. */
  override async createDirectory(
    body: backend.CreateDirectoryRequestBody,
  ): Promise<backend.CreatedDirectory> {
    const parentDirectoryPath =
      body.parentId == null ? this.projectManager.rootDirectory : extractTypeAndId(body.parentId).id
    const path = joinPath(parentDirectoryPath, body.title)
    await this.projectManager.createDirectory(path)
    return {
      id: newDirectoryId(path),
      parentId: newDirectoryId(parentDirectoryPath),
      title: body.title,
    }
  }

  /**
   * Change the parent directory of an asset.
   * Changing the description is NOT supported.
   */
  override async updateAsset(
    assetId: backend.AssetId,
    body: backend.UpdateAssetRequestBody,
  ): Promise<void> {
    // Changing description is not supported on the Local Backend.
    const { parentDirectoryId, title } = body

    const typeAndId = extractTypeAndId(assetId)

    const currentParentDirectoryPath = (() => {
      return typeAndId.type !== backend.AssetType.project ?
          typeAndId.id
        : this.projectManager.getProjectPath(typeAndId.id)
    })()

    const newParentDirectoryPath = (() => {
      const fileName = title == null ? getFileName(currentParentDirectoryPath) : title

      if (parentDirectoryId == null) {
        return joinPath(
          projectManager.Path(currentParentDirectoryPath.split('/').slice(0, -1).join('/')),
          fileName,
        )
      }

      return joinPath(extractTypeAndId(parentDirectoryId).id, fileName)
    })()

    await this.projectManager.moveFile(currentParentDirectoryPath, newParentDirectoryPath)

    // Changing the folder name for a project is _not_ enough,
    // we also need to change the name in the package.yaml file.
    if (typeAndId.type === backend.AssetType.project && title != null) {
      await this.projectManager.renameProject({
        projectId: typeAndId.id,
        name: projectManager.ProjectName(title),
      })
    }
  }

  /** Begin uploading a large file. */
  override async uploadFileStart(
    body: backend.UploadFileRequestParams,
    file: File,
  ): Promise<backend.UploadLargeFileMetadata> {
    const parentPath =
      body.parentDirectoryId == null ?
        this.projectManager.rootDirectory
      : extractTypeAndId(body.parentDirectoryId).id
    const filePath = joinPath(parentPath, body.fileName)
    const uploadId = uniqueString()
    if (backend.fileIsNotProject(file)) {
      const searchParams = new URLSearchParams([
        ['file_name', body.fileName],
        ...(body.parentDirectoryId == null ? [] : [['directory', parentPath]]),
      ]).toString()
      const path = `/api/upload-file?${searchParams}`
      await fetch(path, { method: 'POST', body: file })
      this.uploadedFiles.set(uploadId, { id: newFileId(filePath), project: null })
    } else {
      const title = backend.stripProjectExtension(body.fileName)
      let id: string
      if (
        'backendApi' in window &&
        // This non-standard property is defined in Electron.
        'path' in file &&
        typeof file.path === 'string' &&
        file.path !== ''
      ) {
        const projectInfo = await window.backendApi.importProjectFromPath(
          file.path,
          parentPath,
          title,
        )
        id = projectInfo.id
      } else {
        const searchParams = new URLSearchParams({
          directory: parentPath,
          name: title,
        }).toString()
        const path = `/api/upload-project?${searchParams}`
        const response = await fetch(path, { method: 'POST', body: file })
        id = await response.text()
      }
      const projectId = newProjectId(projectManager.UUID(id), parentPath)
      const project = await this.getProjectDetails(projectId)
      this.uploadedFiles.set(uploadId, { id: projectId, project })
    }
    return { presignedUrls: [], uploadId, sourcePath: backend.S3FilePath('') }
  }

  /** Upload a chunk of a large file. */
  override uploadFileChunk(): Promise<backend.S3MultipartPart> {
    // Do nothing, the entire file has already been uploaded in `uploadFileStart`.
    return Promise.resolve({ eTag: '', partNumber: 0 })
  }

  /** Finish uploading a large file. */
  override uploadFileEnd(
    body: backend.UploadFileEndRequestBody,
  ): Promise<backend.UploadedLargeAsset> {
    // Do nothing, the entire file has already been uploaded in `uploadFileStart`.
    const file = this.uploadedFiles.get(body.uploadId)
    invariant(file, 'Uploaded file not found')
    return Promise.resolve(file)
  }

  /** Change the name of a file. */
  override async updateFile(
    fileId: backend.FileId,
    body: backend.UpdateFileRequestBody,
  ): Promise<void> {
    const typeAndId = extractTypeAndId(fileId)
    const from = typeAndId.id
    const folderPath = getFolderPath(from)
    const to = joinPath(projectManager.Path(folderPath), body.title)
    await this.projectManager.moveFile(from, to)
  }

  /** Get the path of a project. */
  getProjectPath(id: backend.ProjectId) {
    return this.projectManager.getProjectPath(extractTypeAndId(id).id)
  }

  /** Construct a new path using the given parent directory and a file name. */
  joinPath(parentId: backend.DirectoryId, fileName: string) {
    return joinPath(extractTypeAndId(parentId).id, fileName)
  }

  /** Change the name of a directory. */
  override async updateDirectory(
    directoryId: backend.DirectoryId,
    body: backend.UpdateDirectoryRequestBody,
  ): Promise<backend.UpdatedDirectory> {
    const from = extractTypeAndId(directoryId).id
    const folderPath = projectManager.Path(getFolderPath(from))
    const to = joinPath(folderPath, body.title)
    await this.projectManager.moveFile(from, to)
    return {
      id: newDirectoryId(to),
      parentId: newDirectoryId(folderPath),
      title: body.title,
    }
  }

  /** Download an asset. */
  override async download(
    id: backend.AssetId,
    title: string,
    _targetDirectoryId: backend.DirectoryId | null,
    shouldUnpackProject = true,
  ) {
    const asset = backend.extractTypeFromId(id)
    if (asset.type === backend.AssetType.project) {
      const typeAndId = extractTypeAndId(asset.id)
      const queryString = new URLSearchParams({
        projectsDirectory: typeAndId.directory,
      }).toString()

      await download({
        url: `/api/project-manager/projects/${typeAndId.id}/enso-project?${queryString}`,
        name: `${title}.enso-project`,
        electronOptions: {
          shouldUnpackProject,
        },
      })
    }
    await Promise.resolve()
  }

  /** Invalid operation. */
  override restoreAsset() {
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
  override listProjectSessions() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getProjectSessionLogs() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createProjectExecution() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getProjectExecutionDetails() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override updateProjectExecution() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteProjectExecution() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override listProjectExecutions() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override syncProjectExecution() {
    return this.invalidOperation()
  }

  /**
   * Get the content of a file.
   *
   * Versioning is not supported on the Local Backend, thus the `versionId` parameter is ignored.
   */
  override getFileContent(projectId: backend.ProjectId) {
    return this.projectManager.getFileContent(extractTypeAndId(projectId).id)
  }

  /**
   * Resolve the path of a project asset relative to the project `src` directory.
   */
  override resolveProjectAssetPath(projectId: backend.ProjectId, relativePath: string) {
    const projectPath = this.getProjectPath(projectId)

    return Promise.resolve(`enso://${projectPath}/src/${relativePath.replace('./', '')}`)
  }

  /** Invalid operation. */
  override createDatalink() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getDatalink() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteDatalink() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createSecret() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createCredential() {
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

  /**
   * Return an empty array. This function is required to be implemented as it is unconditionally
   * called, but its result should never need to be used.
   */
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
  override createUserGroup() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createCheckoutSession() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteUserGroup() {
    return this.invalidOperation()
  }

  /** Return an empty array. */
  override listUserGroups() {
    return Promise.resolve([])
  }

  /** Invalid operation. */
  override getCheckoutSession() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override listInvitations() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteInvitation() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override resendInvitation() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override acceptInvitation() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override declineInvitation() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override getLogEvents() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override logEvent() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createCustomerPortalSession() {
    return this.invalidOperation()
  }
}

markRaw(LocalBackend.prototype)
