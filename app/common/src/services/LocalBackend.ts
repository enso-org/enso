/**
 * @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link LocalBackend} in this module corresponds to an API endpoint.
 * The functions are asynchronous and return a {@link Promise} that resolves to the response from
 * the API.
 */
import { markRaw } from 'vue'
import { PRODUCT_NAME } from '../constants.js'
import type { DownloadOptions } from '../download.js'
import type { DefaultGetText } from '../text.js'
import { toReadableIsoString } from '../utilities/data/dateTime.js'
import { tryGetMessage } from '../utilities/errors.js'
import {
  fileExtension,
  getDirectoryAndName,
  getFileName,
  getFolderPath,
  joinPath,
  normalizePath,
  normalizeSlashes,
} from '../utilities/file.js'
import { uniqueString } from '../utilities/uniqueString.js'
import * as backend from './Backend.js'
import { downloadProjectPath, EXPORT_ARCHIVE_PATH } from './Backend/remoteBackendPaths.js'
import { HttpClient } from './HttpClient.js'
import type { ProjectManager } from './ProjectManager/ProjectManager.js'
import {
  MissingComponentAction,
  Path,
  ProjectName,
  type IpWithSocket,
} from './ProjectManager/types.js'

const LOCAL_API_URL = '/api/'

/** Convert a {@link projectManager.IpWithSocket} to a {@link backend.Address}. */
function ipWithSocketToAddress(ipWithSocket: IpWithSocket) {
  return backend.Address(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

export const DIRECTORY_ID_PREFIX = `${backend.AssetType.directory}-`
export const PROJECT_ID_PREFIX = `${backend.AssetType.project}-`
export const FILE_ID_PREFIX = `${backend.AssetType.file}-`

/** Create a {@link backend.DirectoryId} from a path. */
export function newDirectoryId(path: Path) {
  return backend.DirectoryId(`${DIRECTORY_ID_PREFIX}${encodeURIComponent(path)}` as const)
}

/** Create a {@link backend.ProjectId} from a path. */
export function newProjectId(path: Path) {
  return backend.ProjectId(`${PROJECT_ID_PREFIX}${encodeURIComponent(path)}`)
}

/** Check if given string resembles KSUID. */
function isKsuid(candidate: string) {
  return /^[a-zA-Z0-9]{27}$/.test(candidate)
}

/** Check if given {@link backend.ProjectId} represents a local project. */
export function isLocalProjectId(projectId: backend.ProjectId): boolean {
  // Local projects use path after the prefix, cloud projects have a KSUID right after prefix.
  return (
    projectId.startsWith(PROJECT_ID_PREFIX) &&
    !isKsuid(projectId.substring(PROJECT_ID_PREFIX.length))
  )
}

/** Create a {@link backend.FileId} from a path. */
export function newFileId(path: Path) {
  return backend.FileId(`${FILE_ID_PREFIX}${encodeURIComponent(path)}`)
}

/**
 * Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard.
 */
export class LocalBackend extends backend.Backend {
  static readonly type = backend.BackendType.local
  override readonly type = LocalBackend.type
  override readonly baseUrl = new URL(LOCAL_API_URL, location.href)
  /** All files that have been uploaded to the Project Manager. */
  uploadedFiles: Map<string, backend.UploadedAsset> = new Map()
  private readonly projectManager: ProjectManager
  private readonly getLocalRootDirectory: () => Path | null
  private readonly getFilePath: ((item: File) => string) | undefined

  /** Create a {@link LocalBackend}. */
  constructor(
    getText: DefaultGetText,
    projectManagerInstance: ProjectManager,
    client = new HttpClient(),
    downloader: (options: DownloadOptions) => void | Promise<void>,
    getLocalRootDirectory: () => Path | null,
    getFilePath: ((item: File) => string) | undefined,
  ) {
    super(getText, client, downloader)
    this.projectManager = projectManagerInstance
    this.getLocalRootDirectory = getLocalRootDirectory
    this.getFilePath = getFilePath
  }

  /** The root directory of this backend. */
  rootPath() {
    // TODO: We have settings in Electron for this, but not in the node CLI.
    // We need to figure out where to store this setting for the node CLI as well,
    // so that it is synced between both.
    return this.getLocalRootDirectory() ?? this.projectManager.rootDirectory
  }

  /** Return the ID of the root directory. */
  override rootDirectoryId(
    _user: backend.User,
    _organization: backend.OrganizationInfo | null,
  ): backend.DirectoryId {
    return newDirectoryId(this.rootPath())
  }

  /**
   * Return a list of assets in a directory.
   * @throws An error if the JSON-RPC call fails.
   */
  override async listDirectory(
    query: backend.ListDirectoryRequestParams & { readonly recursive?: boolean },
  ): Promise<backend.ListDirectoryResponseBody> {
    if (query.filterBy != null && query.filterBy !== backend.FilterBy.active) {
      return { assets: [], paginationToken: null }
    }
    const { rootPath = this.rootPath() } = query
    const parentIdRaw =
      query.parentId == null ? null : backend.extractTypeAndPath(query.parentId).path
    const parentId = query.parentId ?? newDirectoryId(this.projectManager.rootDirectory)

    // Catch the case where the directory does not exist.
    let result: backend.AnyRealAsset[] = []
    try {
      const entries = await this.projectManager.listDirectory(parentIdRaw)
      result = await Promise.all(
        entries.map(async (entry) => {
          const virtualParentsPath = entry.path.replace(rootPath, '').replace(/^[/\\]|[/\\]$/g, '')

          const parentsPath = (() => {
            const parentsPathArray: backend.DirectoryId[] =
              entry.path.startsWith(rootPath) ? [newDirectoryId(rootPath)] : []
            const splitPath = virtualParentsPath.split('/')
            let previousPath = entry.path.startsWith(rootPath) ? rootPath : backend.Path('')
            for (const directory of splitPath) {
              if (directory === '') continue
              previousPath = backend.Path((previousPath + '/' + directory).replace(/\/$/g, ''))
              parentsPathArray.push(newDirectoryId(previousPath))
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
          } satisfies Partial<backend.DirectoryAsset>

          switch (entry.type) {
            case 'DirectoryEntry': {
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
            case 'ProjectEntry': {
              return {
                ...shared,
                type: backend.AssetType.project,
                id: newProjectId(entry.path),
                title: entry.metadata.name,
                modifiedAt: entry.metadata.lastOpened ?? entry.metadata.created,
                parentId,
                projectState: {
                  type:
                    (await this.projectManager.getProject(entry.path))?.state ??
                    backend.ProjectState.closed,
                },
              } satisfies backend.ProjectAsset
            }
            case 'FileEntry': {
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
        }),
      )
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
    result.sort((a, b) => backend.compareAssets(a, b, query.sortExpression, query.sortDirection))
    const from = query.from as backend.AssetId | null
    const index = from == null ? 0 : result.findIndex((asset) => asset.id === from) + 1
    const assets = result.slice(index, query.pageSize != null ? index + query.pageSize : undefined)
    const last = assets.at(-1)
    return {
      assets,
      paginationToken: last ? backend.PaginationToken(String(last.id)) : null,
    }
  }

  /** Recursively search for assets in a directory. */
  override async searchDirectory(
    query: backend.SearchDirectoryRequestParams,
  ): Promise<backend.ListDirectoryResponseBody> {
    const result = await this.listDirectory({
      parentId: query.parentId,
      filterBy: null,
      labels: query.labels,
      sortDirection: query.sortDirection,
      sortExpression: query.sortExpression,
      recentProjects: false,
      from: null,
      pageSize: null,
      recursive: true,
    })
    const fullAssetList = result.assets.filter(backend.doesAssetMatchQuery(query))
    const from = query.from as backend.AssetId | null
    const index = from == null ? 0 : fullAssetList.findIndex((asset) => asset.id === from) + 1
    const assets = fullAssetList.slice(
      index,
      query.pageSize != null ? index + query.pageSize : undefined,
    )
    const last = assets.at(-1)
    return {
      assets,
      paginationToken: last ? backend.PaginationToken(String(last.id)) : null,
    }
  }

  /**
   * Create a project.
   * @throws An error if the JSON-RPC call fails.
   */
  override async createProject(
    body: backend.CreateProjectRequestBody,
  ): Promise<backend.CreatedProject> {
    const projectsDirectory =
      body.parentDirectoryId == null ?
        this.projectManager.rootDirectory
      : backend.extractTypeAndPath(body.parentDirectoryId).path
    const project = await this.projectManager.createProject({
      name: ProjectName(body.projectName),
      missingComponentAction: MissingComponentAction.install,
      projectsDirectory,
    })
    return {
      name: project.projectName,
      organizationId: backend.OrganizationId('organization-'),
      projectId: newProjectId(project.projectPath),
      packageName: project.projectName,
      state: { type: backend.ProjectState.closed, volumeId: '' },
      ensoPath: backend.EnsoPath(`${projectsDirectory}/${project.projectNormalizedName}`),
    }
  }

  /**
   * Close the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails.
   */
  override async closeProject(projectId: backend.ProjectId, title: string | null): Promise<void> {
    const { path } = backend.extractTypeAndPath(projectId)
    try {
      await this.projectManager.closeProject({ projectPath: path })
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
  override async getAssetDetails<Id extends backend.AssetId>(
    assetId: Id,
    rootPath: backend.Path | undefined,
  ) {
    const { path } = backend.extractTypeAndPath(assetId)
    const { directoryPath } = getDirectoryAndName(path)
    const { assets } = await this.listDirectory({
      parentId: newDirectoryId(directoryPath),
      filterBy: null,
      labels: null,
      recentProjects: false,
      rootPath: rootPath ?? this.rootPath(),
      from: null,
      pageSize: null,
      sortDirection: null,
      sortExpression: null,
    })
    const entry = assets.find((content) => content.id === assetId)
    if (entry == null) {
      if (backend.isDirectoryId(assetId)) {
        throw new backend.DirectoryDoesNotExistError()
      }
      throw new backend.AssetDoesNotExistError()
    }
    return entry as unknown as backend.AssetDetailsResponse<Id>
  }

  /** Get the UUID of a project. */
  getProjectId(path: backend.Path) {
    return this.projectManager.getProjectId(path)
  }

  /**
   * Returns information about the project identified by the given project ID.
   * @throws An error if the JSON-RPC call fails.
   */
  override async getProjectDetails(
    projectId: backend.ProjectId,
    _getPresignedUrl = false,
  ): Promise<backend.Project> {
    const { path } = backend.extractTypeAndPath(projectId)
    const { directoryPath } = getDirectoryAndName(path)
    const state = await this.projectManager.getProject(path)
    if (state == null) {
      const entries = await this.projectManager.listDirectory(directoryPath)
      const project = entries
        .filter((entry) => entry.type === 'ProjectEntry')
        .find((metadata) => metadata.path === path)?.metadata
      if (project == null) {
        throw new Error(`Could not get details of project.`)
      } else {
        const ensoPathRaw = normalizePath(path)
        const ensoPath = backend.EnsoPath(ensoPathRaw)
        return {
          name: project.name,
          jsonAddress: null,
          binaryAddress: null,
          ydocAddress: null,
          organizationId: backend.OrganizationId('organization-'),
          packageName: project.name,
          projectId,
          state: { type: backend.ProjectState.closed, volumeId: '' },
          url: backend.HttpsUrl(this.resolvePath(downloadProjectPath(projectId))),
          ensoPath,
        }
      }
    } else {
      const cachedProject = await state.data
      return {
        name: cachedProject.projectName,
        jsonAddress: ipWithSocketToAddress(cachedProject.languageServerJsonAddress),
        binaryAddress: ipWithSocketToAddress(cachedProject.languageServerBinaryAddress),
        ydocAddress:
          cachedProject.languageServerYdocAddress ?
            ipWithSocketToAddress(cachedProject.languageServerYdocAddress)
          : backend.Address('ws://localhost:5976'),
        organizationId: backend.OrganizationId('organization-'),
        packageName: cachedProject.projectNormalizedName,
        projectId,
        state: {
          type: backend.ProjectState.opened,
          volumeId: '',
        },
        url: backend.HttpsUrl(this.resolvePath(downloadProjectPath(projectId))),
        ensoPath: backend.EnsoPath(`${directoryPath}/${cachedProject.projectNormalizedName}`),
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
    const { path } = backend.extractTypeAndPath(projectId)
    try {
      await this.projectManager.openProject({
        projectPath: path,
        missingComponentAction: MissingComponentAction.install,
        ...(body?.openHybridProjectParameters != null ?
          { cloud: body.openHybridProjectParameters }
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
    const { path } = backend.extractTypeAndPath(projectId)
    if (body.projectName != null) {
      await this.projectManager.renameProject({
        projectPath: path,
        name: ProjectName(body.projectName),
      })
    }
    const parentPath = getDirectoryAndName(path).directoryPath
    const result = await this.projectManager.listDirectory(parentPath)
    const project = result.flatMap((listedProject) =>
      listedProject.type === 'ProjectEntry' && listedProject.path === path ?
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
    const { path } = backend.extractTypeAndPath(projectId)
    const project = await this.projectManager.duplicateProject({ projectPath: path })
    return {
      projectId: newProjectId(project.projectPath),
      name: project.projectName,
      packageName: project.projectNormalizedName,
      organizationId: backend.OrganizationId('organization-'),
      state: { type: backend.ProjectState.closed, volumeId: '' },
      ensoPath: backend.EnsoPath(`${path}/${project.projectNormalizedName}`),
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
    const { type, path } = backend.extractTypeAndPath(assetId)
    switch (type) {
      case backend.AssetType.directory:
      case backend.AssetType.file: {
        await this.projectManager.deleteFile(path)
        return
      }
      case backend.AssetType.project: {
        try {
          await this.projectManager.deleteProject({ projectPath: path })
          return
        } catch (error) {
          throw new Error(
            `Could not delete project ${
              title != null ? `'${title}'` : `with ID '${path}'`
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
    const { type, path } = backend.extractTypeAndPath(assetId)
    if (type !== backend.AssetType.project) {
      throw new Error('Only projects can be copied on the Local Backend.')
    } else {
      const project = await this.projectManager.duplicateProject({ projectPath: path })
      const parentPath = getDirectoryAndName(path).directoryPath
      if (parentPath !== backend.extractTypeAndPath(parentDirectoryId).path) {
        throw new Error('Cannot duplicate project to a different directory on the Local Backend.')
      }
      const asset = await this.getAssetDetails(newProjectId(project.projectPath), undefined)
      if (!asset) {
        throw new Error('Could not retrieve details of duplicated project.')
      }
      return { asset }
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

  /** Do nothing. This function should never need to be called. */
  override getPaymentsConfig() {
    return this.invalidOperation()
  }

  /** Create a directory. */
  override async createDirectory(
    body: backend.CreateDirectoryRequestBody,
  ): Promise<backend.CreatedDirectory> {
    const parentDirectoryPath =
      body.parentId == null ?
        this.projectManager.rootDirectory
      : backend.extractTypeAndPath(body.parentId).path
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
    const { type, path } = backend.extractTypeAndPath(assetId)
    // FIXME: This is the path, not the `currentParentDirectoryPath`, I'm pretty sure
    const currentParentDirectoryPath = path

    const newParentDirectoryPath = (() => {
      const fileName = title == null ? getFileName(currentParentDirectoryPath) : title

      if (parentDirectoryId == null) {
        return joinPath(
          Path(currentParentDirectoryPath.split('/').slice(0, -1).join('/')),
          fileName,
        )
      }

      return joinPath(backend.extractTypeAndPath(parentDirectoryId).path, fileName)
    })()

    await this.projectManager.moveFile(currentParentDirectoryPath, newParentDirectoryPath)

    // Changing the folder name for a project is not enough,
    // we also need to change the name in the package.yaml file.
    if (type === backend.AssetType.project && title != null) {
      await this.projectManager.renameProject({
        projectPath: path,
        name: ProjectName(title),
      })
    }
  }

  /** Begin uploading a large file. */
  override async uploadFileStart(
    body: backend.UploadFileRequestParams,
    file: File | null,
  ): Promise<backend.UploadLargeFileMetadata> {
    const parentPath =
      body.parentDirectoryId == null ?
        this.projectManager.rootDirectory
      : backend.extractTypeAndPath(body.parentDirectoryId).path
    const filePath = joinPath(parentPath, body.fileName)
    const uploadId = uniqueString()
    const sourcePath = body.filePath ?? (file && this.getFilePath?.(file))
    const searchParams = new URLSearchParams([
      ['directory', newDirectoryId(parentPath)],
      ['file_name', body.fileName],
      ...(sourcePath != null ? [['file_path', sourcePath]] : []),
    ]).toString()
    const path = `/api/upload-file?${searchParams}`
    const response = await fetch(path, {
      method: 'POST',
      ...(sourcePath != null ? {} : { body: file }),
    })
    if (!response.ok) {
      return this.throw(response, 'uploadFileBackendError')
    }
    if (backend.fileNameIsProject(body.fileName)) {
      const projectPath = normalizeSlashes(await response.text())
      const projectId = newProjectId(projectPath)
      const project = await this.getProjectDetails(projectId)
      this.uploadedFiles.set(uploadId, { id: projectId, project, jobId: null })
    } else if (backend.fileNameIsArchive(body.fileName)) {
      this.uploadedFiles.set(uploadId, {
        id: newFileId(filePath),
        project: null,
        jobId: backend.UnzipAssetsJobId(await response.text()),
      })
    } else {
      this.uploadedFiles.set(uploadId, { id: newFileId(filePath), project: null, jobId: null })
    }
    return { presignedUrls: [], uploadId, sourcePath: backend.S3FilePath('') }
  }

  /** Upload a chunk of a large file. */
  override uploadFileChunk(): Promise<{ part: backend.S3MultipartPart; size: number }> {
    // Do nothing, the entire file has already been uploaded in `uploadFileStart`.
    return Promise.resolve({ part: { eTag: '', partNumber: 0 }, size: 0 })
  }

  /** Finish uploading a large file. */
  override uploadFileEnd(body: { uploadId: string }): Promise<backend.UploadedAsset> {
    // Do nothing, the entire file has already been uploaded in `uploadFileStart`.
    const file = this.uploadedFiles.get(body.uploadId)
    if (!file) {
      throw new Error('Uploaded file not found')
    }
    return Promise.resolve(file)
  }

  /**
   * Upload set of Images, resolving any possible conflicts. The sum of file sizes may not
   * exceed cloud message limit.
   */
  override uploadImage(
    _parentDirectoryId: backend.DirectoryId,
    _files: { data: Blob; name: string }[],
  ): Promise<backend.UploadedImages> {
    this.invalidOperation()
  }

  /** Change the name of a file. */
  override async updateFile(
    fileId: backend.FileId,
    body: backend.UpdateFileRequestBody,
  ): Promise<void> {
    const typeAndId = backend.extractTypeAndPath(fileId)
    const from = typeAndId.path
    const folderPath = getFolderPath(from)
    const to = joinPath(Path(folderPath), body.title)
    await this.projectManager.moveFile(from, to)
  }

  /** Construct a new path using the given parent directory and a file name. */
  joinPath(parentId: backend.DirectoryId, fileName: string) {
    return joinPath(backend.extractTypeAndPath(parentId).path, fileName)
  }

  /** Change the name of a directory. */
  override async updateDirectory(
    directoryId: backend.DirectoryId,
    body: backend.UpdateDirectoryRequestBody,
  ): Promise<backend.UpdatedDirectory> {
    const from = backend.extractTypeAndPath(directoryId).path
    const folderPath = Path(getFolderPath(from))
    const to = joinPath(folderPath, body.title)
    await this.projectManager.moveFile(from, to)
    return {
      id: newDirectoryId(to),
      parentId: newDirectoryId(folderPath),
      title: body.title,
    }
  }

  /** Resolve path to asset. In case of LocalBackend, this is just the filesystem path. */
  override resolveEnsoPath(path: backend.EnsoPath): Promise<backend.AnyAsset> {
    const { directoryPath } = getDirectoryAndName(Path(path as string))
    return this.findAsset(directoryPath, 'ensoPath', path)
  }

  /** Resolve the data of a project asset relative to the project root directory. */
  override async resolveProjectAssetData(
    projectId: backend.ProjectId,
    relativePath: string,
  ): Promise<Response> {
    return await this.projectManager.getFileContent(
      backend.extractTypeAndPath(projectId).path,
      relativePath,
    )
  }

  /** Download an asset. */
  override async download(
    id: backend.AssetId,
    title: string,
    targetDirectoryId: backend.DirectoryId | null,
    shouldUnpackProject = false,
  ) {
    const asset = backend.extractTypeFromId(id)
    const targetPath = targetDirectoryId ? backend.extractTypeAndPath(targetDirectoryId).path : null

    switch (asset.type) {
      case backend.AssetType.project: {
        const details = await this.getProjectDetails(asset.id, true)
        if (details.url == null) {
          throw new Error('The download URL of the project must be present.')
        }
        await this.downloader({
          url: details.url,
          name: `${title}.enso-project`,
          electronOptions: {
            shouldUnpackProject,
            path: targetPath,
            showFileDialog: true,
          },
        })
        break
      }
      case backend.AssetType.file: {
        const details = await this.getFileDetails(asset.id, title, true)
        if (details.url == null) {
          throw new Error('The download URL of the file must be present.')
        }
        await this.downloader({
          url: details.url,
          name: details.file.fileName ?? '',
          electronOptions: {
            path: targetPath,
            showFileDialog: true,
          },
        })
        break
      }
      case backend.AssetType.datalink:
      case backend.AssetType.secret:
      case backend.AssetType.directory: {
        throw new Error(`'${asset.type}' assets cannot be downloaded.`)
      }
    }
  }

  /** Export multiple files and pack into an archive. */
  override async exportArchive(
    params: backend.ExportArchiveParams,
  ): Promise<backend.ExportedArchive> {
    const { filePath, ...body } = params
    const searchParams = new URLSearchParams(filePath != null ? { filePath } : {}).toString()
    const path = `${EXPORT_ARCHIVE_PATH}?${searchParams}`
    if (params.filePath != null) {
      // Assume it is Electron, copy files through Electron server directly
      const response = await this.post<backend.ExportedArchive>(path, body)
      if (!response.ok) {
        return this.throw(response, 'exportArchiveBackendError')
      }
      return await response.json()
    } else {
      // Download files as HTTP stream
      const secondsString = new Date().getSeconds().toString().padStart(2, '0')
      const dateString = `${toReadableIsoString(new Date()).replace(/[:]/g, ' ')} ${secondsString}`
      await this.downloader({
        url: this.resolvePath(path),
        name: `${PRODUCT_NAME} ${dateString}.zip`,
      })
      return { filePath: null }
    }
  }

  /** Start watching project directory. */
  async startWatchingHybridProject(
    assetId: backend.AssetId,
    localProjectId: backend.ProjectId,
    parentDirectoryId: backend.DirectoryId,
    defaultHeaders: Record<string, string>,
  ): Promise<void> {
    const localProjectDirectory = backend.extractTypeAndPath(localProjectId).path
    const queryString = new URLSearchParams({
      assetId,
      parentDirectoryId,
      directory: localProjectDirectory,
    }).toString()
    const response = await this.post(
      new URL(`/api/watcher/start?${queryString}`, location.href).toString(),
      defaultHeaders,
    )
    if (!response.ok) {
      return await this.throw(response, 'resolveProjectAssetPathBackendError')
    }
  }

  /**
   * Stop watching project directory.
   * @returns true if the project directory has unsaved changes.
   */
  async stopWatchingHybridProject(assetId: backend.AssetId): Promise<boolean> {
    const queryString = new URLSearchParams({
      assetId,
    }).toString()
    const response = await this.post(
      new URL(`/api/watcher/stop?${queryString}`, location.href).toString(),
      null,
    )
    if (!response.ok) {
      return await this.throw(response, 'resolveProjectAssetPathBackendError')
    }
    const httpStatusIsDirty = 201
    return response.status === httpStatusIsDirty
  }

  /**
   * Get the state of the watched project directory.
   * @returns true if the project directory has unsaved changes.
   */
  async getStateOfWatchedHybridProject(assetId: backend.AssetId): Promise<boolean> {
    const queryString = new URLSearchParams({
      assetId,
    }).toString()
    const response = await this.get(
      new URL(`/api/watcher/state?${queryString}`, location.href).toString(),
    )
    if (!response.ok) {
      return await this.throw(response, 'resolveProjectAssetPathBackendError')
    }
    const httpStatusIsDirty = 201
    return response.status === httpStatusIsDirty
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
  override cancelSubscription() {
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

  /** Invalid operation. */
  override listApiKeys() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override createApiKey() {
    return this.invalidOperation()
  }

  /** Invalid operation. */
  override deleteApiKey() {
    return this.invalidOperation()
  }

  /** Invalid operation */
  override getMapboxToken() {
    return this.invalidOperation()
  }

  /** Find asset details using directory listing. */
  private async findAsset<Key extends keyof backend.AnyAsset>(
    directory: Path,
    key: Key,
    value: backend.AnyAsset[Key],
  ) {
    const directoryContents = await this.listDirectory({
      parentId: newDirectoryId(directory),
      filterBy: null,
      labels: null,
      recentProjects: false,
      rootPath: this.rootPath(),
      sortExpression: null,
      sortDirection: null,
      from: null,
      pageSize: null,
    })
    const entry = directoryContents.assets.find((content) => content[key] === value)
    if (entry == null) {
      if (backend.isDirectoryId(value)) {
        throw new backend.DirectoryDoesNotExistError()
      }
      throw new backend.AssetDoesNotExistError()
    }
    return entry as never
  }
}

markRaw(LocalBackend.prototype)
