/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as detect from 'enso-common/src/detect'

import type * as loggerProvider from '#/providers/LoggerProvider'
import * as backendModule from '#/services/backend'
import * as remoteBackendPaths from '#/services/remoteBackendPaths'
import * as config from '#/utilities/config'
import * as errorModule from '#/utilities/error'
import type * as http from '#/utilities/http'
import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_FIRST = 200
/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_LAST = 299
/** HTTP status indicating that the server encountered a fatal exception. */
const STATUS_SERVER_ERROR = 500

/** The number of milliseconds in one day. */
const ONE_DAY_MS = 86_400_000

/** Default HTTP body for an "open project" request. */
const DEFAULT_OPEN_PROJECT_BODY: backendModule.OpenProjectRequestBody = {
  forceCreate: false,
  executeAsync: false,
}

// ============================
// === responseIsSuccessful ===
// ============================

/** Whether a response has a success HTTP status code (200-299). */
function responseIsSuccessful(response: Response) {
  return response.status >= STATUS_SUCCESS_FIRST && response.status <= STATUS_SUCCESS_LAST
}

// ===============================
// === waitUntilProjectIsReady ===
// ===============================

/** The interval between requests checking whether the IDE is ready. */
const CHECK_STATUS_INTERVAL_MS = 5000

/** Return a {@link Promise} that resolves only when a project is ready to open. */
export async function waitUntilProjectIsReady(
  backend: backendModule.Backend,
  item: backendModule.ProjectAsset,
  abortController: AbortController = new AbortController()
) {
  let project = await backend.getProjectDetails(item.id, item.title)
  if (!backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[project.state.type]) {
    await backend.openProject(item.id, null, item.title)
  }
  let nextCheckTimestamp = 0
  while (
    !abortController.signal.aborted &&
    project.state.type !== backendModule.ProjectState.opened
  ) {
    await new Promise<void>(resolve => {
      const delayMs = nextCheckTimestamp - Number(new Date())
      setTimeout(resolve, Math.max(0, delayMs))
    })
    nextCheckTimestamp = Number(new Date()) + CHECK_STATUS_INTERVAL_MS
    project = await backend.getProjectDetails(item.id, item.title)
  }
}

// =============
// === Types ===
// =============

/** HTTP response body for the "list users" endpoint. */
export interface ListUsersResponseBody {
  users: backendModule.SimpleUser[]
}

/** HTTP response body for the "list directory" endpoint. */
export interface ListDirectoryResponseBody {
  assets: backendModule.AnyAsset[]
}

/** HTTP response body for the "list tags" endpoint. */
export interface ListTagsResponseBody {
  tags: backendModule.Label[]
}

// =====================
// === Smart objects ===
// =====================

/** A wrapper around a subset of the API endpoints. */
class SmartObject<T> implements backendModule.SmartObject<T> {
  /** Create a {@link SmartObject}. */
  constructor(
    protected readonly client: http.Client,
    protected readonly logger: loggerProvider.Logger,
    readonly value: T
  ) {}

  /** Return a copy of this object, but with a different value. */
  withValue(value: T): this {
    // This is SAFE (as long as no child classes override the constructor),
    // as it uses runtime reflection to get the constructor of the current object.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-return, @typescript-eslint/no-unsafe-call, no-restricted-syntax, @typescript-eslint/no-explicit-any
    return new (this.constructor as any)(this.client, this.logger, value)
  }

  /** Log an error message and throws an {@link Error} with the specified message.
   * @throws {Error} Always. */
  protected throw(message: string): never {
    this.logger.error(message)
    throw new Error(message)
  }

  /** Send an HTTP GET request to the given path. */
  protected httpGet<R = void>(path: string) {
    return this.client.get<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }

  /** Send a JSON HTTP POST request to the given path. */
  protected httpPost<R = void>(path: string, payload: object) {
    return this.client.post<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a binary HTTP POST request to the given path. */
  protected httpPostBinary<R = void>(path: string, payload: Blob) {
    return this.client.postBinary<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  protected httpPatch<R = void>(path: string, payload: object) {
    return this.client.patch<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PUT request to the given path. */
  protected httpPut<R = void>(path: string, payload: object) {
    return this.client.put<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send an HTTP DELETE request to the given path. */
  protected httpDelete<R = void>(path: string) {
    return this.client.delete<R>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }
}

/** A smart wrapper around a {@link backendModule.UserOrOrganization}. */
class SmartUser
  extends SmartObject<backendModule.UserOrOrganization>
  implements backendModule.SmartUser
{
  /** Change the username of the current user. */
  async update(body: backendModule.UpdateUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.UPDATE_CURRENT_USER_PATH
    const response = await this.httpPut(path, body)
    if (!responseIsSuccessful(response)) {
      if (body.username != null) {
        return this.throw('Could not change username.')
      } else {
        return this.throw('Could not update user.')
      }
    } else {
      return
    }
  }

  /** Delete the current user. */
  async delete(): Promise<void> {
    const response = await this.httpDelete(remoteBackendPaths.DELETE_USER_PATH)
    if (!responseIsSuccessful(response)) {
      return this.throw('Could not delete user.')
    } else {
      return
    }
  }

  /** Upload a new profile picture for the current user. */
  async uploadPicture(
    params: backendModule.UploadUserPictureRequestParams,
    file: Blob
  ): Promise<backendModule.UserOrOrganization> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_USER_PICTURE_PATH}?${paramsString}`
    const response = await this.httpPostBinary<backendModule.UserOrOrganization>(path, file)
    if (!responseIsSuccessful(response)) {
      return this.throw('Could not upload user profile picture.')
    } else {
      return await response.json()
    }
  }

  /** Get the root directory for this user. */
  rootDirectory(): backendModule.SmartDirectory {
    const rootDirectory = backendModule.createRootDirectoryAsset(this.value.rootDirectoryId)
    return new SmartDirectory(this.client, this.logger, rootDirectory)
  }
}

/** A smart wrapper around a {@link backendModule.AnyAsset}. */
class SmartAsset<T extends backendModule.AnyAsset = backendModule.AnyAsset>
  extends SmartObject<T>
  implements backendModule.SmartAsset
{
  /** The type of the wrapped value. */
  get type(): T['type'] {
    return this.value.type
  }

  /** Change the parent directory of an asset. */
  async update(body: backendModule.UpdateAssetRequestBody): Promise<unknown> {
    const path = remoteBackendPaths.updateAssetPath(this.value.id)
    const response = await this.httpPatch(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not update '${this.value.title}'.`)
    } else {
      return
    }
  }

  /** Move an arbitrary asset to the trash. */
  async delete(): Promise<void> {
    const path = remoteBackendPaths.deleteAssetPath(this.value.id)
    const response = await this.httpDelete(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Unable to delete '${this.value.title}'.`)
    } else {
      return
    }
  }

  /** Restore an arbitrary asset from the trash. */
  async undoDelete(): Promise<void> {
    const path = remoteBackendPaths.UNDO_DELETE_ASSET_PATH
    const response = await this.httpPatch(path, { assetId: this.value.id })
    if (!responseIsSuccessful(response)) {
      return this.throw(`Unable to restore ${this.value.title} from Trash.`)
    } else {
      return
    }
  }

  /** Copy an arbitrary asset to another directory. */
  async copy(
    parentDirectoryId: backendModule.DirectoryId,
    parentDirectoryTitle: string
  ): Promise<backendModule.CopyAssetResponse> {
    const path = remoteBackendPaths.copyAssetPath(this.value.id)
    const response = await this.httpPost<backendModule.CopyAssetResponse>(path, {
      parentDirectoryId,
    })
    if (!responseIsSuccessful(response)) {
      return this.throw(`Unable to copy '${this.value.title}' to '${parentDirectoryTitle}'.`)
    } else {
      return await response.json()
    }
  }

  /** Return a copy of this object, but with a different value. */
  // @ts-expect-error This is SAFE, under the same conditions as `SmartObject.withValue`.
  override withValue(value: T): this {
    return super.withValue(value)
  }
}

/** Converts an {@link backendModule.AnyAsset} into its corresponding
 * {@link backendModule.AnySmartAsset}. */
function intoSmartAsset(client: http.Client, logger: loggerProvider.Logger) {
  return (asset: backendModule.AnyAsset): backendModule.AnySmartAsset => {
    switch (asset.type) {
      case backendModule.AssetType.project: {
        throw new Error('Not implemented yet: AssetType.project case')
      }
      case backendModule.AssetType.file: {
        throw new Error('Not implemented yet: AssetType.file case')
      }
      case backendModule.AssetType.secret: {
        return new SmartSecret(client, logger, asset)
      }
      case backendModule.AssetType.directory: {
        return new SmartDirectory(client, logger, asset)
      }
      case backendModule.AssetType.specialLoading:
      case backendModule.AssetType.specialEmpty: {
        throw new Error(
          `'${asset.type}' is a special asset type that should never be returned by the backend.`
        )
      }
    }
  }
}

/** A smart wrapper around a {@link backendModule.DirectoryAsset}. */
class SmartDirectory
  extends SmartAsset<backendModule.DirectoryAsset>
  implements backendModule.SmartDirectory
{
  /** Return a list of assets in a directory. */
  async list(
    query: backendModule.ListDirectoryRequestParams
  ): Promise<backendModule.AnySmartAsset[]> {
    const paramsString = new URLSearchParams([
      ['parent_id', this.value.id],
      ...(query.filterBy != null ? [['filter_by', query.filterBy]] : []),
      ...(query.recentProjects ? [['recent_projects', String(true)]] : []),
      ...(query.labels != null ? query.labels.map(label => ['label', label]) : []),
    ]).toString()
    const path = remoteBackendPaths.LIST_DIRECTORY_PATH + '?' + paramsString
    const response = await this.httpGet<ListDirectoryResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      if (response.status === STATUS_SERVER_ERROR) {
        // The directory is probably empty.
        return []
      } else {
        return this.throw('Could not list root folder.')
      }
    } else {
      const assets = (await response.json()).assets
        .map(asset =>
          object.merge(asset, {
            // eslint-disable-next-line no-restricted-syntax
            type: asset.id.match(/^(.+?)-/)?.[1] as backendModule.AssetType,
          })
        )
        .map(asset =>
          object.merge(asset, {
            permissions: [...(asset.permissions ?? [])].sort(backendModule.compareUserPermissions),
          })
        )
      return assets.map(intoSmartAsset(this.client, this.logger))
    }
  }

  /** Change the name or description of a directory. */
  override async update(
    body: backendModule.UpdateAssetOrDirectoryRequestBody
  ): Promise<backendModule.UpdatedDirectory> {
    const path = remoteBackendPaths.updateDirectoryPath(this.value.id)
    const updateAssetRequest =
      body.description == null && body.parentDirectoryId == null
        ? Promise.resolve(null)
        : super.update({
            description: body.description ?? null,
            parentDirectoryId: body.parentDirectoryId ?? null,
          })
    const updateDirectoryRequest =
      body.title == null
        ? Promise.resolve(null)
        : this.httpPut<backendModule.UpdatedDirectory>(path, { title: body.title })
    const [updateAssetResponse, updateDirectoryResponse] = await Promise.allSettled([
      updateAssetRequest,
      updateDirectoryRequest,
    ])
    if (
      updateAssetResponse.status === 'rejected' ||
      updateDirectoryResponse.status === 'rejected' ||
      (updateDirectoryResponse.value != null &&
        !responseIsSuccessful(updateDirectoryResponse.value))
    ) {
      return this.throw(`Could not update folder '${this.value.title}'.`)
    } else if (updateDirectoryResponse.value != null) {
      return await updateDirectoryResponse.value.json()
    } else {
      return { id: this.value.id, parentId: this.value.parentId, title: this.value.title }
    }
  }
}

/** A smart wrapper around a {@link backendModule.SecretAsset}. */
class SmartSecret
  extends SmartAsset<backendModule.SecretAsset>
  implements backendModule.SmartSecret
{
  /** Return a secret environment variable. */
  async getValue(): Promise<backendModule.Secret> {
    const path = remoteBackendPaths.getSecretPath(this.value.id)
    const response = await this.httpGet<backendModule.Secret>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not get secret '${this.value.title}'.`)
    } else {
      return await response.json()
    }
  }

  /** Change the value or description of a secret environment variable. */
  override async update(body: backendModule.UpdateAssetOrSecretRequestBody): Promise<void> {
    const path = remoteBackendPaths.updateSecretPath(this.value.id)
    const updateAssetRequest =
      body.description == null && body.parentDirectoryId == null
        ? Promise.resolve(null)
        : super.update({
            description: body.description ?? null,
            parentDirectoryId: body.parentDirectoryId ?? null,
          })
    const updateSecretRequest =
      body.value == null ? Promise.resolve(null) : this.httpPut(path, { value: body.value })
    const [updateAssetResponse, updateSecretResponse] = await Promise.allSettled([
      updateAssetRequest,
      updateSecretRequest,
    ])
    if (
      updateAssetResponse.status === 'rejected' ||
      updateSecretResponse.status === 'rejected' ||
      (updateSecretResponse.value != null && !responseIsSuccessful(updateSecretResponse.value))
    ) {
      return this.throw(`Could not update secret '${this.value.title}'.`)
    } else {
      return
    }
  }
}

// =====================
// === RemoteBackend ===
// =====================

/** Information for a cached default version. */
interface DefaultVersionInfo {
  version: backendModule.VersionNumber
  lastUpdatedEpochMs: number
}

/** Class for sending requests to the Cloud backend API endpoints. */
export class RemoteBackend extends backendModule.Backend {
  readonly type = backendModule.BackendType.remote
  protected defaultVersions: Partial<Record<backendModule.VersionType, DefaultVersionInfo>> = {}

  /** Create a new instance of the {@link RemoteBackend} API client.
   * @throws An error if the `Authorization` header is not set on the given `client`. */
  constructor(
    private readonly client: http.Client,
    private readonly logger: loggerProvider.Logger
  ) {
    super()
    // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
    // set.
    if (!new Headers(this.client.defaultHeaders).has('Authorization')) {
      return this.throw('Authorization header not set.')
    } else {
      if (detect.IS_DEV_MODE) {
        // @ts-expect-error This exists only for debugging purposes. It does not have types
        // because it MUST NOT be used in this codebase.
        window.remoteBackend = this
      }
      return
    }
  }

  /** Return a list of all users in the same organization. */
  override async listUsers(): Promise<backendModule.SimpleUser[]> {
    const path = remoteBackendPaths.LIST_USERS_PATH
    const response = await this.get<ListUsersResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not list users in the organization.`)
    } else {
      return (await response.json()).users
    }
  }

  /** Set the username and parent organization of the current user. */
  override async createUser(
    body: backendModule.CreateUserRequestBody
  ): Promise<backendModule.UserOrOrganization> {
    const path = remoteBackendPaths.CREATE_USER_PATH
    const response = await this.post<backendModule.UserOrOrganization>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw('Could not create user.')
    } else {
      return await response.json()
    }
  }

  /** Invite a new user to the organization by email. */
  override async inviteUser(body: backendModule.InviteUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.INVITE_USER_PATH
    const response = await this.post(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not invite user '${body.userEmail}'.`)
    } else {
      return
    }
  }

  /** Adds a permission for a specific user on a specific asset. */
  override async createPermission(body: backendModule.CreatePermissionRequestBody): Promise<void> {
    const path = remoteBackendPaths.CREATE_PERMISSION_PATH
    const response = await this.post<backendModule.UserOrOrganization>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not set permissions.`)
    } else {
      return
    }
  }

  /** Return organization info for the current user.
   * @returns `null` if a non-successful status code (not 200-299) was received. */
  override async self(): Promise<SmartUser | null> {
    const path = remoteBackendPaths.USERS_ME_PATH
    const response = await this.get<backendModule.UserOrOrganization>(path)
    if (!responseIsSuccessful(response)) {
      // This user has probably not finished registering.
      return null
    } else {
      const json = await response.json()
      return new SmartUser(this.client, this.logger, json)
    }
  }

  /** Create a directory.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createDirectory(
    body: backendModule.CreateDirectoryRequestBody
  ): Promise<backendModule.CreatedDirectory> {
    const path = remoteBackendPaths.CREATE_DIRECTORY_PATH
    const response = await this.post<backendModule.CreatedDirectory>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not create folder with name '${body.title}'.`)
    } else {
      return await response.json()
    }
  }

  /** Change the name of a directory.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async updateDirectory(
    directoryId: backendModule.DirectoryId,
    body: backendModule.UpdateDirectoryRequestBody,
    title: string | null
  ) {
    const path = remoteBackendPaths.updateDirectoryPath(directoryId)
    const response = await this.put<backendModule.UpdatedDirectory>(path, body)
    if (!responseIsSuccessful(response)) {
      const name = title != null ? `'${title}'` : `with ID '${directoryId}'`
      return this.throw(`Could not update folder ${name}.`)
    } else {
      return await response.json()
    }
  }

  /** Create a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createProject(
    body: backendModule.CreateProjectRequestBody
  ): Promise<backendModule.CreatedProject> {
    const path = remoteBackendPaths.CREATE_PROJECT_PATH
    const response = await this.post<backendModule.CreatedProject>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not create project with name '${body.projectName}'.`)
    } else {
      return await response.json()
    }
  }

  /** Close a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async closeProject(
    projectId: backendModule.ProjectId,
    title: string | null
  ): Promise<void> {
    const path = remoteBackendPaths.closeProjectPath(projectId)
    const response = await this.post(path, {})
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not close project ${title != null ? `'${title}'` : `with ID '${projectId}'`}.`
      )
    } else {
      return
    }
  }

  /** Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getProjectDetails(
    projectId: backendModule.ProjectId,
    title: string | null
  ): Promise<backendModule.Project> {
    const path = remoteBackendPaths.getProjectDetailsPath(projectId)
    const response = await this.get<backendModule.ProjectRaw>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not get details of project ${
          title != null ? `'${title}'` : `with ID '${projectId}'`
        }.`
      )
    } else {
      const project = await response.json()
      const ideVersion =
        project.ide_version ?? (await this.getDefaultVersion(backendModule.VersionType.ide))
      return {
        ...project,
        ideVersion,
        engineVersion: project.engine_version,
        jsonAddress:
          project.address != null ? backendModule.Address(`${project.address}json`) : null,
        binaryAddress:
          project.address != null ? backendModule.Address(`${project.address}binary`) : null,
      }
    }
  }

  /** Prepare a project for execution.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async openProject(
    projectId: backendModule.ProjectId,
    body: backendModule.OpenProjectRequestBody | null,
    title: string | null
  ): Promise<void> {
    const path = remoteBackendPaths.openProjectPath(projectId)
    const response = await this.post(path, body ?? DEFAULT_OPEN_PROJECT_BODY)
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}.`
      )
    } else {
      return
    }
  }

  /** Update the name or AMI of a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async updateProject(
    projectId: backendModule.ProjectId,
    body: backendModule.UpdateProjectRequestBody,
    title: string | null
  ): Promise<backendModule.UpdatedProject> {
    const path = remoteBackendPaths.projectUpdatePath(projectId)
    const response = await this.put<backendModule.UpdatedProject>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not update project ${title != null ? `'${title}'` : `with ID '${projectId}'`}.`
      )
    } else {
      return await response.json()
    }
  }

  /** Return the resource usage of a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async checkResources(
    projectId: backendModule.ProjectId,
    title: string | null
  ): Promise<backendModule.ResourceUsage> {
    const path = remoteBackendPaths.checkResourcesPath(projectId)
    const response = await this.get<backendModule.ResourceUsage>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not get resource usage for project ${
          title != null ? `'${title}'` : `with ID '${projectId}'`
        }.`
      )
    } else {
      return await response.json()
    }
  }

  /** Upload a file.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async uploadFile(
    params: backendModule.UploadFileRequestParams,
    file: Blob
  ): Promise<backendModule.FileInfo> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      file_name: params.fileName,
      ...(params.fileId != null ? { file_id: params.fileId } : {}),
      ...(params.parentDirectoryId ? { parent_directory_id: params.parentDirectoryId } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_FILE_PATH}?${paramsString}`
    const response = await this.postBinary<backendModule.FileInfo>(path, file)
    if (!responseIsSuccessful(response)) {
      let suffix = '.'
      try {
        const error = errorModule.tryGetError<unknown>(await response.json())
        if (error != null) {
          suffix = `: ${error}`
        }
      } catch {
        // Ignored.
      }
      if (params.fileId != null) {
        return this.throw(`Could not upload file with ID '${params.fileId}'${suffix}`)
      } else {
        return this.throw(`Could not upload file${suffix}`)
      }
    } else {
      return await response.json()
    }
  }

  /** Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getFileDetails(
    fileId: backendModule.FileId,
    title: string | null
  ): Promise<backendModule.FileDetails> {
    const path = remoteBackendPaths.getFileDetailsPath(fileId)
    const response = await this.get<backendModule.FileDetails>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(
        `Could not get details of project ${title != null ? `'${title}'` : `with ID '${fileId}'`}.`
      )
    } else {
      return await response.json()
    }
  }

  /** Create a secret environment variable.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createSecret(
    body: backendModule.CreateSecretRequestBody
  ): Promise<backendModule.SecretId> {
    const path = remoteBackendPaths.CREATE_SECRET_PATH
    const response = await this.post<backendModule.SecretId>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not create secret with name '${body.name}'.`)
    } else {
      return await response.json()
    }
  }

  /** Create a label used for categorizing assets.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createTag(body: backendModule.CreateTagRequestBody): Promise<backendModule.Label> {
    const path = remoteBackendPaths.CREATE_TAG_PATH
    const response = await this.post<backendModule.Label>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not create label '${body.value}'.`)
    } else {
      return await response.json()
    }
  }

  /** Return all labels accessible by the user.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listTags(): Promise<backendModule.Label[]> {
    const path = remoteBackendPaths.LIST_TAGS_PATH
    const response = await this.get<ListTagsResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not list labels.`)
    } else {
      return (await response.json()).tags
    }
  }

  /** Set the full list of labels for a specific asset.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async associateTag(
    assetId: backendModule.AssetId,
    labels: backendModule.LabelName[],
    title: string | null
  ) {
    const path = remoteBackendPaths.associateTagPath(assetId)
    const response = await this.patch<ListTagsResponseBody>(path, { labels })
    if (!responseIsSuccessful(response)) {
      const name = title != null ? `'${title}'` : `with ID '${assetId}'`
      return this.throw(`Could not set labels for asset ${name}.`)
    } else {
      return
    }
  }

  /** Delete a label.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async deleteTag(
    tagId: backendModule.TagId,
    value: backendModule.LabelName
  ): Promise<void> {
    const path = remoteBackendPaths.deleteTagPath(tagId)
    const response = await this.delete(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not delete label '${value}'.`)
    } else {
      return
    }
  }

  /** Return a list of backend or IDE versions.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  protected async listVersions(
    params: backendModule.ListVersionsRequestParams
  ): Promise<backendModule.Version[]> {
    /** HTTP response body for this endpoint. */
    interface Body {
      versions: [backendModule.Version, ...backendModule.Version[]]
    }
    const paramsString = new URLSearchParams({
      // eslint-disable-next-line @typescript-eslint/naming-convention
      version_type: params.versionType,
      default: String(params.default),
    }).toString()
    const path = remoteBackendPaths.LIST_VERSIONS_PATH + '?' + paramsString
    const response = await this.get<Body>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(`Could not list versions of type '${params.versionType}'.`)
    } else {
      return (await response.json()).versions
    }
  }

  /** Get the default version given the type of version (IDE or backend). */
  protected async getDefaultVersion(versionType: backendModule.VersionType) {
    const cached = this.defaultVersions[versionType]
    const nowEpochMs = Number(new Date())
    if (cached != null && nowEpochMs - cached.lastUpdatedEpochMs < ONE_DAY_MS) {
      return cached.version
    } else {
      const version = (await this.listVersions({ versionType, default: true }))[0]?.number
      if (version == null) {
        return this.throw(`No default ${versionType} version found.`)
      } else {
        const info: DefaultVersionInfo = { version, lastUpdatedEpochMs: nowEpochMs }
        this.defaultVersions[versionType] = info
        return info.version
      }
    }
  }

  /** Log an error message and throws an {@link Error} with the specified message.
   * @throws {Error} Always. */
  private throw(message: string): never {
    this.logger.error(message)
    throw new Error(message)
  }

  /** Send an HTTP GET request to the given path. */
  private get<T = void>(path: string) {
    return this.client.get<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }

  /** Send a JSON HTTP POST request to the given path. */
  private post<T = void>(path: string, payload: object) {
    return this.client.post<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a binary HTTP POST request to the given path. */
  private postBinary<T = void>(path: string, payload: Blob) {
    return this.client.postBinary<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  private patch<T = void>(path: string, payload: object) {
    return this.client.patch<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send a JSON HTTP PUT request to the given path. */
  private put<T = void>(path: string, payload: object) {
    return this.client.put<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
  }

  /** Send an HTTP DELETE request to the given path. */
  private delete<T = void>(path: string) {
    return this.client.delete<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
  }
}
