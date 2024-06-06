/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as detect from 'enso-common/src/detect'

import type * as loggerProvider from '#/providers/LoggerProvider'
import type * as textProvider from '#/providers/TextProvider'

import Backend, * as backend from '#/services/Backend'
import * as remoteBackendPaths from '#/services/remoteBackendPaths'

import type HttpClient from '#/utilities/HttpClient'
import type * as httpClient from '#/utilities/HttpClient'
import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_FIRST = 200
/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_LAST = 299
/** HTTP status indicating that the resource does not exist. */
const STATUS_NOT_FOUND = 404

/** The number of milliseconds in one day. */
const ONE_DAY_MS = 86_400_000

/** The interval between requests checking whether a project is ready to be opened in the IDE. */
const CHECK_STATUS_INTERVAL_MS = 5000

// =============
// === Types ===
// =============

/** The format of all errors returned by the backend. */
interface RemoteBackendError {
  readonly type: string
  readonly code: string
  readonly message: string
  readonly param: string
}

// ============================
// === responseIsSuccessful ===
// ============================

/** Whether a response has a success HTTP status code (200-299). */
function responseIsSuccessful(response: Response) {
  return response.status >= STATUS_SUCCESS_FIRST && response.status <= STATUS_SUCCESS_LAST
}

// =============
// === Types ===
// =============

/** HTTP response body for the "list users" endpoint. */
export interface ListUsersResponseBody {
  readonly users: backend.User[]
}

/** HTTP response body for the "list projects" endpoint. */
export interface ListDirectoryResponseBody {
  readonly assets: backend.AnyAsset[]
}

/** HTTP response body for the "list projects" endpoint. */
export interface ListProjectsResponseBody {
  readonly projects: backend.ListedProjectRaw[]
}

/** HTTP response body for the "list files" endpoint. */
export interface ListFilesResponseBody {
  readonly files: backend.FileLocator[]
}

/** HTTP response body for the "list secrets" endpoint. */
export interface ListSecretsResponseBody {
  readonly secrets: backend.SecretInfo[]
}

/** HTTP response body for the "list tag" endpoint. */
export interface ListTagsResponseBody {
  readonly tags: backend.Label[]
}

/** HTTP response body for the "list versions" endpoint. */
export interface ListVersionsResponseBody {
  readonly versions: [backend.Version, ...backend.Version[]]
}

// =====================
// === RemoteBackend ===
// =====================

/** A function that turns a text ID (and a list of replacements, if required) to
 * human-readable text. */
type GetText = ReturnType<typeof textProvider.useText>['getText']

/** Information for a cached default version. */
interface DefaultVersionInfo {
  readonly version: backend.VersionNumber
  readonly lastUpdatedEpochMs: number
}

/** Options for {@link RemoteBackend.post} private method. */
interface RemoteBackendPostOptions {
  readonly keepalive?: boolean
}

/** Class for sending requests to the Cloud backend API endpoints. */
export default class RemoteBackend extends Backend {
  readonly type = backend.BackendType.remote
  private defaultVersions: Partial<Record<backend.VersionType, DefaultVersionInfo>> = {}

  /** Create a new instance of the {@link RemoteBackend} API client.
   * @throws An error if the `Authorization` header is not set on the given `client`. */
  constructor(
    private readonly client: HttpClient,
    private readonly logger: loggerProvider.Logger,
    private getText: ReturnType<typeof textProvider.useText>['getText']
  ) {
    super()
    // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
    // set.
    if (!new Headers(this.client.defaultHeaders).has('Authorization')) {
      const message = 'Authorization header not set.'
      this.logger.error(message)
      throw new Error(message)
    } else {
      if (detect.IS_DEV_MODE) {
        // @ts-expect-error This exists only for debugging purposes. It does not have types
        // because it MUST NOT be used in this codebase.
        window.remoteBackend = this
      }
      return
    }
  }

  /** Set `this.getText`. This function is exposed rather than the property itself to make it clear
   * that it is intended to be mutable. */
  setGetText(getText: GetText) {
    this.getText = getText
  }

  /** Return the ID of the root directory. */
  override rootDirectoryId(user: backend.User | null): backend.DirectoryId | null {
    return user?.rootDirectoryId ?? null
  }

  /** Return a list of all users in the same organization. */
  override async listUsers(): Promise<backend.User[]> {
    const path = remoteBackendPaths.LIST_USERS_PATH
    const response = await this.get<ListUsersResponseBody>(path)
    return (await response.json()).users
  }

  /** Set the username and parent organization of the current user. */
  override async createUser(body: backend.CreateUserRequestBody): Promise<backend.User> {
    const path = remoteBackendPaths.CREATE_USER_PATH
    const response = await this.post<backend.User>(path, body)
    return await response.json()
  }

  /** Change the username of the current user. */
  override async updateUser(body: backend.UpdateUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.UPDATE_CURRENT_USER_PATH
    await this.put(path, body)
  }

  /** Restore a user that has been soft-deleted. */
  async restoreUser(): Promise<void> {
    await this.put(remoteBackendPaths.UPDATE_CURRENT_USER_PATH, { clearRemoveAt: true })
  }

  /** Delete the current user. */
  override async deleteUser(): Promise<void> {
    await this.delete(remoteBackendPaths.DELETE_USER_PATH)
  }

  /** Delete a user.
   * FIXME: Not implemented on backend yet. */
  override async removeUser(): Promise<void> {
    await Promise.resolve()
    throw new Error('Has not yet been implemented on the backend.')
  }

  /** Invite a new user to the organization by email. */
  override async inviteUser(body: backend.InviteUserRequestBody): Promise<void> {
    await this.post(remoteBackendPaths.INVITE_USER_PATH, body)
  }

  /** List all invitations. */
  override async listInvitations(): Promise<backend.Invitation[]> {
    const response = await this.get<backend.InvitationListRequestBody>(
      remoteBackendPaths.INVITATION_PATH
    )
    return (await response.json()).invitations
  }

  /** Delete an invitation. */
  override async deleteInvitation(userEmail: backend.EmailAddress): Promise<void> {
    await this.delete(remoteBackendPaths.INVITATION_PATH, { userEmail })
  }

  /** Resend an invitation to a user. */
  override async resendInvitation(userEmail: backend.EmailAddress): Promise<void> {
    await this.post(remoteBackendPaths.INVITATION_PATH, {
      userEmail,
      resend: true,
    })
  }

  /** Upload a new profile picture for the current user. */
  override async uploadUserPicture(
    params: backend.UploadPictureRequestParams,
    file: Blob
  ): Promise<backend.User> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_USER_PICTURE_PATH}?${paramsString}`
    const response = await this.putBinary<backend.User>(path, file)
    return await response.json()
  }

  /** Set the list of groups a user is in. */
  override async changeUserGroup(
    userId: backend.UserId,
    userGroups: backend.ChangeUserGroupRequestBody
  ): Promise<backend.User> {
    const path = remoteBackendPaths.changeUserGroupPath(userId)
    const response = await this.put<backend.User>(path, userGroups)
    return await response.json()
  }

  /** Return details for the current organization.
   * @returns `null` if a non-successful status code (not 200-299) was received. */
  override async getOrganization(): Promise<backend.OrganizationInfo | null> {
    const path = remoteBackendPaths.GET_ORGANIZATION_PATH
    const response = await this.get<backend.OrganizationInfo>(path)
    if (response.status === STATUS_NOT_FOUND) {
      // Organization info has not yet been created.
      return null
    } else {
      return await response.json()
    }
  }

  /** Update details for the current organization. */
  override async updateOrganization(
    body: backend.UpdateOrganizationRequestBody
  ): Promise<backend.OrganizationInfo | null> {
    const path = remoteBackendPaths.UPDATE_ORGANIZATION_PATH
    const response = await this.patch<backend.OrganizationInfo>(path, body)
    if (response.status === STATUS_NOT_FOUND) {
      // Organization info has not yet been created.
      return null
    } else {
      return await response.json()
    }
  }

  /** Upload a new profile picture for the current organization. */
  override async uploadOrganizationPicture(
    params: backend.UploadPictureRequestParams,
    file: Blob
  ): Promise<backend.OrganizationInfo> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_ORGANIZATION_PICTURE_PATH}?${paramsString}`
    const response = await this.putBinary<backend.OrganizationInfo>(path, file)
    return await response.json()
  }

  /** Adds a permission for a specific user on a specific asset. */
  override async createPermission(body: backend.CreatePermissionRequestBody): Promise<void> {
    const path = remoteBackendPaths.CREATE_PERMISSION_PATH
    await this.post(path, body)
  }

  /** Return details for the current user.
   * @returns `null` if a non-successful status code (not 200-299) was received. */
  override async usersMe(): Promise<backend.User | null> {
    const path = remoteBackendPaths.USERS_ME_PATH
    const response = await this.get<backend.User>(path)
    if (!responseIsSuccessful(response)) {
      return null
    } else {
      return await response.json()
    }
  }

  /** Return a list of assets in a directory.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listDirectory(
    query: backend.ListDirectoryRequestParams
  ): Promise<backend.AnyAsset[]> {
    const path = remoteBackendPaths.LIST_DIRECTORY_PATH
    const response = await this.get<ListDirectoryResponseBody>(
      path +
        '?' +
        new URLSearchParams(
          query.recentProjects
            ? [['recent_projects', String(true)]]
            : [
                ...(query.parentId != null ? [['parent_id', query.parentId]] : []),
                ...(query.filterBy != null ? [['filter_by', query.filterBy]] : []),
                ...(query.labels != null ? query.labels.map(label => ['label', label]) : []),
              ]
        ).toString()
    )
    return (await response.json()).assets
      .map(asset =>
        object.merge(asset, {
          // eslint-disable-next-line no-restricted-syntax
          type: asset.id.match(/^(.+?)-/)?.[1] as backend.AssetType,
        })
      )
      .map(asset =>
        object.merge(asset, {
          permissions: [...(asset.permissions ?? [])].sort(backend.compareAssetPermissions),
        })
      )
  }

  /** Create a directory.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createDirectory(
    body: backend.CreateDirectoryRequestBody
  ): Promise<backend.CreatedDirectory> {
    const path = remoteBackendPaths.CREATE_DIRECTORY_PATH
    const response = await this.post<backend.CreatedDirectory>(path, body)

    return await response.json()
  }

  /** Change the name of a directory.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async updateDirectory(
    directoryId: backend.DirectoryId,
    body: backend.UpdateDirectoryRequestBody
  ) {
    const path = remoteBackendPaths.updateDirectoryPath(directoryId)
    const response = await this.put<backend.UpdatedDirectory>(path, body)
    return await response.json()
  }

  /** List all previous versions of an asset. */
  override async listAssetVersions(assetId: backend.AssetId): Promise<backend.AssetVersions> {
    const path = remoteBackendPaths.listAssetVersionsPath(assetId)
    const response = await this.get<backend.AssetVersions>(path)
    return await response.json()
  }

  /** Fetch the content of the `Main.enso` file of a project. */
  override async getFileContent(projectId: backend.ProjectId, version: string): Promise<string> {
    const path = remoteBackendPaths.getProjectContentPath(projectId, version)
    const response = await this.get<string>(path)
    return await response.text()
  }

  /** Change the parent directory or description of an asset.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async updateAsset(assetId: backend.AssetId, body: backend.UpdateAssetRequestBody) {
    const path = remoteBackendPaths.updateAssetPath(assetId)
    await this.patch(path, body)
  }

  /** Delete an arbitrary asset.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async deleteAsset(assetId: backend.AssetId, body: backend.DeleteAssetRequestBody) {
    const paramsString = new URLSearchParams([['force', String(body.force)]]).toString()
    const path = remoteBackendPaths.deleteAssetPath(assetId) + '?' + paramsString
    await this.delete(path)
  }

  /** Restore an arbitrary asset from the trash.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async undoDeleteAsset(assetId: backend.AssetId): Promise<void> {
    const path = remoteBackendPaths.UNDO_DELETE_ASSET_PATH
    await this.patch(path, { assetId })
  }

  /** Copy an arbitrary asset to another directory.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async copyAsset(
    assetId: backend.AssetId,
    parentDirectoryId: backend.DirectoryId
  ): Promise<backend.CopyAssetResponse> {
    const response = await this.post<backend.CopyAssetResponse>(
      remoteBackendPaths.copyAssetPath(assetId),
      { parentDirectoryId }
    )
    return await response.json()
  }

  /** Return a list of projects belonging to the current user.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listProjects(): Promise<backend.ListedProject[]> {
    const path = remoteBackendPaths.LIST_PROJECTS_PATH
    const response = await this.get<ListProjectsResponseBody>(path)
    return (await response.json()).projects.map(project => ({
      ...project,
      jsonAddress: project.address != null ? backend.Address(`${project.address}json`) : null,
      binaryAddress: project.address != null ? backend.Address(`${project.address}binary`) : null,
    }))
  }

  /** Create a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createProject(
    body: backend.CreateProjectRequestBody
  ): Promise<backend.CreatedProject> {
    const path = remoteBackendPaths.CREATE_PROJECT_PATH
    const response = await this.post<backend.CreatedProject>(path, body)
    return await response.json()
  }

  /** Restore a project from a different version. */
  override async restoreProject(
    projectId: backend.ProjectId,
    versionId: backend.S3ObjectVersionId
  ): Promise<void> {
    const path = remoteBackendPaths.restoreProjectPath(projectId)
    await this.post(path, { versionId })
  }

  /** Duplicate a specific version of a project. */
  override async duplicateProject(
    projectId: backend.ProjectId,
    versionId: backend.S3ObjectVersionId
  ): Promise<backend.CreatedProject> {
    const path = remoteBackendPaths.duplicateProjectPath(projectId)
    const response = await this.post<backend.CreatedProject>(path, { versionId })
    return await response.json()
  }

  /** Close a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async closeProject(projectId: backend.ProjectId): Promise<void> {
    const path = remoteBackendPaths.closeProjectPath(projectId)
    await this.post(path, {})
  }

  /** Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getProjectDetails(projectId: backend.ProjectId): Promise<backend.Project> {
    const path = remoteBackendPaths.getProjectDetailsPath(projectId)
    const response = await this.get<backend.ProjectRaw>(path)
    const project = await response.json()
    const ideVersion =
      project.ide_version ?? (await this.getDefaultVersion(backend.VersionType.ide))
    return {
      ...project,
      ideVersion,
      engineVersion: project.engine_version,
      jsonAddress: project.address != null ? backend.Address(`${project.address}json`) : null,
      binaryAddress: project.address != null ? backend.Address(`${project.address}binary`) : null,
    }
  }

  /** Prepare a project for execution.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async openProject(
    projectId: backend.ProjectId,
    body: backend.OpenProjectRequestBody
  ): Promise<void> {
    const path = remoteBackendPaths.openProjectPath(projectId)
    if (body.cognitoCredentials == null) {
      throw new Error('Could not open project because Cognito credentials are missing.')
    } else {
      const credentials = body.cognitoCredentials
      const exactCredentials: backend.CognitoCredentials = {
        accessToken: credentials.accessToken,
        clientId: credentials.clientId,
        expireAt: credentials.expireAt,
        refreshToken: credentials.refreshToken,
        refreshUrl: credentials.refreshUrl,
      }
      const filteredBody: Omit<backend.OpenProjectRequestBody, 'parentId'> = {
        ...body,
        cognitoCredentials: exactCredentials,
      }
      await this.post(path, filteredBody)
      return
    }
  }

  /** Update the name or AMI of a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async updateProject(
    projectId: backend.ProjectId,
    body: backend.UpdateProjectRequestBody
  ): Promise<backend.UpdatedProject> {
    const path = remoteBackendPaths.projectUpdatePath(projectId)
    const response = await this.put<backend.UpdatedProject>(path, body)
    return await response.json()
  }

  /** Return the resource usage of a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async checkResources(projectId: backend.ProjectId): Promise<backend.ResourceUsage> {
    const path = remoteBackendPaths.checkResourcesPath(projectId)
    const response = await this.get<backend.ResourceUsage>(path)
    return await response.json()
  }

  /** Return a list of files accessible by the current user.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listFiles(): Promise<backend.FileLocator[]> {
    const path = remoteBackendPaths.LIST_FILES_PATH
    const response = await this.get<ListFilesResponseBody>(path)
    return (await response.json()).files
  }

  /** Upload a file.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async uploadFile(
    params: backend.UploadFileRequestParams,
    file: Blob
  ): Promise<backend.FileInfo> {
    const paramsString = new URLSearchParams({
      /* eslint-disable @typescript-eslint/naming-convention */
      file_name: params.fileName,
      ...(params.fileId != null ? { file_id: params.fileId } : {}),
      ...(params.parentDirectoryId ? { parent_directory_id: params.parentDirectoryId } : {}),
      /* eslint-enable @typescript-eslint/naming-convention */
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_FILE_PATH}?${paramsString}`
    const response = await this.postBinary<backend.FileInfo>(path, file)
    return await response.json()
  }

  /** Change the name of a file. */
  override async updateFile(): Promise<void> {
    await Promise.resolve()
    throw new Error('Files currently cannot be renamed on the Cloud backend.')
  }

  /** Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getFileDetails(fileId: backend.FileId): Promise<backend.FileDetails> {
    const path = remoteBackendPaths.getFileDetailsPath(fileId)
    const response = await this.get<backend.FileDetails>(path)
    return await response.json()
  }

  /** Return a Datalink.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createDatalink(
    body: backend.CreateDatalinkRequestBody
  ): Promise<backend.DatalinkInfo> {
    const path = remoteBackendPaths.CREATE_DATALINK_PATH
    const response = await this.post<backend.DatalinkInfo>(path, body)
    return await response.json()
  }

  /** Return a Datalink.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getDatalink(datalinkId: backend.DatalinkId): Promise<backend.Datalink> {
    const path = remoteBackendPaths.getDatalinkPath(datalinkId)
    const response = await this.get<backend.Datalink>(path)
    return await response.json()
  }

  /** Delete a Datalink.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async deleteDatalink(datalinkId: backend.DatalinkId): Promise<void> {
    const path = remoteBackendPaths.getDatalinkPath(datalinkId)
    await this.delete(path)
  }

  /** Create a secret environment variable.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createSecret(body: backend.CreateSecretRequestBody): Promise<backend.SecretId> {
    const path = remoteBackendPaths.CREATE_SECRET_PATH
    const response = await this.post<backend.SecretId>(path, body)
    return await response.json()
  }

  /** Return a secret environment variable.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getSecret(secretId: backend.SecretId): Promise<backend.Secret> {
    const path = remoteBackendPaths.getSecretPath(secretId)
    const response = await this.get<backend.Secret>(path)
    return await response.json()
  }

  /** Update a secret environment variable.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async updateSecret(
    secretId: backend.SecretId,
    body: backend.UpdateSecretRequestBody
  ): Promise<void> {
    const path = remoteBackendPaths.updateSecretPath(secretId)
    await this.put(path, body)
  }

  /** Return the secret environment variables accessible by the user.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listSecrets(): Promise<backend.SecretInfo[]> {
    const path = remoteBackendPaths.LIST_SECRETS_PATH
    const response = await this.get<ListSecretsResponseBody>(path)
    return (await response.json()).secrets
  }

  /** Create a label used for categorizing assets.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createTag(body: backend.CreateTagRequestBody): Promise<backend.Label> {
    const path = remoteBackendPaths.CREATE_TAG_PATH
    const response = await this.post<backend.Label>(path, body)
    return await response.json()
  }

  /** Return all labels accessible by the user.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listTags(): Promise<backend.Label[]> {
    const path = remoteBackendPaths.LIST_TAGS_PATH
    const response = await this.get<ListTagsResponseBody>(path)
    return (await response.json()).tags
  }

  /** Set the full list of labels for a specific asset.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async associateTag(assetId: backend.AssetId, labels: backend.LabelName[]) {
    const path = remoteBackendPaths.associateTagPath(assetId)
    await this.patch(path, { labels })
  }

  /** Delete a label.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async deleteTag(tagId: backend.TagId): Promise<void> {
    const path = remoteBackendPaths.deleteTagPath(tagId)
    await this.delete(path)
  }

  /** Create a user group. */
  override async createUserGroup(
    body: backend.CreateUserGroupRequestBody
  ): Promise<backend.UserGroupInfo> {
    const path = remoteBackendPaths.CREATE_USER_GROUP_PATH
    const response = await this.post<backend.UserGroupInfo>(path, body)
    return await response.json()
  }

  /** Delete a user group. */
  override async deleteUserGroup(userGroupId: backend.UserGroupId): Promise<void> {
    const path = remoteBackendPaths.deleteUserGroupPath(userGroupId)
    await this.delete(path)
  }

  /** List all roles in the organization.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listUserGroups(): Promise<backend.UserGroupInfo[]> {
    const path = remoteBackendPaths.LIST_USER_GROUPS_PATH
    const response = await this.get<backend.UserGroupInfo[]>(path)
    return await response.json()
  }

  /** Return a list of backend or IDE versions.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async listVersions(
    params: backend.ListVersionsRequestParams
  ): Promise<backend.Version[]> {
    const paramsString = new URLSearchParams({
      // eslint-disable-next-line @typescript-eslint/naming-convention
      version_type: params.versionType,
      default: String(params.default),
    }).toString()
    const path = remoteBackendPaths.LIST_VERSIONS_PATH + '?' + paramsString
    const response = await this.get<ListVersionsResponseBody>(path)
    return (await response.json()).versions
  }

  /** Create a payment checkout session.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async createCheckoutSession(
    body: backend.CreateCheckoutSessionRequestBody
  ): Promise<backend.CheckoutSession> {
    const response = await this.post<backend.CheckoutSession>(
      remoteBackendPaths.CREATE_CHECKOUT_SESSION_PATH,
      body
    )
    return await response.json()
  }

  /** Gets the status of a payment checkout session.
   * @throws An error if a non-successful status code (not 200-299) was received. */
  override async getCheckoutSession(
    sessionId: backend.CheckoutSessionId
  ): Promise<backend.CheckoutSessionStatus> {
    const path = remoteBackendPaths.getCheckoutSessionPath(sessionId)
    const response = await this.get<backend.CheckoutSessionStatus>(path)
    return await response.json()
  }

  /** List events in the organization's audit log. */
  override async getLogEvents(): Promise<backend.Event[]> {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly events: backend.Event[]
    }
    const path = remoteBackendPaths.GET_LOG_EVENTS_PATH
    const response = await this.get<ResponseBody>(path)
    return (await response.json()).events
  }

  /** Log an event that will be visible in the organization audit log. */
  async logEvent(
    message: string,
    projectId?: string | null,
    metadata?: object | null
  ): Promise<void> {
    const path = remoteBackendPaths.POST_LOG_EVENT_PATH
    await this.post(
      path,
      {
        message,
        projectId,
        metadata: {
          timestamp: new Date().toISOString(),
          ...(metadata ?? {}),
        },
      },
      {
        keepalive: true,
      }
    )
  }

  /** Return a {@link Promise} that resolves only when a project is ready to open. */
  override async waitUntilProjectIsReady(
    projectId: backend.ProjectId,
    _directory: backend.DirectoryId | null,
    abortController: AbortController = new AbortController()
  ) {
    let project = await this.getProjectDetails(projectId)
    while (!abortController.signal.aborted && project.state.type !== backend.ProjectState.opened) {
      await new Promise<void>(resolve => {
        setTimeout(resolve, CHECK_STATUS_INTERVAL_MS)
      })
      project = await this.getProjectDetails(projectId)
    }
    return project
  }

  /** Get the default version given the type of version (IDE or backend). */
  protected async getDefaultVersion(versionType: backend.VersionType) {
    const cached = this.defaultVersions[versionType]
    const nowEpochMs = Number(new Date())
    if (cached != null && nowEpochMs - cached.lastUpdatedEpochMs < ONE_DAY_MS) {
      return cached.version
    } else {
      const version = (await this.listVersions({ versionType, default: true }))[0]?.number
      if (version == null) {
        throw new Error('No default version found.')
      } else {
        const info: DefaultVersionInfo = { version, lastUpdatedEpochMs: nowEpochMs }
        this.defaultVersions[versionType] = info
        return info.version
      }
    }
  }

  /** Log an error message and throws an {@link Error} with the specified message.
   * @throws {Error} Always. */
  private async throw(response: Response | null): Promise<never> {
    const error =
      response == null
        ? { message: 'unknown error' }
        : // This is SAFE only when the response has been confirmed to have an erroring status code.
          // eslint-disable-next-line no-restricted-syntax
          ((await response.json()) as RemoteBackendError)
    const message = error.message
    this.logger.error(message)
    const status = response?.status
    const errorObject = new Error(message)
    if (status != null) {
      // @ts-expect-error This is a custom property.
      errorObject.status = status
    }
    throw error
  }

  /** Throw an error if the response is not successful, otherwise return the response unchanged. */
  private async ensureSuccessful<T>(responsePromise: Promise<httpClient.ResponseWithTypedJson<T>>) {
    const response = await responsePromise
    if (!responseIsSuccessful(response)) {
      return this.throw(response)
    } else {
      return response
    }
  }

  /** Send an HTTP GET request to the given path. */
  private get<T = void>(path: string) {
    return this.ensureSuccessful(this.client.get<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`))
  }

  /** Send a JSON HTTP POST request to the given path. */
  private post<T = void>(path: string, payload: object, options?: RemoteBackendPostOptions) {
    return this.ensureSuccessful(
      this.client.post<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload, options)
    )
  }

  /** Send a binary HTTP POST request to the given path. */
  private postBinary<T = void>(path: string, payload: Blob) {
    return this.ensureSuccessful(
      this.client.postBinary<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
    )
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  private patch<T = void>(path: string, payload: object) {
    return this.ensureSuccessful(
      this.client.patch<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
    )
  }

  /** Send a JSON HTTP PUT request to the given path. */
  private put<T = void>(path: string, payload: object) {
    return this.ensureSuccessful(
      this.client.put<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
    )
  }

  /** Send a binary HTTP PUT request to the given path. */
  private putBinary<T = void>(path: string, payload: Blob) {
    return this.ensureSuccessful(
      this.client.putBinary<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
    )
  }

  /** Send an HTTP DELETE request to the given path. */
  private delete<T = void>(path: string, payload?: Record<string, unknown>) {
    return this.ensureSuccessful(
      this.client.delete<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
    )
  }
}
