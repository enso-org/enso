/**
 * @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API.
 */
import * as detect from 'enso-common/src/detect'
import type * as text from 'enso-common/src/text'

import type * as loggerProvider from '#/providers/LoggerProvider'
import type * as textProvider from '#/providers/TextProvider'

import Backend, * as backend from '#/services/Backend'
import * as remoteBackendPaths from '#/services/remoteBackendPaths'

import * as download from '#/utilities/download'
import type HttpClient from '#/utilities/HttpClient'
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
/** HTTP status indicating that the server encountered a fatal exception. */
const STATUS_SERVER_ERROR = 500
/** HTTP status indicating that the request was successful, but the user is not authorized to access. */
const STATUS_NOT_AUTHORIZED = 401
/** HTTP status indicating that authorized user doesn't have access to the given resource */
const STATUS_NOT_ALLOWED = 403

/** The number of milliseconds in one day. */
const ONE_DAY_MS = 86_400_000

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

// ====================================
// === isSpecialReadonlyDirectoryId ===
// ====================================

/** Whether the given directory is a special directory that cannot be written to. */
export function isSpecialReadonlyDirectoryId(id: backend.AssetId) {
  return (
    id === remoteBackendPaths.USERS_DIRECTORY_ID || id === remoteBackendPaths.TEAMS_DIRECTORY_ID
  )
}

// =============
// === Types ===
// =============

/** HTTP response body for the "list users" endpoint. */
export interface ListUsersResponseBody {
  readonly users: readonly backend.User[]
}

/** HTTP response body for the "list projects" endpoint. */
export interface ListDirectoryResponseBody {
  readonly assets: readonly backend.AnyAsset[]
}

/** HTTP response body for the "list projects" endpoint. */
export interface ListProjectsResponseBody {
  readonly projects: readonly backend.ListedProjectRaw[]
}

/** HTTP response body for the "list files" endpoint. */
export interface ListFilesResponseBody {
  readonly files: readonly backend.FileLocator[]
}

/** HTTP response body for the "list secrets" endpoint. */
export interface ListSecretsResponseBody {
  readonly secrets: readonly backend.SecretInfo[]
}

/** HTTP response body for the "list tag" endpoint. */
export interface ListTagsResponseBody {
  readonly tags: readonly backend.Label[]
}

/** HTTP response body for the "list versions" endpoint. */
export interface ListVersionsResponseBody {
  readonly versions: readonly [backend.Version, ...backend.Version[]]
}

// =====================
// === RemoteBackend ===
// =====================

/**
 * A function that turns a text ID (and a list of replacements, if required) to
 * human-readable text.
 */
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
  private user: object.Mutable<backend.User> | null = null

  /**
   * Create a new instance of the {@link RemoteBackend} API client.
   * @throws An error if the `Authorization` header is not set on the given `client`.
   */
  constructor(
    private readonly client: HttpClient,
    private readonly logger: loggerProvider.Logger,
    private getText: ReturnType<typeof textProvider.useText>['getText'],
  ) {
    super()
  }

  /**
   * Set `this.getText`. This function is exposed rather than the property itself to make it clear
   * that it is intended to be mutable.
   */
  setGetText(getText: GetText) {
    this.getText = getText
  }

  /**
   * Log an error message and throws an {@link Error} with the specified message.
   * @throws {Error} Always.
   */
  async throw<K extends Extract<text.TextId, `${string}BackendError`>>(
    response: Response | null,
    textId: backend.NetworkError | K,
    ...replacements: text.Replacements[K]
  ): Promise<never> {
    if (textId instanceof backend.NetworkError) {
      this.logger.error(textId.message)

      throw textId
    } else {
      const error =
        response == null || response.headers.get('Content-Type') !== 'application/json' ?
          { message: 'unknown error' }
          // This is SAFE only when the response has been confirmed to have an erroring status code.
          // eslint-disable-next-line no-restricted-syntax
        : ((await response.json()) as RemoteBackendError)
      const message = `${this.getText(textId, ...replacements)}: ${error.message}.`
      this.logger.error(message)

      const status = response?.status

      throw new backend.NetworkError(message, status)
    }
  }

  /** The path to the root directory of this {@link Backend}. */
  override rootPath(user: backend.User) {
    switch (user.plan) {
      case undefined:
      case backend.Plan.free:
      case backend.Plan.solo: {
        return `enso://Users/${user.name}`
      }
      case backend.Plan.team:
      case backend.Plan.enterprise: {
        return 'enso://'
      }
    }
  }

  /** Return the ID of the root directory. */
  override rootDirectoryId(
    user: backend.User,
    organization: backend.OrganizationInfo | null,
  ): backend.DirectoryId | null {
    switch (user.plan) {
      case undefined:
      case backend.Plan.free:
      case backend.Plan.solo: {
        return user.rootDirectoryId
      }
      case backend.Plan.team:
      case backend.Plan.enterprise: {
        return organization == null ? null : (
            backend.DirectoryId(`directory-${organization.id.replace(/^organization-/, '')}`)
          )
      }
    }
  }

  /** Return a list of all users in the same organization. */
  override async listUsers(): Promise<readonly backend.User[]> {
    const path = remoteBackendPaths.LIST_USERS_PATH
    const response = await this.get<ListUsersResponseBody>(path)
    if (response.status === STATUS_NOT_ALLOWED) {
      return []
    } else if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listUsersBackendError')
    } else {
      return (await response.json()).users
    }
  }

  /** Set the username and parent organization of the current user. */
  override async createUser(body: backend.CreateUserRequestBody): Promise<backend.User> {
    const path = remoteBackendPaths.CREATE_USER_PATH
    const response = await this.post<backend.User>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createUserBackendError')
    } else {
      return await response.json()
    }
  }

  /** Change the username of the current user. */
  override async updateUser(body: backend.UpdateUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.UPDATE_CURRENT_USER_PATH
    const response = await this.put(path, body)
    if (!responseIsSuccessful(response)) {
      return body.username != null ?
          await this.throw(response, 'updateUsernameBackendError')
        : await this.throw(response, 'updateUserBackendError')
    } else {
      if (this.user != null && body.username != null) {
        this.user.name = body.username
      }
      return
    }
  }

  /** Restore a user that has been soft-deleted. */
  async restoreUser(): Promise<void> {
    const response = await this.put(remoteBackendPaths.UPDATE_CURRENT_USER_PATH, {
      clearRemoveAt: true,
    })
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'restoreUserBackendError')
    } else {
      return
    }
  }

  /** Delete the current user. */
  override async deleteUser(): Promise<void> {
    const response = await this.delete(remoteBackendPaths.DELETE_USER_PATH)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'deleteUserBackendError')
    } else {
      return
    }
  }

  /**
   * Delete a user.
   * FIXME: Not implemented on backend yet.
   */
  override async removeUser(): Promise<void> {
    return await this.throw(null, 'removeUserBackendError')
  }

  /** Invite a new user to the organization by email. */
  override async inviteUser(body: backend.InviteUserRequestBody): Promise<void> {
    const response = await this.post(remoteBackendPaths.INVITE_USER_PATH, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'inviteUserBackendError', body.userEmail)
    } else {
      return
    }
  }

  /** List all invitations. */
  override async listInvitations() {
    const response = await this.get<backend.ListInvitationsResponseBody>(
      remoteBackendPaths.INVITATION_PATH,
    )

    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listInvitationsBackendError')
    } else {
      return await response.json()
    }
  }

  /** Delete an outgoing invitation. */
  override async deleteInvitation(userEmail: backend.EmailAddress): Promise<void> {
    const response = await this.delete(remoteBackendPaths.INVITATION_PATH, { userEmail })

    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'deleteInvitationBackendError')
    } else {
      return
    }
  }

  /** Resend an outgoing invitation to a user. */
  override async resendInvitation(userEmail: backend.EmailAddress): Promise<void> {
    await this.inviteUser({ userEmail, resend: true })
  }

  /** Accept an invitation to a new organization. */
  override async acceptInvitation(): Promise<void> {
    const response = await this.patch(remoteBackendPaths.ACCEPT_INVITATION_PATH, {})
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'acceptInvitationBackendError')
    } else {
      return
    }
  }

  /** Decline an invitation to a new organization. */
  override async declineInvitation(userEmail: backend.EmailAddress): Promise<void> {
    await this.deleteInvitation(userEmail)
  }

  /** Upload a new profile picture for the current user. */
  override async uploadUserPicture(
    params: backend.UploadPictureRequestParams,
    file: Blob,
  ): Promise<backend.User> {
    const paramsString = new URLSearchParams({
      // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_USER_PICTURE_PATH}?${paramsString}`
    const response = await this.putBinary<backend.User>(path, file)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'uploadUserPictureBackendError')
    } else {
      return await response.json()
    }
  }

  /** Set the list of groups a user is in. */
  override async changeUserGroup(
    userId: backend.UserId,
    userGroups: backend.ChangeUserGroupRequestBody,
    name: string,
  ): Promise<backend.User> {
    const path = remoteBackendPaths.changeUserGroupPath(userId)
    const response = await this.put<backend.User>(path, userGroups)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'changeUserGroupsBackendError', name)
    } else {
      return await response.json()
    }
  }

  /**
   * Return details for the current organization.
   * @returns `null` if a non-successful status code (not 200-299) was received.
   */
  override async getOrganization(): Promise<backend.OrganizationInfo | null> {
    const path = remoteBackendPaths.GET_ORGANIZATION_PATH
    const response = await this.get<backend.OrganizationInfo>(path)
    if ([STATUS_NOT_ALLOWED, STATUS_NOT_FOUND].includes(response.status)) {
      // Organization info has not yet been created.
      // or the user is not eligible to create an organization.
      return null
    } else if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getOrganizationBackendError')
    } else {
      return await response.json()
    }
  }

  /** Update details for the current organization. */
  override async updateOrganization(
    body: backend.UpdateOrganizationRequestBody,
  ): Promise<backend.OrganizationInfo | null> {
    const path = remoteBackendPaths.UPDATE_ORGANIZATION_PATH
    const response = await this.patch<backend.OrganizationInfo>(path, body)

    if (response.status === STATUS_NOT_FOUND) {
      // Organization info has not yet been created.
      return null
    } else if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'updateOrganizationBackendError')
    } else {
      return await response.json()
    }
  }

  /** Upload a new profile picture for the current organization. */
  override async uploadOrganizationPicture(
    params: backend.UploadPictureRequestParams,
    file: Blob,
  ): Promise<backend.OrganizationInfo> {
    const paramsString = new URLSearchParams({
      // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
      ...(params.fileName != null ? { file_name: params.fileName } : {}),
    }).toString()
    const path = `${remoteBackendPaths.UPLOAD_ORGANIZATION_PICTURE_PATH}?${paramsString}`
    const response = await this.putBinary<backend.OrganizationInfo>(path, file)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'uploadOrganizationPictureBackendError')
    } else {
      return await response.json()
    }
  }

  /** Adds a permission for a specific user on a specific asset. */
  override async createPermission(body: backend.CreatePermissionRequestBody): Promise<void> {
    const path = remoteBackendPaths.CREATE_PERMISSION_PATH
    const response = await this.post(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createPermissionBackendError')
    } else {
      return
    }
  }

  /**
   * Return details for the current user.
   * @returns `null` if a non-successful status code (not 200-299) was received.
   */
  override async usersMe(): Promise<backend.User | null> {
    const response = await this.get<backend.User>(remoteBackendPaths.USERS_ME_PATH)

    if (response.status === STATUS_NOT_FOUND) {
      // User info has not yet been created, we should redirect to the onboarding page.
      return null
    } else if (response.status === STATUS_NOT_AUTHORIZED) {
      // User is not authorized, we should redirect to the login page.
      return await this.throw(
        response,
        new backend.NotAuthorizedError(this.getText('notAuthorizedBackendError')),
      )
    } else if (!responseIsSuccessful(response)) {
      // Arbitrary error, might be a server error or a network error.
      return this.throw(response, 'usersMeBackendError')
    } else {
      const user = await response.json()
      this.user = { ...user }

      return user
    }
  }

  /**
   * Return a list of assets in a directory.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listDirectory(
    query: backend.ListDirectoryRequestParams,
    title: string,
  ): Promise<readonly backend.AnyAsset[]> {
    const path = remoteBackendPaths.LIST_DIRECTORY_PATH
    const response = await this.get<ListDirectoryResponseBody>(
      path +
        '?' +
        new URLSearchParams(
          query.recentProjects ?
            [['recent_projects', String(true)]]
          : [
              ...(query.parentId != null ? [['parent_id', query.parentId]] : []),
              ...(query.filterBy != null ? [['filter_by', query.filterBy]] : []),
              ...(query.labels != null ? query.labels.map((label) => ['label', label]) : []),
            ],
        ).toString(),
    )
    if (!responseIsSuccessful(response)) {
      if (response.status === STATUS_SERVER_ERROR) {
        this.logger.error(
          query.parentId != null ?
            `Error listing directory '${query.parentId}'`
          : `Error listing root directory`,
        )
        // The directory is probably empty.
        return []
      } else if (query.parentId != null) {
        return await this.throw(response, 'listFolderBackendError', title)
      } else {
        return await this.throw(response, 'listRootFolderBackendError')
      }
    } else {
      const ret = (await response.json()).assets
        .map((asset) =>
          object.merge(asset, {
            // eslint-disable-next-line no-restricted-syntax
            type: asset.id.match(/^(.+?)-/)?.[1] as backend.AssetType,
            // `Users` and `Teams` folders are virtual, so their children incorrectly have
            // the organization root id as their parent id.
            parentId: query.parentId ?? asset.parentId,
          }),
        )
        .map((asset) =>
          object.merge(asset, {
            permissions: [...(asset.permissions ?? [])].sort(backend.compareAssetPermissions),
          }),
        )
        .map((asset) => this.dynamicAssetUser(asset))
      return ret
    }
  }

  /**
   * Create a directory.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createDirectory(
    body: backend.CreateDirectoryRequestBody,
  ): Promise<backend.CreatedDirectory> {
    const path = remoteBackendPaths.CREATE_DIRECTORY_PATH
    const response = await this.post<backend.CreatedDirectory>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createFolderBackendError', body.title)
    } else {
      return await response.json()
    }
  }

  /**
   * Change the name of a directory.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async updateDirectory(
    directoryId: backend.DirectoryId,
    body: backend.UpdateDirectoryRequestBody,
    title: string,
  ) {
    const path = remoteBackendPaths.updateDirectoryPath(directoryId)
    const response = await this.put<backend.UpdatedDirectory>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'updateFolderBackendError', title)
    } else {
      return await response.json()
    }
  }

  /** List all previous versions of an asset. */
  override async listAssetVersions(
    assetId: backend.AssetId,
    title: string,
  ): Promise<backend.AssetVersions> {
    const path = remoteBackendPaths.listAssetVersionsPath(assetId)
    const response = await this.get<backend.AssetVersions>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listAssetVersionsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /** Fetch the content of the `Main.enso` file of a project. */
  override async getFileContent(
    projectId: backend.ProjectId,
    version: string,
    title: string,
  ): Promise<string> {
    const path = remoteBackendPaths.getProjectContentPath(projectId, version)
    const response = await this.get<string>(path)

    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getFileContentsBackendError', title)
    } else {
      return await response.text()
    }
  }

  /**
   * Change the parent directory or description of an asset.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async updateAsset(
    assetId: backend.AssetId,
    body: backend.UpdateAssetRequestBody,
    title: string,
  ) {
    const path = remoteBackendPaths.updateAssetPath(assetId)
    const response = await this.patch(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'updateAssetBackendError', title)
    } else {
      return
    }
  }

  /**
   * Delete an arbitrary asset.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async deleteAsset(
    assetId: backend.AssetId,
    body: backend.DeleteAssetRequestBody,
    title: string,
  ) {
    const paramsString = new URLSearchParams([['force', String(body.force)]]).toString()
    const path = remoteBackendPaths.deleteAssetPath(assetId) + '?' + paramsString
    const response = await this.delete(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'deleteAssetBackendError', title)
    } else {
      return
    }
  }

  /**
   * Restore an arbitrary asset from the trash.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async undoDeleteAsset(assetId: backend.AssetId, title: string): Promise<void> {
    const path = remoteBackendPaths.UNDO_DELETE_ASSET_PATH
    const response = await this.patch(path, { assetId })
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'undoDeleteAssetBackendError', title)
    } else {
      return
    }
  }

  /**
   * Copy an arbitrary asset to another directory.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async copyAsset(
    assetId: backend.AssetId,
    parentDirectoryId: backend.DirectoryId,
    title: string,
    parentDirectoryTitle: string,
  ): Promise<backend.CopyAssetResponse> {
    const response = await this.post<backend.CopyAssetResponse>(
      remoteBackendPaths.copyAssetPath(assetId),
      { parentDirectoryId },
    )
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'copyAssetBackendError', title, parentDirectoryTitle)
    } else {
      return await response.json()
    }
  }

  /**
   * Return a list of projects belonging to the current user.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listProjects(): Promise<backend.ListedProject[]> {
    const path = remoteBackendPaths.LIST_PROJECTS_PATH
    const response = await this.get<ListProjectsResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listProjectsBackendError')
    } else {
      return (await response.json()).projects.map((project) => ({
        ...project,
        jsonAddress: project.address != null ? backend.Address(`${project.address}json`) : null,
        binaryAddress: project.address != null ? backend.Address(`${project.address}binary`) : null,
        ydocAddress: project.address != null ? backend.Address(`${project.address}project`) : null,
      }))
    }
  }

  /**
   * Create a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createProject(
    body: backend.CreateProjectRequestBody,
  ): Promise<backend.CreatedProject> {
    const path = remoteBackendPaths.CREATE_PROJECT_PATH
    const response = await this.post<backend.CreatedProject>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createProjectBackendError', body.projectName)
    } else {
      return await response.json()
    }
  }

  /** Restore a project from a different version. */
  override async restoreProject(
    projectId: backend.ProjectId,
    versionId: backend.S3ObjectVersionId,
    title: string,
  ): Promise<void> {
    const path = remoteBackendPaths.restoreProjectPath(projectId)
    const response = await this.post(path, { versionId })
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'restoreProjectBackendError', title)
    } else {
      return
    }
  }

  /** Duplicate a specific version of a project. */
  override async duplicateProject(
    projectId: backend.ProjectId,
    versionId: backend.S3ObjectVersionId,
    title: string,
  ): Promise<backend.CreatedProject> {
    const path = remoteBackendPaths.duplicateProjectPath(projectId)
    const response = await this.post<backend.CreatedProject>(path, { versionId })
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'duplicateProjectBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Close a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async closeProject(projectId: backend.ProjectId, title: string): Promise<void> {
    const path = remoteBackendPaths.closeProjectPath(projectId)
    const response = await this.post(path, {})
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'closeProjectBackendError', title)
    } else {
      return
    }
  }

  /**
   * List project sessions for a specific project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listProjectSessions(
    projectId: backend.ProjectId,
    title: string,
  ): Promise<backend.ProjectSession[]> {
    const paramsString = new URLSearchParams({ projectId }).toString()
    const path = `${remoteBackendPaths.LIST_PROJECT_SESSIONS_PATH}?${paramsString}`
    const response = await this.get<backend.ProjectSession[]>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listProjectSessionsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getProjectDetails(
    projectId: backend.ProjectId,
    _directory: backend.DirectoryId | null,
    title: string,
  ): Promise<backend.Project> {
    const path = remoteBackendPaths.getProjectDetailsPath(projectId)
    const response = await this.get<backend.ProjectRaw>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getProjectDetailsBackendError', title)
    } else {
      const project = await response.json()
      const ideVersion =
        project.ide_version ?? (await this.getDefaultVersion(backend.VersionType.ide))
      return {
        ...project,
        ideVersion,
        engineVersion: project.engine_version,
        jsonAddress: project.address != null ? backend.Address(`${project.address}json`) : null,
        binaryAddress: project.address != null ? backend.Address(`${project.address}binary`) : null,
        ydocAddress: project.address != null ? backend.Address(`${project.address}project`) : null,
      }
    }
  }

  /**
   * Return Language Server logs for a project session.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getProjectSessionLogs(
    projectSessionId: backend.ProjectSessionId,
    title: string,
  ): Promise<string[]> {
    const path = remoteBackendPaths.getProjectSessionLogsPath(projectSessionId)
    const response = await this.get<string[]>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getProjectLogsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Prepare a project for execution.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async openProject(
    projectId: backend.ProjectId,
    bodyRaw: backend.OpenProjectRequestBody,
    title: string,
  ): Promise<void> {
    const body = object.omit(bodyRaw, 'parentId')
    const path = remoteBackendPaths.openProjectPath(projectId)
    if (body.cognitoCredentials == null) {
      return this.throw(null, 'openProjectMissingCredentialsBackendError', title)
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
      const response = await this.post(path, filteredBody)

      if (!responseIsSuccessful(response)) {
        return this.throw(response, 'openProjectBackendError', title)
      } else {
        return
      }
    }
  }

  /**
   * Update the name or AMI of a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async updateProject(
    projectId: backend.ProjectId,
    body: backend.UpdateProjectRequestBody,
    title: string,
  ): Promise<backend.UpdatedProject> {
    const path = remoteBackendPaths.projectUpdatePath(projectId)
    const response = await this.put<backend.UpdatedProject>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'updateProjectBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Return the resource usage of a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async checkResources(
    projectId: backend.ProjectId,
    title: string,
  ): Promise<backend.ResourceUsage> {
    const path = remoteBackendPaths.checkResourcesPath(projectId)
    const response = await this.get<backend.ResourceUsage>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'checkResourcesBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Return a list of files accessible by the current user.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listFiles(): Promise<readonly backend.FileLocator[]> {
    const path = remoteBackendPaths.LIST_FILES_PATH
    const response = await this.get<ListFilesResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listFilesBackendError')
    } else {
      return (await response.json()).files
    }
  }

  /**
   * Begin uploading a large file.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async uploadFileStart(
    body: backend.UploadFileRequestParams,
    file: File,
  ): Promise<backend.UploadLargeFileMetadata> {
    const path = remoteBackendPaths.UPLOAD_FILE_START_PATH
    const requestBody: backend.UploadFileStartRequestBody = {
      fileName: body.fileName,
      size: file.size,
    }
    const response = await this.post<backend.UploadLargeFileMetadata>(path, requestBody)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'uploadFileStartBackendError')
    } else {
      return await response.json()
    }
  }

  /**
   * Upload a chunk of a large file.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async uploadFileChunk(
    url: backend.HttpsUrl,
    file: Blob,
    index: number,
  ): Promise<backend.S3MultipartPart> {
    const start = index * backend.S3_CHUNK_SIZE_BYTES
    const end = Math.min(start + backend.S3_CHUNK_SIZE_BYTES, file.size)
    const body = file.slice(start, end)
    const response = await fetch(url, { method: 'PUT', body })
    const eTag = response.headers.get('ETag')
    if (!responseIsSuccessful(response) || eTag == null) {
      return await this.throw(response, 'uploadFileChunkBackendError')
    } else {
      return { eTag, partNumber: index + 1 }
    }
  }

  /**
   * Finish uploading a large file.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async uploadFileEnd(
    body: backend.UploadFileEndRequestBody,
  ): Promise<backend.UploadedLargeAsset> {
    const path = remoteBackendPaths.UPLOAD_FILE_END_PATH
    const response = await this.post<backend.UploadedLargeAsset>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'uploadFileEndBackendError')
    } else {
      return await response.json()
    }
  }

  /** Change the name of a file. */
  override async updateFile(): Promise<void> {
    await this.throw(null, 'updateFileNotImplementedBackendError')
  }

  /**
   * Return details for a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getFileDetails(
    fileId: backend.FileId,
    title: string,
  ): Promise<backend.FileDetails> {
    const path = remoteBackendPaths.getFileDetailsPath(fileId)
    const response = await this.get<backend.FileDetails>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getFileDetailsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Return a Datalink.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createDatalink(
    body: backend.CreateDatalinkRequestBody,
  ): Promise<backend.DatalinkInfo> {
    const path = remoteBackendPaths.CREATE_DATALINK_PATH
    const response = await this.post<backend.DatalinkInfo>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createDatalinkBackendError', body.name)
    } else {
      return await response.json()
    }
  }

  /**
   * Return a Datalink.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getDatalink(
    datalinkId: backend.DatalinkId,
    title: string,
  ): Promise<backend.Datalink> {
    const path = remoteBackendPaths.getDatalinkPath(datalinkId)
    const response = await this.get<backend.Datalink>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getDatalinkBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Delete a Datalink.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async deleteDatalink(datalinkId: backend.DatalinkId, title: string): Promise<void> {
    const path = remoteBackendPaths.getDatalinkPath(datalinkId)
    const response = await this.delete(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'deleteDatalinkBackendError', title)
    } else {
      return
    }
  }

  /**
   * Create a secret environment variable.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createSecret(body: backend.CreateSecretRequestBody): Promise<backend.SecretId> {
    const path = remoteBackendPaths.CREATE_SECRET_PATH
    const response = await this.post<backend.SecretId>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createSecretBackendError', body.name)
    } else {
      return await response.json()
    }
  }

  /**
   * Return a secret environment variable.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getSecret(secretId: backend.SecretId, title: string): Promise<backend.Secret> {
    const path = remoteBackendPaths.getSecretPath(secretId)
    const response = await this.get<backend.Secret>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getSecretBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Update a secret environment variable.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async updateSecret(
    secretId: backend.SecretId,
    body: backend.UpdateSecretRequestBody,
    title: string,
  ): Promise<void> {
    const path = remoteBackendPaths.updateSecretPath(secretId)
    const response = await this.put(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'updateSecretBackendError', title)
    } else {
      return
    }
  }

  /**
   * Return the secret environment variables accessible by the user.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listSecrets(): Promise<readonly backend.SecretInfo[]> {
    const path = remoteBackendPaths.LIST_SECRETS_PATH
    const response = await this.get<ListSecretsResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listSecretsBackendError')
    } else {
      return (await response.json()).secrets
    }
  }

  /**
   * Create a label used for categorizing assets.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createTag(body: backend.CreateTagRequestBody): Promise<backend.Label> {
    const path = remoteBackendPaths.CREATE_TAG_PATH
    const response = await this.post<backend.Label>(path, body)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createLabelBackendError', body.value)
    } else {
      return await response.json()
    }
  }

  /**
   * Return all labels accessible by the user.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listTags(): Promise<readonly backend.Label[]> {
    const path = remoteBackendPaths.LIST_TAGS_PATH
    const response = await this.get<ListTagsResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listLabelsBackendError')
    } else {
      return (await response.json()).tags
    }
  }

  /**
   * Set the full list of labels for a specific asset.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async associateTag(
    assetId: backend.AssetId,
    labels: backend.LabelName[],
    title: string,
  ) {
    const path = remoteBackendPaths.associateTagPath(assetId)
    const response = await this.patch<ListTagsResponseBody>(path, { labels })
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'associateLabelsBackendError', title)
    } else {
      return
    }
  }

  /**
   * Delete a label.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async deleteTag(tagId: backend.TagId, value: backend.LabelName): Promise<void> {
    const path = remoteBackendPaths.deleteTagPath(tagId)
    const response = await this.delete(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'deleteLabelBackendError', value)
    } else {
      return
    }
  }

  /** Create a user group. */
  override async createUserGroup(
    body: backend.CreateUserGroupRequestBody,
  ): Promise<backend.UserGroupInfo> {
    const path = remoteBackendPaths.CREATE_USER_GROUP_PATH
    const response = await this.post<backend.UserGroupInfo>(path, body)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'createUserGroupBackendError', body.name)
    } else {
      return await response.json()
    }
  }

  /** Delete a user group. */
  override async deleteUserGroup(userGroupId: backend.UserGroupId, name: string): Promise<void> {
    const path = remoteBackendPaths.deleteUserGroupPath(userGroupId)
    const response = await this.delete(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'deleteUserGroupBackendError', name)
    } else {
      return
    }
  }

  /**
   * List all roles in the organization.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listUserGroups(): Promise<backend.UserGroupInfo[]> {
    const path = remoteBackendPaths.LIST_USER_GROUPS_PATH
    const response = await this.get<backend.UserGroupInfo[]>(path)
    if (response.status === STATUS_NOT_ALLOWED) {
      return [] as const
    } else if (!responseIsSuccessful(response)) {
      return this.throw(response, 'listUserGroupsBackendError')
    } else {
      return await response.json()
    }
  }

  /**
   * Return a list of backend or IDE versions.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listVersions(
    params: backend.ListVersionsRequestParams,
  ): Promise<readonly backend.Version[]> {
    const paramsString = new URLSearchParams({
      // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
      version_type: params.versionType,
      default: String(params.default),
    }).toString()
    const path = remoteBackendPaths.LIST_VERSIONS_PATH + '?' + paramsString
    const response = await this.get<ListVersionsResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'listVersionsBackendError', params.versionType)
    } else {
      return (await response.json()).versions
    }
  }

  /**
   * Create a payment checkout session.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createCheckoutSession(
    params: backend.CreateCheckoutSessionRequestBody,
  ): Promise<backend.CheckoutSession> {
    const response = await this.post<backend.CheckoutSession>(
      remoteBackendPaths.CREATE_CHECKOUT_SESSION_PATH,
      params,
    )
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'createCheckoutSessionBackendError', params.plan)
    } else {
      return await response.json()
    }
  }

  /**
   * Gets the status of a payment checkout session.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getCheckoutSession(
    sessionId: backend.CheckoutSessionId,
  ): Promise<backend.CheckoutSessionStatus> {
    const path = remoteBackendPaths.getCheckoutSessionPath(sessionId)
    const response = await this.get<backend.CheckoutSessionStatus>(path)
    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getCheckoutSessionBackendError', sessionId)
    } else {
      return await response.json()
    }
  }

  /** List events in the organization's audit log. */
  override async getLogEvents(): Promise<backend.Event[]> {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly events: backend.Event[]
    }

    const path = remoteBackendPaths.GET_LOG_EVENTS_PATH
    const response = await this.get<ResponseBody>(path)
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'getLogEventsBackendError')
    } else {
      const json = await response.json()
      return json.events
    }
  }

  /** Log an event that will be visible in the organization audit log. */
  async logEvent(message: string, projectId?: string | null, metadata?: object | null) {
    // Prevent events from being logged in dev mode, since we are often using production environment
    // and are polluting real logs.
    if (detect.IS_DEV_MODE && process.env.ENSO_CLOUD_ENVIRONMENT === 'production') {
      return
    }

    const path = remoteBackendPaths.POST_LOG_EVENT_PATH
    const response = await this.post(
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
      },
    )
    if (!responseIsSuccessful(response)) {
      return this.throw(response, 'logEventBackendError', message)
    }
  }

  /** Download from an arbitrary URL that is assumed to originate from this backend. */
  override async download(url: string, name?: string) {
    download.download(url, name)
    return Promise.resolve()
  }

  /** Fetch the URL of the customer portal. */
  override async createCustomerPortalSession() {
    const response = await this.post<backend.CreateCustomerPortalSessionResponse>(
      remoteBackendPaths.getCustomerPortalSessionPath(),
      {},
    )

    if (!responseIsSuccessful(response)) {
      return await this.throw(response, 'getCustomerPortalUrlBackendError')
    } else {
      return (await response.json()).url
    }
  }

  /** Get the default version given the type of version (IDE or backend). */
  private async getDefaultVersion(versionType: backend.VersionType) {
    const cached = this.defaultVersions[versionType]
    const nowEpochMs = Number(new Date())
    if (cached != null && nowEpochMs - cached.lastUpdatedEpochMs < ONE_DAY_MS) {
      return cached.version
    } else {
      const version = (await this.listVersions({ versionType, default: true }))[0]?.number
      if (version == null) {
        return await this.throw(null, 'getDefaultVersionBackendError', versionType)
      } else {
        const info: DefaultVersionInfo = { version, lastUpdatedEpochMs: nowEpochMs }
        this.defaultVersions[versionType] = info
        return info.version
      }
    }
  }

  /**
   * Replaces the `user` of all permissions for the current user on an asset, so that they always
   * return the up-to-date user.
   */
  private dynamicAssetUser<Asset extends backend.AnyAsset>(asset: Asset) {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    const self = this
    let foundSelfPermission = (() => false)()
    const permissions = asset.permissions?.map((permission) => {
      if (!('user' in permission) || permission.user.userId !== this.user?.userId) {
        return permission
      } else {
        foundSelfPermission = true
        return {
          ...permission,
          /** Return a dynamic reference to the current user. */
          get user() {
            return self.user
          },
        }
      }
    })
    return !foundSelfPermission ? asset : { ...asset, permissions }
  }

  /** Send an HTTP GET request to the given path. */
  private get<T = void>(path: string) {
    return this.client.get<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`)
  }

  /** Send a JSON HTTP POST request to the given path. */
  private post<T = void>(path: string, payload: object, options?: RemoteBackendPostOptions) {
    return this.client.post<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload, options)
  }

  /** Send a binary HTTP POST request to the given path. */
  private postBinary<T = void>(path: string, payload: Blob) {
    return this.client.postBinary<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  private patch<T = void>(path: string, payload: object) {
    return this.client.patch<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a JSON HTTP PUT request to the given path. */
  private put<T = void>(path: string, payload: object) {
    return this.client.put<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send a binary HTTP PUT request to the given path. */
  private putBinary<T = void>(path: string, payload: Blob) {
    return this.client.putBinary<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }

  /** Send an HTTP DELETE request to the given path. */
  private delete<T = void>(path: string, payload?: Record<string, unknown>) {
    return this.client.delete<T>(`${process.env.ENSO_CLOUD_API_URL}/${path}`, payload)
  }
}
