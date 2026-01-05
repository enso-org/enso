/**
 * @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API.
 */
import { markRaw } from 'vue'
import { z } from 'zod'
import type { DownloadOptions } from '../download.js'
import type { DefaultGetText } from '../text.js'
import { delay } from '../utilities/async.js'
import * as objects from '../utilities/data/object.js'
import * as detect from '../utilities/detect.js'
import { getFileName, getFolderPath } from '../utilities/file.js'
import * as backend from './Backend.js'
import * as remoteBackendPaths from './Backend/remoteBackendPaths.js'
import type { HttpClient } from './HttpClient.js'
import { organizationIdToDirectoryId } from './RemoteBackend/ids.js'

/** HTTP status indicating that the resource does not exist. */
const STATUS_NOT_FOUND = 404
/** HTTP status indicating that authorized user doesn't have access to the given resource */
const STATUS_NOT_ALLOWED = 403
/** The interval between checks for the export status. */
const EXPORT_STATUS_INTERVAL_MS = 5_000
/** The interval between checks for the import status. */
const IMPORT_STATUS_INTERVAL_MS = 5_000

export type DownloadCloudProjectFunction = (
  this: RemoteBackend,
  params: {
    downloadUrl: backend.HttpsUrl
    projectId: backend.ProjectId
  },
) => Promise<{
  readonly projectRootDirectory: string
  readonly parentDirectory: string
}>

export type GetProjectArchiveFunction = (
  this: RemoteBackend,
  directoryId: backend.DirectoryId,
  fileName: string,
) => Promise<File>

/** Class for sending requests to the Cloud backend API endpoints. */
export class RemoteBackend extends backend.Backend {
  static readonly type = backend.BackendType.remote
  override readonly type = RemoteBackend.type
  override readonly baseUrl: URL
  private user: objects.Mutable<backend.User> | null = null
  private readonly downloadCloudProject: DownloadCloudProjectFunction
  readonly getProjectArchive: GetProjectArchiveFunction

  /** Create a {@link RemoteBackend}. */
  constructor({
    apiUrl,
    getText,
    client,
    downloader,
    downloadCloudProject,
    getProjectArchive,
  }: {
    apiUrl: string
    getText: DefaultGetText
    client: HttpClient
    downloader: (options: DownloadOptions) => void | Promise<void>
    downloadCloudProject: DownloadCloudProjectFunction
    getProjectArchive: GetProjectArchiveFunction
  }) {
    super(getText, client, downloader)
    const baseOrigin = typeof location !== 'undefined' ? location.href : 'https://example.com'
    this.baseUrl = new URL(apiUrl, baseOrigin)
    this.downloadCloudProject = downloadCloudProject
    this.getProjectArchive = getProjectArchive
  }

  /** The path to the root directory of this {@link Backend}. */
  override rootPath(user: backend.User) {
    switch (user.plan) {
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
      case backend.Plan.free:
      case backend.Plan.solo: {
        return user.rootDirectoryId
      }
      case backend.Plan.team:
      case backend.Plan.enterprise: {
        return organization == null ? null : organizationIdToDirectoryId(organization.id)
      }
    }
  }

  /** Return a list of all users in the same organization. */
  override async listUsers(): Promise<readonly Omit<backend.User, 'groups'>[]> {
    const path = remoteBackendPaths.LIST_USERS_PATH
    const response = await this.get<backend.ListUsersResponseBody>(path)
    if (response.status === STATUS_NOT_ALLOWED) {
      return []
    } else if (!response.ok) {
      return await this.throw(response, 'listUsersBackendError')
    } else {
      return (await response.json()).users
    }
  }

  /** Set the username and parent organization of the current user. */
  override async createUser(body: backend.CreateUserRequestBody): Promise<backend.User> {
    const path = remoteBackendPaths.CREATE_USER_PATH
    const response = await this.post<backend.User>(path, body)
    if (!response.ok) {
      return await this.throw(response, 'createUserBackendError')
    } else {
      return await response.json()
    }
  }

  /** Change the username of the current user. */
  override async updateUser(body: backend.UpdateUserRequestBody): Promise<void> {
    const path = remoteBackendPaths.UPDATE_CURRENT_USER_PATH
    const response = await this.put(path, body)
    if (!response.ok) {
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
    if (!response.ok) {
      return await this.throw(response, 'restoreUserBackendError')
    } else {
      return
    }
  }

  /** Delete the current user. */
  override async deleteUser(): Promise<void> {
    const response = await this.delete(remoteBackendPaths.DELETE_USER_PATH)
    if (!response.ok) {
      return await this.throw(response, 'deleteUserBackendError')
    } else {
      return
    }
  }

  /**
   * Delete a user.
   */
  override async removeUser(userId: backend.UserId): Promise<void> {
    const response = await this.delete(remoteBackendPaths.removeUserPath(userId))
    if (!response.ok) {
      return await this.throw(response, 'removeUserBackendError')
    } else {
      return
    }
  }

  /** Invite a new user to the organization by email. */
  override async inviteUser(body: backend.InviteUserRequestBody): Promise<void> {
    const response = await this.post(remoteBackendPaths.INVITE_USER_PATH, body)
    if (!response.ok) {
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

    if (!response.ok) {
      return await this.throw(response, 'listInvitationsBackendError')
    } else {
      return await response.json()
    }
  }

  /** Delete an outgoing invitation. */
  override async deleteInvitation(userEmail: backend.EmailAddress): Promise<void> {
    const response = await this.delete(remoteBackendPaths.INVITATION_PATH, { userEmail })

    if (!response.ok) {
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
    if (!response.ok) {
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
    const paramsString = new URLSearchParams(
      // eslint-disable-next-line camelcase
      params.fileName != null ? { file_name: params.fileName } : {},
    ).toString()
    const path = `${remoteBackendPaths.UPLOAD_USER_PICTURE_PATH}?${paramsString}`
    const response = await this.putBinary<backend.User>(path, file)
    if (!response.ok) {
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
    if (!response.ok) {
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
    }

    if (!response.ok) {
      return await this.throw(response, 'getOrganizationBackendError')
    }

    return await response.json()
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
    } else if (!response.ok) {
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
    const paramsString = new URLSearchParams(
      // eslint-disable-next-line camelcase
      params.fileName != null ? { file_name: params.fileName } : {},
    ).toString()
    const path = `${remoteBackendPaths.UPLOAD_ORGANIZATION_PICTURE_PATH}?${paramsString}`
    const response = await this.putBinary<backend.OrganizationInfo>(path, file)
    if (!response.ok) {
      return await this.throw(response, 'uploadOrganizationPictureBackendError')
    } else {
      return await response.json()
    }
  }

  /** Adds a permission for a specific user on a specific asset. */
  override async createPermission(body: backend.CreatePermissionRequestBody): Promise<void> {
    const path = remoteBackendPaths.CREATE_PERMISSION_PATH
    const response = await this.post(path, body)
    if (!response.ok) {
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
    }

    if (!response.ok) {
      // Arbitrary error, might be a server error or a network error.
      return this.throw(response, 'usersMeBackendError')
    }

    const user = await response.json()

    const plan = user.plan

    if (plan == null) {
      // @ts-expect-error The property is declared as read-only, but it's not enforced.
      // We assume it's read-only for external use.
      // backend may return null for the plan, but this means that the user is on the free plan.
      // so we normalize it to the free plan.
      user.plan = backend.Plan.free
    }

    Object.defineProperty(user, 'isEnsoTeamMember', {
      value: user.email.endsWith('@enso.org') || user.email.endsWith('@ensoanalytics.com'),
      writable: false,
      configurable: false,
      enumerable: true,
    })

    this.user = user

    return user
  }

  /**
   * Return a list of assets in a directory.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listDirectory(
    query: backend.ListDirectoryRequestParams,
    title: string,
  ): Promise<backend.ListDirectoryResponseBody> {
    if (query.recentProjects && query.from) {
      return { assets: [], paginationToken: null }
    }
    const paramsString = new URLSearchParams(
      query.recentProjects === true ?
        [['recent_projects', String(true)]]
      : [
          ...(query.parentId != null ? [['parent_id', query.parentId]] : []),
          ...(query.filterBy != null ? [['filter_by', query.filterBy]] : []),
          ...(query.from != null ? [['from', query.from]] : []),
          ...(query.pageSize != null ? [['page_size', String(query.pageSize)]] : []),
          ...(query.labels?.map((label) => ['label', label]) ?? []),
          ...(query.sortExpression != null ? [['sort_expression', query.sortExpression]] : []),
          ...(query.sortDirection != null ? [['sort_direction', query.sortDirection]] : []),
          ...(query.labels != null ? query.labels.map((label) => ['label', label]) : []),
        ],
    ).toString()
    const response = await this.get<backend.ListDirectoryResponseBody>(
      `${remoteBackendPaths.LIST_DIRECTORY_PATH}?${paramsString}`,
    )
    if (!response.ok) {
      if (query.parentId != null) {
        return await this.throw(response, 'listFolderBackendError', title)
      } else {
        return await this.throw(response, 'listRootFolderBackendError')
      }
    } else {
      const responseBody = await response.json()
      return {
        ...responseBody,
        assets: this.listDirectoryResponseToAssetList(responseBody.assets, query.parentId),
      }
    }
  }

  /**
   * Search for assets in a directory.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async searchDirectory(
    query: backend.SearchDirectoryRequestParams,
  ): Promise<backend.ListDirectoryResponseBody> {
    const paramsString = new URLSearchParams([
      ...(query.parentId != null ? [['parent_id', query.parentId]] : []),
      ...(query.query != null ? [['query', query.query]] : []),
      ...(query.title != null ? [['title', query.title]] : []),
      ...(query.description != null ? [['description', query.description]] : []),
      ...(query.type != null ? [['type', query.type]] : []),
      ...(query.extension != null ? [['extension', query.extension]] : []),
      ...(query.labels?.map((label) => ['label', label]) ?? []),
      ...(query.sortExpression != null ? [['sort_expression', query.sortExpression]] : []),
      ...(query.sortDirection != null ? [['sort_direction', query.sortDirection]] : []),
      ...(query.from != null ? [['from', query.from]] : []),
      ...(query.pageSize != null ? [['pageSize', String(query.pageSize)]] : []),
    ]).toString()
    const path = `${remoteBackendPaths.SEARCH_DIRECTORY_PATH}?${paramsString}`
    const response = await this.get<backend.ListDirectoryResponseBody>(path)
    if (!response.ok) {
      return await this.throw(response, 'searchFolderBackendError')
    } else {
      const responseBody = await response.json()
      return {
        ...responseBody,
        assets: this.listDirectoryResponseToAssetList(responseBody.assets, query.parentId),
      }
    }
  }

  /**
   * Create a directory.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createDirectory(
    body: backend.CreateDirectoryRequestBody,
    discardTitle = true,
  ): Promise<backend.CreatedDirectory> {
    const path = remoteBackendPaths.CREATE_DIRECTORY_PATH

    // Remote backend doesn't need the title in the body.
    // It's generated on the server side.
    const { title, ...rest } = body

    const response = await this.post<backend.CreatedDirectory>(path, discardTitle ? rest : body)
    if (!response.ok) {
      return await this.throw(response, 'createFolderBackendError', title)
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
    if (!response.ok) {
      return await this.throw(response, 'updateFolderBackendError', title)
    } else {
      return await response.json()
    }
  }

  /** List all previous versions of an asset. */
  override async listAssetVersions(
    assetId: backend.DatalinkId | backend.FileId | backend.ProjectId,
  ): Promise<backend.AssetVersions> {
    const path = remoteBackendPaths.listAssetVersionsPath(assetId)
    const response = await this.get<backend.AssetVersions>(path)
    if (!response.ok) {
      return await this.throw(response, 'listAssetVersionsBackendError')
    } else {
      return await response.json()
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

    if (!response.ok) {
      await this.throw(response, 'updateAssetBackendError', title).catch((error) => {
        if (isDuplicateAssetError(error)) {
          throw new backend.DuplicateAssetError(error.message)
        }

        throw error
      })
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
    if (!response.ok) {
      return await this.throw(response, 'deleteAssetBackendError', title)
    } else {
      return
    }
  }

  /**
   * Restore an arbitrary asset from the trash.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async undoDeleteAsset(
    assetId: backend.AssetId,
    parentDirectoryId: backend.DirectoryId | null,
  ): Promise<void> {
    const path = remoteBackendPaths.UNDO_DELETE_ASSET_PATH
    const response = await this.patch(path, { assetId, parentDirectoryId })

    if (!response.ok) {
      return await this.throw(response, 'undoDeleteAssetBackendError')
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
    versionId?: backend.S3ObjectVersionId,
  ): Promise<backend.CopyAssetResponse> {
    const response = await this.post<backend.CopyAssetResponse>(
      remoteBackendPaths.copyAssetPath(assetId),
      { parentDirectoryId, versionId },
    )

    if (!response.ok) {
      return await this.throw(response, 'copyAssetBackendError').catch((error) => {
        if (isDuplicateAssetError(error)) {
          throw new backend.DuplicateAssetError(error.message)
        }

        throw error
      })
    }

    const { asset } = await response.json()
    return { asset: this.normalizeAsset(asset, parentDirectoryId) }
  }

  /**
   * Create a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createProject(
    body: backend.CreateProjectRequestBody,
  ): Promise<backend.CreatedProject> {
    const path = remoteBackendPaths.CREATE_PROJECT_PATH
    // Remote backend doesn't need the project name in the body.
    // It's generated on the server side.
    const { projectName, ...rest } = body

    const response = await this.post<backend.CreatedProject>(path, rest)
    if (!response.ok) {
      return await this.throw(response, 'createProjectBackendError', projectName)
    } else {
      return await response.json()
    }
  }

  /** Restore a project from a different version. */
  override async restoreAsset(
    assetId: backend.AssetId,
    versionId: backend.S3ObjectVersionId,
  ): Promise<void> {
    const path = remoteBackendPaths.restoreAssetPath(assetId)
    const response = await this.post(path, { versionId })
    if (!response.ok) {
      return await this.throw(response, 'restoreAssetBackendError')
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
    if (!response.ok) {
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
    if (!response.ok) {
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
    if (!response.ok) {
      return await this.throw(response, 'listProjectSessionsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Create a project execution.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createProjectExecution(
    body: backend.CreateProjectExecutionRequestBody,
    title: string,
  ): Promise<backend.ProjectExecution> {
    const { projectId, ...rest } = body
    const path = remoteBackendPaths.createProjectExecutionPath(projectId)
    const response = await this.post<backend.ProjectExecution>(path, rest)
    if (!response.ok) {
      return await this.throw(response, 'createProjectExecutionBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Create a project execution.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getProjectExecutionDetails(
    executionId: backend.ProjectExecutionId,
    title: string,
  ): Promise<backend.ProjectExecution> {
    const path = remoteBackendPaths.getProjectExecutionDetailsPath(executionId)
    const response = await this.get<backend.ProjectExecution>(path)
    if (!response.ok) {
      return await this.throw(response, 'getProjectExecutionDetailsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Update a project execution.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async updateProjectExecution(
    executionId: backend.ProjectExecutionId,
    body: backend.UpdateProjectExecutionRequestBody,
    projectTitle: string,
  ): Promise<backend.ProjectExecution> {
    const path = remoteBackendPaths.updateProjectExecutionPath(executionId)
    const response = await this.post<backend.ProjectExecution>(path, body)
    if (!response.ok) {
      return await this.throw(response, 'updateProjectExecutionBackendError', projectTitle)
    } else {
      return await response.json()
    }
  }

  /**
   * Delete a project execution.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async deleteProjectExecution(
    executionId: backend.ProjectExecutionId,
    projectTitle: string,
  ): Promise<void> {
    const path = remoteBackendPaths.deleteProjectExecutionPath(executionId)
    const response = await this.delete<backend.ProjectExecution>(path)
    if (!response.ok) {
      return await this.throw(response, 'createProjectExecutionBackendError', projectTitle)
    } else {
      return
    }
  }

  /**
   * Return a list of executions for a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listProjectExecutions(
    projectId: backend.ProjectId,
    title: string,
  ): Promise<readonly backend.ProjectExecution[]> {
    const path = remoteBackendPaths.listProjectExecutionsPath(projectId)
    const response = await this.get<readonly backend.ProjectExecution[]>(path)
    if (!response.ok) {
      return await this.throw(response, 'listProjectExecutionsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Update a project execution to use the latest version of a project.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async syncProjectExecution(
    executionId: backend.ProjectExecutionId,
    projectTitle: string,
  ): Promise<backend.ProjectExecution> {
    const path = remoteBackendPaths.syncProjectExecutionPath(executionId)

    const response = await this.post<backend.ProjectExecution>(path, {})
    if (!response.ok) {
      return await this.throw(response, 'syncProjectExecutionBackendError', projectTitle)
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
    getPresignedUrl = false,
  ): Promise<backend.Project> {
    const paramsString = new URLSearchParams({
      presigned: `${getPresignedUrl}`,
    }).toString()
    const path = `${remoteBackendPaths.getProjectDetailsPath(projectId)}?${paramsString}`
    const response = await this.get<backend.ProjectRaw>(path)
    if (!response.ok) {
      return await this.throw(response, 'getProjectDetailsBackendError')
    } else {
      const project = await response.json()
      const { address, ...rest } = project
      return {
        ...rest,
        jsonAddress: address != null ? backend.Address(`${address}json`) : null,
        binaryAddress: address != null ? backend.Address(`${address}binary`) : null,
        ydocAddress: address != null ? backend.Address(`${address}project`) : null,
      }
    }
  }

  /**
   * Return asset details.
   * @throws An error if a non-successful status code (not 200-299) was received.
   * @throws An {@link AssetDoesNotExistError} if the asset does not exist.
   * @throws An {@link DirectoryDoesNotExistError} if the asset is a directory and does not exist.
   * @returns The asset details. Returns `null` if the asset is a root directory.
   */
  override async getAssetDetails<Id extends backend.AssetId>(assetId: Id) {
    const path = remoteBackendPaths.getAssetDetailsPath(assetId)
    const response = await this.get<backend.AssetDetailsResponse<Id>>(path)

    if (!response.ok) {
      if (response.status === STATUS_NOT_FOUND) {
        if (backend.isDirectoryId(assetId)) {
          throw new backend.DirectoryDoesNotExistError()
        }

        throw new backend.AssetDoesNotExistError()
      }

      return await this.throw(response, 'getAssetDetailsBackendError')
    }

    return await response.json()
  }
  /**
   * Return Language Server logs for a project session.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getProjectSessionLogs(
    projectSessionId: backend.ProjectSessionId,
    params: backend.GetProjectSessionLogsRequestParams,
    title: string,
  ): Promise<backend.ProjectSessionLogs> {
    const queryParams = new URLSearchParams(
      params.scrollId != null ? { scrollId: params.scrollId } : {},
    )
    const path = remoteBackendPaths.getProjectSessionLogsPath(projectSessionId)
    const response = await this.get<backend.ProjectSessionLogs>(path, queryParams)
    if (!response.ok) {
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
    body: backend.OpenProjectRequestBody,
    title: string,
  ): Promise<void> {
    const path = remoteBackendPaths.openProjectPath(projectId)
    // `cognitoCredentials` is a legacy field, should be removed when no longer needed by the runtime.
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
      if (!response.ok) {
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
    if (!response.ok) {
      return await this.throw(response, 'updateProjectBackendError', title)
    } else {
      return await response.json()
    }
  }

  /**
   * Begin uploading a large file.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async uploadFileStart(
    body: backend.UploadFileRequestParams,
    file: File,
    abort?: AbortSignal,
  ): Promise<backend.UploadLargeFileMetadata> {
    const path = remoteBackendPaths.UPLOAD_FILE_START_PATH
    const requestBody: backend.UploadFileStartRequestBody = {
      fileName: body.fileName,
      size: file.size,
    }
    const response = await this.post<backend.UploadLargeFileMetadata>(path, requestBody, { abort })
    if (!response.ok) {
      return await this.throw(response, 'uploadFileStartBackendError', body.fileName)
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
    abort?: AbortSignal,
  ): Promise<{ part: backend.S3MultipartPart; size: number }> {
    const start = index * backend.S3_CHUNK_SIZE_BYTES
    const end = Math.min(start + backend.S3_CHUNK_SIZE_BYTES, file.size)
    const body = file.slice(start, end)
    const response = await fetch(url, { method: 'PUT', body, ...(abort ? { signal: abort } : {}) })
    const eTag = response.headers.get('ETag')
    if (!response.ok || eTag == null) {
      return await this.throw(response, 'uploadFileChunkBackendError')
    } else {
      return { part: { eTag, partNumber: index + 1 }, size: body.size }
    }
  }

  /**
   * Finish uploading a large file.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async uploadFileEnd(
    body: backend.UploadFileEndRequestBody,
    abort?: AbortSignal,
  ): Promise<backend.UploadedAsset> {
    const path = remoteBackendPaths.UPLOAD_FILE_END_PATH
    const response = await this.post<backend.UploadedAsset>(path, body, { abort })
    if (!response.ok) {
      return await this.throw(response, 'uploadFileEndBackendError', body.fileName)
    } else {
      const result = await response.json()
      if (result.jobId != null) {
        const statusPath = remoteBackendPaths.getImportArchiveJobStatusPath(result.jobId)
        while (true) {
          const statusResponse = await this.get<unknown>(statusPath)
          const statusResult = statusResponse.ok ? await statusResponse.json() : null
          if (statusResult == null) {
            await delay(IMPORT_STATUS_INTERVAL_MS)
            continue
          }
          return result
        }
      }
      return result
    }
  }

  /**
   * Upload set of Images, resolving any possible conflicts. The sum of file sizes may not
   * exceed cloud message limit.
   */
  override async uploadImage(
    parentDirectoryId: backend.DirectoryId,
    files: { data: Blob; name: string }[],
  ) {
    const path = remoteBackendPaths.UPLOAD_IMAGE_PATH
    const query = new URLSearchParams({ parentDirectoryId })
    const data = new FormData()
    for (const file of files) {
      data.append('image', file.data, file.name)
    }
    const response = await this.postFormData<backend.UploadedImages>(`${path}?${query}`, data)
    if (!response.ok) {
      return this.throw(response, 'uploadImageBackendError')
    } else {
      return response.json()
    }
  }

  /** Change the name of a file. */
  override async updateFile(): Promise<void> {
    await this.throw(null, 'updateFileNotImplementedBackendError')
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
    if (!response.ok) {
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
    if (!response.ok) {
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
    if (!response.ok) {
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
    if (!response.ok) {
      return await this.throw(response, 'createSecretBackendError', body.name)
    } else {
      return await response.json()
    }
  }

  /**
   * Create an OAuth credential.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createCredential(
    body: backend.CreateCredentialRequestBody,
  ): Promise<backend.SecretId> {
    const path = remoteBackendPaths.CREATE_CREDENTIAL_PATH
    const response = await this.post<backend.SecretId>(path, body)
    if (!response.ok) {
      return await this.throw(response, 'createCredentialBackendError', body.name)
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
    if (!response.ok) {
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
    if (!response.ok) {
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
    const response = await this.get<backend.ListSecretsResponseBody>(path)
    if (!response.ok) {
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
    if (!response.ok) {
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
    const response = await this.get<backend.ListTagsResponseBody>(path)
    if (!response.ok) {
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
    const response = await this.patch<backend.ListTagsResponseBody>(path, { labels })
    if (!response.ok) {
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
    if (!response.ok) {
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
    if (!response.ok) {
      return this.throw(response, 'createUserGroupBackendError', body.name)
    } else {
      return await response.json()
    }
  }

  /** Delete a user group. */
  override async deleteUserGroup(userGroupId: backend.UserGroupId, name: string): Promise<void> {
    const path = remoteBackendPaths.deleteUserGroupPath(userGroupId)
    const response = await this.delete(path)
    if (!response.ok) {
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
    } else if (!response.ok) {
      return this.throw(response, 'listUserGroupsBackendError')
    } else {
      return await response.json()
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
    if (!response.ok) {
      return await this.throw(response, 'createCheckoutSessionBackendError', params.price)
    } else {
      return await response.json()
    }
  }

  /**
   * Fetches a configuration for a payment pricing page.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async getPaymentsConfig(): Promise<backend.PaymentsConfig> {
    const response = await this.get<backend.PaymentsConfig>(remoteBackendPaths.PAYMENTS_CONFIG_PATH)
    if (!response.ok) {
      return await this.throw(response, 'getPaymentsConfigBackendError')
    } else {
      return await response.json()
    }
  }

  /**
   * List all personal access tokens for the current user.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async listApiKeys(): Promise<readonly backend.ApiKey[]> {
    const response = await this.get<backend.ListApiKeysResponse>(
      remoteBackendPaths.LIST_API_KEYS_PATH,
    )
    if (!response.ok) {
      return await this.throw(response, 'listApiKeysBackendError')
    } else {
      return (await response.json()).credentials
    }
  }

  /**
   * Create a new personal access token for the current user.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async createApiKey(body: backend.CreateApiKeyRequestBody): Promise<backend.ApiKey> {
    const response = await this.post<backend.ApiKey>(remoteBackendPaths.LIST_API_KEYS_PATH, body)
    if (!response.ok) {
      return await this.throw(response, 'createApiKeyBackendError')
    } else {
      return await response.json()
    }
  }

  /**
   * Delete a personal access token for the current user.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async deleteApiKey(apiKeyId: backend.ApiKeyId) {
    const path = remoteBackendPaths.deleteApiKeyPath(apiKeyId)
    const response = await this.delete(path)
    if (!response.ok) {
      return await this.throw(response, 'deleteApiKeyBackendError')
    } else {
      return
    }
  }

  /** Retrieve Mapbox token for the current user. */
  override async getMapboxToken(): Promise<backend.MapboxToken> {
    const response = await this.get(remoteBackendPaths.GET_MAPBOX_TOKEN_PATH)
    if (!response.ok) {
      return await this.throw(response, 'getMapboxTokenBackendError')
    } else {
      return backend.MAPBOX_TOKEN_SCHEMA.parse(await response.json())
    }
  }

  /**
   * Cancel given subscription.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  override async cancelSubscription(subscriptionId: backend.SubscriptionId): Promise<void> {
    const response = await this.delete(
      remoteBackendPaths.cancelSubscriptionPath(subscriptionId),
      {},
    )
    if (!response.ok) {
      return await this.throw(response, 'cancelSubscriptionBackendError')
    } else {
      return
    }
  }

  /** List events in the organization's audit log. */
  override async getLogEvents(
    params: backend.GetLogEventsRequestParams,
  ): Promise<readonly backend.AuditLogEvent[]> {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly events: backend.AuditLogEvent[]
    }

    const paramsString = new URLSearchParams({
      /* eslint-disable camelcase */
      ...(params.userEmail != null ? { user_email: params.userEmail } : {}),
      ...(params.lambdaKind != null ? { lambda_kind: params.lambdaKind } : {}),
      ...(params.startDate != null ? { start_date: params.startDate } : {}),
      ...(params.endDate != null ? { end_date: params.endDate } : {}),
      ...(params.from != null ? { from: String(params.from) } : {}),
      ...(params.pageSize != null ? { page_size: String(params.pageSize) } : {}),
      /* eslint-enable camelcase */
    }).toString()
    const path = `${remoteBackendPaths.GET_LOG_EVENTS_PATH}?${paramsString}`
    const response = await this.get<ResponseBody>(path)
    if (!response.ok) {
      return this.throw(response, 'getLogEventsBackendError')
    } else {
      const json = await response.json()
      return json.events
    }
  }

  /** Log an event that will be visible in the organization audit log. */
  override async logEvent(message: string, projectId?: string | null, metadata?: object | null) {
    // Prevent events from being logged in dev mode, since we are often using production environment
    // and are polluting real logs.
    if (detect.IS_DEV_MODE) {
      return
    }

    const path = remoteBackendPaths.POST_LOG_EVENT_PATH
    const response = await this.post(
      path,
      {
        message,
        projectId,
        metadata: { timestamp: new Date().toISOString(), ...metadata },
      },
      {
        keepalive: true,
      },
    )
    if (!response.ok) {
      return this.throw(response, 'logEventBackendError', message)
    }
  }

  /** Download an asset. */
  override async download(
    id: backend.AssetId,
    title: string,
    targetDirectoryId: backend.DirectoryId | null,
    shouldUnpackProject = true,
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
          electronOptions: { shouldUnpackProject, path: targetPath },
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
          electronOptions: { path: targetPath },
        })
        break
      }
      case backend.AssetType.datalink: {
        const value = await this.getDatalink(asset.id, title)
        const fileName = `${title}.datalink`
        const fileObjectUrl = URL.createObjectURL(
          new File([JSON.stringify(value)], fileName, {
            type: 'application/json+x-enso-data-link',
          }),
        )
        try {
          await this.downloader({
            url: fileObjectUrl,
            name: fileName,
            electronOptions: { path: targetPath },
          })
        } finally {
          URL.revokeObjectURL(fileObjectUrl)
        }
        break
      }
      case backend.AssetType.secret:
      case backend.AssetType.directory:
      default: {
        throw new Error(`'${asset.type}' assets cannot be downloaded.`)
      }
    }
  }

  /** Download the project to a temporary location. */
  async downloadProject(id: backend.ProjectId) {
    const details = await this.getProjectDetails(id, true)
    if (details.url == null) {
      return this.throw(null, 'getProjectDetailsBackendError')
    }
    const responseBody = await this.downloadCloudProject({
      downloadUrl: details.url,
      projectId: id,
    })
    return {
      projectRootId: backend.DirectoryId(`directory-${responseBody.projectRootDirectory}`),
      parentId: backend.DirectoryId(`directory-${responseBody.parentDirectory}`),
    }
  }

  /** Fetch the URL of the customer portal. */
  override async createCustomerPortalSession() {
    // A dummy query parameter is required due to issues with backend validation.
    const queryString = new URLSearchParams({ ignored: '' }).toString()
    const path = `${remoteBackendPaths.CUSTOMER_PORTAL_SESSION_CREATE_PATH}?${queryString}`
    const response = await this.post<backend.CreateCustomerPortalSessionResponse>(path, null)

    if (!response.ok) {
      return await this.throw(response, 'getCustomerPortalUrlBackendError')
    } else {
      return (await response.json()).url
    }
  }

  /** Resolve asset metadata from an enso path. */
  override async resolveEnsoPath(path: backend.EnsoPath): Promise<backend.AnyAsset> {
    const effectivePath = backend.EnsoPath(path.replace(/%20/g, ' '))
    const response = await this.get<backend.AnyAsset>(remoteBackendPaths.RESOLVE_ENSO_PATH, {
      path: effectivePath,
    })

    if (!response.ok) return this.throw(response, 'resolveEnsoPathBackendError', path)
    const asset = await response.json()
    // `ensoPath` is currently necessary; the response (supposedly) does not include it.
    return this.normalizeAsset({ ...asset, ensoPath: path }, null)
  }

  /**
   * Resolve the data of a project asset relative to the project root directory.
   */
  override async resolveProjectAssetData(
    projectId: backend.ProjectId,
    relativePath: string,
    versionId?: backend.S3ObjectVersionId,
    abort?: AbortSignal,
  ): Promise<Response> {
    const searchParams = new URLSearchParams()
    if (versionId != null) {
      searchParams.set('versionId', versionId)
    }

    const response = await this.get(
      remoteBackendPaths.getProjectAssetPath(projectId, relativePath),
      searchParams,
      abort,
    )
    if (!response.ok) return this.throw(response, 'getFileContentsBackendError')
    return response
  }

  /** Set state of the project running in Hybrid mode as open in progress. */
  async setHybridOpenInProgress(
    id: backend.ProjectId,
    title: string,
  ): Promise<backend.ProjectSessionId> {
    /** The type of the response body of this endpoint. */
    interface ResponseBody {
      readonly projectSessionId: backend.ProjectSessionId
    }

    const path = remoteBackendPaths.getHybridSetOpenInProgressPath(id)
    const response = await this.post<ResponseBody>(path, {})
    if (!response.ok) {
      return await this.throw(response, 'openProjectBackendError', title)
    } else {
      const body = await response.json()
      return body.projectSessionId
    }
  }

  /** Set state of the project running in Hybrid mode as opened. */
  async setHybridOpened(id: backend.ProjectId, title: string): Promise<void> {
    const path = remoteBackendPaths.getHybridSetOpenedPath(id)
    const response = await this.post(path, {})
    if (!response.ok) {
      return await this.throw(response, 'openProjectBackendError', title)
    } else {
      return
    }
  }

  /** Send ping notifying the backend that the project is running. */
  async ping(id: backend.ProjectId): Promise<void> {
    const path = remoteBackendPaths.getHybridProjectPingPath(id)
    await this.post(path, {})
  }

  /** Export multiple files and pack into an archive. */
  override async exportArchive(
    params: backend.ExportArchiveParams,
  ): Promise<backend.ExportedArchive> {
    const { assetIds, filePath } = params
    const path = remoteBackendPaths.EXPORT_ARCHIVE_PATH
    const response = await this.post<{ readonly jobId: backend.ZipAssetsJobId }>(path, { assetIds })
    const { jobId } = await response.json()
    const statusPath = remoteBackendPaths.getExportArchiveJobStatusPath(jobId)
    while (true) {
      const statusResponse = await this.get<{ readonly url: backend.HttpsUrl | null }>(statusPath)
      const url = await statusResponse.json().then(
        (json) => json.url,
        () => null,
      )
      if (url == null) {
        await delay(EXPORT_STATUS_INTERVAL_MS)
        continue
      }
      await this.downloader({
        url,
        name: filePath != null ? getFileName(filePath) : undefined,
        electronOptions: {
          path: filePath != null ? backend.Path(getFolderPath(filePath)) : null,
        },
      })
      return { filePath }
    }
  }

  /** Convert a {@link ListDirectoryResponseBody} to an array of {@link backend.AnyAsset}. */
  private listDirectoryResponseToAssetList(
    assets: readonly backend.AnyAsset[],
    parentId: backend.DirectoryId | null,
  ): readonly backend.AnyAsset[] {
    return assets.map((asset) => this.normalizeAsset(asset, parentId))
  }

  private normalizeAsset<T extends backend.AssetType>(
    asset: backend.AnyAsset<T>,
    parentId: backend.DirectoryId | null,
  ): backend.AnyAsset<T> {
    return objects.merge(asset, {
      type: backend.getAssetTypeFromId(asset.id),
      // `Users` and `Teams` folders are virtual, so their children incorrectly have
      // the organization root id as their parent id.
      parentId: parentId ?? asset.parentId,
    } as Partial<backend.AnyAsset<T>>)
  }
}

markRaw(RemoteBackend.prototype)

/** The schema that checks if the error is a duplicate asset error. */
const DUPLICATE_ASSET_ERROR_SCHEMA = z.object({
  message: z.string().includes('A resource with that title already exists.'),
})

/** Check if the error is a duplicate asset error. */
function isDuplicateAssetError(error: unknown): error is Error {
  return DUPLICATE_ASSET_ERROR_SCHEMA.safeParse(error).success
}
