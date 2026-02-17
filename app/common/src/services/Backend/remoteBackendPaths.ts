/** @file Paths used by the `RemoteBackend`. */
import {
  ApiKeyId,
  DirectoryId,
  HttpsUrl,
  type AssetId,
  type CredentialInput,
  type DatalinkId,
  type FileId,
  type ProjectExecutionId,
  type ProjectId,
  type ProjectSessionId,
  type SecretId,
  type SubscriptionId,
  type TagId,
  type UnzipAssetsJobId,
  type UserGroupId,
  type UserId,
  type ZipAssetsJobId,
} from './types.js'

/** Relative HTTP path to the "list users" endpoint of the Cloud backend API. */
export const LIST_USERS_PATH = 'users'
/** Relative HTTP path to the "create user" endpoint of the Cloud backend API. */
export const CREATE_USER_PATH = 'users'
/** Relative HTTP path to the "get user" endpoint of the Cloud backend API. */
export const USERS_ME_PATH = 'users/me'
/** Relative HTTP path to the "update current user" endpoint of the Cloud backend API. */
export const UPDATE_CURRENT_USER_PATH = 'users/me'
/** Relative HTTP path to the "delete user" endpoint of the Cloud backend API. */
export const DELETE_USER_PATH = 'users/me'
/** Relative HTTP path to the "upload user picture" endpoint of the Cloud backend API. */
export const UPLOAD_USER_PICTURE_PATH = 'users/me/picture'
/** Relative HTTP path to the "get organization" endpoint of the Cloud backend API. */
export const GET_ORGANIZATION_PATH = 'organizations/me'
/** Relative HTTP path to the "update organization" endpoint of the Cloud backend API. */
export const UPDATE_ORGANIZATION_PATH = 'organizations/me'
/** Relative HTTP path to the "delete organization" endpoint of the Cloud backend API. */
export const DELETE_ORGANIZATION_PATH = 'organizations/me'
/** Relative HTTP path to the "upload organization picture" endpoint of the Cloud backend API. */
export const UPLOAD_ORGANIZATION_PICTURE_PATH = 'organizations/me/picture'
/** Relative HTTP path to the "invite user" endpoint of the Cloud backend API. */
export const INVITE_USER_PATH = 'users/invite'
/** Relative HTTP path to the "list invitations" endpoint of the Cloud backend API. */
export const INVITATION_PATH = 'invitations'
/** Relative HTTP path to the "accept invitation" endpoint of the Cloud backend API. */
export const ACCEPT_INVITATION_PATH = 'invitations/accept'
/** Relative HTTP path to the "delete invitation" endpoint of the Cloud backend API. */
export const DECLINE_INVITATION_PATH = 'invitations'
/** Relative HTTP path to the "create permission" endpoint of the Cloud backend API. */
export const CREATE_PERMISSION_PATH = 'permissions'
/** Relative HTTP path to the "list directory" endpoint of the Cloud backend API. */
export const LIST_DIRECTORY_PATH = 'directories'
/** Relative HTTP path to the "search directory" endpoint of the Cloud backend API. */
export const SEARCH_DIRECTORY_PATH = 'directories/search'
/** Relative HTTP path to the "create directory" endpoint of the Cloud backend API. */
export const CREATE_DIRECTORY_PATH = 'directories'
/** Relative HTTP path to the "undo delete asset" endpoint of the Cloud backend API. */
export const UNDO_DELETE_ASSET_PATH = 'assets'
/** Relative HTTP path to the "create project" endpoint of the Cloud backend API. */
export const CREATE_PROJECT_PATH = 'projects'
/** Relative HTTP path to the "upload file start" endpoint of the Cloud backend API. */
export const UPLOAD_FILE_START_PATH = 'files/upload/start'
/** Relative HTTP path to the "upload file end" endpoint of the Cloud backend API. */
export const UPLOAD_FILE_END_PATH = 'files/upload/end'
/** Relative HTTP path to the "upload image" endpoint of the Cloud backend API */
export const UPLOAD_IMAGE_PATH = 'images'
/** Relative HTTP path to the "create secret" endpoint of the Cloud backend API. */
export const CREATE_SECRET_PATH = 'secrets'
/** Relative HTTP path to the "list secrets" endpoint of the Cloud backend API. */
export const LIST_SECRETS_PATH = 'secrets'
/** Relative HTTP path to the "create credential" endpoint of the Cloud backend API. */
export const CREATE_CREDENTIAL_PATH = 'secrets'
/** Relative HTTP path to the "list project sessions" endpoint of the Cloud backend API. */
export const LIST_PROJECT_SESSIONS_PATH = 'project-sessions'
/** Relative HTTP path to the "create datalink" endpoint of the Cloud backend API. */
export const CREATE_DATALINK_PATH = 'datalinks'
/** Relative HTTP path to the "create tag" endpoint of the Cloud backend API. */
export const CREATE_TAG_PATH = 'tags'
/** Relative HTTP path to the "list tags" endpoint of the Cloud backend API. */
export const LIST_TAGS_PATH = 'tags'
/** Relative HTTP path to the "create user group" endpoint of the Cloud backend API. */
export const CREATE_USER_GROUP_PATH = 'usergroups'
/** Relative HTTP path to the "list user groups" endpoint of the Cloud backend API. */
export const LIST_USER_GROUPS_PATH = 'usergroups'
/** Relative HTTP path to the "create checkout session" endpoint of the Cloud backend API. */
export const CREATE_CHECKOUT_SESSION_PATH = 'payments/checkout/sessions'
/** Relative HTTP path to the "get log events" endpoint of the Cloud backend API. */
export const GET_LOG_EVENTS_PATH = 'log_events'
/** Relative HTTP path to the "post log event" endpoint of the Cloud backend API. */
export const POST_LOG_EVENT_PATH = 'logs'
/** Relative HTTP path to the "get payments config" endpoint of the Cloud backend API. */
export const PAYMENTS_CONFIG_PATH = 'payments/config'
/** Relative HTTP path to the "resolve an enso URL path" endpoint of the Cloud backend API. */
export const RESOLVE_ENSO_PATH = 'path/resolve'
/** Relative HTTP path to the "get customer portal session" endpoint of the Cloud backend API. */
export const CUSTOMER_PORTAL_SESSION_CREATE_PATH = 'payments/customer-portal-sessions/create'
/** Relative HTTP path to the "API keys" endpoint of the Cloud backend API. */
export const LIST_API_KEYS_PATH = 'credentials'
/** Relative HTTP path to the "create API key" endpoint of the Cloud backend API. */
export const CREATE_API_KEY_PATH = 'credentials'
/** Relative HTTP path to the "delete API key" endpoint of the Cloud backend API. */
export function deleteApiKeyPath(apiKeyId: ApiKeyId) {
  return `credentials/${apiKeyId}`
}
export const GET_MAPBOX_TOKEN_PATH = 'mapbox/token'

/** Relative HTTP path to the "cancel subscription" endpoint of the Cloud backend API. */
export function cancelSubscriptionPath(subscriptionId: SubscriptionId) {
  return `payments/subscriptions/${subscriptionId}`
}

/** Relative HTTP path to the "delete user" endpoint of the Cloud backend API. */
export function removeUserPath(userId: UserId) {
  return `users/${userId}`
}
/** Relative HTTP path to the "change user groups" endpoint of the Cloud backend API. */
export function changeUserGroupPath(userId: UserId) {
  return HttpsUrl(`users/${userId}/usergroups`)
}
/** Relative HTTP path to the "list asset versions" endpoint of the Cloud backend API. */
export function listAssetVersionsPath(assetId: AssetId) {
  return HttpsUrl(`assets/${assetId}/versions`)
}
/** Relative HTTP path to the "get project asset" endpoint of the Cloud backend API. */
export function getProjectAssetPath(projectId: ProjectId, relativePath: string) {
  return `projects/${projectId}/files/${relativePath}`.replace('/./', '/').replace(/\/$/, '')
}

/**
 * Relative HTTP path to the "get asset details" endpoint of the Cloud backend API.
 */
export function getAssetDetailsPath(assetId: AssetId) {
  return HttpsUrl(`assets/${assetId}` as const)
}
/** Relative HTTP path to the upload project endpoint of the Cloud backend API. */
export function getProjectUploadPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/upload`)
}

/** Relative HTTP path to the "update asset" endpoint of the Cloud backend API. */
export function updateAssetPath(assetId: AssetId) {
  return HttpsUrl(`assets/${assetId}`)
}
/** Relative HTTP path to the "delete asset" endpoint of the Cloud backend API. */
export function deleteAssetPath(assetId: AssetId) {
  return HttpsUrl(`assets/${assetId}`)
}
/** Relative HTTP path to the "copy asset" endpoint of the Cloud backend API. */
export function copyAssetPath(assetId: AssetId) {
  return HttpsUrl(`assets/${assetId}/copy`)
}
/** Relative HTTP path to the "update directory" endpoint of the Cloud backend API. */
export function updateDirectoryPath(directoryId: DirectoryId) {
  return HttpsUrl(`directories/${directoryId}`)
}
/** Relative HTTP path to the "close project" endpoint of the Cloud backend API. */
export function closeProjectPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/close`)
}
/** Relative HTTP path to the "get project details" endpoint of the Cloud backend API. */
export function getProjectDetailsPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}`)
}
export const GET_PROJECT_DETAILS_REGEX = /^[/]projects[/](?<projectId>[^/]+)$/
/** Relative HTTP path to the "download project" endpoint of the Local backend API. */
export function downloadProjectPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/download`)
}
export const DOWNLOAD_PROJECT_REGEX = /^[/]projects[/](?<projectId>[^/]+)[/]download$/
/** Relative HTTP path to the "get project logs" endpoint of the Cloud backend API. */
export function getProjectSessionLogsPath(projectSessionId: ProjectSessionId) {
  return HttpsUrl(`project-sessions/${projectSessionId}/logs`)
}
/** Relative HTTP path to the "duplicate project" endpoint of the Cloud backend API. */
export function duplicateProjectPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/versions/clone`)
}
/** Relative HTTP path to the "restore asset" endpoint of the Cloud backend API. */
export function restoreAssetPath(assetId: AssetId) {
  return HttpsUrl(`assets/${assetId}/versions/restore`)
}
/** Relative HTTP path to the "open project" endpoint of the Cloud backend API. */
export function openProjectPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/open`)
}
/** Relative HTTP path to the "project update" endpoint of the Cloud backend API. */
export function projectUpdatePath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}`)
}
/** Relative HTTP path to the "list project executions" endpoint of the Cloud backend API. */
export function listProjectExecutionsPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/executions`)
}
/** Relative HTTP path to the "create project execution" endpoint of the Cloud backend API. */
export function createProjectExecutionPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/executions/new`)
}
/** Relative HTTP path to the "get project execution details" endpoint of the Cloud backend API. */
export function getProjectExecutionDetailsPath(executionId: ProjectExecutionId) {
  return HttpsUrl(`executions/${executionId}`)
}
/** Relative HTTP path to the "update project execution" endpoint of the Cloud backend API. */
export function updateProjectExecutionPath(executionId: ProjectExecutionId) {
  return HttpsUrl(`executions/${executionId}`)
}
/** Relative HTTP path to the "sync project execution" endpoint of the Cloud backend API. */
export function syncProjectExecutionPath(executionId: ProjectExecutionId) {
  return HttpsUrl(`executions/${executionId}/sync`)
}
/** Relative HTTP path to the "delete project execution" endpoint of the Cloud backend API. */
export function deleteProjectExecutionPath(executionId: ProjectExecutionId) {
  return HttpsUrl(`executions/${executionId}`)
}
/** Relative HTTP path to the "get file details" endpoint of the backend API. */
export function getFileDetailsPath(fileId: FileId) {
  return HttpsUrl(`files/${fileId}`)
}
export const GET_FILE_DETAILS_REGEX = /^[/]files[/](?<fileId>[^/]+)$/
/** Relative HTTP path to the "download file" endpoint of the Local backend API. */
export function downloadFilePath(fileId: FileId) {
  return HttpsUrl(`files/${fileId}/download`)
}
export const DOWNLOAD_FILE_REGEX = /^[/]files[/](?<fileId>[^/]+)[/]download$/
/** Relative HTTP path to the "update secret" endpoint of the Cloud backend API. */
export function updateSecretPath(secretId: SecretId) {
  return HttpsUrl(`secrets/${secretId}`)
}
/** Relative HTTP path to the "get secret" endpoint of the Cloud backend API. */
export function getSecretPath(secretId: SecretId) {
  return HttpsUrl(`secrets/${secretId}`)
}
/** Relative HTTP path to the "get datalink" endpoint of the Cloud backend API. */
export function getDatalinkPath(datalinkId: DatalinkId) {
  return HttpsUrl(`datalinks/${datalinkId}`)
}
/** Relative HTTP path to the "associate tag" endpoint of the Cloud backend API. */
export function associateTagPath(assetId: AssetId) {
  return HttpsUrl(`assets/${assetId}/labels`)
}
/** Relative HTTP path to the "delete tag" endpoint of the Cloud backend API. */
export function deleteTagPath(tagId: TagId) {
  return HttpsUrl(`tags/${tagId}`)
}
/** Relative HTTP path to the "delete user group" endpoint of the Cloud backend API. */
export function deleteUserGroupPath(groupId: UserGroupId) {
  return HttpsUrl(`usergroups/${groupId}`)
}
/** Relative HTTP path to the "get oauth callback" endpoint of the Cloud backend API. */
export function getOauthCallbackPath(service: CredentialInput['type']) {
  const normalized = service.toLowerCase()
  return HttpsUrl(`oauth/${normalized}/callback`)
}

/** Relative HTTP path to the "hybrid set open in progress" endpoint of the Cloud backend API. */
export function getHybridSetOpenInProgressPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/hybrid_set_open_in_progress`)
}
/** Relative HTTP path to the "hybrid set opened" endpoint of the Cloud backend API. */
export function getHybridSetOpenedPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/hybrid_set_opened`)
}
/** Relative HTTP path to the "hybrid ping" endpoint of the Cloud backend API. */
export function getHybridProjectPingPath(projectId: ProjectId) {
  return HttpsUrl(`projects/${projectId}/hybrid_ping`)
}

export const EXPORT_ARCHIVE_PATH = 'assets/zip'
/** Relative HTTP path to the "export archive job status" endpoint of the Cloud backend API. */
export function getExportArchiveJobStatusPath(jobId: ZipAssetsJobId) {
  return HttpsUrl(`assets/zip/${jobId}`)
}
/** Relative HTTP path to the "import archive job status" endpoint of the Cloud backend API. */
export function getImportArchiveJobStatusPath(jobId: UnzipAssetsJobId) {
  return HttpsUrl(`assets/unzip/${jobId}`)
}

/** The ID of the directory containing the home directories of all users. */
export const USERS_DIRECTORY_ID = DirectoryId('directory-0000000000000000000000users')
/** The ID of the directory containing home directories of all teams. */
export const TEAMS_DIRECTORY_ID = DirectoryId('directory-0000000000000000000000teams')
