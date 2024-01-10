/** @file Paths used by the `RemoteBackend`. */
import type * as backend from '#/services/backend'

// =============
// === Paths ===
// =============

/** Relative HTTP path to the "list users" endpoint of the Cloud backend API. */
export const LIST_USERS_PATH = 'users'
/** Relative HTTP path to the "set username" endpoint of the Cloud backend API. */
export const CREATE_USER_PATH = 'users'
/** Relative HTTP path to the "invite user" endpoint of the Cloud backend API. */
export const INVITE_USER_PATH = 'users/invite'
/** Relative HTTP path to the "create permission" endpoint of the Cloud backend API. */
export const CREATE_PERMISSION_PATH = 'permissions'
/** Relative HTTP path to the "get user" endpoint of the Cloud backend API. */
export const USERS_ME_PATH = 'users/me'
/** Relative HTTP path to the "list directory" endpoint of the Cloud backend API. */
export const LIST_DIRECTORY_PATH = 'directories'
/** Relative HTTP path to the "create directory" endpoint of the Cloud backend API. */
export const CREATE_DIRECTORY_PATH = 'directories'
/** Relative HTTP path to the "undo delete asset" endpoint of the Cloud backend API. */
export const UNDO_DELETE_ASSET_PATH = 'assets'
/** Relative HTTP path to the "list projects" endpoint of the Cloud backend API. */
export const LIST_PROJECTS_PATH = 'projects'
/** Relative HTTP path to the "create project" endpoint of the Cloud backend API. */
export const CREATE_PROJECT_PATH = 'projects'
/** Relative HTTP path to the "list files" endpoint of the Cloud backend API. */
export const LIST_FILES_PATH = 'files'
/** Relative HTTP path to the "upload file" endpoint of the Cloud backend API. */
export const UPLOAD_FILE_PATH = 'files'
/** Relative HTTP path to the "create secret" endpoint of the Cloud backend API. */
export const CREATE_SECRET_PATH = 'secrets'
/** Relative HTTP path to the "list secrets" endpoint of the Cloud backend API. */
export const LIST_SECRETS_PATH = 'secrets'
/** Relative HTTP path to the "create tag" endpoint of the Cloud backend API. */
export const CREATE_TAG_PATH = 'tags'
/** Relative HTTP path to the "list tags" endpoint of the Cloud backend API. */
export const LIST_TAGS_PATH = 'tags'
/** Relative HTTP path to the "list versions" endpoint of the Cloud backend API. */
export const LIST_VERSIONS_PATH = 'versions'
/** Relative HTTP path to the "update asset" endpoint of the Cloud backend API. */
export function updateAssetPath(assetId: backend.AssetId) {
    return `assets/${assetId}`
}
/** Relative HTTP path to the "delete asset" endpoint of the Cloud backend API. */
export function deleteAssetPath(assetId: backend.AssetId) {
    return `assets/${assetId}`
}
/** Relative HTTP path to the "copy asset" endpoint of the Cloud backend API. */
export function copyAssetPath(assetId: backend.AssetId) {
    return `assets/${assetId}/copy`
}
/** Relative HTTP path to the "update directory" endpoint of the Cloud backend API. */
export function updateDirectoryPath(directoryId: backend.DirectoryId) {
    return `directories/${directoryId}`
}
/** Relative HTTP path to the "close project" endpoint of the Cloud backend API. */
export function closeProjectPath(projectId: backend.ProjectId) {
    return `projects/${projectId}/close`
}
/** Relative HTTP path to the "get project details" endpoint of the Cloud backend API. */
export function getProjectDetailsPath(projectId: backend.ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "open project" endpoint of the Cloud backend API. */
export function openProjectPath(projectId: backend.ProjectId) {
    return `projects/${projectId}/open`
}
/** Relative HTTP path to the "project update" endpoint of the Cloud backend API. */
export function projectUpdatePath(projectId: backend.ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "check resources" endpoint of the Cloud backend API. */
export function checkResourcesPath(projectId: backend.ProjectId) {
    return `projects/${projectId}/resources`
}
/** Relative HTTP path to the "update secret" endpoint of the Cloud backend API. */
export function updateSecretPath(secretId: backend.SecretId) {
    return `s3cr3tz/${secretId}`
}
/** Relative HTTP path to the "get secret" endpoint of the Cloud backend API. */
export function getSecretPath(secretId: backend.SecretId) {
    return `secrets/${secretId}`
}
/** Relative HTTP path to the "associate tag" endpoint of the Cloud backend API. */
export function associateTagPath(assetId: backend.AssetId) {
    return `assets/${assetId}/labels`
}
/** Relative HTTP path to the "delete tag" endpoint of the Cloud backend API. */
export function deleteTagPath(tagId: backend.TagId) {
    return `tags/${tagId}`
}
