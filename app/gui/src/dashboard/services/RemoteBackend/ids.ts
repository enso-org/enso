/**
 * @file ID encoding and decoding that is specific to cloud backend.
 */

import type * as backend from '#/services/Backend'
import { DirectoryId, UserGroupId, UserId } from '#/services/Backend'
import * as remoteBackendPaths from '#/services/remoteBackendPaths'

/** Whether the given directory is a special directory that cannot be written to. */
export function isSpecialReadonlyDirectoryId(id: backend.AssetId) {
  return (
    id === remoteBackendPaths.USERS_DIRECTORY_ID || id === remoteBackendPaths.TEAMS_DIRECTORY_ID
  )
}

/**
 * Extract the ID from the given user group ID.
 * Removes the `usergroup-` prefix.
 * @param id - The user group ID.
 * @returns The ID.
 */
export function extractIdFromUserGroupId(id: backend.UserGroupId) {
  return id.replace(/^usergroup-/, '')
}

/**
 * Extract the ID from the given organization ID.
 * Removes the `organization-` prefix.
 */
export function extractIdFromOrganizationId(id: backend.OrganizationId) {
  return id.replace(/^organization-/, '')
}

/**
 * Extract the ID from the given directory ID.
 * Removes the `directory-` prefix.
 */
export function extractIdFromDirectoryId(id: backend.DirectoryId) {
  return id.replace(/^directory-/, '')
}

/**
 * Extract the ID from the given user ID.
 * Removes the `user-` prefix.
 */
export function extractIdFromUserId(id: backend.UserId) {
  return id.replace(/^user-/, '')
}

/** Convert a user group ID to a directory ID. */
export function userGroupIdToDirectoryId(id: backend.UserGroupId): backend.DirectoryId {
  return DirectoryId(`directory-${extractIdFromUserGroupId(id)}`)
}

/** Convert a user ID to a directory ID. */
export function userIdToDirectoryId(id: backend.UserId): backend.DirectoryId {
  return DirectoryId(`directory-${extractIdFromUserId(id)}`)
}

/**
 * Convert a directory ID to a user ID.
 * @param id - The directory ID.
 * @returns The user ID.
 */
export function directoryIdToUserId(id: backend.DirectoryId): backend.UserId {
  return UserId(`user-${extractIdFromDirectoryId(id)}`)
}

/** Convert organization ID to a directory ID. */
export function organizationIdToDirectoryId(id: backend.OrganizationId): backend.DirectoryId {
  return DirectoryId(`directory-${extractIdFromOrganizationId(id)}`)
}

/**
 * Convert a directory ID to a user group ID.
 * @param id - The directory ID.
 * @returns The user group ID.
 */
export function directoryIdToUserGroupId(id: backend.DirectoryId): backend.UserGroupId {
  return UserGroupId(`usergroup-${extractIdFromDirectoryId(id)}`)
}

/**
 * Whether the given string is a valid organization ID.
 * @param id - The string to check.
 * @returns Whether the string is a valid organization ID.
 */
export function isOrganizationId(id: string): id is backend.OrganizationId {
  return id.startsWith('organization-')
}

/**
 * Whether the given string is a valid user ID.
 * @param id - The string to check.
 * @returns Whether the string is a valid user ID.
 */
export function isUserId(id: string): id is backend.UserId {
  return id.startsWith('user-')
}

/**
 * Whether the given string is a valid user group ID.
 * @param id - The string to check.
 * @returns Whether the string is a valid user group ID.
 */
export function idIsUserGroupId(id: string): id is backend.UserGroupId {
  return id.startsWith('usergroup-')
}
