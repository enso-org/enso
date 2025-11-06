/** @file ID encoding and decoding that is specific to cloud backend. */
import { DirectoryId, UserGroupId, UserId, type AssetId, type OrganizationId } from '../Backend.js'
import { TEAMS_DIRECTORY_ID, USERS_DIRECTORY_ID } from '../Backend/remoteBackendPaths.js'

/** Whether the given directory is a special directory that cannot be written to. */
export function isSpecialReadonlyDirectoryId(id: AssetId) {
  return id === USERS_DIRECTORY_ID || id === TEAMS_DIRECTORY_ID
}

/**
 * Extract the ID from the given user group ID.
 * Removes the `usergroup-` prefix.
 * @param id - The user group ID.
 * @returns The ID.
 */
export function extractIdFromUserGroupId(id: UserGroupId) {
  return id.replace(/^usergroup-/, '')
}

/**
 * Extract the ID from the given organization ID.
 * Removes the `organization-` prefix.
 */
export function extractIdFromOrganizationId(id: OrganizationId) {
  return id.replace(/^organization-/, '')
}

/**
 * Extract the ID from the given directory ID.
 * Removes the `directory-` prefix.
 */
export function extractIdFromDirectoryId(id: DirectoryId) {
  return id.replace(/^directory-/, '')
}

/**
 * Extract the ID from the given user ID.
 * Removes the `user-` prefix.
 */
export function extractIdFromUserId(id: UserId) {
  return id.replace(/^user-/, '')
}

/** Convert a user group ID to a directory ID. */
export function userGroupIdToDirectoryId(id: UserGroupId): DirectoryId {
  return DirectoryId(`directory-${extractIdFromUserGroupId(id)}`)
}

/** Convert a user ID to a directory ID. */
export function userIdToDirectoryId(id: UserId): DirectoryId {
  return DirectoryId(`directory-${extractIdFromUserId(id)}`)
}

/**
 * Convert a directory ID to a user ID.
 * @param id - The directory ID.
 * @returns The user ID.
 */
export function directoryIdToUserId(id: DirectoryId): UserId {
  return UserId(`user-${extractIdFromDirectoryId(id)}`)
}

/** Convert organization ID to a directory ID. */
export function organizationIdToDirectoryId(id: OrganizationId): DirectoryId {
  return DirectoryId(`directory-${extractIdFromOrganizationId(id)}`)
}

/**
 * Convert a directory ID to a user group ID.
 * @param id - The directory ID.
 * @returns The user group ID.
 */
export function directoryIdToUserGroupId(id: DirectoryId): UserGroupId {
  return UserGroupId(`usergroup-${extractIdFromDirectoryId(id)}`)
}

/**
 * Whether the given string is a valid organization ID.
 * @param id - The string to check.
 * @returns Whether the string is a valid organization ID.
 */
export function isOrganizationId(id: string): id is OrganizationId {
  return id.startsWith('organization-')
}

/**
 * Whether the given string is a valid user ID.
 * @param id - The string to check.
 * @returns Whether the string is a valid user ID.
 */
export function isUserId(id: string): id is UserId {
  return id.startsWith('user-')
}

/**
 * Whether the given string is a valid user group ID.
 * @param id - The string to check.
 * @returns Whether the string is a valid user group ID.
 */
export function idIsUserGroupId(id: string): id is UserGroupId {
  return id.startsWith('usergroup-')
}
