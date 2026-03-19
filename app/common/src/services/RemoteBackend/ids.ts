/** @file ID encoding and decoding that is specific to cloud backend. */
import { DirectoryId, UserGroupId, UserId, type OrganizationId } from '../Backend.js'

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

/** Convert organization ID to a directory ID. */
export function organizationIdToDirectoryId(id: OrganizationId): DirectoryId {
  return DirectoryId(`directory-${extractIdFromOrganizationId(id)}`)
}
