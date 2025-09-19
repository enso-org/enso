import * as newtype from '../../utilities/data/newtype.js'
import type { IdType } from '../Backend.js'

/** A KSUID.*/
export type KSUID = newtype.Newtype<string, 'KSUID'>
export const KSUID = newtype.newtypeConstructor<KSUID>()

/** Unique identifier for an organization. */
export type OrganizationId = newtype.Newtype<`organization-${string}`, 'OrganizationId'>
export const OrganizationId = newtype.newtypeConstructor<OrganizationId>()
/** Whether a given {@link string} is an {@link OrganizationId}. */
export function isOrganizationId(id: unknown): id is OrganizationId {
  return typeof id === 'string' && id.startsWith('organization-')
}

/** Unique identifier for a user in an organization. */
export type UserId = newtype.Newtype<string, 'UserId'>
export const UserId = newtype.newtypeConstructor<UserId>()
/** Whether a given {@link string} is an {@link UserId}. */
export function isUserId(id: unknown): id is UserId {
  return typeof id === 'string' && id.startsWith('user-')
}

/** Unique identifier for a user group. */
export type UserGroupId = newtype.Newtype<`usergroup-${string}`, 'UserGroupId'>
export const UserGroupId = newtype.newtypeConstructor<UserGroupId>()
/** Whether a given {@link string} is an {@link UserGroupId}. */
export function isUserGroupId(id: unknown): id is UserGroupId {
  return typeof id === 'string' && id.startsWith('usergroup-')
}

/** Unique identifier for a directory. */
export type DirectoryId = newtype.Newtype<`directory-${string}`, 'DirectoryId'>
export const DirectoryId = newtype.newtypeConstructor<DirectoryId>()
/** Whether a given {@link unknown} is an {@link DirectoryId}. */
export function isDirectoryId(id: unknown): id is DirectoryId {
  return typeof id === 'string' && id.startsWith('directory-')
}

/**
 * Unique identifier for an asset representing the items inside a directory for which the
 * request to retrive the items has not yet completed.
 */
export type LoadingAssetId = newtype.Newtype<string, 'LoadingAssetId'>
export const LoadingAssetId = newtype.newtypeConstructor<LoadingAssetId>()

/** Unique identifier for an asset representing the nonexistent children of an empty directory. */
export type EmptyAssetId = newtype.Newtype<string, 'EmptyAssetId'>
export const EmptyAssetId = newtype.newtypeConstructor<EmptyAssetId>()

/** Unique identifier for an asset representing the parent directory. */
export type UpAssetId = newtype.Newtype<string, 'UpAssetId'>
export const UpAssetId = newtype.newtypeConstructor<UpAssetId>()
/**
 * Unique identifier for an asset representing the nonexistent children of a directory
 * that failed to fetch.
 */
export type ErrorAssetId = newtype.Newtype<string, 'ErrorAssetId'>
export const ErrorAssetId = newtype.newtypeConstructor<ErrorAssetId>()

/** Unique identifier for a user's project. */
export type ProjectId = newtype.Newtype<string, 'ProjectId'>
export const ProjectId = newtype.newtypeConstructor<ProjectId>()
/** Whether a given {@link unknown} is an {@link ProjectId}. */
export function isProjectId(id: unknown): id is ProjectId {
  return typeof id === 'string' && id.startsWith('project-')
}

/** Unique identifier for an uploaded file. */
export type FileId = newtype.Newtype<string, 'FileId'>
export const FileId = newtype.newtypeConstructor<FileId>()

/** Unique identifier for a secret environment variable. */
export type SecretId = newtype.Newtype<string, 'SecretId'>
export const SecretId = newtype.newtypeConstructor<SecretId>()

/** Unique identifier for a project session. */
export type ProjectSessionId = newtype.Newtype<string, 'ProjectSessionId'>
export const ProjectSessionId = newtype.newtypeConstructor<ProjectSessionId>()

/** Unique identifier for a project execution. */
export type ProjectExecutionId = newtype.Newtype<string, 'ProjectExecutionId'>
export const ProjectExecutionId = newtype.newtypeConstructor<ProjectExecutionId>()

/** Unique identifier for a Datalink. */
export type DatalinkId = newtype.Newtype<string, 'DatalinkId'>
export const DatalinkId = newtype.newtypeConstructor<DatalinkId>()

/** Unique identifier for a version of an S3 object. */
export type S3ObjectVersionId = newtype.Newtype<string, 'S3ObjectVersionId'>
export const S3ObjectVersionId = newtype.newtypeConstructor<S3ObjectVersionId>()

/** Unique identifier for an arbitrary asset. */
export type AssetId = IdType[keyof IdType]
export const AssetId = newtype.newtypeConstructor<AssetId>()

/** Unique identifier for metadata. */
export type MetadataId = newtype.Newtype<`metadata-${KSUID}`, 'MetadataId'>
export const MetadataId = newtype.newtypeConstructor<MetadataId>()

/** Unique identifier for a subscription. */
export type SubscriptionId = newtype.Newtype<string, 'SubscriptionId'>
export const SubscriptionId = newtype.newtypeConstructor<SubscriptionId>()

/** Unique identifier for a task to archive some assets to a `.zip`. */
export type ZipAssetsJobId = newtype.Newtype<string, 'ZipAssetsJobId'>
export const ZipAssetsJobId = newtype.newtypeConstructor<ZipAssetsJobId>()

/** Unique identifier for a task to archive some assets to a `.zip`. */
export type UnzipAssetsJobId = newtype.Newtype<string, 'UnzipAssetsJobId'>
export const UnzipAssetsJobId = newtype.newtypeConstructor<UnzipAssetsJobId>()

/** The name of an asset label. */
export type LabelName = newtype.Newtype<string, 'LabelName'>
export const LabelName = newtype.newtypeConstructor<LabelName>()

/** Unique identifier for a label. */
export type TagId = newtype.Newtype<string, 'TagId'>
export const TagId = newtype.newtypeConstructor<TagId>()

/** A URL. */
export type Address = newtype.Newtype<string, 'Address'>
export const Address = newtype.newtypeConstructor<Address>()

/** A HTTPS URL. */
export type HttpsUrl = newtype.Newtype<string, 'HttpsUrl'>
export const HttpsUrl = newtype.newtypeConstructor<HttpsUrl>()

/** An email address. */
export type EmailAddress = newtype.Newtype<string, 'EmailAddress'>
export const EmailAddress = newtype.newtypeConstructor<EmailAddress>()

/** An AWS S3 file path. */
export type S3FilePath = newtype.Newtype<string, 'S3FilePath'>
export const S3FilePath = newtype.newtypeConstructor<S3FilePath>()

/** An AWS machine configuration. */
export type Ami = newtype.Newtype<string, 'Ami'>
export const Ami = newtype.newtypeConstructor<Ami>()

/** An identifier for an entity with an {@link AssetPermission} for an {@link Asset}. */
export type UserPermissionIdentifier = UserGroupId | UserId

/** An filesystem path. Only present on the local backend. */
export type Path = newtype.Newtype<string, 'Path'>
export const Path = newtype.newtypeConstructor<Path>()

/** A project UUID. Only present on the local backend. */
export type UUID = newtype.Newtype<string, 'UUID'>
export const UUID = newtype.newtypeConstructor<UUID>()

/** The path of ids to this asset. */
export type ParentsPath = newtype.Newtype<string, 'ParentsPath'>
export const ParentsPath = newtype.newtypeConstructor<ParentsPath>()

/** The path of directory names to this asset, excluding the root directory. */
export type VirtualParentsPath = newtype.Newtype<string, 'VirtualParentsPath'>
export const VirtualParentsPath = newtype.newtypeConstructor<VirtualParentsPath>()

/** The path of this asset, including the root directory. */
export type EnsoPath = newtype.Newtype<string, 'EnsoPath'>
export const EnsoPath = newtype.newtypeConstructor<EnsoPath>()

/** The path string of this asset, including the root directory. */
export type EnsoPathValue = newtype.Newtype<string, 'EnsoPathValue'>
export const EnsoPathValue = newtype.newtypeConstructor<EnsoPathValue>()

/** A pagination token for an arbitrary endpoint. */
export type PaginationToken = newtype.Newtype<string, 'PaginationToken'>
export const PaginationToken = newtype.newtypeConstructor<PaginationToken>()

/** User settings for a Snowflake credential. */
export interface SnowflakeCredentialInput {
  readonly type: 'Snowflake'
  readonly account: string
  readonly clientId: string
  readonly clientSecret: string
  readonly role: string | null
}

/** User settings for a Google credential. */
export interface GoogleCredentialInput {
  readonly type: 'Google'
  readonly scopes: readonly string[]
}

/** User settings for a Strava credential. */
export interface StravaCredentialInput {
  readonly type: 'Strava'
  readonly scopes: readonly string[]
}

/** User settings for an arbitrary credential. */
export type CredentialInput =
  | SnowflakeCredentialInput
  | GoogleCredentialInput
  | StravaCredentialInput
