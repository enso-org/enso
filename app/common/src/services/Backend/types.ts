import { newtypeConstructor, type Newtype } from '../../utilities/data/newtype.js'
import type { IdType } from '../Backend.js'
export { EnsoPath, EnsoPathValue } from './ensoPath.js'

/** A KSUID. */
export type KSUID = Newtype<string, 'KSUID'>
export const KSUID = newtypeConstructor<KSUID>()

/** Unique identifier for an organization. */
export type OrganizationId = Newtype<`organization-${string}`, 'OrganizationId'>
export const OrganizationId = newtypeConstructor<OrganizationId>()
/** Whether a given {@link string} is an {@link OrganizationId}. */
export function isOrganizationId(id: unknown): id is OrganizationId {
  return typeof id === 'string' && id.startsWith('organization-')
}

/** Unique identifier for a user in an organization. */
export type UserId = Newtype<string, 'UserId'>
export const UserId = newtypeConstructor<UserId>()
/** Whether a given {@link string} is an {@link UserId}. */
export function isUserId(id: unknown): id is UserId {
  return typeof id === 'string' && id.startsWith('user-')
}

/** Unique identifier for a user group. */
export type UserGroupId = Newtype<`usergroup-${string}`, 'UserGroupId'>
export const UserGroupId = newtypeConstructor<UserGroupId>()
/** Whether a given {@link string} is an {@link UserGroupId}. */
export function isUserGroupId(id: unknown): id is UserGroupId {
  return typeof id === 'string' && id.startsWith('usergroup-')
}

/** Unique identifier for a directory. */
export type DirectoryId = Newtype<`directory-${string}`, 'DirectoryId'>
export const DirectoryId = newtypeConstructor<DirectoryId>()
/** Whether a given {@link unknown} is an {@link DirectoryId}. */
export function isDirectoryId(id: unknown): id is DirectoryId {
  return typeof id === 'string' && id.startsWith('directory-')
}

/**
 * Unique identifier for an asset representing the items inside a directory for which the
 * request to retrive the items has not yet completed.
 */
export type LoadingAssetId = Newtype<string, 'LoadingAssetId'>
export const LoadingAssetId = newtypeConstructor<LoadingAssetId>()

/** Unique identifier for an asset representing the nonexistent children of an empty directory. */
export type EmptyAssetId = Newtype<string, 'EmptyAssetId'>
export const EmptyAssetId = newtypeConstructor<EmptyAssetId>()

/** Unique identifier for an asset representing the parent directory. */
export type UpAssetId = Newtype<string, 'UpAssetId'>
export const UpAssetId = newtypeConstructor<UpAssetId>()
/**
 * Unique identifier for an asset representing the nonexistent children of a directory
 * that failed to fetch.
 */
export type ErrorAssetId = Newtype<string, 'ErrorAssetId'>
export const ErrorAssetId = newtypeConstructor<ErrorAssetId>()

/** Unique identifier for a user's project. */
export type ProjectId = Newtype<string, 'ProjectId'>
export const ProjectId = newtypeConstructor<ProjectId>()
/** Whether a given {@link unknown} is an {@link ProjectId}. */
export function isProjectId(id: unknown): id is ProjectId {
  return typeof id === 'string' && id.startsWith('project-')
}

/** Unique identifier for an uploaded file. */
export type FileId = Newtype<string, 'FileId'>
export const FileId = newtypeConstructor<FileId>()

/** Unique identifier for a secret environment variable. */
export type SecretId = Newtype<string, 'SecretId'>
export const SecretId = newtypeConstructor<SecretId>()

/** Unique identifier for a project session. */
export type ProjectSessionId = Newtype<string, 'ProjectSessionId'>
export const ProjectSessionId = newtypeConstructor<ProjectSessionId>()

/** Unique identifier for a project execution. */
export type ProjectExecutionId = Newtype<string, 'ProjectExecutionId'>
export const ProjectExecutionId = newtypeConstructor<ProjectExecutionId>()

/** Unique identifier for a Datalink. */
export type DatalinkId = Newtype<string, 'DatalinkId'>
export const DatalinkId = newtypeConstructor<DatalinkId>()

/** Unique identifier for a version of an S3 object. */
export type S3ObjectVersionId = Newtype<string, 'S3ObjectVersionId'>
export const S3ObjectVersionId = newtypeConstructor<S3ObjectVersionId>()

/** Unique identifier for an arbitrary asset. */
export type AssetId = IdType[keyof IdType]
export const AssetId = newtypeConstructor<AssetId>()

/** Unique identifier for metadata. */
export type MetadataId = Newtype<`metadata-${KSUID}`, 'MetadataId'>
export const MetadataId = newtypeConstructor<MetadataId>()

/** Unique identifier for a subscription. */
export type SubscriptionId = Newtype<string, 'SubscriptionId'>
export const SubscriptionId = newtypeConstructor<SubscriptionId>()

/** Unique identifier for a task to archive some assets to a `.zip`. */
export type ZipAssetsJobId = Newtype<string, 'ZipAssetsJobId'>
export const ZipAssetsJobId = newtypeConstructor<ZipAssetsJobId>()

/** Unique identifier for a task to archive some assets to a `.zip`. */
export type UnzipAssetsJobId = Newtype<string, 'UnzipAssetsJobId'>
export const UnzipAssetsJobId = newtypeConstructor<UnzipAssetsJobId>()

/** Unique identifier for an API key. */
export type ApiKeyId = Newtype<string, 'ApiKeyId'>
export const ApiKeyId = newtypeConstructor<ApiKeyId>()

/** The name of an asset label. */
export type LabelName = Newtype<string, 'LabelName'>
export const LabelName = newtypeConstructor<LabelName>()

/** Unique identifier for a label. */
export type TagId = Newtype<string, 'TagId'>
export const TagId = newtypeConstructor<TagId>()

/** A URL. */
export type Address = Newtype<string, 'Address'>
export const Address = newtypeConstructor<Address>()

/** A HTTPS URL. */
export type HttpsUrl = Newtype<string, 'HttpsUrl'>
export const HttpsUrl = newtypeConstructor<HttpsUrl>()

/** An email address. */
export type EmailAddress = Newtype<string, 'EmailAddress'>
export const EmailAddress = newtypeConstructor<EmailAddress>()

/** An AWS S3 file path. */
export type S3FilePath = Newtype<string, 'S3FilePath'>
export const S3FilePath = newtypeConstructor<S3FilePath>()

/** An AWS machine configuration. */
export type Ami = Newtype<string, 'Ami'>
export const Ami = newtypeConstructor<Ami>()

/** An identifier for an entity with an {@link AssetPermission} for an {@link Asset}. */
export type UserPermissionIdentifier = UserGroupId | UserId

/** An filesystem path. Only present on the local backend. */
export type Path = Newtype<string, 'Path'>
export const Path = newtypeConstructor<Path>()

/** A project UUID. Only present on the local backend. */
export type UUID = Newtype<string, 'UUID'>
export const UUID = newtypeConstructor<UUID>()

/** The path of ids to this asset. */
export type ParentsPath = Newtype<string, 'ParentsPath'>
export const ParentsPath = newtypeConstructor<ParentsPath>()

/** The path of directory names to this asset, excluding the root directory. */
export type VirtualParentsPath = Newtype<string, 'VirtualParentsPath'>
export const VirtualParentsPath = newtypeConstructor<VirtualParentsPath>()

/** A pagination token for an arbitrary endpoint. */
export type PaginationToken = Newtype<string, 'PaginationToken'>
export const PaginationToken = newtypeConstructor<PaginationToken>()

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

/** User settings for an MS365 credential. */
export interface MS365CredentialInput {
  readonly type: 'MS365'
  readonly scopes: readonly string[]
}

/** User settings for an arbitrary credential. */
export type CredentialInput =
  | SnowflakeCredentialInput
  | GoogleCredentialInput
  | StravaCredentialInput
  | MS365CredentialInput
