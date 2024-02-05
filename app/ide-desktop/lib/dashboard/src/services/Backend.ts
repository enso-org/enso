/** @file Type definitions common between all backends. */
import type * as React from 'react'

import type * as color from '#/utilities/color'
import * as dateTime from '#/utilities/dateTime'
import * as newtype from '#/utilities/newtype'
import * as permissions from '#/utilities/permissions'
import * as uniqueString from '#/utilities/uniqueString'

// ================
// === Newtypes ===
// ================

// These are constructor functions that construct values of the type they are named after.
/* eslint-disable @typescript-eslint/no-redeclare */

/** Unique identifier for a user/organization. */
export type UserOrOrganizationId = newtype.Newtype<string, 'UserOrOrganizationId'>
export const UserOrOrganizationId = newtype.newtypeConstructor<UserOrOrganizationId>()

/** Unique identifier for a directory. */
export type DirectoryId = newtype.Newtype<string, 'DirectoryId'>
export const DirectoryId = newtype.newtypeConstructor<DirectoryId>()

/** Unique identifier for an asset representing the items inside a directory for which the
 * request to retrive the items has not yet completed. */
export type LoadingAssetId = newtype.Newtype<string, 'LoadingAssetId'>
export const LoadingAssetId = newtype.newtypeConstructor<LoadingAssetId>()

/** Unique identifier for an asset representing the nonexistent children of an empty directory. */
export type EmptyAssetId = newtype.Newtype<string, 'EmptyAssetId'>
export const EmptyAssetId = newtype.newtypeConstructor<EmptyAssetId>()

/** Unique identifier for a user's project. */
export type ProjectId = newtype.Newtype<string, 'ProjectId'>
export const ProjectId = newtype.newtypeConstructor<ProjectId>()

/** Unique identifier for an uploaded file. */
export type FileId = newtype.Newtype<string, 'FileId'>
export const FileId = newtype.newtypeConstructor<FileId>()

/** Unique identifier for a secret environment variable. */
export type SecretId = newtype.Newtype<string, 'SecretId'>
export const SecretId = newtype.newtypeConstructor<SecretId>()

/** Unique identifier for an arbitrary asset. */
export type AssetId = IdType[keyof IdType]

/** The name of an asset label. */
export type LabelName = newtype.Newtype<string, 'LabelName'>
export const LabelName = newtype.newtypeConstructor<LabelName>()

/** Unique identifier for a label. */
export type TagId = newtype.Newtype<string, 'TagId'>
export const TagId = newtype.newtypeConstructor<TagId>()

/** A URL. */
export type Address = newtype.Newtype<string, 'Address'>
export const Address = newtype.newtypeConstructor<Address>()

/** An email address. */
export type EmailAddress = newtype.Newtype<string, 'EmailAddress'>
export const EmailAddress = newtype.newtypeConstructor<EmailAddress>()

/** An AWS S3 file path. */
export type S3FilePath = newtype.Newtype<string, 'S3FilePath'>
export const S3FilePath = newtype.newtypeConstructor<S3FilePath>()

/** An AWS machine configuration. */
export type Ami = newtype.Newtype<string, 'Ami'>
export const Ami = newtype.newtypeConstructor<Ami>()

/** An AWS user ID. */
export type Subject = newtype.Newtype<string, 'Subject'>
export const Subject = newtype.newtypeConstructor<Subject>()

/* eslint-enable @typescript-eslint/no-redeclare */

// =============
// === Types ===
// =============

/** The {@link Backend} variant. If a new variant is created, it should be added to this enum. */
export enum BackendType {
  local = 'local',
  remote = 'remote',
}

/** A user/organization in the application. These are the primary owners of a project. */
export interface UserOrOrganization {
  id: UserOrOrganizationId
  name: string
  email: EmailAddress
  /** A URL. */
  profilePicture: string | null
  /** If `false`, this account is awaiting acceptance from an admin, and endpoints other than
   * `usersMe` will not work. */
  isEnabled: boolean
  rootDirectoryId: DirectoryId
}

/** Possible states that a project can be in. */
export enum ProjectState {
  created = 'Created',
  new = 'New',
  openInProgress = 'OpenInProgress',
  provisioned = 'Provisioned',
  opened = 'Opened',
  closed = 'Closed',
  /** A frontend-specific state, representing a project that should be displayed as
   * `openInProgress`, but has not yet been added to the backend. */
  placeholder = 'Placeholder',
  /** A frontend-specific state, representing a project that should be displayed as `closed`,
   * but is still in the process of shutting down. */
  closing = 'Closing',
}

/** Wrapper around a project state value. */
export interface ProjectStateType {
  type: ProjectState
  /* eslint-disable @typescript-eslint/naming-convention */
  volume_id: string
  instance_id?: string
  execute_async?: boolean
  address?: string
  security_group_id?: string
  ec2_id?: string
  ec2_public_ip_address?: string
  current_session_id?: string
  opened_by?: EmailAddress
  /* eslint-enable @typescript-eslint/naming-convention */
}

export const DOES_PROJECT_STATE_INDICATE_VM_EXISTS: Record<ProjectState, boolean> = {
  [ProjectState.created]: false,
  [ProjectState.new]: false,
  [ProjectState.openInProgress]: true,
  [ProjectState.provisioned]: true,
  [ProjectState.opened]: true,
  [ProjectState.closed]: false,
  [ProjectState.placeholder]: true,
  [ProjectState.closing]: false,
}

/** A user/organization's project containing and/or currently executing code. */
export interface Project {
  organizationId: string
  projectId: ProjectId
  name: string
  state: ProjectStateType
  packageName: string
  binaryAddress: Address | null
  jsonAddress: Address | null
  ideVersion: VersionNumber | null
  engineVersion: VersionNumber | null
  openedBy?: EmailAddress
}

/** A user/organization's project containing and/or currently executing code. */
export interface BackendProject extends Project {
  /** This must not be null as it is required to determine the base URL for backend assets. */
  ideVersion: VersionNumber
}

/** Information required to open a project. */
export interface SavedProjectStartupInfo {
  backendType: BackendType
  parentId: DirectoryId
  id: ProjectId
  title: string
  accessToken: string | null
}

/** Information related to the currently open project. */
export interface ProjectStartupInfo {
  details: Project
  projectAsset: SmartProject
  // This MUST BE optional because it is lost when `JSON.stringify`ing to put in `localStorage`,
  // and cannot be reconstructed. It is mainly used for updating the row when editing the asset
  // using the "Share" button on the top right.
  setProjectAsset?: React.Dispatch<React.SetStateAction<ProjectAsset>>
  backendType: BackendType
  accessToken: string | null
}

/** Metadata describing an uploaded file. */
export interface File {
  fileId: FileId
  fileName: string | null
  path: S3FilePath
}

/** All metadata related to a file. */
export interface FileDetails {
  file: File
}

/** A secret environment variable. */
export interface Secret {
  id: SecretId
  value: string
}

/** A secret environment variable and metadata uniquely identifying it. */
export interface SecretAndInfo {
  id: SecretId
  name: string
  value: string
}

/** Metadata uniquely identifying a secret environment variable. */
export interface SecretInfo {
  name: string
  id: SecretId
}

/** A label. */
export interface Label {
  id: TagId
  value: LabelName
  color: color.LChColor
}

/** Type of application that a {@link Version} applies to.
 *
 * We keep track of both backend and IDE versions, so that we can update the two independently.
 * However the format of the version numbers is the same for both, so we can use the same type for
 * both. We just need this enum to disambiguate. */
export enum VersionType {
  backend = 'Backend',
  ide = 'Ide',
}

/** Stability of an IDE or backend version. */
export enum VersionLifecycle {
  stable = 'Stable',
  releaseCandidate = 'ReleaseCandidate',
  nightly = 'Nightly',
  development = 'Development',
}

/** Version number of an IDE or backend. */
export interface VersionNumber {
  value: string
  lifecycle: VersionLifecycle
}

/** A version describing a release of the backend or IDE. */
export interface Version {
  number: VersionNumber
  ami: Ami | null
  created: dateTime.Rfc3339DateTime
  // This does not follow our naming convention because it's defined this way in the backend,
  // so we need to match it.
  // eslint-disable-next-line @typescript-eslint/naming-convention
  version_type: VersionType
}

/** Resource usage of a VM. */
export interface ResourceUsage {
  /** Percentage of memory used. */
  memory: number
  /** Percentage of CPU time used since boot. */
  cpu: number
  /** Percentage of disk space used. */
  storage: number
}

/** Metadata uniquely identifying a user. */
export interface User {
  /* eslint-disable @typescript-eslint/naming-convention */
  pk: Subject
  user_name: string
  user_email: EmailAddress
  organization_id: UserOrOrganizationId
  /* eslint-enable @typescript-eslint/naming-convention */
}

/** Metadata uniquely identifying a user inside an organization.
 * This is similar to {@link User}, but without `organization_id`. */
export interface SimpleUser {
  id: Subject
  name: string
  email: EmailAddress
}

/** User permission for a specific user. */
export interface UserPermission {
  user: User
  permission: permissions.PermissionAction
}

/** The type returned from the "create directory" endpoint. */
export interface Directory extends DirectoryAsset {}

/** The subset of asset fields returned by the "copy asset" endpoint. */
export interface CopiedAsset {
  id: AssetId
  parentId: DirectoryId
  title: string
}

/** The type returned from the "copy asset" endpoint. */
export interface CopyAssetResponse {
  asset: CopiedAsset
}

/** Possible filters for the "list directory" endpoint. */
export enum FilterBy {
  all = 'All',
  active = 'Active',
  recent = 'Recent',
  trashed = 'Trashed',
}

// ===================================
// === serializeProjectStartupInfo ===
// ===================================

/** Convert a {@link ProjectStartupInfo} into a serializable form. */
export function serializeProjectStartupInfo(
  projectStartupInfo: ProjectStartupInfo
): SavedProjectStartupInfo {
  const { backendType, accessToken, projectAsset } = projectStartupInfo
  const { id, parentId, title } = projectAsset.value
  return { backendType, accessToken, id, parentId, title }
}

// =================
// === AssetType ===
// =================

/** All possible types of directory entries. */
export enum AssetType {
  project = 'project',
  file = 'file',
  secret = 'secret',
  directory = 'directory',
  /** A special {@link AssetType} representing the unknown items of a directory, before the
   * request to retrieve the items completes. */
  specialLoading = 'special-loading',
  /** A special {@link AssetType} representing the sole child of an empty directory. */
  specialEmpty = 'special-empty',
}

/** The corresponding ID newtype for each {@link AssetType}. */
export interface IdType {
  [AssetType.project]: ProjectId
  [AssetType.file]: FileId
  [AssetType.secret]: SecretId
  [AssetType.directory]: DirectoryId
  [AssetType.specialLoading]: LoadingAssetId
  [AssetType.specialEmpty]: EmptyAssetId
}

/** The english name of each asset type. */
export const ASSET_TYPE_NAME: Record<AssetType, string> = {
  [AssetType.directory]: 'folder',
  [AssetType.project]: 'project',
  [AssetType.file]: 'file',
  [AssetType.secret]: 'secret',
  [AssetType.specialLoading]: 'special loading asset',
  [AssetType.specialEmpty]: 'special empty asset',
} as const

/** Integers (starting from 0) corresponding to the order in which each asset type should appear
 * in a directory listing. */
export const ASSET_TYPE_ORDER: Record<AssetType, number> = {
  // This is a sequence of numbers, not magic numbers. `999` and `1000` are arbitrary numbers
  // that are higher than the number of possible asset types.
  /* eslint-disable @typescript-eslint/no-magic-numbers */
  [AssetType.directory]: 0,
  [AssetType.project]: 1,
  [AssetType.file]: 2,
  [AssetType.secret]: 3,
  [AssetType.specialLoading]: 999,
  [AssetType.specialEmpty]: 1000,
  /* eslint-enable @typescript-eslint/no-magic-numbers */
}

// =============
// === Asset ===
// =============

/** Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories. */
export interface BaseAsset {
  id: AssetId
  title: string
  modifiedAt: dateTime.Rfc3339DateTime
  /** This is defined as a generic {@link AssetId} in the backend, however it is more convenient
   * (and currently safe) to assume it is always a {@link DirectoryId}. */
  parentId: DirectoryId
  permissions: UserPermission[] | null
  labels: LabelName[] | null
  description: string | null
}

/** Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories. */
export interface Asset<Type extends AssetType = AssetType> extends BaseAsset {
  type: Type
  id: IdType[Type]
  projectState: Type extends AssetType.project ? ProjectStateType : null
}

/** A convenience alias for {@link Asset}<{@link AssetType.directory}>. */
export interface DirectoryAsset extends Asset<AssetType.directory> {}

/** A convenience alias for {@link Asset}<{@link AssetType.project}>. */
export interface ProjectAsset extends Asset<AssetType.project> {}

/** A convenience alias for {@link Asset}<{@link AssetType.file}>. */
export interface FileAsset extends Asset<AssetType.file> {}

/** A convenience alias for {@link Asset}<{@link AssetType.secret}>. */
export interface SecretAsset extends Asset<AssetType.secret> {}

/** A convenience alias for {@link Asset}<{@link AssetType.specialLoading}>. */
export interface SpecialLoadingAsset extends Asset<AssetType.specialLoading> {}

/** A convenience alias for {@link Asset}<{@link AssetType.specialEmpty}>. */
export interface SpecialEmptyAsset extends Asset<AssetType.specialEmpty> {}

/** Creates a {@link DirectoryAsset} representing the root directory for the organization,
 * with all irrelevant fields initialized to default values. */
export function createRootDirectoryAsset(directoryId: DirectoryId): DirectoryAsset {
  return {
    type: AssetType.directory,
    title: '(root)',
    id: directoryId,
    modifiedAt: dateTime.toRfc3339(new Date()),
    parentId: DirectoryId(''),
    permissions: [],
    projectState: null,
    labels: [],
    description: null,
  }
}

/** A union of all possible {@link Asset} variants. */
export type AnyAsset =
  | DirectoryAsset
  | FileAsset
  | ProjectAsset
  | SecretAsset
  | SpecialEmptyAsset
  | SpecialLoadingAsset

/** A type guard that returns whether an {@link Asset} is a specific type of asset. */
export function assetIsType<Type extends AssetType>(type: Type) {
  return (asset: AnyAsset): asset is Extract<AnyAsset, Asset<Type>> => asset.type === type
}

/** An asset with a specific type. */
interface HasType<Type extends AssetType> {
  type: Type
}

/** A type guard that returns whether an {@link SmartAsset} is a specific type of asset. */
export function smartAssetIsType<Type extends AssetType>(type: Type) {
  return (asset: AnySmartAsset): asset is Extract<AnySmartAsset, HasType<Type>> =>
    asset.type === type
}

/** Creates a new placeholder asset id for the given asset type. */
export function createPlaceholderAssetId<Type extends AssetType>(
  type: Type,
  id?: string
): IdType[Type] {
  // This is required so that TypeScript can check the `switch` for exhaustiveness.
  const assetType: AssetType = type
  id ??= uniqueString.uniqueString()
  let result: AssetId
  switch (assetType) {
    case AssetType.directory: {
      result = DirectoryId(id)
      break
    }
    case AssetType.project: {
      result = ProjectId(id)
      break
    }
    case AssetType.file: {
      result = FileId(id)
      break
    }
    case AssetType.secret: {
      result = SecretId(id)
      break
    }
    case AssetType.specialLoading: {
      result = LoadingAssetId(id)
      break
    }
    case AssetType.specialEmpty: {
      result = EmptyAssetId(id)
      break
    }
  }
  // This is SAFE, just too complex for TypeScript to correctly typecheck.
  // eslint-disable-next-line no-restricted-syntax
  return result as IdType[Type]
}

// These are functions, and so their names should be camelCase.
/* eslint-disable no-restricted-syntax */
/** A type guard that returns whether an {@link Asset} is a {@link ProjectAsset}. */
export const assetIsProject = assetIsType(AssetType.project)
/** A type guard that returns whether an {@link Asset} is a {@link DirectoryAsset}. */
export const assetIsDirectory = assetIsType(AssetType.directory)
/** A type guard that returns whether an {@link Asset} is a {@link SecretAsset}. */
export const assetIsSecret = assetIsType(AssetType.secret)
/** A type guard that returns whether an {@link Asset} is a {@link FileAsset}. */
export const assetIsFile = assetIsType(AssetType.file)
/** A type guard that returns whether a {@link SmartAsset} is a {@link SmartProject}. */
export const smartAssetIsProject = smartAssetIsType(AssetType.project)
/** A type guard that returns whether a {@link SmartAsset} is a {@link SmartDirectory}. */
export const smartAssetIsDirectory = smartAssetIsType(AssetType.directory)
/** A type guard that returns whether a {@link SmartAsset} is a {@link SmartSecret}. */
export const smartAssetIsSecret = smartAssetIsType(AssetType.secret)
/** A type guard that returns whether a {@link SmartAsset} is a {@link SmartFile}. */
export const smartAssetIsFile = smartAssetIsType(AssetType.file)
/* eslint-disable no-restricted-syntax */

/** Metadata describing a specific version of an asset. */
export interface S3ObjectVersion {
  versionId: string
  lastModified: dateTime.Rfc3339DateTime
  isLatest: boolean
}

/** A list of asset versions. */
export interface AssetVersions {
  versions: S3ObjectVersion[]
}

// ==============================
// === compareUserPermissions ===
// ==============================

/** A value returned from a compare function passed to {@link Array.sort}, indicating that the
 * first argument was less than the second argument. */
const COMPARE_LESS_THAN = -1

/** Return a positive number when `a > b`, a negative number when `a < b`, and `0`
 * when `a === b`. */
export function compareUserPermissions(a: UserPermission, b: UserPermission) {
  const relativePermissionPrecedence =
    permissions.PERMISSION_ACTION_PRECEDENCE[a.permission] -
    permissions.PERMISSION_ACTION_PRECEDENCE[b.permission]
  if (relativePermissionPrecedence !== 0) {
    return relativePermissionPrecedence
  } else {
    const aName = a.user.user_name
    const bName = b.user.user_name
    const aEmail = a.user.user_email
    const bEmail = b.user.user_email
    return aName < bName
      ? COMPARE_LESS_THAN
      : aName > bName
      ? 1
      : aEmail < bEmail
      ? COMPARE_LESS_THAN
      : aEmail > bEmail
      ? 1
      : 0
  }
}

// =================
// === Endpoints ===
// =================

/** HTTP request body for the "set username" endpoint. */
export interface CreateUserRequestBody {
  userName: string
  userEmail: EmailAddress
  organizationId: UserOrOrganizationId | null
}

/** HTTP request body for the "update user" endpoint. */
export interface UpdateUserRequestBody {
  username: string | null
}

/** HTTP request body for the "invite user" endpoint. */
export interface InviteUserRequestBody {
  organizationId: UserOrOrganizationId
  userEmail: EmailAddress
}

/** HTTP request body for the "create permission" endpoint. */
export interface CreatePermissionRequestBody {
  userSubjects: Subject[]
  action: permissions.PermissionAction | null
}

/** HTTP request body for the "update directory" endpoint. */
export interface UpdateDirectoryRequestBody {
  title: string
}

/** HTTP request body for the "update asset" endpoint. */
export interface UpdateAssetRequestBody {
  parentDirectoryId?: DirectoryId | null
  description?: string | null
}

/** HTTP request body for the "update project" endpoint.
 * Only updates of the `projectName` or `ami` are allowed. */
export interface UpdateProjectRequestBody {
  projectName: string | null
  ami: Ami | null
  ideVersion: VersionNumber | null
}

/** HTTP request body for the "upload file" endpoint. */
export interface UploadFileRequestBody {
  file: globalThis.File
}

/** HTTP request body for the "open project" endpoint. */
export interface OpenProjectRequestBody {
  forceCreate: boolean
  executeAsync: boolean
}

/** HTTP request body for the "update secret" endpoint. */
export interface UpdateSecretRequestBody {
  value: string
}

/** Body for the "update directory" API function. */
export interface UpdateAssetOrDirectoryRequestBody
  extends UpdateAssetRequestBody,
    Partial<{ [K in keyof UpdateDirectoryRequestBody]: UpdateDirectoryRequestBody[K] | null }> {}

/** Body for the "update project" API function. */
export interface UpdateAssetOrProjectRequestBody
  extends UpdateAssetRequestBody,
    Partial<{ [K in keyof UpdateProjectRequestBody]: UpdateProjectRequestBody[K] | null }>,
    Partial<{ [K in keyof UploadFileRequestBody]: UploadFileRequestBody[K] | null }> {}

/** Body for the "update file" API function. */
export interface UpdateAssetOrFileRequestBody
  extends UpdateAssetRequestBody,
    Partial<{ [K in keyof UploadFileRequestBody]: UploadFileRequestBody[K] | null }> {}

/** Body for the "update secret" API function. */
export interface UpdateAssetOrSecretRequestBody
  extends UpdateAssetRequestBody,
    Partial<{ [K in keyof UpdateSecretRequestBody]: UpdateSecretRequestBody[K] | null }> {}

/** HTTP request body for the "create tag" endpoint. */
export interface CreateTagRequestBody {
  value: string
  color: color.LChColor
}

/** URL query string parameters for the "list directory" endpoint. */
export interface ListDirectoryRequestParams {
  filterBy: FilterBy | null
  labels: LabelName[] | null
  recentProjects: boolean
}

/** URL query string parameters for the "upload user profile picture" endpoint. */
export interface UploadUserPictureRequestParams {
  fileName: string | null
}

// ==============================
// === detectVersionLifecycle ===
// ==============================

/** Extract the {@link VersionLifecycle} from a version string. */
export function detectVersionLifecycle(version: string) {
  if (/rc/i.test(version)) {
    return VersionLifecycle.releaseCandidate
  } else if (/\bnightly\b/i.test(version)) {
    return VersionLifecycle.nightly
  } else if (/\bdev\b|\balpha\b/i.test(version)) {
    return VersionLifecycle.development
  } else {
    return VersionLifecycle.stable
  }
}

// =====================
// === compareAssets ===
// =====================

/** Return a positive number if `a > b`, a negative number if `a < b`, and zero if `a === b`. */
export function compareAssets(a: AnyAsset, b: AnyAsset) {
  const relativeTypeOrder = ASSET_TYPE_ORDER[a.type] - ASSET_TYPE_ORDER[b.type]
  if (relativeTypeOrder !== 0) {
    return relativeTypeOrder
  }
  return a.title > b.title ? 1 : a.title < b.title ? COMPARE_LESS_THAN : 0
}

// ==================
// === getAssetId ===
// ==================

/** A convenience function to get the `id` of an {@link Asset}.
 * This is useful to avoid React re-renders as it is not re-created on each function call. */
export function getAssetId<Type extends AssetType>(asset: Asset<Type>) {
  return asset.id
}

// =====================
// === fileIsProject ===
// =====================

/** A subset of properties of the JS `File` type. */
interface JSFile {
  name: string
}

/** Whether a `File` is a project. */
export function fileIsProject(file: JSFile) {
  return (
    file.name.endsWith('.tar.gz') ||
    file.name.endsWith('.zip') ||
    file.name.endsWith('.enso-project')
  )
}

/** Whether a `File` is not a project. */
export function fileIsNotProject(file: JSFile) {
  return !fileIsProject(file)
}

// =============================
// === stripProjectExtension ===
// =============================

/** Remove the extension of the project file name (if any). */
export function stripProjectExtension(name: string) {
  return name.replace(/[.](?:tar[.]gz|zip|enso-project)$/, '')
}

/** Return both the name and extension of the project file name (if any).
 * Otherwise, returns the entire name as the basename. */
export function extractProjectExtension(name: string) {
  const [, basename, extension] = name.match(/^(.*)[.](tar[.]gz|zip|enso-project)$/) ?? []
  return { basename: basename ?? name, extension: extension ?? '' }
}

// =====================
// === Smart objects ===
// =====================

/** A wrapper around a subset of the API endpoints. */
export interface SmartObject<T> {
  readonly value: T
  /** Return a copy of this object, but with a different value. */
  readonly withValue: (value: T) => this
}

/** A smart wrapper around a {@link UserOrOrganization}. */
export interface SmartUser extends SmartObject<UserOrOrganization> {
  /** Change the username of the current user. */
  readonly update: (body: UpdateUserRequestBody) => Promise<void>
  /** Delete the current user. */
  readonly delete: () => Promise<void>
  /** Upload a new profile picture for the current user. */
  readonly uploadPicture: (
    params: UploadUserPictureRequestParams,
    file: Blob
  ) => Promise<UserOrOrganization>
  /** Get the root directory for this user. */
  readonly rootDirectory: () => SmartDirectory
  /** Invite a new user to the organization by email. */
  readonly invite: (body: InviteUserRequestBody) => Promise<void>
  /** Return a list of all users in the same organization. */
  readonly listUsers: () => Promise<SimpleUser[]>
}

/** A smart wrapper around an {@link AnyAsset}. */
export interface SmartAsset<T extends AnyAsset = AnyAsset> extends SmartObject<T> {
  /** This is required to exist so that {@link AnySmartAsset} is a discriminated union. */
  readonly type: T['type']
  /** If this is a placeholder asset, return its non-placeholder equivalent after creating it on
   * the backend. Otherwise, return `this`. */
  readonly materialize: () => Promise<this>
  /** Change the parent directory of an asset. */
  readonly update: (body: UpdateAssetRequestBody) => Promise<this>
  /** Move an arbitrary asset to the trash. */
  readonly delete: () => Promise<void>
  /** Restore an arbitrary asset from the trash. */
  readonly undoDelete: () => Promise<void>
  /** Copy an arbitrary asset to another directory. */
  readonly copy: (
    parentDirectoryId: DirectoryId,
    parentDirectoryTitle: string
  ) => Promise<CopyAssetResponse>
  /** List all versions of this asset. Only works for projects and files. */
  readonly listVersions: () => Promise<AssetVersions>
  /** Set permissions for a user. */
  readonly setPermissions: (body: CreatePermissionRequestBody) => Promise<void>
  /** Replace the list of labels. */
  readonly setTags: (tagIds: LabelName[]) => Promise<void>
}

/** A smart wrapper around a {@link DirectoryAsset}. */
export interface SmartDirectory extends SmartAsset<DirectoryAsset> {
  /** Return a list of assets in a directory. */
  readonly list: (query: ListDirectoryRequestParams) => Promise<AnySmartAsset[]>
  /** Change the name or description of a directory. */
  readonly update: (body: UpdateAssetOrDirectoryRequestBody) => Promise<this>
  /** Create a {@link SpecialLoadingAsset}. */
  readonly createSpecialLoadingAsset: () => SmartSpecialLoadingAsset
  /** Create a {@link SpecialEmptyAsset}. */
  readonly createSpecialEmptyAsset: () => SmartSpecialEmptyAsset
  /** Create a {@link SmartDirectory} that is to be uploaded on the backend via `.materialize()` */
  readonly createPlaceholderDirectory: (
    title: string,
    permissions: UserPermission[]
  ) => SmartDirectory
  /** Create a {@link SmartProject} that is to be uploaded on the backend via `.materialize()` */
  readonly createPlaceholderProject: (
    title: string,
    fileOrTemplateName: globalThis.File | string | null,
    permissions: UserPermission[]
  ) => SmartProject
  /** Create a {@link SmartFile} that is to be uploaded on the backend via `.materialize()` */
  readonly createPlaceholderFile: (
    title: string,
    file: globalThis.File,
    permissions: UserPermission[]
  ) => SmartFile
  /** Create a {@link SmartSecret} that is to be uploaded on the backend via `.materialize()` */
  readonly createPlaceholderSecret: (
    title: string,
    value: string,
    permissions: UserPermission[]
  ) => SmartSecret
}

/** A smart wrapper around a {@link ProjectAsset}. */
export interface SmartProject extends SmartAsset<ProjectAsset> {
  readonly update: (body: UpdateAssetOrProjectRequestBody) => Promise<this>
  /** Set a project to an open state. */
  readonly open: (body?: OpenProjectRequestBody) => Promise<void>
  /** Return project details. */
  readonly getDetails: () => Promise<Project>
  /** Return project memory, processor and storage usage. */
  readonly getResourceUsage: () => Promise<ResourceUsage>
  /** Close a project. */
  readonly close: () => Promise<void>
  /** Resolve only when the project is ready to be opened. */
  readonly waitUntilReady: (abortController?: AbortController) => Promise<void>
}

/** A smart wrapper around a {@link FileAsset}. */
export interface SmartFile extends SmartAsset<FileAsset> {
  readonly update: (body: UpdateAssetOrFileRequestBody) => Promise<this>
  /** Return file details. */
  readonly getDetails: () => Promise<FileDetails>
}

/** A smart wrapper around a {@link SecretAsset}. */
export interface SmartSecret extends SmartAsset<SecretAsset> {
  /** Return a secret environment variable. */
  readonly getValue: () => Promise<Secret>
  /** Change the value or description of a secret environment variable. */
  readonly update: (body: UpdateAssetOrSecretRequestBody) => Promise<this>
}

/** A smart wrapper around a {@link SpecialLoadingAsset}. */
export interface SmartSpecialLoadingAsset extends SmartAsset<SpecialLoadingAsset> {}

/** A smart wrapper around a {@link SpecialEmptyAsset}. */
export interface SmartSpecialEmptyAsset extends SmartAsset<SpecialEmptyAsset> {}

/** All possible types of smart asset. */
export type AnySmartAsset =
  | SmartDirectory
  | SmartFile
  | SmartProject
  | SmartSecret
  | SmartSpecialEmptyAsset
  | SmartSpecialLoadingAsset

// ===============
// === Backend ===
// ===============

/** Interface for sending requests to a backend that manages assets and runs projects. */
export default abstract class Backend {
  abstract readonly type: BackendType

  /** Return user details for the current user. */
  abstract self(): Promise<SmartUser | null>
  /** Set the username of the current user. */
  abstract createUser(body: CreateUserRequestBody): Promise<UserOrOrganization>
  /** Create a label used for categorizing assets. */
  abstract createTag(body: CreateTagRequestBody): Promise<Label>
  /** Return all labels accessible by the user. */
  abstract listTags(): Promise<Label[]>
  /** Delete a label. */
  abstract deleteTag(tagId: TagId, value: LabelName): Promise<void>
}
