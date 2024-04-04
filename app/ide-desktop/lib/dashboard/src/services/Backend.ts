/** @file Type definitions common between all backends. */
import type * as React from 'react'

import * as array from '#/utilities/array'
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

/** Unique identifier for an organization. */
export type OrganizationId = newtype.Newtype<string, 'OrganizationId'>
export const OrganizationId = newtype.newtypeConstructor<OrganizationId>()

/** Unique identifier for a user. */
export type UserId = newtype.Newtype<string, 'UserId'>
export const UserId = newtype.newtypeConstructor<UserId>()

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

/** Unique identifier for a Data Link. */
export type ConnectorId = newtype.Newtype<string, 'ConnectorId'>
export const ConnectorId = newtype.newtypeConstructor<ConnectorId>()

/** Unique identifier for an arbitrary asset. */
export type AssetId = IdType[keyof IdType]

/** Unique identifier for a payment checkout session. */
export type CheckoutSessionId = newtype.Newtype<string, 'CheckoutSessionId'>
export const CheckoutSessionId = newtype.newtypeConstructor<CheckoutSessionId>()

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

/** An AWS user ID. */
export type Subject = newtype.Newtype<string, 'Subject'>
export const Subject = newtype.newtypeConstructor<Subject>()

/** An filesystem path. Only present on the local backend. */
export type Path = newtype.Newtype<string, 'Path'>
export const Path = newtype.newtypeConstructor<Path>()

/* eslint-enable @typescript-eslint/no-redeclare */

// =============
// === Types ===
// =============

/** The {@link Backend} variant. If a new variant is created, it should be added to this enum. */
export enum BackendType {
  local = 'local',
  remote = 'remote',
}

/** Metadata uniquely identifying a user inside an organization. */
export interface UserInfo {
  /** The ID of the parent organization. If this is a sole user, they are implicitly in an
   * organization consisting of only themselves. */
  readonly organizationId: OrganizationId
  /** The ID of this user.
   *
   * The user ID is globally unique. Thus, the user ID is always sufficient to uniquely identify a
   * user. The user ID is guaranteed to never change, once assigned. For these reasons, the user ID
   * should be the preferred way to uniquely refer to a user. That is, when referring to a user,
   * prefer this field over `name`, `email`, `subject`, or any other mechanism, where possible. */
  readonly userId: UserId
  readonly name: string
  readonly email: EmailAddress
}

/** A user in the application. These are the primary owners of a project. */
export interface User extends UserInfo {
  /** A URL. */
  readonly profilePicture: string | null
  /** If `false`, this account is awaiting acceptance from an admin, and endpoints other than
   * `usersMe` will not work. */
  readonly isEnabled: boolean
  readonly rootDirectoryId: DirectoryId
}

/** Possible states that a project can be in. */
export enum ProjectState {
  created = 'Created',
  new = 'New',
  scheduled = 'Scheduled',
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
  readonly type: ProjectState
  /* eslint-disable @typescript-eslint/naming-convention */
  readonly volume_id: string
  readonly instance_id?: string
  readonly execute_async?: boolean
  readonly address?: string
  readonly security_group_id?: string
  readonly ec2_id?: string
  readonly ec2_public_ip_address?: string
  readonly current_session_id?: string
  readonly opened_by?: EmailAddress
  /** Only present on the Local backend. */
  readonly path?: Path
  /* eslint-enable @typescript-eslint/naming-convention */
}

export const IS_OPENING: Readonly<Record<ProjectState, boolean>> = {
  [ProjectState.created]: false,
  [ProjectState.new]: false,
  [ProjectState.scheduled]: true,
  [ProjectState.openInProgress]: true,
  [ProjectState.provisioned]: true,
  [ProjectState.opened]: false,
  [ProjectState.closed]: false,
  [ProjectState.placeholder]: true,
  [ProjectState.closing]: false,
}

export const IS_OPENING_OR_OPENED: Readonly<Record<ProjectState, boolean>> = {
  [ProjectState.created]: false,
  [ProjectState.new]: false,
  [ProjectState.scheduled]: true,
  [ProjectState.openInProgress]: true,
  [ProjectState.provisioned]: true,
  [ProjectState.opened]: true,
  [ProjectState.closed]: false,
  [ProjectState.placeholder]: true,
  [ProjectState.closing]: false,
}

/** A user/organization's project containing and/or currently executing code. */
export interface Project {
  readonly organizationId: string
  readonly projectId: ProjectId
  readonly name: string
  readonly state: ProjectStateType
  readonly packageName: string
  readonly binaryAddress: Address | null
  readonly jsonAddress: Address | null
  readonly ideVersion: VersionNumber | null
  readonly engineVersion: VersionNumber | null
  readonly openedBy?: EmailAddress
  /** On the Remote (Cloud) Backend, this is a S3 url that is valid for only 120 seconds. */
  readonly url?: HttpsUrl
}

/** A user/organization's project containing and/or currently executing code. */
export interface BackendProject extends Project {
  /** This must not be null as it is required to determine the base URL for backend assets. */
  readonly ideVersion: VersionNumber
}

/** Information required to open a project. */
export interface SavedProjectStartupInfo {
  readonly backendType: BackendType
  readonly parentId: DirectoryId
  readonly id: ProjectId
  readonly title: string
  readonly accessToken: string | null
}

/** Information related to the currently open project. */
export interface ProjectStartupInfo {
  readonly details: Project
  readonly projectAsset: SmartProject
  // This MUST BE optional because it is lost when `JSON.stringify`ing to put in `localStorage`,
  // and cannot be reconstructed. It is mainly used for updating the row when editing the asset
  // using the "Share" button on the top right.
  readonly setProjectAsset?: React.Dispatch<React.SetStateAction<ProjectAsset>>
  readonly backendType: BackendType
  readonly accessToken: string | null
}

/** Metadata describing the location of an uploaded file. */
export interface FileLocator {
  readonly fileId: FileId
  readonly fileName: string | null
  readonly path: S3FilePath
}

/** Metadata for a file. */
export interface FileMetadata {
  readonly size: number
}

/** All metadata related to a file. */
export interface FileDetails {
  readonly file: FileLocator
  readonly metadata: FileMetadata
  /** On the Remote (Cloud) Backend, this is a S3 url that is valid for only 120 seconds. */
  readonly url?: string
}

/** A secret environment variable. */
export interface Secret {
  readonly id: SecretId
  readonly value: string
}

/** A secret environment variable and metadata uniquely identifying it. */
export interface SecretAndInfo {
  readonly id: SecretId
  readonly name: string
  readonly value: string
}

/** Metadata uniquely identifying a secret environment variable. */
export interface SecretInfo {
  readonly name: string
  readonly id: SecretId
}

/** A Data Link. */
export type Connector = newtype.Newtype<unknown, 'Connector'>

/** Metadata uniquely identifying a Data Link. */
export interface ConnectorInfo {
  readonly id: ConnectorId
}

/** A label. */
export interface Label {
  readonly id: TagId
  readonly value: LabelName
  readonly color: color.LChColor
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
  readonly value: string
  readonly lifecycle: VersionLifecycle
}

/** A version describing a release of the backend or IDE. */
export interface Version {
  readonly number: VersionNumber
  readonly ami: Ami | null
  readonly created: dateTime.Rfc3339DateTime
  // This does not follow our naming convention because it's defined this way in the backend,
  // so we need to match it.
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly version_type: VersionType
}

/** Credentials that need to be passed to libraries to give them access to the Cloud API. */
export interface CognitoCredentials {
  readonly accessToken: string
  readonly refreshToken: string
  readonly refreshUrl: string
  readonly clientId: string
  readonly expireAt: dateTime.Rfc3339DateTime
}

/** Subscription plans. */
export enum Plan {
  solo = 'solo',
  team = 'team',
}

export const PLANS = Object.values(Plan)

// This is a function, even though it does not look like one.
// eslint-disable-next-line no-restricted-syntax
export const isPlan = array.includesPredicate(PLANS)

/** Metadata uniquely describing a payment checkout session. */
export interface CheckoutSession {
  /** ID of the checkout session, suffixed with a secret value. */
  readonly clientSecret: string
  /** ID of the checkout session. */
  readonly id: CheckoutSessionId
}

/** Metadata describing the status of a payment checkout session. */
export interface CheckoutSessionStatus {
  /** Status of the payment for the checkout session. */
  readonly paymentStatus: string
  /** Status of the checkout session. */
  readonly status: string
}

/** Resource usage of a VM. */
export interface ResourceUsage {
  /** Percentage of memory used. */
  readonly memory: number
  /** Percentage of CPU time used since boot. */
  readonly cpu: number
  /** Percentage of disk space used. */
  readonly storage: number
}

/** Metadata for an organization. */
export interface OrganizationInfo {
  readonly id: OrganizationId
  readonly name: string | null
  readonly email: EmailAddress | null
  readonly website: HttpsUrl | null
  readonly address: string | null
  readonly picture: HttpsUrl | null
}

/** User permission for a specific user. */
export interface UserPermission {
  readonly user: UserInfo
  readonly permission: permissions.PermissionAction
}

/** The type returned from the "create directory" endpoint. */
export interface Directory extends DirectoryAsset {}

/** The subset of asset fields returned by the "copy asset" endpoint. */
export interface CopiedAsset {
  readonly id: AssetId
  readonly parentId: DirectoryId
  readonly title: string
}

/** The type returned from the "copy asset" endpoint. */
export interface CopyAssetResponse {
  readonly asset: CopiedAsset
}

/** Possible filters for the "list directory" endpoint. */
export enum FilterBy {
  all = 'All',
  active = 'Active',
  recent = 'Recent',
  trashed = 'Trashed',
}

// =============
// === Event ===
// =============

/** An event in an audit log. */
export interface Event {
  readonly organizationId: OrganizationId
  readonly userEmail: EmailAddress
  readonly timestamp: dateTime.Rfc3339DateTime | null
  // Called `EventKind` in the backend.
  readonly metadata: EventMetadata
}

/** Possible types of event in an audit log. */
export enum EventType {
  GetSecret = 'getSecret',
  DeleteAssets = 'deleteAssets',
  ListSecrets = 'listSecrets',
  OpenProject = 'openProject',
  UploadFile = 'uploadFile',
}

export const EVENT_TYPES = Object.freeze(Object.values(EventType))

/** An event indicating that a secret was accessed. */
interface GetSecretEventMetadata {
  readonly type: EventType.GetSecret
  readonly secretId: SecretId
}

/** An event indicating that one or more assets were deleted. */
interface DeleteAssetsEventMetadata {
  readonly type: EventType.DeleteAssets
}

/** An event indicating that all secrets were listed. */
interface ListSecretsEventMetadata {
  readonly type: EventType.ListSecrets
}

/** An event indicating that a project was opened. */
interface OpenProjectEventMetadata {
  readonly type: EventType.OpenProject
}

/** An event indicating that a file was uploaded. */
interface UploadFileEventMetadata {
  readonly type: EventType.UploadFile
}

/** All possible types of metadata for an event in the audit log. */
export type EventMetadata =
  | DeleteAssetsEventMetadata
  | GetSecretEventMetadata
  | ListSecretsEventMetadata
  | OpenProjectEventMetadata
  | UploadFileEventMetadata

/** A color in the LCh colorspace. */
export interface LChColor {
  readonly lightness: number
  readonly chroma: number
  readonly hue: number
  readonly alpha?: number
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
  dataLink = 'connector',
  directory = 'directory',
  /** A special {@link AssetType} representing the unknown items of a directory, before the
   * request to retrieve the items completes. */
  specialLoading = 'specialLoading',
  /** A special {@link AssetType} representing the sole child of an empty directory. */
  specialEmpty = 'specialEmpty',
}

/** The corresponding ID newtype for each {@link AssetType}. */
export interface IdType {
  readonly [AssetType.project]: ProjectId
  readonly [AssetType.file]: FileId
  readonly [AssetType.dataLink]: ConnectorId
  readonly [AssetType.secret]: SecretId
  readonly [AssetType.directory]: DirectoryId
  readonly [AssetType.specialLoading]: LoadingAssetId
  readonly [AssetType.specialEmpty]: EmptyAssetId
}

/** Integers (starting from 0) corresponding to the order in which each asset type should appear
 * in a directory listing. */
export const ASSET_TYPE_ORDER: Readonly<Record<AssetType, number>> = {
  // This is a sequence of numbers, not magic numbers. `999` and `1000` are arbitrary numbers
  // that are higher than the number of possible asset types.
  /* eslint-disable @typescript-eslint/no-magic-numbers */
  [AssetType.directory]: 0,
  [AssetType.project]: 1,
  [AssetType.file]: 2,
  [AssetType.dataLink]: 3,
  [AssetType.secret]: 4,
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
  readonly id: AssetId
  readonly title: string
  readonly modifiedAt: dateTime.Rfc3339DateTime
  /** This is defined as a generic {@link AssetId} in the backend, however it is more convenient
   * (and currently safe) to assume it is always a {@link DirectoryId}. */
  readonly parentId: DirectoryId
  readonly permissions: UserPermission[] | null
  readonly labels: LabelName[] | null
  readonly description: string | null
}

/** Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories. */
export interface Asset<Type extends AssetType = AssetType> extends BaseAsset {
  readonly type: Type
  readonly id: IdType[Type]
  readonly projectState: Type extends AssetType.project ? ProjectStateType : null
}

/** A convenience alias for {@link Asset}<{@link AssetType.directory}>. */
export interface DirectoryAsset extends Asset<AssetType.directory> {}

/** A convenience alias for {@link Asset}<{@link AssetType.project}>. */
export interface ProjectAsset extends Asset<AssetType.project> {}

/** A convenience alias for {@link Asset}<{@link AssetType.file}>. */
export interface FileAsset extends Asset<AssetType.file> {}

/** A convenience alias for {@link Asset}<{@link AssetType.dataLink}>. */
export interface DataLinkAsset extends Asset<AssetType.dataLink> {}

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

/** An {@link Asset} with a specific type. */
interface HasType<Type extends AssetType> {
  readonly type: Type
}

/** A union of all possible {@link Asset} variants. */
export type AnyAsset<Type extends AssetType = AssetType> = Extract<
  | DataLinkAsset
  | DirectoryAsset
  | FileAsset
  | ProjectAsset
  | SecretAsset
  | SpecialEmptyAsset
  | SpecialLoadingAsset,
  HasType<Type>
>

/** A type guard that returns whether an {@link Asset} is a specific type of asset. */
export function assetIsType<Type extends AssetType>(type: Type) {
  return (asset: AnyAsset): asset is Extract<AnyAsset, Asset<Type>> => asset.type === type
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
    case AssetType.dataLink: {
      result = ConnectorId(id)
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
  // This is SAFE, just too dynamic for TypeScript to correctly typecheck.
  // eslint-disable-next-line no-restricted-syntax
  return result as IdType[Type]
}

// These are functions, and so their names should be camelCase.
/* eslint-disable no-restricted-syntax */
/** A type guard that returns whether an {@link Asset} is a {@link ProjectAsset}. */
export const assetIsProject = assetIsType(AssetType.project)
/** A type guard that returns whether an {@link Asset} is a {@link DirectoryAsset}. */
export const assetIsDirectory = assetIsType(AssetType.directory)
/** A type guard that returns whether an {@link Asset} is a {@link DataLinkAsset}. */
export const assetIsDataLink = assetIsType(AssetType.dataLink)
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
  /**
   * The field points to an archive containing the all the project files object in the S3 bucket,
   */
  key: string
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
    // NOTE [NP]: Although `userId` is unique, and therefore sufficient to sort permissions, sort
    // name first, so that it's easier to find a permission in a long list (i.e., for readability).
    const aName = a.user.name
    const bName = b.user.name
    const aUserId = a.user.userId
    const bUserId = b.user.userId
    return aName < bName
      ? COMPARE_LESS_THAN
      : aName > bName
        ? 1
        : aUserId < bUserId
          ? COMPARE_LESS_THAN
          : aUserId > bUserId
            ? 1
            : 0
  }
}

// =================
// === Endpoints ===
// =================

/** HTTP request body for the "set username" endpoint. */
export interface CreateUserRequestBody {
  readonly userName: string
  readonly userEmail: EmailAddress
  readonly organizationId: OrganizationId | null
}

/** HTTP request body for the "update user" endpoint. */
export interface UpdateUserRequestBody {
  username: string | null
}

/** HTTP request body for the "update organization" endpoint. */
export interface UpdateOrganizationRequestBody {
  name?: string
  email?: EmailAddress
  website?: HttpsUrl
  location?: string
}

/** HTTP request body for the "create permission" endpoint. */
export interface CreatePermissionRequestBody {
  readonly actorsIds: UserId[]
  readonly action: permissions.PermissionAction | null
}

/** HTTP request body for the "update asset" endpoint. */
export interface UpdateAssetRequestBody {
  readonly parentDirectoryId?: DirectoryId | null
  readonly description?: string | null
  /** Only present on the Local backend. */
  readonly projectPath?: Path
}

/** HTTP request body for the "delete asset" endpoint. */
export interface DeleteAssetRequestBody {
  readonly force: boolean
  /** Only used by the Local backend. */
  readonly parentId: DirectoryId
}

/** HTTP request body for the "open project" endpoint. */
export interface OpenProjectRequestBody {
  readonly executeAsync: boolean
  /** MUST be present on Remote backend; NOT REQUIRED on Local backend. */
  readonly cognitoCredentials: CognitoCredentials | null
  /** Only used by the Local backend. */
  readonly parentId: DirectoryId
}

/** Body for the "update directory" API function. */
export interface UpdateAssetOrDirectoryRequestBody extends UpdateAssetRequestBody {
  readonly title?: string
}

/** Body for the "update project" API function. */
export interface UpdateAssetOrProjectRequestBody extends UpdateAssetRequestBody {
  // Common parameters
  /** Only used by the Local backend. */
  readonly parentId?: DirectoryId
  // Project update parameters
  readonly projectName?: string | null
  readonly ami?: Ami | null
  /** Updating this field is not allowed. */
  readonly ideVersion?: VersionNumber | null
  // File update parameters
  readonly file?: globalThis.File
}

/** Body for the "update file" API function. */
export interface UpdateAssetOrFileRequestBody extends UpdateAssetRequestBody {
  readonly file?: globalThis.File
}

/** Body for the "update Data Link" API function. */
export interface UpdateAssetOrDataLinkRequestBody extends UpdateAssetRequestBody {
  readonly value?: unknown
}

/** Body for the "update secret" API function. */
export interface UpdateAssetOrSecretRequestBody extends UpdateAssetRequestBody {
  readonly value?: string
}

/** HTTP request body for the "create connector" endpoint. */
export interface CreateConnectorRequestBody {
  name: string
  value: unknown
  parentDirectoryId: DirectoryId | null
  connectorId: ConnectorId | null
}

/** HTTP request body for the "create tag" endpoint. */
export interface CreateTagRequestBody {
  readonly value: string
  readonly color: color.LChColor
}

/** HTTP request body for the "create checkout session" endpoint. */
export interface CreateCheckoutSessionRequestBody {
  plan: Plan
}

/** URL query string parameters for the "list directory" endpoint. */
export interface ListDirectoryRequestParams {
  readonly filterBy: FilterBy | null
  readonly labels: LabelName[] | null
}

/** URL query string parameters for the "upload profile picture" endpoint. */
export interface UploadPictureRequestParams {
  readonly fileName: string | null
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
  const aModified = Number(new Date(a.modifiedAt))
  const bModified = Number(new Date(b.modifiedAt))
  const modifiedDelta = aModified - bModified
  if (modifiedDelta !== 0) {
    // Sort by date descending, rather than ascending.
    return -modifiedDelta
  }
  return a.title > b.title ? 1 : a.title < b.title ? -1 : 0
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

/** A smart wrapper around a {@link User}. */
export interface SmartUser extends SmartObject<User> {
  /** Change the username of the current user. */
  readonly update: (body: UpdateUserRequestBody) => Promise<void>
  /** Delete the current user. */
  readonly delete: () => Promise<void>
  /** Upload a new profile picture for the current user. */
  readonly uploadPicture: (params: UploadPictureRequestParams, file: Blob) => Promise<User>
  /** Get the root directory for this user. */
  readonly rootDirectory: () => SmartDirectory
  /** Invite a new user to the organization by email. */
  readonly invite: (email: EmailAddress) => Promise<void>
  /** List all users in the same organization. */
  readonly listUsers: () => Promise<UserInfo[]>
  /** List recently modified assets. */
  readonly listRecentFiles: () => Promise<AnySmartAsset[]>
  /** List all secrets in all directories. */
  readonly listSecrets: () => Promise<SecretInfo[]>
  /** Get the details of the current user's organization. */
  readonly getOrganization: () => Promise<SmartOrganization>
  /** List events in the organization's audit log. */
  readonly getLogEvents: () => Promise<Event[]>
}

/** A smart wrapper around an {@link OrganizationInfo}. */
export interface SmartOrganization extends SmartObject<OrganizationInfo | null> {
  /** Change the details of the current user's organization. */
  readonly update: (body: UpdateOrganizationRequestBody) => Promise<OrganizationInfo | null>
  /** Upload a new profile picture for the current user's organization. */
  readonly uploadPicture: (
    params: UploadPictureRequestParams,
    file: Blob
  ) => Promise<OrganizationInfo>
}

/** A smart wrapper around an {@link AnyAsset}. */
export interface SmartAsset<T extends AnyAsset = AnyAsset> extends SmartObject<T> {
  /** This is required to exist so that {@link AnySmartAsset} is a discriminated union. */
  readonly type: T['type']
  /** If this is a placeholder asset, return its non-placeholder equivalent after creating it on
   * the backend. Otherwise, return `this`, as a non-{@link Promise}.
   *
   * If returning `this` as a {@link Promise}, it causes a race condition when reopening the last
   * opened project on application start, as it is opened on the first frame, but the project state
   * is overwritten by the return value of this on the next render cycle. */
  readonly materialize: () => Promise<this> | this
  /** Change the parent directory of an asset. */
  readonly update: (body: UpdateAssetRequestBody) => Promise<this>
  /** Move an arbitrary asset to the trash. */
  readonly delete: (body: DeleteAssetRequestBody) => Promise<void>
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
  /** Create a {@link SmartDataLink} that is to be uploaded on the backend via `.materialize()` */
  readonly createPlaceholderDataLink: (
    title: string,
    value: unknown,
    permissions: UserPermission[]
  ) => SmartDataLink
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
  /** Fetch the content of the `Main.enso` file. */
  readonly getMainFile: (version: string) => Promise<string>
  /** Close a project. */
  readonly close: () => Promise<void>
  /** Resolve only when the project is ready to be opened. */
  readonly waitUntilReady: (abortController?: AbortController) => Promise<Project>
}

/** A smart wrapper around a {@link FileAsset}. */
export interface SmartFile extends SmartAsset<FileAsset> {
  readonly update: (body: UpdateAssetOrFileRequestBody) => Promise<this>
  /** Return file details. */
  readonly getDetails: () => Promise<FileDetails>
}

/** A smart wrapper around a {@link DataLinkAsset}. */
export interface SmartDataLink extends SmartAsset<DataLinkAsset> {
  /** Return the JSON value of this Data Link. */
  readonly getValue: () => Promise<Connector>
  readonly update: (body: UpdateAssetOrDataLinkRequestBody) => Promise<this>
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

/** A {@link SmartAsset} with a specific value. */
interface HasValue<Value extends AnyAsset> {
  readonly value: Value
}

/** All possible types of smart asset. */
export type AnySmartAsset<Type extends AssetType = AssetType> = Extract<
  | SmartDataLink
  | SmartDirectory
  | SmartFile
  | SmartProject
  | SmartSecret
  | SmartSpecialEmptyAsset
  | SmartSpecialLoadingAsset,
  HasValue<AnyAsset<Type>>
>

// ===============
// === Backend ===
// ===============

/** Interface for sending requests to a backend that manages assets and runs projects. */
export default abstract class Backend {
  abstract readonly type: BackendType

  /** Return user details for the current user. */
  abstract self(): Promise<SmartUser | null>
  /** Set the username of the current user. */
  abstract createUser(body: CreateUserRequestBody): Promise<User>
  /** Create a label used for categorizing assets. */
  abstract createTag(body: CreateTagRequestBody): Promise<Label>
  /** Return all labels accessible by the user. */
  abstract listTags(): Promise<Label[]>
  /** Delete a label. */
  abstract deleteTag(tagId: TagId, value: LabelName): Promise<void>
  /** Create a payment checkout session. */
  abstract createCheckoutSession(plan: Plan): Promise<CheckoutSession>
  /** Get the status of a payment checkout session. */
  abstract getCheckoutSession(sessionId: CheckoutSessionId): Promise<CheckoutSessionStatus>
}
