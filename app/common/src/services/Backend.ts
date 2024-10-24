/** @file Type definitions common between all backends. */

import * as array from '../utilities/data/array'
import * as dateTime from '../utilities/data/dateTime'
import * as newtype from '../utilities/data/newtype'
import * as permissions from '../utilities/permissions'
import * as uniqueString from '../utilities/uniqueString'

/** The size, in bytes, of the chunks which the backend accepts. */
export const S3_CHUNK_SIZE_BYTES = 10_000_000

// ================
// === Newtypes ===
// ================

/** Unique identifier for an organization. */
export type OrganizationId = newtype.Newtype<string, 'OrganizationId'>
export const OrganizationId = newtype.newtypeConstructor<OrganizationId>()

/** Unique identifier for a user in an organization. */
export type UserId = newtype.Newtype<string, 'UserId'>
export const UserId = newtype.newtypeConstructor<UserId>()

/** Unique identifier for a user group. */
export type UserGroupId = newtype.Newtype<string, 'UserGroupId'>
export const UserGroupId = newtype.newtypeConstructor<UserGroupId>()

/** Unique identifier for a directory. */
export type DirectoryId = newtype.Newtype<string, 'DirectoryId'>
export const DirectoryId = newtype.newtypeConstructor<DirectoryId>()

/**
 * Unique identifier for an asset representing the items inside a directory for which the
 * request to retrive the items has not yet completed.
 */
export type LoadingAssetId = newtype.Newtype<string, 'LoadingAssetId'>
export const LoadingAssetId = newtype.newtypeConstructor<LoadingAssetId>()

/** Unique identifier for an asset representing the nonexistent children of an empty directory. */
export type EmptyAssetId = newtype.Newtype<string, 'EmptyAssetId'>
export const EmptyAssetId = newtype.newtypeConstructor<EmptyAssetId>()

/**
 * Unique identifier for an asset representing the nonexistent children of a directory
 * that failed to fetch.
 */
export type ErrorAssetId = newtype.Newtype<string, 'ErrorAssetId'>
export const ErrorAssetId = newtype.newtypeConstructor<ErrorAssetId>()

/** Unique identifier for a user's project. */
export type ProjectId = newtype.Newtype<string, 'ProjectId'>
export const ProjectId = newtype.newtypeConstructor<ProjectId>()

/** Unique identifier for an uploaded file. */
export type FileId = newtype.Newtype<string, 'FileId'>
export const FileId = newtype.newtypeConstructor<FileId>()

/** Unique identifier for a secret environment variable. */
export type SecretId = newtype.Newtype<string, 'SecretId'>
export const SecretId = newtype.newtypeConstructor<SecretId>()

/** Unique identifier for a project session. */
export type ProjectSessionId = newtype.Newtype<string, 'ProjectSessionId'>
export const ProjectSessionId = newtype.newtypeConstructor<ProjectSessionId>()

/** Unique identifier for a Datalink. */
export type DatalinkId = newtype.Newtype<string, 'DatalinkId'>
export const DatalinkId = newtype.newtypeConstructor<DatalinkId>()

/** Unique identifier for a version of an S3 object. */
export type S3ObjectVersionId = newtype.Newtype<string, 'S3ObjectVersionId'>
export const S3ObjectVersionId = newtype.newtypeConstructor<S3ObjectVersionId>()

/** Unique identifier for an arbitrary asset. */
export type AssetId = IdType[keyof IdType]

/** Unique identifier for a payment checkout session. */
export type CheckoutSessionId = newtype.Newtype<string, 'CheckoutSessionId'>
export const CheckoutSessionId = newtype.newtypeConstructor<CheckoutSessionId>()

/** Unique identifier for a subscription. */
export type SubscriptionId = newtype.Newtype<string, 'SubscriptionId'>
export const SubscriptionId = newtype.newtypeConstructor<SubscriptionId>()

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

/** Whether a given {@link string} is an {@link UserId}. */
export function isUserId(id: string): id is UserId {
  return id.startsWith('user-')
}

/** Whether a given {@link string} is an {@link UserGroupId}. */
export function isUserGroupId(id: string): id is UserGroupId {
  return id.startsWith('usergroup-')
}

const PLACEHOLDER_USER_GROUP_PREFIX = 'usergroup-placeholder-'

/**
 * Whether a given {@link UserGroupId} represents a user group that does not yet exist on the
 * server.
 */
export function isPlaceholderUserGroupId(id: string) {
  return id.startsWith(PLACEHOLDER_USER_GROUP_PREFIX)
}

/**
 * Return a new {@link UserGroupId} that represents a placeholder user group that is yet to finish
 * being created on the backend.
 */
export function newPlaceholderUserGroupId() {
  return UserGroupId(`${PLACEHOLDER_USER_GROUP_PREFIX}${uniqueString.uniqueString()}`)
}

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
  /**
   * The ID of the parent organization. If this is a sole user, they are implicitly in an
   * organization consisting of only themselves.
   */
  readonly organizationId: OrganizationId
  /** The name of the parent organization. */
  readonly organizationName?: string
  /**
   * The ID of this user.
   *
   * The user ID is globally unique. Thus, the user ID is always sufficient to uniquely identify a
   * user. The user ID is guaranteed to never change, once assigned. For these reasons, the user ID
   * should be the preferred way to uniquely refer to a user. That is, when referring to a user,
   * prefer this field over `name`, `email`, `subject`, or any other mechanism, where possible.
   */
  readonly userId: UserId
  readonly name: string
  readonly email: EmailAddress
  readonly newOrganizationName?: string
  readonly newOrganizationInvite?: 'error' | 'pending'
}

/** A user in the application. These are the primary owners of a project. */
export interface User extends UserInfo {
  /**
   * If `false`, this account is awaiting acceptance from an administrator, and endpoints other than
   * `usersMe` will not work.
   */
  readonly isEnabled: boolean
  readonly isOrganizationAdmin: boolean
  readonly rootDirectoryId: DirectoryId
  readonly profilePicture?: HttpsUrl
  readonly userGroups: readonly UserGroupId[] | null
  readonly removeAt?: dateTime.Rfc3339DateTime | null
  readonly plan?: Plan | undefined
}

/** A `Directory` returned by `createDirectory`. */
export interface CreatedDirectory {
  readonly id: DirectoryId
  readonly parentId: DirectoryId
  readonly title: string
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
  /**
   * A frontend-specific state, representing a project that should be displayed as
   * `openInProgress`, but has not yet been added to the backend.
   */
  placeholder = 'Placeholder',
  /**
   * A frontend-specific state, representing a project that should be displayed as `closed`,
   * but is still in the process of shutting down.
   */
  closing = 'Closing',
}

/** Wrapper around a project state value. */
export interface ProjectStateType {
  readonly type: ProjectState
  readonly volumeId: string
  readonly instanceId?: string
  readonly executeAsync?: boolean
  readonly address?: string
  readonly securityGroupId?: string
  readonly ec2Id?: string
  readonly ec2PublicIpAddress?: string
  readonly currentSessionId?: string
  readonly openedBy?: EmailAddress
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

/** Common `Project` fields returned by all `Project`-related endpoints. */
export interface BaseProject {
  readonly organizationId: OrganizationId
  readonly projectId: ProjectId
  readonly name: string
}

/** A `Project` returned by `createProject`. */
export interface CreatedProject extends BaseProject {
  readonly state: ProjectStateType
  readonly packageName: string
}

/** A `Project` returned by the `listProjects` endpoint. */
export interface ListedProjectRaw extends CreatedProject {
  readonly address?: Address
}

/** A `Project` returned by `listProjects`. */
export interface ListedProject extends CreatedProject {
  readonly binaryAddress: Address | null
  readonly jsonAddress: Address | null
  readonly ydocAddress: Address | null
}

/** A `Project` returned by `updateProject`. */
export interface UpdatedProject extends BaseProject {
  readonly ami: Ami | null
  readonly ideVersion: VersionNumber | null
  readonly engineVersion: VersionNumber | null
}

/** A user/organization's project containing and/or currently executing code. */
export interface ProjectRaw extends ListedProjectRaw {
  readonly ide_version: VersionNumber | null
  readonly engine_version: VersionNumber | null
}

/** A user/organization's project containing and/or currently executing code. */
export interface Project extends ListedProject {
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

/** A specific session of a project being opened and used. */
export interface ProjectSession {
  readonly projectId: ProjectId
  readonly projectSessionId: ProjectSessionId
  readonly createdAt: dateTime.Rfc3339DateTime
  readonly closedAt?: dateTime.Rfc3339DateTime
  readonly userEmail: EmailAddress
}

/** Metadata describing the location of an uploaded file. */
export interface FileLocator {
  readonly fileId: FileId
  readonly fileName: string | null
  readonly path: S3FilePath
}

/** Metadata uniquely identifying an uploaded file. */
export interface FileInfo {
  /* TODO: Should potentially be S3FilePath,
   * but it's just string on the backend. */
  readonly path: string
  readonly id: FileId
  readonly project: CreatedProject | null
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
  readonly path: string
}

/** A Datalink. */
export type Datalink = newtype.Newtype<unknown, 'Datalink'>

/** Metadata uniquely identifying a Datalink. */
export interface DatalinkInfo {
  readonly id: DatalinkId
}

/** A label. */
export interface Label {
  readonly id: TagId
  readonly value: LabelName
  readonly color: LChColor
}

/**
 * Type of application that a {@link Version} applies to.
 *
 * We keep track of both backend and IDE versions, so that we can update the two independently.
 * However the format of the version numbers is the same for both, so we can use the same type for
 * both. We just need this enum to disambiguate.
 */
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
  free = 'free',
  solo = 'solo',
  team = 'team',
  enterprise = 'enterprise',
}

export const PLANS = Object.values(Plan)

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
  readonly status: 'active' | 'trialing' | (string & NonNullable<unknown>)
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

/** Metadata for a subscription. */
export interface Subscription {
  readonly id?: SubscriptionId
  readonly plan?: Plan
  readonly trialStart?: dateTime.Rfc3339DateTime | null
  readonly trialEnd?: dateTime.Rfc3339DateTime | null
}

/** Metadata for an organization. */
export interface OrganizationInfo {
  readonly id: OrganizationId
  readonly name: string | null
  readonly email: EmailAddress | null
  readonly website: HttpsUrl | null
  readonly address: string | null
  readonly picture: HttpsUrl | null
  readonly subscription: Subscription | null
}

/** A user group and its associated metadata. */
export interface UserGroupInfo {
  readonly organizationId: OrganizationId
  readonly id: UserGroupId
  readonly groupName: string
}

/** User permission for a specific user. */
export interface UserPermission {
  readonly user: UserInfo
  readonly permission: permissions.PermissionAction
}

/** User permission for a specific user group. */
export interface UserGroupPermission {
  readonly userGroup: UserGroupInfo
  readonly permission: permissions.PermissionAction
}

/** User permission for a specific user or user group. */
export type AssetPermission = UserGroupPermission | UserPermission

/**
 * Response from the "create customer portal session" endpoint.
 * Returns a URL that the user can use to access the customer portal and manage their subscription.
 */
export interface CreateCustomerPortalSessionResponse {
  readonly url: string | null
}

/** Whether the user is on a plan associated with an organization. */
export function isUserOnPlanWithOrganization(user: User) {
  switch (user.plan) {
    case undefined:
    case Plan.free:
    case Plan.solo: {
      return false
    }
    case Plan.team:
    case Plan.enterprise: {
      return true
    }
  }
}

/** Whether an {@link AssetPermission} is a {@link UserPermission}. */
export function isUserPermission(permission: AssetPermission): permission is UserPermission {
  return 'user' in permission
}

/** Whether an {@link AssetPermission} is a {@link UserPermission} with an additional predicate. */
export function isUserPermissionAnd(predicate: (permission: UserPermission) => boolean) {
  return (permission: AssetPermission): permission is UserPermission =>
    isUserPermission(permission) && predicate(permission)
}

/** Whether an {@link AssetPermission} is a {@link UserGroupPermission}. */
export function isUserGroupPermission(
  permission: AssetPermission,
): permission is UserGroupPermission {
  return 'userGroup' in permission
}

/** Whether an {@link AssetPermission} is a {@link UserGroupPermission} with an additional predicate. */
export function isUserGroupPermissionAnd(predicate: (permission: UserGroupPermission) => boolean) {
  return (permission: AssetPermission): permission is UserGroupPermission =>
    isUserGroupPermission(permission) && predicate(permission)
}

/** Get the property representing the name on an arbitrary variant of {@link UserPermission}. */
export function getAssetPermissionName(permission: AssetPermission) {
  return isUserPermission(permission) ? permission.user.name : permission.userGroup.groupName
}

/** Get the property representing the id on an arbitrary variant of {@link UserPermission}. */
export function getAssetPermissionId(permission: AssetPermission): UserPermissionIdentifier {
  return isUserPermission(permission) ? permission.user.userId : permission.userGroup.id
}

/** The type returned from the "update directory" endpoint. */
export interface UpdatedDirectory {
  readonly id: DirectoryId
  readonly parentId: DirectoryId
  readonly title: string
}

/** The type returned from the "create directory" endpoint. */
export type Directory = DirectoryAsset

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
  readonly alpha?: number | undefined
}

/** A pre-selected list of colors to be used in color pickers. */
export const COLORS = [
  // Red
  { lightness: 50, chroma: 66, hue: 7 },
  // Orange
  { lightness: 50, chroma: 66, hue: 34 },
  // Yellow
  { lightness: 50, chroma: 66, hue: 80 },
  // Turquoise
  { lightness: 50, chroma: 66, hue: 139 },
  // Teal
  { lightness: 50, chroma: 66, hue: 172 },
  // Blue
  { lightness: 50, chroma: 66, hue: 271 },
  // Lavender
  { lightness: 50, chroma: 66, hue: 295 },
  // Pink
  { lightness: 50, chroma: 66, hue: 332 },
  // Light blue
  { lightness: 50, chroma: 22, hue: 252 },
  // Dark blue
  { lightness: 22, chroma: 13, hue: 252 },
] as const satisfies LChColor[]

/** Converts a {@link LChColor} to a CSS color string. */
export function lChColorToCssColor(color: LChColor): string {
  const alpha = 'alpha' in color ? ` / ${color.alpha}` : ''
  return `lch(${color.lightness}% ${color.chroma} ${color.hue}${alpha})`
}

export const COLOR_STRING_TO_COLOR = new Map(
  COLORS.map(color => [lChColorToCssColor(color), color]),
)

export const INITIAL_COLOR_COUNTS = new Map(COLORS.map(color => [lChColorToCssColor(color), 0]))

/** The color that is used for the least labels. Ties are broken by order. */
export function findLeastUsedColor(labels: Iterable<Label>) {
  const colorCounts = new Map(INITIAL_COLOR_COUNTS)
  for (const label of labels) {
    const colorString = lChColorToCssColor(label.color)
    colorCounts.set(colorString, (colorCounts.get(colorString) ?? 0) + 1)
  }
  const min = Math.min(...colorCounts.values())
  const [minColor] = [...colorCounts.entries()].find(kv => kv[1] === min) ?? []
  return minColor == null ? COLORS[0] : COLOR_STRING_TO_COLOR.get(minColor) ?? COLORS[0]
}

// =================
// === AssetType ===
// =================

/** All possible types of directory entries. */
export enum AssetType {
  project = 'project',
  file = 'file',
  secret = 'secret',
  datalink = 'datalink',
  directory = 'directory',
  /**
   * A special {@link AssetType} representing the unknown items of a directory, before the
   * request to retrieve the items completes.
   */
  specialLoading = 'specialLoading',
  /** A special {@link AssetType} representing a directory listing that is empty. */
  specialEmpty = 'specialEmpty',
  /** A special {@link AssetType} representing a directory listing that errored. */
  specialError = 'specialError',
}

/** The corresponding ID newtype for each {@link AssetType}. */
export interface IdType {
  readonly [AssetType.project]: ProjectId
  readonly [AssetType.file]: FileId
  readonly [AssetType.datalink]: DatalinkId
  readonly [AssetType.secret]: SecretId
  readonly [AssetType.directory]: DirectoryId
  readonly [AssetType.specialLoading]: LoadingAssetId
  readonly [AssetType.specialEmpty]: EmptyAssetId
  readonly [AssetType.specialError]: ErrorAssetId
}

/**
 * Integers (starting from 0) corresponding to the order in which each asset type should appear
 * in a directory listing.
 */
export const ASSET_TYPE_ORDER: Readonly<Record<AssetType, number>> = {
  [AssetType.directory]: 0,
  [AssetType.project]: 1,
  [AssetType.file]: 2,
  [AssetType.datalink]: 3,
  [AssetType.secret]: 4,
  [AssetType.specialLoading]: 1000,
  [AssetType.specialEmpty]: 1000,
  [AssetType.specialError]: 1000,
}

// =============
// === Asset ===
// =============

/**
 * Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories.
 */
export interface BaseAsset {
  readonly id: AssetId
  readonly title: string
  readonly modifiedAt: dateTime.Rfc3339DateTime
  /**
   * This is defined as a generic {@link AssetId} in the backend, however it is more convenient
   * (and currently safe) to assume it is always a {@link DirectoryId}.
   */
  readonly parentId: DirectoryId
  readonly permissions: readonly AssetPermission[] | null
  readonly labels: readonly LabelName[] | null
  readonly description: string | null
}

/**
 * Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories.
 */
export interface Asset<Type extends AssetType = AssetType> extends BaseAsset {
  readonly type: Type
  readonly id: IdType[Type]
  readonly projectState: Type extends AssetType.project ? ProjectStateType : null
}

/** A convenience alias for {@link Asset}<{@link AssetType.directory}>. */
export type DirectoryAsset = Asset<AssetType.directory>

/** A convenience alias for {@link Asset}<{@link AssetType.project}>. */
export type ProjectAsset = Asset<AssetType.project>

/** A convenience alias for {@link Asset}<{@link AssetType.file}>. */
export type FileAsset = Asset<AssetType.file>

/** A convenience alias for {@link Asset}<{@link AssetType.datalink}>. */
export type DatalinkAsset = Asset<AssetType.datalink>

/** A convenience alias for {@link Asset}<{@link AssetType.secret}>. */
export type SecretAsset = Asset<AssetType.secret>

/** A convenience alias for {@link Asset}<{@link AssetType.specialLoading}>. */
export type SpecialLoadingAsset = Asset<AssetType.specialLoading>

/** A convenience alias for {@link Asset}<{@link AssetType.specialEmpty}>. */
export type SpecialEmptyAsset = Asset<AssetType.specialEmpty>

/** A convenience alias for {@link Asset}<{@link AssetType.specialError}>. */
export type SpecialErrorAsset = Asset<AssetType.specialError>

/**
 * Creates a {@link DirectoryAsset} representing the root directory for the organization,
 * with all irrelevant fields initialized to default values.
 */
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

/** Creates a {@link FileAsset} using the given values. */
export function createPlaceholderFileAsset(
  title: string,
  parentId: DirectoryId,
  assetPermissions: readonly AssetPermission[],
): FileAsset {
  return {
    type: AssetType.file,
    id: FileId(uniqueString.uniqueString()),
    title,
    parentId,
    permissions: assetPermissions,
    modifiedAt: dateTime.toRfc3339(new Date()),
    projectState: null,
    labels: [],
    description: null,
  }
}

/** Creates a {@link ProjectAsset} using the given values. */
export function createPlaceholderProjectAsset(
  title: string,
  parentId: DirectoryId,
  assetPermissions: readonly AssetPermission[],
  organization: User | null,
  path: Path | null,
): ProjectAsset {
  return {
    type: AssetType.project,
    id: ProjectId(uniqueString.uniqueString()),
    title,
    parentId,
    permissions: assetPermissions,
    modifiedAt: dateTime.toRfc3339(new Date()),
    projectState: {
      type: ProjectState.new,
      volumeId: '',
      ...(organization != null ? { openedBy: organization.email } : {}),
      ...(path != null ? { path } : {}),
    },
    labels: [],
    description: null,
  }
}

/**
 * Creates a {@link SpecialLoadingAsset}, with all irrelevant fields initialized to default
 * values.
 */
export function createSpecialLoadingAsset(directoryId: DirectoryId): SpecialLoadingAsset {
  return {
    type: AssetType.specialLoading,
    title: '',
    id: LoadingAssetId(uniqueString.uniqueString()),
    modifiedAt: dateTime.toRfc3339(new Date()),
    parentId: directoryId,
    permissions: [],
    projectState: null,
    labels: [],
    description: null,
  }
}

/**
 * Creates a {@link SpecialEmptyAsset}, with all irrelevant fields initialized to default
 * values.
 */
export function createSpecialEmptyAsset(directoryId: DirectoryId): SpecialEmptyAsset {
  return {
    type: AssetType.specialEmpty,
    title: '',
    id: EmptyAssetId(uniqueString.uniqueString()),
    modifiedAt: dateTime.toRfc3339(new Date()),
    parentId: directoryId,
    permissions: [],
    projectState: null,
    labels: [],
    description: null,
  }
}

/**
 * Creates a {@link SpecialErrorAsset}, with all irrelevant fields initialized to default
 * values.
 */
export function createSpecialErrorAsset(directoryId: DirectoryId): SpecialErrorAsset {
  return {
    type: AssetType.specialError,
    title: '',
    id: ErrorAssetId(uniqueString.uniqueString()),
    modifiedAt: dateTime.toRfc3339(new Date()),
    parentId: directoryId,
    permissions: [],
    projectState: null,
    labels: [],
    description: null,
  }
}

/** Any object with a `type` field matching the given `AssetType`. */
interface HasType<Type extends AssetType> {
  readonly type: Type
}

/** A union of all possible {@link Asset} variants. */
export type AnyAsset<Type extends AssetType = AssetType> = Extract<
  | DatalinkAsset
  | DirectoryAsset
  | FileAsset
  | ProjectAsset
  | SecretAsset
  | SpecialEmptyAsset
  | SpecialErrorAsset
  | SpecialLoadingAsset,
  HasType<Type>
>

/** A type guard that returns whether an {@link Asset} is a specific type of asset. */
export function assetIsType<Type extends AssetType>(type: Type) {
  return (asset: AnyAsset): asset is Extract<AnyAsset, Asset<Type>> => asset.type === type
}

/** Creates a new placeholder asset id for the given asset type. */
export function createPlaceholderAssetId<Type extends AssetType>(
  type: Type,
  id?: string,
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
    case AssetType.datalink: {
      result = DatalinkId(id)
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
    case AssetType.specialError: {
      result = ErrorAssetId(id)
      break
    }
  }
  return result as IdType[Type]
}

/** A type guard that returns whether an {@link Asset} is a {@link ProjectAsset}. */
export const assetIsProject = assetIsType(AssetType.project)
/** A type guard that returns whether an {@link Asset} is a {@link DirectoryAsset}. */
export const assetIsDirectory = assetIsType(AssetType.directory)
/** A type guard that returns whether an {@link Asset} is a {@link DatalinkAsset}. */
export const assetIsDatalink = assetIsType(AssetType.datalink)
/** A type guard that returns whether an {@link Asset} is a {@link SecretAsset}. */
export const assetIsSecret = assetIsType(AssetType.secret)
/** A type guard that returns whether an {@link Asset} is a {@link FileAsset}. */
export const assetIsFile = assetIsType(AssetType.file)

/** Metadata describing a specific version of an asset. */
export interface S3ObjectVersion {
  readonly versionId: S3ObjectVersionId
  readonly lastModified: dateTime.Rfc3339DateTime
  readonly isLatest: boolean
  /** An archive containing the all the project files object in the S3 bucket. */
  readonly key: string
}

/** A list of asset versions. */
export interface AssetVersions {
  readonly versions: S3ObjectVersion[]
}

// ===============================
// === compareAssetPermissions ===
// ===============================

/**
 * Return a positive number when `a > b`, a negative number when `a < b`, and `0`
 * when `a === b`.
 */
export function compareAssetPermissions(a: AssetPermission, b: AssetPermission) {
  const relativePermissionPrecedence =
    permissions.PERMISSION_ACTION_PRECEDENCE[a.permission] -
    permissions.PERMISSION_ACTION_PRECEDENCE[b.permission]
  if (relativePermissionPrecedence !== 0) {
    return relativePermissionPrecedence
  } else {
    // NOTE [NP]: Although `userId` is unique, and therefore sufficient to sort permissions, sort
    // name first, so that it's easier to find a permission in a long list (i.e., for readability).
    const aName = 'user' in a ? a.user.name : a.userGroup.groupName
    const bName = 'user' in b ? b.user.name : b.userGroup.groupName
    const aUserId = 'user' in a ? a.user.userId : a.userGroup.id
    const bUserId = 'user' in b ? b.user.userId : b.userGroup.id
    return (
      aName < bName ? -1
      : aName > bName ? 1
      : aUserId < bUserId ? -1
      : aUserId > bUserId ? 1
      : 0
    )
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
  readonly username: string | null
}

/** HTTP request body for the "change user group" endpoint. */
export interface ChangeUserGroupRequestBody {
  readonly userGroups: UserGroupId[]
}

/** HTTP request body for the "update organization" endpoint. */
export interface UpdateOrganizationRequestBody {
  readonly name?: string
  readonly email?: EmailAddress
  readonly website?: HttpsUrl
  readonly address?: string
}

/** HTTP request body for the "invite user" endpoint. */
export interface InviteUserRequestBody {
  readonly userEmail: EmailAddress
  readonly resend?: boolean
}

/** HTTP response body for the "list invitations" endpoint. */
export interface ListInvitationsResponseBody {
  readonly invitations: readonly Invitation[]
  readonly availableLicenses: number
}

/** Invitation to join an organization. */
export interface Invitation {
  readonly organizationId: OrganizationId
  readonly userEmail: EmailAddress
  readonly expireAt: dateTime.Rfc3339DateTime
}

/** HTTP request body for the "create permission" endpoint. */
export interface CreatePermissionRequestBody {
  readonly actorsIds: readonly UserPermissionIdentifier[]
  readonly resourceId: AssetId
  readonly action: permissions.PermissionAction | null
}

/** HTTP request body for the "create directory" endpoint. */
export interface CreateDirectoryRequestBody {
  readonly title: string
  readonly parentId: DirectoryId | null
}

/** HTTP request body for the "update directory" endpoint. */
export interface UpdateDirectoryRequestBody {
  readonly title: string
}

/** HTTP request body for the "update file" endpoint. */
export interface UpdateFileRequestBody {
  readonly title: string
}

/** HTTP request body for the "update asset" endpoint. */
export interface UpdateAssetRequestBody {
  readonly parentDirectoryId: DirectoryId | null
  readonly description: string | null
}

/** HTTP request body for the "delete asset" endpoint. */
export interface DeleteAssetRequestBody {
  readonly force: boolean
}

/** HTTP request body for the "create project" endpoint. */
export interface CreateProjectRequestBody {
  readonly projectName: string
  readonly projectTemplateName?: string
  readonly parentDirectoryId?: DirectoryId
  readonly datalinkId?: DatalinkId
}

/**
 * HTTP request body for the "update project" endpoint.
 * Only updates of the `projectName` or `ami` are allowed.
 */
export interface UpdateProjectRequestBody {
  readonly projectName: string | null
  readonly ami: Ami | null
  readonly ideVersion: VersionNumber | null
}

/** HTTP request body for the "open project" endpoint. */
export interface OpenProjectRequestBody {
  readonly executeAsync: boolean
  /** MUST be present on Remote backend; NOT REQUIRED on Local backend. */
  readonly cognitoCredentials: CognitoCredentials | null
  /** Only used by the Local backend. */
  readonly parentId: DirectoryId
}

/** HTTP request body for the "create secret" endpoint. */
export interface CreateSecretRequestBody {
  readonly name: string
  readonly value: string
  readonly parentDirectoryId: DirectoryId | null
}

/** HTTP request body for the "update secret" endpoint. */
export interface UpdateSecretRequestBody {
  readonly value: string
}

/** HTTP request body for the "create datalink" endpoint. */
export interface CreateDatalinkRequestBody {
  readonly name: string
  readonly value: unknown
  readonly parentDirectoryId: DirectoryId | null
  readonly datalinkId: DatalinkId | null
}

/** HTTP request body for the "create tag" endpoint. */
export interface CreateTagRequestBody {
  readonly value: string
  readonly color: LChColor
}

/** HTTP request body for the "create user group" endpoint. */
export interface CreateUserGroupRequestBody {
  readonly name: string
}

/** HTTP request body for the "create checkout session" endpoint. */
export interface CreateCheckoutSessionRequestBody {
  readonly plan: Plan
  readonly paymentMethodId: string
  readonly quantity: number
  readonly interval: number
}

/** URL query string parameters for the "list directory" endpoint. */
export interface ListDirectoryRequestParams {
  readonly parentId: DirectoryId | null
  readonly filterBy: FilterBy | null
  readonly labels: LabelName[] | null
  readonly recentProjects: boolean
}

/** URL query string parameters for the "upload file" endpoint. */
export interface UploadFileRequestParams {
  readonly fileId: AssetId | null
  // Marked as optional in the data type, however it is required by the actual route handler.
  readonly fileName: string
  readonly parentDirectoryId: DirectoryId | null
}

/** HTTP request body for the "upload file start" endpoint. */
export interface UploadFileStartRequestBody {
  readonly size: number
  readonly fileName: string
}

/** Metadata required to uploading a large file. */
export interface UploadLargeFileMetadata {
  readonly presignedUrls: readonly HttpsUrl[]
  readonly uploadId: string
  readonly sourcePath: S3FilePath
}

/** Metadata for each multipart upload. */
export interface S3MultipartPart {
  readonly eTag: string
  readonly partNumber: number
}

/** HTTP request body for the "upload file end" endpoint. */
export interface UploadFileEndRequestBody {
  readonly parentDirectoryId: DirectoryId | null
  readonly parts: readonly S3MultipartPart[]
  readonly sourcePath: S3FilePath
  readonly uploadId: string
  readonly assetId: AssetId | null
  readonly fileName: string
}

/** A large file that has finished uploading. */
export interface UploadedLargeFile {
  readonly id: FileId
  readonly project: null
}

/** A large project that has finished uploading. */
export interface UploadedLargeProject {
  readonly id: ProjectId
  readonly project: Project
}

/** A large asset (file or project) that has finished uploading. */
export type UploadedLargeAsset = UploadedLargeFile | UploadedLargeProject

/** URL query string parameters for the "upload profile picture" endpoint. */
export interface UploadPictureRequestParams {
  readonly fileName: string | null
}

/** URL query string parameters for the "list versions" endpoint. */
export interface ListVersionsRequestParams {
  readonly versionType: VersionType
  readonly default: boolean
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
  } else {
    const aModified = Number(new Date(a.modifiedAt))
    const bModified = Number(new Date(b.modifiedAt))
    const modifiedDelta = aModified - bModified
    if (modifiedDelta !== 0) {
      // Sort by date descending, rather than ascending.
      return -modifiedDelta
    } else {
      return (
        a.title > b.title ? 1
        : a.title < b.title ? -1
        : 0
      )
    }
  }
}

// ==================
// === getAssetId ===
// ==================

/**
 * A convenience function to get the `id` of an {@link Asset}.
 * This is useful to avoid React re-renders as it is not re-created on each function call.
 */
export function getAssetId<Type extends AssetType>(asset: Asset<Type>) {
  return asset.id
}

// ================================
// === userHasUserAndTeamSpaces ===
// ================================

/** Whether a user's root directory has the "Users" and "Teams" subdirectories. */
export function userHasUserAndTeamSpaces(user: User | null) {
  switch (user?.plan ?? null) {
    case null:
    case Plan.free:
    case Plan.solo: {
      return false
    }
    case Plan.team:
    case Plan.enterprise: {
      return true
    }
  }
}

// =====================
// === fileIsProject ===
// =====================

/** A subset of properties of the JS `File` type. */
interface JSFile {
  readonly name: string
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

/**
 * Return both the name and extension of the project file name (if any).
 * Otherwise, returns the entire name as the basename.
 */
export function extractProjectExtension(name: string) {
  const [, basename, extension] = name.match(/^(.*)[.](tar[.]gz|zip|enso-project)$/) ?? []
  return { basename: basename ?? name, extension: extension ?? '' }
}

/** Network error class. */
export class NetworkError extends Error {
  /**
   * Create a new instance of the {@link NetworkError} class.
   * @param message - The error message.
   * @param status - The HTTP status code.
   */
  constructor(
    message: string,
    readonly status?: number,
  ) {
    super(message)
  }
}
/** Error class for when the user is not authorized to access a resource. */
export class NotAuthorizedError extends NetworkError {}

// ===============
// === Backend ===
// ===============

/** Interface for sending requests to a backend that manages assets and runs projects. */
export default abstract class Backend {
  abstract readonly type: BackendType

  /** The path to the root directory of this {@link Backend}. */
  abstract rootPath(user: User): string
  /** Return the ID of the root directory, if known. */
  abstract rootDirectoryId(
    user: User,
    organization: OrganizationInfo | null,
    localRootDirectory: Path | null | undefined,
  ): DirectoryId | null
  /** Return a list of all users in the same organization. */
  abstract listUsers(): Promise<readonly User[]>
  /** Set the username of the current user. */
  abstract createUser(body: CreateUserRequestBody): Promise<User>
  /** Change the username of the current user. */
  abstract updateUser(body: UpdateUserRequestBody): Promise<void>
  /** Restore the current user. */
  abstract restoreUser(): Promise<void>
  /** Delete the current user. */
  abstract deleteUser(): Promise<void>
  /** Delete a user. */
  abstract removeUser(userId: UserId): Promise<void>
  /** Upload a new profile picture for the current user. */
  abstract uploadUserPicture(params: UploadPictureRequestParams, file: Blob): Promise<User>
  /** Set the list of groups a user is in. */
  abstract changeUserGroup(
    userId: UserId,
    userGroups: ChangeUserGroupRequestBody,
    name: string | null,
  ): Promise<User>
  /** Invite a new user to the organization by email. */
  abstract inviteUser(body: InviteUserRequestBody): Promise<void>
  /** Return a list of invitations to the organization. */
  abstract listInvitations(): Promise<ListInvitationsResponseBody>
  /** Delete an outgoing invitation. */
  abstract deleteInvitation(userEmail: EmailAddress): Promise<void>
  /** Resend an outgoing invitation. */
  abstract resendInvitation(userEmail: EmailAddress): Promise<void>
  /** Accept an incoming invitation to a new organization. */
  abstract acceptInvitation(): Promise<void>
  /** Decline an incoming invitation to a new organization. */
  abstract declineInvitation(userEmail: string): Promise<void>
  /** Get the details of the current organization. */
  abstract getOrganization(): Promise<OrganizationInfo | null>
  /** Change the details of the current organization. */
  abstract updateOrganization(body: UpdateOrganizationRequestBody): Promise<OrganizationInfo | null>
  /** Upload a new profile picture for the current organization. */
  abstract uploadOrganizationPicture(
    params: UploadPictureRequestParams,
    file: Blob,
  ): Promise<OrganizationInfo>
  /** Adds a permission for a specific user on a specific asset. */
  abstract createPermission(body: CreatePermissionRequestBody): Promise<void>
  /** Return user details for the current user. */
  abstract usersMe(): Promise<User | null>
  /** Return a list of assets in a directory. */
  abstract listDirectory(
    query: ListDirectoryRequestParams,
    title: string,
  ): Promise<readonly AnyAsset[]>
  /** Create a directory. */
  abstract createDirectory(body: CreateDirectoryRequestBody): Promise<CreatedDirectory>
  /** Change the name of a directory. */
  abstract updateDirectory(
    directoryId: DirectoryId,
    body: UpdateDirectoryRequestBody,
    title: string,
  ): Promise<UpdatedDirectory>
  /** List previous versions of an asset. */
  abstract listAssetVersions(assetId: AssetId, title: string | null): Promise<AssetVersions>
  /** Change the parent directory of an asset. */
  abstract updateAsset(assetId: AssetId, body: UpdateAssetRequestBody, title: string): Promise<void>
  /** Delete an arbitrary asset. */
  abstract deleteAsset(assetId: AssetId, body: DeleteAssetRequestBody, title: string): Promise<void>
  /** Restore an arbitrary asset from the trash. */
  abstract undoDeleteAsset(assetId: AssetId, title: string): Promise<void>
  /** Copy an arbitrary asset to another directory. */
  abstract copyAsset(
    assetId: AssetId,
    parentDirectoryId: DirectoryId,
    title: string,
    parentDirectoryTitle: string,
  ): Promise<CopyAssetResponse>
  /** Return a list of projects belonging to the current user. */
  abstract listProjects(): Promise<readonly ListedProject[]>
  /** Create a project for the current user. */
  abstract createProject(body: CreateProjectRequestBody): Promise<CreatedProject>
  /** Close a project. */
  abstract closeProject(projectId: ProjectId, title: string): Promise<void>
  /** Return a list of sessions for the current project. */
  abstract listProjectSessions(
    projectId: ProjectId,
    title: string,
  ): Promise<readonly ProjectSession[]>
  /** Restore a project from a different version. */
  abstract restoreProject(
    projectId: ProjectId,
    versionId: S3ObjectVersionId,
    title: string,
  ): Promise<void>
  /** Duplicate a specific version of a project. */
  abstract duplicateProject(
    projectId: ProjectId,
    versionId: S3ObjectVersionId,
    title: string,
  ): Promise<CreatedProject>
  /** Return project details. */
  abstract getProjectDetails(
    projectId: ProjectId,
    directoryId: DirectoryId | null,
    title: string,
  ): Promise<Project>
  /** Return Language Server logs for a project session. */
  abstract getProjectSessionLogs(
    projectSessionId: ProjectSessionId,
    title: string,
  ): Promise<readonly string[]>
  /** Set a project to an open state. */
  abstract openProject(
    projectId: ProjectId,
    body: OpenProjectRequestBody | null,
    title: string,
  ): Promise<void>
  /** Change the AMI or IDE version of a project. */
  abstract updateProject(
    projectId: ProjectId,
    body: UpdateProjectRequestBody,
    title: string,
  ): Promise<UpdatedProject>
  /** Fetch the content of the `Main.enso` file of a project. */
  abstract getFileContent(projectId: ProjectId, version: string, title: string): Promise<string>
  /** Return project memory, processor and storage usage. */
  abstract checkResources(projectId: ProjectId, title: string): Promise<ResourceUsage>
  /** Return a list of files accessible by the current user. */
  abstract listFiles(): Promise<readonly FileLocator[]>
  /** Begin uploading a large file. */
  abstract uploadFileStart(
    body: UploadFileRequestParams,
    file: File,
  ): Promise<UploadLargeFileMetadata>
  /** Upload a chunk of a large file. */
  abstract uploadFileChunk(url: HttpsUrl, file: Blob, index: number): Promise<S3MultipartPart>
  /** Finish uploading a large file. */
  abstract uploadFileEnd(body: UploadFileEndRequestBody): Promise<UploadedLargeAsset>
  /** Change the name of a file. */
  abstract updateFile(fileId: FileId, body: UpdateFileRequestBody, title: string): Promise<void>
  /** Return file details. */
  abstract getFileDetails(fileId: FileId, title: string): Promise<FileDetails>
  /** Create a Datalink. */
  abstract createDatalink(body: CreateDatalinkRequestBody): Promise<DatalinkInfo>
  /** Return a Datalink. */
  abstract getDatalink(datalinkId: DatalinkId, title: string | null): Promise<Datalink>
  /** Delete a Datalink. */
  abstract deleteDatalink(datalinkId: DatalinkId, title: string | null): Promise<void>
  /** Create a secret environment variable. */
  abstract createSecret(body: CreateSecretRequestBody): Promise<SecretId>
  /** Return a secret environment variable. */
  abstract getSecret(secretId: SecretId, title: string): Promise<Secret>
  /** Change the value of a secret. */
  abstract updateSecret(
    secretId: SecretId,
    body: UpdateSecretRequestBody,
    title: string,
  ): Promise<void>
  /** Return the secret environment variables accessible by the user. */
  abstract listSecrets(): Promise<readonly SecretInfo[]>
  /** Create a label used for categorizing assets. */
  abstract createTag(body: CreateTagRequestBody): Promise<Label>
  /** Return all labels accessible by the user. */
  abstract listTags(): Promise<readonly Label[]>
  /** Set the full list of labels for a specific asset. */
  abstract associateTag(
    assetId: AssetId,
    tagIds: readonly LabelName[],
    title: string,
  ): Promise<void>
  /** Delete a label. */
  abstract deleteTag(tagId: TagId, value: LabelName): Promise<void>
  /** Create a user group. */
  abstract createUserGroup(body: CreateUserGroupRequestBody): Promise<UserGroupInfo>
  /** Delete a user group. */
  abstract deleteUserGroup(userGroupId: UserGroupId, name: string): Promise<void>
  /** Return all user groups in the organization. */
  abstract listUserGroups(): Promise<readonly UserGroupInfo[]>
  /** Return a list of backend or IDE versions. */
  abstract listVersions(params: ListVersionsRequestParams): Promise<readonly Version[]>
  /** Create a payment checkout session. */
  abstract createCheckoutSession(body: CreateCheckoutSessionRequestBody): Promise<CheckoutSession>
  /** Get the status of a payment checkout session. */
  abstract getCheckoutSession(sessionId: CheckoutSessionId): Promise<CheckoutSessionStatus>
  /** List events in the organization's audit log. */
  abstract getLogEvents(): Promise<readonly Event[]>
  /** Log an event that will be visible in the organization audit log. */
  abstract logEvent(
    message: string,
    projectId?: string | null,
    metadata?: object | null,
  ): Promise<void>
  /** Download from an arbitrary URL that is assumed to originate from this backend. */
  abstract download(url: string, name?: string): Promise<void>

  /**
   * Get the URL for the customer portal.
   * @see https://stripe.com/docs/billing/subscriptions/integrating-customer-portal
   * @param returnUrl - The URL to redirect to after the customer visits the portal.
   */
  abstract createCustomerPortalSession(returnUrl: string): Promise<string | null>
}
