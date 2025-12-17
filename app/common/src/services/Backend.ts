/** @file Type definitions common between all backends. */
import { z } from 'zod'
import type { DownloadOptions } from '../download.js'
import {
  getText,
  resolveDictionary,
  type DefaultGetText,
  type Replacements,
  type TextId,
} from '../text.js'
import * as dateTime from '../utilities/data/dateTime.js'
import * as newtype from '../utilities/data/newtype.js'
import * as permissions from '../utilities/permissions.js'
import { getFileDetailsPath } from './Backend/remoteBackendPaths.js'
import {
  ApiKeyId,
  DatalinkId,
  DirectoryId,
  EnsoPath,
  FileId,
  MetadataId,
  PaginationToken,
  ParentsPath,
  Path,
  ProjectId,
  SecretId,
  VirtualParentsPath,
  type Address,
  type AssetId,
  type CredentialInput,
  type EmailAddress,
  type HttpsUrl,
  type LabelName,
  type OrganizationId,
  type ProjectExecutionId,
  type ProjectSessionId,
  type S3FilePath,
  type S3ObjectVersionId,
  type SubscriptionId,
  type TagId,
  type UnzipAssetsJobId,
  type UserGroupId,
  type UserId,
  type UserPermissionIdentifier,
} from './Backend/types.js'
import { HttpClient, type HttpClientPostOptions, type ResponseWithTypedJson } from './HttpClient.js'
export { prettifyError } from 'zod/v4'

export * from './Backend/types.js'

/** HTTP status indicating that the request was successful, but the user is not authorized to access. */
const STATUS_NOT_AUTHORIZED = 401

/** The size, in bytes, of the chunks which the backend accepts. */
export const S3_CHUNK_SIZE_BYTES = 10_000_000

/** The internal asset type and properly typed corresponding internal ID of an arbitrary asset. */
interface AssetTypeAndIdRaw<Type extends AssetType> {
  readonly type: Type
  readonly path: Path
}

/** The internal asset type and properly typed corresponding internal ID of an arbitrary asset. */
type AssetTypeAndId<Id extends AssetId = AssetId> =
  | (DirectoryId extends Id ? AssetTypeAndIdRaw<AssetType.directory> : never)
  | (FileId extends Id ? AssetTypeAndIdRaw<AssetType.file> : never)
  | (ProjectId extends Id ? AssetTypeAndIdRaw<AssetType.project> : never)

export function extractTypeAndPath<Id extends AssetId>(id: Id): AssetTypeAndId<Id>
/**
 * Extracts the asset type and its corresponding internal ID from a {@link AssetId}.
 * @throws {Error} if the id has an unknown type.
 */
export function extractTypeAndPath<Id extends AssetId>(id: Id): AssetTypeAndId {
  const [, typeRaw, idRaw = ''] = id.match(/(.+?)-(.+)/) ?? []

  switch (typeRaw) {
    case AssetType.directory:
    case AssetType.project:
    case AssetType.file: {
      return {
        type: typeRaw,
        path: Path(decodeURIComponent(idRaw)),
      }
    }
    case undefined:
    default: {
      throw new Error(`Invalid type '${typeRaw}'`)
    }
  }
}

/**
 * Interface used to log logs, errors, etc.
 *
 * In the browser, this is the `Console` interface. In Electron, this is the `Logger` interface
 * provided by the EnsoGL packager.
 */
export interface Logger {
  /** Log a message to the console. */
  readonly log: (message: unknown, ...optionalParams: unknown[]) => void
  /** Log an error message to the console. */
  readonly error: (message: unknown, ...optionalParams: unknown[]) => void
}

/** The {@link Backend} variant. If a new variant is created, it should be added to this enum. */
export enum BackendType {
  local = 'local',
  remote = 'remote',
}

/** Check if this path points to an asset in cloud drive. */
export function isRemoteAssetPath(ensoPath: EnsoPath): ensoPath is EnsoPath & `enso://${string}` {
  return ensoPath.startsWith('enso://')
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
  /**
   * Contains the IDs of the user groups that the user is a member of.
   * @deprecated Use `groups` instead.
   */
  readonly userGroups: readonly UserGroupId[] | null
  readonly removeAt?: dateTime.Rfc3339DateTime | null
  readonly plan: Plan
  /**
   * Contains the user groups that the user is a member of.
   * Has enriched metadata, like the name of the group and the home directory ID.
   */
  readonly groups?: readonly UserGroup[]
  /** Whether the user is a member of the Enso team. */
  readonly isEnsoTeamMember: boolean
  /** Information about any pending invitation to a different organization / team. */
  readonly invitation?: Invitation
}

/** A user group related to the current user. */
export interface UserGroup {
  readonly id: UserGroupId
  readonly name: string
  readonly homeDirectoryId: DirectoryId
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
  hybridOpenInProgress = 'HybridOpenInProgress',
  hybridOpened = 'HybridOpened',
  closed = 'Closed',
  /**
   * A frontend-specific state, representing a project that should be displayed as
   * `openInProgress`, but has not yet been added to the backend.
   */
  placeholder = 'Placeholder',
}

/** Wrapper around a project state value. */
export interface ProjectStateType {
  readonly type: ProjectState
  readonly volumeId?: string
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
  [ProjectState.hybridOpenInProgress]: true,
  [ProjectState.hybridOpened]: false,
  [ProjectState.closed]: false,
  [ProjectState.placeholder]: true,
}

export const IS_OPENING_OR_OPENED: Readonly<Record<ProjectState, boolean>> = {
  [ProjectState.created]: false,
  [ProjectState.new]: false,
  [ProjectState.scheduled]: true,
  [ProjectState.openInProgress]: true,
  [ProjectState.provisioned]: true,
  [ProjectState.hybridOpenInProgress]: true,
  [ProjectState.opened]: true,
  [ProjectState.hybridOpened]: true,
  [ProjectState.closed]: false,
  [ProjectState.placeholder]: true,
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
  readonly ensoPath: EnsoPath
}

/** A `Project` returned by `updateProject`. */
export interface UpdatedProject {
  readonly organizationId: OrganizationId
  readonly projectId: ProjectId
  readonly name: string
  readonly state: ProjectStateType
  readonly packageName: string
}

/** A user/organization's project containing and/or currently executing code. */
export interface ProjectRaw extends CreatedProject {
  readonly address?: Address
  readonly currentSessionId?: ProjectSessionId
  readonly openedBy?: EmailAddress
  /** On the Remote (Cloud) Backend, this is a S3 url that is valid for only 120 seconds. */
  readonly url?: HttpsUrl
}

/** A user/organization's project containing and/or currently executing code. */
export interface Project extends CreatedProject {
  readonly binaryAddress: Address | null
  readonly jsonAddress: Address | null
  readonly ydocAddress: Address | null
  readonly currentSessionId?: ProjectSessionId
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

export interface ProjectSessionLogs {
  readonly scrollId: string
  readonly hits: readonly string[]
}

export const PROJECT_PARALLEL_MODES = ['ignore', 'restart', 'parallel'] as const

export const PARALLEL_MODE_TO_TEXT_ID = {
  ignore: 'ignoreParallelMode',
  restart: 'restartParallelMode',
  parallel: 'parallelParallelMode',
} satisfies {
  [K in ProjectParallelMode]: TextId & `${K}ParallelMode`
}

export const PARALLEL_MODE_TO_DESCRIPTION_ID = {
  ignore: 'ignoreParallelModeDescription',
  restart: 'restartParallelModeDescription',
  parallel: 'parallelParallelModeDescription',
} satisfies {
  [K in ProjectParallelMode]: TextId & `${K}ParallelModeDescription`
}

/**
 * The behavior when manually starting a new execution when the previous one is not yet complete.
 * One of the following:
 * - `ignore` - do not start the new execution.
 * - `restart` - stop the old execution and start the new execution.
 * - `parallel` - keep the old execution running but also run the new execution.
 */
export type ProjectParallelMode = (typeof PROJECT_PARALLEL_MODES)[number]

export const PROJECT_EXECUTION_REPEAT_TYPES = [
  'none',
  'daily',
  'weekly',
  'monthlyDate',
  'monthlyWeekday',
  'monthlyLastWeekday',
] as const

export const PROJECT_EXECUTION_REPEAT_TYPE_TO_TEXT_ID = {
  none: 'noneProjectExecutionRepeatType',
  daily: 'dailyProjectExecutionRepeatType',
  weekly: 'weeklyProjectExecutionRepeatType',
  monthlyDate: 'monthlyProjectExecutionRepeatType',
  monthlyWeekday: 'monthlyProjectExecutionRepeatType',
  monthlyLastWeekday: 'monthlyProjectExecutionRepeatType',
} satisfies {
  readonly [K in ProjectExecutionRepeatType]: TextId & `${string}ProjectExecutionRepeatType`
}

/** The interval at which a project schedule repeats. */
export type ProjectExecutionRepeatType = ProjectExecutionRepeatInfo['type']

/** Details for a project execution that repeats hourly. */
export interface ProjectExecutionNoneRepeatInfo {
  readonly type: 'none'
}

/** Details for a project execution that repeats daily. */
export interface ProjectExecutionDailyRepeatInfo {
  readonly type: 'daily'
}

/** Details for a project execution that repeats weekly on one or more days. */
export interface ProjectExecutionWeeklyRepeatInfo {
  readonly type: 'weekly'
  readonly daysOfWeek: readonly number[]
}

/** Details for a project execution that repeats monthly on a specific date. */
export interface ProjectExecutionMonthlyDateRepeatInfo {
  readonly type: 'monthlyDate'
  readonly date: number
  readonly months: readonly number[]
}

/**
 * Details for a project execution that repeats monthly on a specific weekday of a specific week
 * of a specific month.
 */
export interface ProjectExecutionMonthlyWeekdayRepeatInfo {
  readonly type: 'monthlyWeekday'
  readonly weekNumber: number
  readonly dayOfWeek: number
  readonly months: readonly number[]
}

/**
 * Details for a project execution that repeats monthly on a specific weekday of the last week
 * of a specific month.
 */
export interface ProjectExecutionMonthlyLastWeekdayRepeatInfo {
  readonly type: 'monthlyLastWeekday'
  readonly dayOfWeek: number
  readonly months: readonly number[]
}

export type ProjectExecutionRepeatInfo =
  | ProjectExecutionDailyRepeatInfo
  | ProjectExecutionWeeklyRepeatInfo
  | ProjectExecutionMonthlyDateRepeatInfo
  | ProjectExecutionMonthlyWeekdayRepeatInfo
  | ProjectExecutionMonthlyLastWeekdayRepeatInfo
  | ProjectExecutionNoneRepeatInfo

/** Metadata for a {@link ProjectExecution}. */
export interface ProjectExecutionInfo {
  readonly projectId: ProjectId
  readonly repeat: ProjectExecutionRepeatInfo
  readonly startDate: dateTime.Rfc3339DateTime
  readonly endDate: dateTime.Rfc3339DateTime | null
  readonly timeZone: dateTime.IanaTimeZone
  readonly maxDurationMinutes: number
  readonly parallelMode: ProjectParallelMode
}

/** A specific execution schedule of a project. */
export interface ProjectExecution extends ProjectExecutionInfo {
  readonly executionId: ProjectExecutionId
  readonly organizationId: OrganizationId
  readonly versionId: S3ObjectVersionId
  readonly nextExecution: dateTime.Rfc3339DateTime
  readonly projectSessions?: readonly ProjectSession[]
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

export const PLANS: readonly Plan[] = Object.values(Plan)

/** Whether a given value is a {@link Plan}. */
export function isPlan(value: unknown): value is Plan {
  const plans: readonly unknown[] = PLANS
  return plans.includes(value)
}

/** Metadata for a payment checkout session. */
export interface CheckoutSession {
  readonly url: HttpsUrl
}

/** Metadata for a single payment card. */
export interface Card {
  readonly plan: Plan
  readonly period: PlanBillingPeriod
  readonly title: string
  readonly subtitle: string
  readonly pricing: string
  readonly features: readonly string[]
}

/** Metadata for a payment pricing page configuration. */
export interface PaymentsConfig {
  readonly cards: readonly Card[]
}

/** Metadata for a subscription. */
export interface Subscription {
  readonly id?: SubscriptionId
  readonly plan?: Plan
  readonly trialStart?: dateTime.Rfc3339DateTime | null
  readonly trialEnd?: dateTime.Rfc3339DateTime | null
  readonly isPaused?: boolean | null
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

/** The format of all errors returned by the backend. */
export interface RemoteBackendError {
  readonly type: string
  readonly code: string
  readonly message: string
  readonly param: string
}

/** HTTP response body for the "list api keys" endpoint. */
export interface ListApiKeysResponse {
  readonly credentials: readonly ApiKey[]
}

/** HTTP response body for the "list users" endpoint. */
export interface ListUsersResponseBody {
  readonly users: readonly User[]
}

/** HTTP response body for the "list directory" endpoint. */
export interface ListDirectoryResponseBody {
  readonly assets: readonly AnyAsset[]
  /** `null` if and only if this is the last page. */
  readonly paginationToken: PaginationToken | null
}

/** HTTP response body for the "list files" endpoint. */
export interface ListFilesResponseBody {
  readonly files: readonly FileLocator[]
}

/** HTTP response body for the "list secrets" endpoint. */
export interface ListSecretsResponseBody {
  readonly secrets: readonly SecretInfo[]
}

/** HTTP response body for the "list tag" endpoint. */
export interface ListTagsResponseBody {
  readonly tags: readonly Label[]
}

/**
 * Response from the "create customer portal session" endpoint.
 * Returns a URL that the user can use to access the customer portal and manage their subscription.
 */
export interface CreateCustomerPortalSessionResponse {
  readonly url: string | null
}

/** Whether a type is `any`. */
type IsAny<T> = 0 extends 1 & T ? true : false

/** Response from "assets/${assetId}" endpoint. */
export type AssetDetailsResponse<Id extends AssetId> =
  // `T extends T` where `T` is a type parameter is a trick to distribute union values,
  // evaluating the conditional type for each member of the union type,
  // and then resolving to a union of the results of this operation.
  IsAny<Id> extends true ? AssetDetailsResponse<AssetId>
  : | (Id extends Id ? AnyAsset<AssetTypeFromId<Id>> & { readonly metadataId: MetadataId } : never)
    | null

/** Whether the user is on a plan with multiple seats (i.e. a plan that supports multiple users). */
export function isUserOnPlanWithMultipleSeats(user: User) {
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

/** The type returned from the "copy asset" endpoint. */
export interface CopyAssetResponse {
  readonly asset: AnyAsset
}

/** Possible filters for the "list directory" endpoint. */
export enum FilterBy {
  all = 'All',
  active = 'Active',
  recent = 'Recent',
  trashed = 'Trashed',
}

/** An event in an audit log. */
export interface AuditLogEvent {
  readonly organizationId: OrganizationId
  readonly userEmail: EmailAddress
  readonly timestamp: dateTime.Rfc3339DateTime | null
  /** The type is called `EventType` in the backend. */
  readonly metadata: EventMetadata | null
  readonly message: string | null
  readonly projectId: ProjectId | null
  readonly url: string | null
  readonly method: string | null
  readonly lambdaKind: string | null
}

/** Possible types of event in an audit log. */
export enum EventType {
  GetSecret = 'getSecret',
  DeleteAssets = 'deleteAssets',
  ListSecrets = 'listSecrets',
  OpenProject = 'openProject',
  UploadFile = 'uploadFile',
  Lib = 'lib',
  Telemetry = 'telemetry',
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

/** An event indicating that an action was performed by the Standard libraries. */
interface LibEventMetadata {
  readonly type: EventType.Lib
}

/** An event indicating telemetry data sent from the IDE. */
interface TelemetryEventMetadata {
  readonly type: EventType.Telemetry
}

/** All possible types of metadata for an event in the audit log. */
export type EventMetadata =
  | DeleteAssetsEventMetadata
  | GetSecretEventMetadata
  | ListSecretsEventMetadata
  | OpenProjectEventMetadata
  | UploadFileEventMetadata
  | LibEventMetadata
  | TelemetryEventMetadata

/** A color in the LCh colorspace. */
export interface LChColor {
  readonly lightness: number
  readonly chroma: number
  readonly hue: number
  readonly alpha?: number | undefined
}

/** Type used when creating api key credential. */
export interface CreateApiKeyRequestBody {
  readonly name: string
  readonly description: string
  readonly expiresIn: ApiKeyExpiresIn
}

/** Api key credential. */
export interface ApiKey {
  readonly id: ApiKeyId
  // Field populated only once after creation.
  readonly secretId: string | null
  readonly name: string
  readonly description: string
  readonly createdAt: dateTime.Rfc3339DateTime
  readonly lastUsedAt: dateTime.Rfc3339DateTime | null
  readonly expiresAt: dateTime.Rfc3339DateTime | null
  readonly expiresIn: ApiKeyExpiresIn
}

/** Possible types of lifetime span for api key credentials. */
export enum ApiKeyExpiresIn {
  Week = 'Week',
  Month = 'Month',
  Year = 'Year',
  Indefinetly = 'Indefinetly',
}

export const API_KEY_EXPIRES_IN_VALUES: readonly ApiKeyExpiresIn[] = Object.values(ApiKeyExpiresIn)

/** A pre-selected list of colors to be used in color pickers. */
export const COLORS = [
  // Red
  { lightness: 50, chroma: 66, hue: 7 },
  // Orange
  { lightness: 50, chroma: 66, hue: 34 },
  // Yellow
  { lightness: 50, chroma: 66, hue: 80 },
  // Green
  { lightness: 50, chroma: 66, hue: 139 },
  // Teal
  { lightness: 50, chroma: 66, hue: 172 },
  // Blue
  { lightness: 50, chroma: 66, hue: 271 },
  // Purple
  { lightness: 50, chroma: 66, hue: 295 },
  // Pink
  { lightness: 50, chroma: 66, hue: 332 },
  // Light blueish grey
  { lightness: 50, chroma: 22, hue: 252 },
  // Dark blueish grey
  { lightness: 22, chroma: 13, hue: 252 },
] as const satisfies LChColor[]

export const FALLBACK_COLOR = COLORS[0]

/** Returns true if the two colors are equal. */
export function colorsAreEqual(a: LChColor, b: LChColor) {
  return (
    a.lightness === b.lightness && a.chroma === b.chroma && a.hue === b.hue && a.alpha === b.alpha
  )
}

/** Converts a {@link LChColor} to a CSS color string. */
export function lChColorToCssColor(color: LChColor): string {
  const alpha = 'alpha' in color ? ` / ${color.alpha}` : ''
  return `lch(${color.lightness}% ${color.chroma} ${color.hue}${alpha})`
}

export const COLOR_STRING_TO_COLOR = new Map(
  COLORS.map((color) => [lChColorToCssColor(color), color]),
)

export const INITIAL_COLOR_COUNTS = new Map(COLORS.map((color) => [lChColorToCssColor(color), 0]))

/** The color that is used for the least labels. Ties are broken by order. */
export function findLeastUsedColor(labels: Iterable<Label>) {
  const colorCounts = new Map(INITIAL_COLOR_COUNTS)
  for (const label of labels) {
    const colorString = lChColorToCssColor(label.color)
    colorCounts.set(colorString, (colorCounts.get(colorString) ?? 0) + 1)
  }
  const min = Math.min(...colorCounts.values())
  const [minColor] = [...colorCounts.entries()].find((kv) => kv[1] === min) ?? []
  return minColor == null ? COLORS[0] : (COLOR_STRING_TO_COLOR.get(minColor) ?? COLORS[0])
}

/** All possible types of directory entries. */
export enum AssetType {
  project = 'project',
  file = 'file',
  secret = 'secret',
  datalink = 'datalink',
  directory = 'directory',
}

export const ASSET_TYPE_TO_TEXT_ID: Readonly<Record<AssetType, TextId>> = {
  [AssetType.directory]: 'directoryAssetType',
  [AssetType.project]: 'projectAssetType',
  [AssetType.file]: 'fileAssetType',
  [AssetType.secret]: 'secretAssetType',
  [AssetType.datalink]: 'datalinkAssetType',
} satisfies { [Type in AssetType]: `${Type}AssetType` }

export enum ReplaceableAssetType {
  project = 'project',
  file = 'file',
  datalink = 'datalink',
  secret = 'secret',
}

/** The types of assets that can be retrieved from the backend. */
export type RealAssetType =
  | AssetType.project
  | AssetType.file
  | AssetType.datalink
  | AssetType.secret
  | AssetType.directory

/** The corresponding ID newtype for each {@link AssetType}. */
export interface IdType {
  readonly [AssetType.project]: ProjectId
  readonly [AssetType.file]: FileId
  readonly [AssetType.datalink]: DatalinkId
  readonly [AssetType.secret]: SecretId
  readonly [AssetType.directory]: DirectoryId
}

type AssetTypeFromId<Id extends AssetId> =
  Id extends ProjectId ? AssetType.project
  : Id extends FileId ? AssetType.file
  : Id extends DatalinkId ? AssetType.datalink
  : Id extends SecretId ? AssetType.secret
  : Id extends DirectoryId ? AssetType.directory
  : never

/**
 * Integers (starting from 0) corresponding to the order in which each asset type should appear
 * in a directory listing.
 */
export const ASSET_TYPE_ORDER: Readonly<Record<AssetType, number>> = {
  [AssetType.directory]: 0,
  [AssetType.project]: -1,
  [AssetType.file]: -2,
  [AssetType.datalink]: -3,
  [AssetType.secret]: -4,
}

/** A state associated with a credential. */
export type CredentialSecretState = 'Expired' | 'Ready' | 'WaitingForAuthentication'

/** Metadata associated with a credential asset. */
export interface CredentialMetadata {
  readonly serviceName: string
  readonly expirationDate?: dateTime.Rfc3339DateTime
  readonly state: CredentialSecretState
}

/**
 * Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories.
 */
export interface Asset<Type extends AssetType = AssetType> {
  readonly type: Type
  readonly id: IdType[Type]
  readonly title: string
  readonly modifiedAt: dateTime.Rfc3339DateTime
  /**
   * This is defined as a generic {@link AssetId} in the backend, however it is more convenient
   * (and currently safe) to assume it is always a {@link DirectoryId}.
   */
  readonly parentId: DirectoryId
  readonly permissions: readonly AssetPermission[] | null
  readonly labels?: readonly LabelName[] | undefined
  readonly description?: string | undefined
  /** Asset data for a project */
  readonly projectState: Type extends AssetType.project ? ProjectStateType : null
  /** Asset data for a file */
  readonly extension: Type extends AssetType.file ? string : null
  /** Asset data for a credential (secret) */
  readonly credentialMetadata?: Type extends AssetType.secret ? CredentialMetadata : undefined
  readonly parentsPath: ParentsPath
  readonly virtualParentsPath: VirtualParentsPath
  /** The display path. */
  readonly ensoPath: EnsoPath
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

/** Whether a given asset represents a credential. */
export function isAssetCredential(
  asset: Asset,
): asset is SecretAsset & { credentialMetadata: CredentialMetadata } {
  return asset.type === 'secret' && asset.credentialMetadata !== undefined
}

/** Whether an asset can be downloaded. */
export function isDownloadableAsset(type: AssetType | undefined) {
  return type !== AssetType.secret
}

/** Any object with a `type` field matching the given `AssetType`. */
interface HasType<Type extends AssetType> {
  readonly type: Type
}

/** A union of all possible {@link Asset} variants. */
export type AnyAsset<Type extends AssetType = AssetType> = Extract<
  DatalinkAsset | DirectoryAsset | FileAsset | ProjectAsset | SecretAsset,
  HasType<Type>
>

/** A union of all {@link Asset} variants that can be retrieved from the backend. */
export type AnyRealAsset = AnyAsset<RealAssetType>

/** A type guard that returns whether an {@link Asset} is a specific type of asset. */
export function assetIsType<Type extends AssetType>(type: Type) {
  return (asset: AnyAsset): asset is Extract<AnyAsset, Asset<Type>> => asset.type === type
}

/** Extract the type of an id and return a discriminated union containing both id and type. */
export function extractTypeFromId(id: AssetId): AnyAsset extends infer T ?
  T extends T ?
    Pick<T, ('id' | 'type') & keyof T>
  : never
: never {
  return {
    type: id.match(/^(.+?)-/)?.[1],
    id,
  } as never
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
  readonly user?: OtherUser
}

/** A user other than the current user */
export interface OtherUser {
  readonly name: string
  readonly email: EmailAddress
  readonly profilePicture?: HttpsUrl | null
}

/** A list of asset versions. */
export interface AssetVersions {
  readonly versions: S3ObjectVersion[]
}

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
    const aName = getAssetPermissionName(a)
    const bName = getAssetPermissionName(b)
    const aUserId = getAssetPermissionId(a)
    const bUserId = getAssetPermissionId(b)
    return (
      aName < bName ? -1
      : aName > bName ? 1
      : aUserId < bUserId ? -1
      : aUserId > bUserId ? 1
      : 0
    )
  }
}

/** HTTP request body for the "set username" endpoint. */
export interface CreateUserRequestBody {
  readonly userName: string
  readonly userEmail: EmailAddress
  readonly organizationId: OrganizationId | null
}

/** HTTP request body for the "update user" endpoint. */
export interface UpdateUserRequestBody {
  readonly username?: string
  readonly organizationId?: OrganizationId
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
  readonly maxLicenses: number
}

/** Invitation to join an organization. */
export interface Invitation {
  readonly organizationId: OrganizationId
  readonly organizationName: string
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
  readonly title: string | null
  readonly metadataId: MetadataId | null
}

/** HTTP request body for the "delete asset" endpoint. */
export interface DeleteAssetRequestBody {
  readonly force: boolean
}

/** HTTP request body for the "create project" endpoint. */
export interface CreateProjectRequestBody {
  readonly projectName: string
  readonly parentDirectoryId?: DirectoryId
  readonly ensoPath?: string
}

/**
 * HTTP request body for the "update project" endpoint.
 * Only updates of the `projectName` or `ami` are allowed.
 */
export interface UpdateProjectRequestBody {
  readonly projectName: string | null
}

/** Extra parameters required when opening the project in hybrid mode. */
export interface OpenHybridProjectParameters {
  /** Cloud project directory path. */
  readonly cloudProjectDirectoryPath: EnsoPath
  /** Cloud project id. */
  readonly cloudProjectId: ProjectId
  /** Cloud project session id. */
  readonly cloudProjectSessionId: ProjectSessionId
}

/** HTTP request body for the "open project" endpoint. */
export interface OpenProjectRequestBody {
  readonly executeAsync: boolean
  /** MUST be present on Remote backend; NOT REQUIRED on Local backend. */
  readonly cognitoCredentials: CognitoCredentials | null
  /** Extra parameters required when running in hybrid mode. */
  readonly openHybridProjectParameters: OpenHybridProjectParameters | null
}

/** HTTP request body for the "create project execution" endpoint. */
export interface CreateProjectExecutionRequestBody extends ProjectExecutionInfo {}

/** HTTP request body for the "update project execution" endpoint. */
export interface UpdateProjectExecutionRequestBody {
  readonly enabled?: boolean | undefined
}

/** HTTP request body for the "create secret or credential" endpoint. */
export type CreateSecretOrCredentialRequestBody =
  | CreateSecretRequestBody
  | CreateCredentialRequestBody

/** HTTP request body for the "create secret" endpoint. */
export interface CreateSecretRequestBody {
  readonly name: string
  readonly value: string
  readonly parentDirectoryId: DirectoryId | null
}

/** Metadata for an arbitrary credential, including a nonce for authentication purposes. */
export interface CredentialConfig {
  readonly nonce: string
  readonly input: CredentialInput
}

/** HTTP request body for the "create credential" endpoint. */
export interface CreateCredentialRequestBody {
  readonly name: string
  readonly value: CredentialConfig
  readonly parentDirectoryId: DirectoryId | null
}

/** HTTP request body for the "update secret" endpoint. */
export interface UpdateSecretRequestBody {
  readonly title: string | null
  readonly value: string | null
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

/** Valid plan intervals. */
export type PlanBillingPeriod = 1 | 12

/** HTTP request body for the "create checkout session" endpoint. */
export interface CreateCheckoutSessionRequestBody {
  readonly price: Plan
  readonly quantity: number
  readonly interval: PlanBillingPeriod
}

/** URL query string parameters for the "get log events" endpoint. */
export interface GetLogEventsRequestParams {
  readonly userEmail?: EmailAddress | null | undefined
  readonly lambdaKind?: string | null | undefined
  readonly startDate?: dateTime.Rfc3339DateTime | null | undefined
  readonly endDate?: dateTime.Rfc3339DateTime | null | undefined
  /** Pagination offset */
  readonly from?: number | null | undefined
  readonly pageSize?: number | null | undefined
}

export type AssetSortExpression = 'asset_id_discriminator_and_modified_at' | 'modified_at' | 'title'

export type AssetSortDirection = 'ascending' | 'descending'

/** URL query string parameters for the "list directory" endpoint. */
export interface ListDirectoryRequestParams {
  readonly parentId: DirectoryId | null
  readonly filterBy?: FilterBy | null
  readonly labels?: readonly LabelName[] | null
  readonly sortExpression?: AssetSortExpression | null
  readonly sortDirection?: AssetSortDirection | null
  readonly recentProjects?: boolean
  /**
   * The root path of the directory to list.
   * This is used to list a subdirectory of a local root directory,
   * because a root could be any local folder on the machine.
   */
  readonly rootPath?: Path | undefined
  readonly from?: PaginationToken | null
  readonly pageSize?: number | null
}

/** URL query string parameters for the "search directory" endpoint. */
export interface SearchDirectoryRequestParams {
  readonly parentId: DirectoryId | null
  readonly query: string | null
  readonly title: string | null
  readonly description: string | null
  readonly type: string | null
  readonly extension: string | null
  readonly labels: readonly LabelName[] | null
  readonly sortExpression: AssetSortExpression | null
  readonly sortDirection: AssetSortDirection | null
  readonly from: PaginationToken | null
  readonly pageSize: number | null
}

/** URL query string parameters for the "get project session logs" endpoint. */
export interface GetProjectSessionLogsRequestParams {
  readonly scrollId: string | null
}

/** URL query string parameters for the "upload file" endpoint. */
export interface UploadFileRequestParams {
  readonly fileId: AssetId | null
  readonly fileName: string
  readonly parentDirectoryId: DirectoryId | null
  /** Only used for the Local backend when there is no {@link File} object available. */
  readonly filePath?: Path
  readonly overwrite?: boolean
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
  readonly overwrite?: boolean
}

/** A large file that has finished uploading. */
export interface UploadedFile {
  readonly id: FileId
  readonly project: null
  readonly jobId: null
}

/** A large archive that has finished uploading. */
export interface UploadedArchive {
  readonly id: FileId
  readonly project: null
  readonly jobId: UnzipAssetsJobId
}

/** A large project that has finished uploading. */
export interface UploadedProject {
  readonly id: ProjectId
  readonly project: Project
  readonly jobId: null
}

/** A large asset (file or project) that has finished uploading. */
export type UploadedAsset = UploadedFile | UploadedArchive | UploadedProject

export interface UploadedImages {
  files: { assetId: AssetId; title: string }[]
}

/** URL query string parameters for the "upload profile picture" endpoint. */
export interface UploadPictureRequestParams {
  readonly fileName: string | null
}

export interface ExportArchiveParams {
  readonly assetIds: readonly AssetId[]
  /** The path of the archive to export to. */
  readonly filePath: Path | null
}

export interface ExportedArchive {
  readonly filePath: Path | null
}

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

/** Get the {@link AssetType} of an asset by its id. */
export function getAssetTypeFromId(id: AssetId) {
  return id.match(/^(.+?)-/)?.[1] as AssetType
}

/** Return a positive number if `a > b`, a negative number if `a < b`, and zero if `a === b`. */
export function compareAssets(
  a: AnyAsset,
  b: AnyAsset,
  sortExpression?: AssetSortExpression | null,
  sortDirection?: AssetSortDirection | null,
) {
  sortExpression ??= 'asset_id_discriminator_and_modified_at'
  sortDirection ??=
    sortExpression == 'asset_id_discriminator_and_modified_at' ? 'descending' : 'ascending'

  const multiplier = sortDirection === 'ascending' ? 1 : -1

  const relativeTypeOrder = multiplier * (ASSET_TYPE_ORDER[a.type] - ASSET_TYPE_ORDER[b.type])
  const modifiedAtDelta =
    multiplier * (Number(new Date(a.modifiedAt)) - Number(new Date(b.modifiedAt)))
  const titleDelta = multiplier * a.title.localeCompare(b.title, 'en-US', { numeric: true })

  switch (sortExpression) {
    case 'asset_id_discriminator_and_modified_at': {
      if (relativeTypeOrder !== 0) {
        return relativeTypeOrder
      }
      // On the Remote backend, ids are KSUIDs so they are implicitly sorted by creation date.
      return modifiedAtDelta
    }
    case 'modified_at': {
      return modifiedAtDelta
    }
    case 'title': {
      return titleDelta
    }
  }
}

/** Whether an asset matches the given backend search query. */
export function doesAssetMatchQuery(query: SearchDirectoryRequestParams) {
  const typeLower = query.type?.toLowerCase()
  const titleLower = query.title?.toLowerCase()
  const extensionLower = query.extension?.toLowerCase()
  const queryLower = query.query?.toLowerCase().split(/\s+/)

  return (asset: AnyAsset) => {
    if (typeLower != null && String(asset.type) !== typeLower) {
      return false
    }
    if (titleLower != null && !asset.title.toLowerCase().includes(titleLower)) {
      return false
    }
    if (
      extensionLower != null &&
      asset.extension?.toLowerCase().includes(extensionLower) !== true
    ) {
      return false
    }
    if (
      queryLower?.some(
        (term) =>
          String(asset.type) !== term &&
          !asset.title.toLowerCase().includes(term) &&
          asset.extension?.toLowerCase().includes(term) !== true,
      ) === true
    ) {
      return false
    }
    return true
  }
}

/**
 * A convenience function to get the `id` of an {@link Asset}.
 * This is useful to avoid React re-renders as it is not re-created on each function call.
 */
export function getAssetId<Type extends AssetType>(asset: Asset<Type>) {
  return asset.id
}

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

/** A subset of properties of the JS {@link File} type. */
interface JSFile {
  readonly name: string
}

/** Whether a file name represents a non-project archive. */
export function fileNameIsArchive(fileName: string) {
  return fileName.endsWith('.zip')
}

/** Whether a file name represents a project. */
export function fileNameIsProject(fileName: string) {
  return fileName.endsWith('.tar.gz') || fileName.endsWith('.enso-project')
}

/** Whether a {@link File} is a non-project archive. */
export function fileIsArchive(file: JSFile) {
  return fileNameIsArchive(file.name)
}

/** Whether a {@link File} is a project. */
export function fileIsProject(file: JSFile) {
  return fileNameIsProject(file.name)
}

/** Whether a {@link File} is not a project. */
export function fileIsNotProject(file: JSFile) {
  return !fileIsProject(file)
}

/** Remove the extension of the project file name (if any). */
export function stripProjectExtension(name: string) {
  return name.replace(/[.](?:tar[.]gz|zip|enso-project)$/, '')
}

/**
 * Escape special characters in a project name to prevent them from being interpreted as path.
 */
export function escapeSpecialCharacters(name: string): string {
  return name.replace(/[<>:"/\\|?*]/g, '_')
}

/**
 * Return both the name and extension of the project file name (if any).
 * Otherwise, returns the entire name as the basename.
 */
export function extractProjectExtension(name: string) {
  const [, basename, extension] = name.match(/^(.*)[.](tar[.]gz|zip|enso-project)$/) ?? []
  return { basename: basename ?? name, extension: extension ?? '' }
}

export interface TitleSchemaOptions {
  readonly id: AssetId
  readonly siblings?: readonly AnyAsset[] | null
}

/** Check if the title contains invalid characters. */
export function doesTitleContainInvalidCharacters(name: string) {
  return (
    name.includes('/') ||
    name.includes('\\') ||
    name.includes('..') ||
    name === '.' ||
    name === '..' ||
    name === '~'
  )
}

/** A regex for matching hybrid project directories. */
export const HYBRID_PROJECT_DIRECTORY_MASK = /^cloud-project-\w+$/

/** A list of regexes for matching invalid names. */
const INVALID_NAME_MASKS = [HYBRID_PROJECT_DIRECTORY_MASK]

/** Check if the title contains invalid names. */
export function doesContainInvalidNames(title: string) {
  return INVALID_NAME_MASKS.some((mask) => mask.test(title))
}

/** A Zod schema for validating a title. */
export function titleSchema(options: TitleSchemaOptions) {
  const { id, siblings } = options

  const dictionary = resolveDictionary()

  return z
    .string()
    .trim()
    .min(1)
    .max(512)
    .refine((value) => !doesContainInvalidNames(value), {
      message: getText(dictionary, 'nameShouldNotContainInvalidCharacters'),
    })
    .refine((value) => !doesTitleContainInvalidCharacters(value), {
      message: getText(dictionary, 'nameShouldNotContainInvalidCharacters'),
    })
    .refine((value) => isNewTitleUnique(id, value, siblings), {
      message: getText(dictionary, 'nameShouldBeUnique'),
    })
}

/** Check whether a new title is unique among an asset's siblings. */
export function isNewTitleUnique(
  id: AssetId,
  newTitle: string,
  siblings?: readonly AnyAsset[] | null,
) {
  siblings ??= []

  return siblings.every(
    (sibling) =>
      // Every sibling must:
      // be the asset itself,
      sibling.id === id ||
      // or have a different title.
      sibling.title.trim().toLowerCase() !== newTitle.trim().toLowerCase(),
  )
}

export const MAPBOX_TOKEN_SCHEMA = z.object({
  token: z.string(),
  expires: z
    .string()
    .datetime({ offset: true })
    .transform((str) => new Date(str)),
})
export type MapboxToken = z.infer<typeof MAPBOX_TOKEN_SCHEMA>

/** Network error class. */
export class NetworkError extends Error {
  /**
   * Create a new instance of the {@link NetworkError} class.
   * @param message - The error message.
   * @param status - The HTTP status code.
   */
  constructor(
    message: string,
    readonly status?: number | null,
  ) {
    super(message)
  }
}

/** Error class for when the user is not authorized to access a resource. */
export class NotAuthorizedError extends NetworkError {}

/** Interface for sending requests to a backend that manages assets and runs projects. */
export abstract class Backend {
  abstract readonly type: BackendType
  abstract readonly baseUrl: URL
  protected getText: DefaultGetText
  private readonly client: HttpClient
  protected readonly downloader: (options: DownloadOptions) => void | Promise<void>

  /** Create a {@link Backend}. */
  constructor(
    getText: DefaultGetText,
    client: HttpClient,
    downloader: (options: DownloadOptions) => void | Promise<void>,
  ) {
    this.getText = getText
    this.client = client
    this.downloader = downloader
  }

  /**
   * Set `this.getText`. This function is exposed rather than the property itself to make it clear
   * that it is intended to be mutable.
   */
  setGetText(getText: DefaultGetText) {
    this.getText = getText
  }

  /**
   * Log an error message and throws an {@link Error} with the specified message.
   * @throws {Error} Always.
   */
  protected async throw<K extends Extract<TextId, `${string}BackendError`>>(
    response: Response | null,
    textId: NetworkError | K,
    ...replacements: Replacements[K]
  ): Promise<never> {
    if (textId instanceof NetworkError) {
      console.error(textId.message)

      throw textId
    }

    const error =
      response == null || response.headers.get('Content-Type') !== 'application/json' ?
        { message: 'unknown error' }
      : await ((): Promise<Error> => response.json())()

    const message = `${this.getText(textId, ...replacements)}: ${error.message}.`
    console.error(message)

    const status = response?.status

    throw new NetworkError(message, status)
  }

  /** The path to the root directory of this {@link Backend}. */
  abstract rootPath(user: User): string
  /** Return the ID of the root directory, if known. */
  abstract rootDirectoryId(user: User, organization: OrganizationInfo | null): DirectoryId | null
  /** Return a list of all users in the same organization. */
  abstract listUsers(): Promise<readonly Omit<User, 'groups'>[]>
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
  ): Promise<ListDirectoryResponseBody>
  /** Return a list of assets recursively in a directory matching a query. */
  abstract searchDirectory(query: SearchDirectoryRequestParams): Promise<ListDirectoryResponseBody>
  /** Create a directory. */
  abstract createDirectory(
    body: CreateDirectoryRequestBody,
    discardTitle?: boolean,
  ): Promise<CreatedDirectory>
  /** Change the name of a directory. */
  abstract updateDirectory(
    directoryId: DirectoryId,
    body: UpdateDirectoryRequestBody,
    title: string,
  ): Promise<UpdatedDirectory>
  /** List previous versions of an asset. */
  abstract listAssetVersions(assetId: AssetId): Promise<AssetVersions>
  /** Change the parent directory of an asset. */
  abstract updateAsset(assetId: AssetId, body: UpdateAssetRequestBody, title: string): Promise<void>
  /** Delete an arbitrary asset. */
  abstract deleteAsset(assetId: AssetId, body: DeleteAssetRequestBody, title: string): Promise<void>
  /** Restore an arbitrary asset from the trash. */
  abstract undoDeleteAsset(assetId: AssetId, parentDirectoryId: DirectoryId | null): Promise<void>
  /** Copy an arbitrary asset to another directory. */
  abstract copyAsset(
    assetId: AssetId,
    parentDirectoryId: DirectoryId,
    versionId?: S3ObjectVersionId,
  ): Promise<CopyAssetResponse>
  /** Create a project for the current user. */
  abstract createProject(body: CreateProjectRequestBody): Promise<CreatedProject>
  /** Close a project. */
  abstract closeProject(projectId: ProjectId, title: string): Promise<void>
  /** Return a list of sessions for a project. */
  abstract listProjectSessions(
    projectId: ProjectId,
    title: string,
  ): Promise<readonly ProjectSession[]>
  /** Create a project execution. */
  abstract createProjectExecution(
    body: CreateProjectExecutionRequestBody,
    title: string,
  ): Promise<ProjectExecution>
  abstract getProjectExecutionDetails(
    executionId: ProjectExecutionId,
    title: string,
  ): Promise<ProjectExecution>
  abstract updateProjectExecution(
    executionId: ProjectExecutionId,
    body: UpdateProjectExecutionRequestBody,
    projectTitle: string,
  ): Promise<ProjectExecution>
  /** Delete a project execution. */
  abstract deleteProjectExecution(
    executionId: ProjectExecutionId,
    projectTitle: string,
  ): Promise<void>
  /** Return a list of executions for a project. */
  abstract listProjectExecutions(
    projectId: ProjectId,
    title: string,
  ): Promise<readonly ProjectExecution[]>
  abstract syncProjectExecution(
    executionId: ProjectExecutionId,
    projectTitle: string,
  ): Promise<ProjectExecution>
  /** Restore a project from a different version. */
  abstract restoreAsset(assetId: AssetId, versionId: S3ObjectVersionId): Promise<void>
  /** Duplicate a specific version of an asset. */
  abstract duplicateProject(
    projectId: ProjectId,
    versionId: S3ObjectVersionId,
    title: string,
  ): Promise<CreatedProject>
  /**
   * Return project details.
   */
  abstract getProjectDetails(projectId: ProjectId, getPresignedUrl?: boolean): Promise<Project>
  /** Return asset details. */
  abstract getAssetDetails<Id extends AssetId>(
    assetId: Id,
    rootPath: Path | undefined,
  ): Promise<AssetDetailsResponse<Id>>

  /** Return Language Server logs for a project session. */
  abstract getProjectSessionLogs(
    projectSessionId: ProjectSessionId,
    params: GetProjectSessionLogsRequestParams,
    title: string,
  ): Promise<ProjectSessionLogs>
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
  async getMainFileContent(projectId: ProjectId, versionId?: S3ObjectVersionId) {
    return (await this.resolveProjectAssetData(projectId, 'src/Main.enso', versionId)).text()
  }
  /** Resolve enso path to an asset */
  abstract resolveEnsoPath(path: EnsoPath): Promise<AnyAsset>
  /** Resolve the data of a project asset relative to the project root directory. */
  abstract resolveProjectAssetData(
    projectId: ProjectId,
    relativePath: string,
    versionId?: S3ObjectVersionId,
    abort?: AbortSignal,
  ): Promise<Response>

  /** Begin uploading a large file. */
  abstract uploadFileStart(
    params: UploadFileRequestParams,
    file: File,
    abort?: AbortSignal,
  ): Promise<UploadLargeFileMetadata>
  /** Upload a chunk of a large file. */
  abstract uploadFileChunk(
    url: HttpsUrl,
    file: Blob,
    index: number,
    abort?: AbortSignal,
  ): Promise<{ part: S3MultipartPart; size: number }>
  /** Finish uploading a large file. */
  abstract uploadFileEnd(
    body: UploadFileEndRequestBody,
    abort?: AbortSignal,
  ): Promise<UploadedAsset>
  /**
   * Upload set of Images, resolving any possible conflicts. The sum of file sizes may not
   * exceed cloud message limit.
   */
  abstract uploadImage(
    parentDirectoryId: DirectoryId,
    files: { data: Blob; name: string }[],
  ): Promise<UploadedImages>
  /** Change the name of a file. */
  abstract updateFile(fileId: FileId, body: UpdateFileRequestBody, title: string): Promise<void>

  /**
   * Return details for a file.
   * @throws An error if a non-successful status code (not 200-299) was received.
   */
  async getFileDetails(
    fileId: FileId,
    title: string,
    getPresignedUrl = false,
  ): Promise<FileDetails> {
    const searchParams = new URLSearchParams({
      presigned: `${getPresignedUrl}`,
    }).toString()
    const path = `${getFileDetailsPath(fileId)}?${searchParams}`
    const response = await this.get<FileDetails>(path)
    if (!response.ok) {
      return await this.throw(response, 'getFileDetailsBackendError', title)
    } else {
      return await response.json()
    }
  }

  /** Create a Datalink. */
  abstract createDatalink(body: CreateDatalinkRequestBody): Promise<DatalinkInfo>
  /** Return a Datalink. */
  abstract getDatalink(datalinkId: DatalinkId, title: string | null): Promise<Datalink>
  /** Delete a Datalink. */
  abstract deleteDatalink(datalinkId: DatalinkId, title: string | null): Promise<void>
  /** Create a secret environment variable. */
  abstract createSecret(body: CreateSecretRequestBody): Promise<SecretId>
  /** Create an OAuth credential. */
  abstract createCredential(body: CreateCredentialRequestBody): Promise<SecretId>
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
  /** Create a payment checkout session. */
  abstract createCheckoutSession(body: CreateCheckoutSessionRequestBody): Promise<CheckoutSession>
  /** Cancel subscription. */
  abstract cancelSubscription(subscriptionId: SubscriptionId): Promise<void>
  /** List events in the organization's audit log. */
  abstract getLogEvents(options: GetLogEventsRequestParams): Promise<readonly AuditLogEvent[]>
  /** Log an event that will be visible in the organization audit log. */
  abstract logEvent(
    message: string,
    projectId?: string | null,
    metadata?: object | null,
  ): Promise<void>
  /** Download an asset. */
  abstract download(
    assetId: AssetId,
    title: string,
    targetDirectoryId: DirectoryId | null,
    shouldUnpackProject?: boolean,
  ): Promise<void>
  /** Export multiple files and pack into an archive. */
  abstract exportArchive(params: ExportArchiveParams): Promise<ExportedArchive>
  /**
   * Get the URL for the customer portal.
   * @see https://stripe.com/docs/billing/subscriptions/integrating-customer-portal
   * @param returnUrl - The URL to redirect to after the customer visits the portal.
   */
  abstract createCustomerPortalSession(returnUrl: string): Promise<string | null>
  /** Fetches pricing page configuration. */
  abstract getPaymentsConfig(): Promise<PaymentsConfig>

  /** List all API keys for the current user. */
  abstract listApiKeys(): Promise<readonly ApiKey[]>
  /** Create a new API key for the current user. */
  abstract createApiKey(body: CreateApiKeyRequestBody): Promise<ApiKey>
  /** Delete a API key for the current user. */
  abstract deleteApiKey(apiKeyId: ApiKeyId): Promise<void>
  /** Retrieve Mapbox token for the current user. */
  abstract getMapboxToken(): Promise<MapboxToken>

  /** Throw a {@link backend.NotAuthorizedError} if the response is a 401 Not Authorized status code. */
  private async checkForAuthenticationError<T>(
    makeRequest: () => Promise<ResponseWithTypedJson<T>>,
  ) {
    const response = await makeRequest()
    if (response.status === STATUS_NOT_AUTHORIZED) {
      // User is not authorized, we should redirect to the login page.
      return await this.throw(
        response,
        new NotAuthorizedError(this.getText('notAuthorizedBackendError')),
      )
    }
    return response
  }

  /** Resolve the path relative to the base URL of this backend. */
  protected resolvePath(path: string) {
    return new URL(path, this.baseUrl).toString()
  }

  /** Send an HTTP GET request to the given path. */
  protected get<T = void>(
    path: string,
    queryParams?: Record<string, string> | URLSearchParams,
    abort?: AbortSignal,
  ) {
    const paramsString = queryParams != null ? new URLSearchParams(queryParams).toString() : ''
    const query = paramsString ? '?' + paramsString : ''
    return this.checkForAuthenticationError(() =>
      this.client.get<T>(this.resolvePath(`${path}${query}`), abort),
    )
  }

  /** Send a JSON HTTP POST request to the given path. */
  protected post<T = void>(path: string, payload: object | null, options?: HttpClientPostOptions) {
    return this.checkForAuthenticationError(() =>
      this.client.post<T>(this.resolvePath(path), payload, options),
    )
  }

  /** Send a binary HTTP POST request to the given path. */
  protected postBinary<T = void>(path: string, payload: Blob, options?: HttpClientPostOptions) {
    return this.checkForAuthenticationError(() =>
      this.client.postBinary<T>(this.resolvePath(path), payload, options),
    )
  }

  protected postFormData<T = void>(
    path: string,
    payload: FormData,
    options?: HttpClientPostOptions,
  ) {
    return this.checkForAuthenticationError(() =>
      this.client.postFormData<T>(this.resolvePath(path), payload, options),
    )
  }

  /** Send a JSON HTTP PATCH request to the given path. */
  protected patch<T = void>(path: string, payload: object) {
    return this.checkForAuthenticationError(() =>
      this.client.patch<T>(this.resolvePath(path), payload),
    )
  }

  /** Send a JSON HTTP PUT request to the given path. */
  protected put<T = void>(path: string, payload: object) {
    return this.checkForAuthenticationError(() =>
      this.client.put<T>(this.resolvePath(path), payload),
    )
  }

  /** Send a binary HTTP PUT request to the given path. */
  protected putBinary<T = void>(path: string, payload: Blob) {
    return this.checkForAuthenticationError(() =>
      this.client.putBinary<T>(this.resolvePath(path), payload),
    )
  }

  /** Send an HTTP DELETE request to the given path. */
  protected delete<T = void>(path: string, payload?: Record<string, unknown>) {
    return this.checkForAuthenticationError(() =>
      this.client.delete<T>(this.resolvePath(path), payload),
    )
  }
}

/**
 * Error thrown when an asset does not exist.
 */
export class AssetDoesNotExistError extends Error {
  /**
   * Create a new instance of the {@link AssetDoesNotExistError} class.
   */
  constructor(message: string = 'Asset could not be found.') {
    super(message)
  }
}

/** More specific error thrown when a directory does not exist. */
export class DirectoryDoesNotExistError extends AssetDoesNotExistError {
  /**
   * Create a new instance of the {@link DirectoryDoesNotExistError} class.
   */
  constructor(message: string = 'Directory does not exist.') {
    super(message)
  }
}

/** Error thrown when an asset already exists. */
export class DuplicateAssetError extends Error {
  /**
   * Create a new instance of the {@link DuplicateAssetError} class.
   */
  constructor(message: string = 'Asset already exists.') {
    super(message)
  }
}
