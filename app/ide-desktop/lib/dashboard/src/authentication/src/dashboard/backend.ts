/** @file Type definitions common between all backends. */

import * as dateTime from './dateTime'
import * as newtype from '../newtype'
import * as permissions from './permissions'

// =============
// === Types ===
// =============

/** The {@link Backend} variant. If a new variant is created, it should be added to this enum. */
export enum BackendType {
    local = 'local',
    remote = 'remote',
}

// These are constructor functions that construct values of the type they are named after.
/* eslint-disable @typescript-eslint/no-redeclare */

/** Unique identifier for a user/organization. */
export type UserOrOrganizationId = newtype.Newtype<string, 'UserOrOrganizationId'>
/** Create a {@link UserOrOrganizationId}. */
export const UserOrOrganizationId = newtype.newtypeConstructor<UserOrOrganizationId>()

/** Unique identifier for a directory. */
export type DirectoryId = newtype.Newtype<string, 'DirectoryId'>
/** Create a {@link DirectoryId}. */
export const DirectoryId = newtype.newtypeConstructor<DirectoryId>()

/** Unique identifier for an asset representing the items inside a directory for which the
 * request to retrive the items has not yet completed. */
export type LoadingAssetId = newtype.Newtype<string, 'LoadingAssetId'>
/** Create a {@link LoadingAssetId}. */
export const LoadingAssetId = newtype.newtypeConstructor<LoadingAssetId>()

/** Unique identifier for an asset representing the nonexistent children of an empty directory. */
export type EmptyAssetId = newtype.Newtype<string, 'EmptyAssetId'>
/** Create a {@link EmptyAssetId}. */
export const EmptyAssetId = newtype.newtypeConstructor<EmptyAssetId>()

/** Unique identifier for a user's project. */
export type ProjectId = newtype.Newtype<string, 'ProjectId'>
/** Create a {@link ProjectId}. */
export const ProjectId = newtype.newtypeConstructor<ProjectId>()

/** Unique identifier for an uploaded file. */
export type FileId = newtype.Newtype<string, 'FileId'>
/** Create a {@link FileId}. */
export const FileId = newtype.newtypeConstructor<FileId>()

/** Unique identifier for a secret environment variable. */
export type SecretId = newtype.Newtype<string, 'SecretId'>
/** Create a {@link SecretId}. */
export const SecretId = newtype.newtypeConstructor<SecretId>()

/** Unique identifier for an arbitrary asset. */
export type AssetId = IdType[keyof IdType]

/** Unique identifier for a file tag or project tag. */
export type TagId = newtype.Newtype<string, 'TagId'>
/** Create a {@link TagId}. */
export const TagId = newtype.newtypeConstructor<TagId>()

/** A URL. */
export type Address = newtype.Newtype<string, 'Address'>
/** Create an {@link Address}. */
export const Address = newtype.newtypeConstructor<Address>()

/** An email address. */
export type EmailAddress = newtype.Newtype<string, 'EmailAddress'>
/** Create an {@link EmailAddress}. */
export const EmailAddress = newtype.newtypeConstructor<EmailAddress>()

/** An AWS S3 file path. */
export type S3FilePath = newtype.Newtype<string, 'S3FilePath'>
/** Create an {@link S3FilePath}. */
export const S3FilePath = newtype.newtypeConstructor<S3FilePath>()

/** An AWS machine configuration. */
export type Ami = newtype.Newtype<string, 'Ami'>
/** Create an {@link Ami}. */
export const Ami = newtype.newtypeConstructor<Ami>()

/** An AWS user ID. */
export type Subject = newtype.Newtype<string, 'Subject'>
/** Create a {@link Subject}. */
export const Subject = newtype.newtypeConstructor<Subject>()

/* eslint-enable @typescript-eslint/no-redeclare */

/** A user/organization in the application. These are the primary owners of a project. */
export interface UserOrOrganization {
    id: UserOrOrganizationId
    name: string
    email: EmailAddress
    /** If `false`, this account is awaiting acceptance from an admin, and endpoints other than
     * `usersMe` will not work. */
    isEnabled: boolean
}

/** A `Directory` returned by `createDirectory`. */
export interface CreatedDirectory {
    id: DirectoryId
    parentId: DirectoryId
    title: string
}

/** Possible states that a project can be in. */
export enum ProjectState {
    created = 'Created',
    new = 'New',
    openInProgress = 'OpenInProgress',
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
}

/** Common `Project` fields returned by all `Project`-related endpoints. */
export interface BaseProject {
    organizationId: string
    projectId: ProjectId
    name: string
}

/** A `Project` returned by `createProject`. */
export interface CreatedProject extends BaseProject {
    state: ProjectStateType
    packageName: string
}

/** A `Project` returned by the `listProjects` endpoint. */
export interface ListedProjectRaw extends CreatedProject {
    address: Address | null
}

/** A `Project` returned by `listProjects`. */
export interface ListedProject extends CreatedProject {
    binaryAddress: Address | null
    jsonAddress: Address | null
}

/** A `Project` returned by `updateProject`. */
export interface UpdatedProject extends BaseProject {
    ami: Ami | null
    ideVersion: VersionNumber | null
    engineVersion: VersionNumber | null
}

/** A user/organization's project containing and/or currently executing code. */
export interface ProjectRaw extends ListedProjectRaw {
    ideVersion: VersionNumber | null
    engineVersion: VersionNumber | null
}

/** A user/organization's project containing and/or currently executing code. */
export interface Project extends ListedProject {
    ideVersion: VersionNumber | null
    engineVersion: VersionNumber | null
}

/** Metadata describing an uploaded file. */
export interface File {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    file_id: FileId
    // eslint-disable-next-line @typescript-eslint/naming-convention
    file_name: string | null
    path: S3FilePath
}

/** Metadata uniquely identifying an uploaded file. */
export interface FileInfo {
    /* TODO: Should potentially be S3FilePath,
     * but it's just string on the backend. */
    path: string
    id: FileId
    project: CreatedProject | null
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

/** The type of asset a specific tag can be applied to. */
export enum TagObjectType {
    file = 'File',
    project = 'Project',
}

/** A file tag or project tag. */
export interface Tag {
    /* eslint-disable @typescript-eslint/naming-convention */
    organization_id: UserOrOrganizationId
    id: TagId
    name: string
    value: string
    object_type: TagObjectType
    object_id: string
    /* eslint-enable @typescript-eslint/naming-convention */
}

/** Metadata uniquely identifying a file tag or project tag. */
export interface TagInfo {
    id: TagId
    name: string
    value: string
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

/** Backend representation of user permission types. */
export enum PermissionAction {
    own = 'Own',
    admin = 'Admin',
    edit = 'Edit',
    read = 'Read',
    readAndDocs = 'Read_docs',
    readAndExec = 'Read_exec',
    view = 'View',
    viewAndDocs = 'View_docs',
    viewAndExec = 'View_exec',
}

/** User permission for a specific user. */
export interface UserPermission {
    user: User
    permission: PermissionAction
}

/** The type returned from the "update directory" endpoint. */
export interface UpdatedDirectory {
    id: DirectoryId
    parentId: DirectoryId
    title: string
}

/** The type returned from the "create directory" endpoint. */
export interface Directory extends DirectoryAsset {}

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
    modifiedAt: dateTime.Rfc3339DateTime | null
    /** This is defined as a generic {@link AssetId} in the backend, however it is more convenient
     * (and currently safe) to assume it is always a {@link DirectoryId}. */
    parentId: DirectoryId
    permissions: UserPermission[] | null
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
/* eslint-disable no-restricted-syntax */

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

/** HTTP request body for the "invite user" endpoint. */
export interface InviteUserRequestBody {
    organizationId: UserOrOrganizationId
    userEmail: EmailAddress
}

/** HTTP request body for the "create permission" endpoint. */
export interface CreatePermissionRequestBody {
    userSubjects: Subject[]
    resourceId: AssetId
    action: PermissionAction | null
}

/** HTTP request body for the "create directory" endpoint. */
export interface CreateDirectoryRequestBody {
    title: string
    parentId: DirectoryId | null
}

/** HTTP request body for the "update directory" endpoint. */
export interface UpdateDirectoryRequestBody {
    title: string
}

/** HTTP request body for the "create project" endpoint. */
export interface CreateProjectRequestBody {
    projectName: string
    projectTemplateName: string | null
    parentDirectoryId: DirectoryId | null
}

/** HTTP request body for the "project update" endpoint.
 * Only updates of the `projectName` or `ami` are allowed. */
export interface ProjectUpdateRequestBody {
    projectName: string | null
    ami: Ami | null
    ideVersion: VersionNumber | null
}

/** HTTP request body for the "open project" endpoint. */
export interface OpenProjectRequestBody {
    forceCreate: boolean
}

/** HTTP request body for the "create secret" endpoint. */
export interface CreateSecretRequestBody {
    secretName: string
    secretValue: string
    parentDirectoryId: DirectoryId | null
}

/** HTTP request body for the "create tag" endpoint. */
export interface CreateTagRequestBody {
    name: string
    value: string
    objectType: TagObjectType
    objectId: string
}

/** URL query string parameters for the "list directory" endpoint. */
export interface ListDirectoryRequestParams {
    parentId: string | null
}

/** URL query string parameters for the "upload file" endpoint. */
export interface UploadFileRequestParams {
    fileId: string | null
    fileName: string | null
    parentDirectoryId: DirectoryId | null
}

/** URL query string parameters for the "list tags" endpoint. */
export interface ListTagsRequestParams {
    tagType: TagObjectType
}

/** URL query string parameters for the "list versions" endpoint. */
export interface ListVersionsRequestParams {
    versionType: VersionType
    default: boolean
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

// =======================
// === rootDirectoryId ===
// =======================

/** Return the id of the root directory for a user or organization. */
export function rootDirectoryId(userOrOrganizationId: UserOrOrganizationId) {
    return DirectoryId(userOrOrganizationId.replace(/^organization-/, `${AssetType.directory}-`))
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

/** Whether a `File` is a project. */
export function stripProjectExtension(name: string) {
    return name.replace(/\.tar\.gz$|\.zip$|\.enso-project/, '')
}

// ===============
// === Backend ===
// ===============

/** Interface for sending requests to a backend that manages assets and runs projects. */
export abstract class Backend {
    abstract readonly type: BackendType

    /** Delete an asset of any type. */
    async deleteAsset(asset: AnyAsset) {
        switch (asset.type) {
            case AssetType.directory: {
                await this.deleteDirectory(asset.id, asset.title)
                break
            }
            case AssetType.project: {
                await this.deleteProject(asset.id, asset.title)
                break
            }
            case AssetType.file: {
                await this.deleteFile(asset.id, asset.title)
                break
            }
            case AssetType.secret: {
                await this.deleteSecret(asset.id, asset.title)
                break
            }
            case AssetType.specialLoading:
            case AssetType.specialEmpty: {
                // Ignored. This should never happen, and because they do not exist on the backend,
                // there are no negative consequences.
                break
            }
        }
    }
    /** Return a list of all users in the same organization. */
    abstract listUsers(): Promise<SimpleUser[]>
    /** Set the username of the current user. */
    abstract createUser(body: CreateUserRequestBody): Promise<UserOrOrganization>
    /** Invite a new user to the organization by email. */
    abstract inviteUser(body: InviteUserRequestBody): Promise<void>
    /** Adds a permission for a specific user on a specific asset. */
    abstract createPermission(body: CreatePermissionRequestBody): Promise<void>
    /** Return user details for the current user. */
    abstract usersMe(): Promise<UserOrOrganization | null>
    /** Return a list of assets in a directory. */
    abstract listDirectory(
        query: ListDirectoryRequestParams,
        title: string | null
    ): Promise<AnyAsset[]>
    /** Create a directory. */
    abstract createDirectory(body: CreateDirectoryRequestBody): Promise<CreatedDirectory>
    /** Change the name of a directory. */
    abstract updateDirectory(
        directoryId: DirectoryId,
        body: UpdateDirectoryRequestBody,
        title: string | null
    ): Promise<UpdatedDirectory>
    /** Delete a directory. */
    abstract deleteDirectory(directoryId: DirectoryId, title: string | null): Promise<void>
    /** Return a list of projects belonging to the current user. */
    abstract listProjects(): Promise<ListedProject[]>
    /** Create a project for the current user. */
    abstract createProject(body: CreateProjectRequestBody): Promise<CreatedProject>
    /** Close the project identified by the given project ID. */
    abstract closeProject(projectId: ProjectId, title: string | null): Promise<void>
    /** Return project details for the specified project ID. */
    abstract getProjectDetails(projectId: ProjectId, title: string | null): Promise<Project>
    /** Set a project to an open state. */
    abstract openProject(
        projectId: ProjectId,
        body: OpenProjectRequestBody | null,
        title: string | null
    ): Promise<void>
    abstract projectUpdate(
        projectId: ProjectId,
        body: ProjectUpdateRequestBody,
        title: string | null
    ): Promise<UpdatedProject>
    /** Delete a project. */
    abstract deleteProject(projectId: ProjectId, title: string | null): Promise<void>
    /** Return project memory, processor and storage usage. */
    abstract checkResources(projectId: ProjectId, title: string | null): Promise<ResourceUsage>
    /** Return a list of files accessible by the current user. */
    abstract listFiles(): Promise<File[]>
    /** Upload a file. */
    abstract uploadFile(params: UploadFileRequestParams, body: Blob): Promise<FileInfo>
    /** Delete a file. */
    abstract deleteFile(fileId: FileId, title: string | null): Promise<void>
    /** Create a secret environment variable. */
    abstract createSecret(body: CreateSecretRequestBody): Promise<SecretAndInfo>
    /** Return a secret environment variable. */
    abstract getSecret(secretId: SecretId, title: string | null): Promise<Secret>
    /** Return the secret environment variables accessible by the user. */
    abstract listSecrets(): Promise<SecretInfo[]>
    /** Delete a secret environment variable. */
    abstract deleteSecret(secretId: SecretId, title: string | null): Promise<void>
    /** Create a file tag or project tag. */
    abstract createTag(body: CreateTagRequestBody): Promise<TagInfo>
    /** Return file tags or project tags accessible by the user. */
    abstract listTags(params: ListTagsRequestParams): Promise<Tag[]>
    /** Delete a file tag or project tag. */
    abstract deleteTag(tagId: TagId): Promise<void>
    /** Return a list of backend or IDE versions. */
    abstract listVersions(params: ListVersionsRequestParams): Promise<[Version, ...Version[]]>
}
