/** @file Type definitions common between all backends. */
import * as newtype from '../newtype'
import * as platform from '../platform'

// =============
// === Types ===
// =============

/** Unique identifier for a user/organization. */
export type UserOrOrganizationId = newtype.Newtype<string, 'UserOrOrganizationId'>

/** Unique identifier for a directory. */
export type DirectoryId = newtype.Newtype<string, 'DirectoryId'>

/** Unique identifier for a user's project. */
export type ProjectId = newtype.Newtype<string, 'ProjectId'>

/** Unique identifier for an uploaded file. */
export type FileId = newtype.Newtype<string, 'FileId'>

/** Unique identifier for a secret environment variable. */
export type SecretId = newtype.Newtype<string, 'SecretId'>

/** Unique identifier for a file tag or project tag. */
export type TagId = newtype.Newtype<string, 'TagId'>

/** A URL. */
export type Address = newtype.Newtype<string, 'Address'>

/** An email address. */
export type EmailAddress = newtype.Newtype<string, 'EmailAddress'>

/** An AWS S3 file path. */
export type S3FilePath = newtype.Newtype<string, 'S3FilePath'>

export type Ami = newtype.Newtype<string, 'Ami'>

export type Subject = newtype.Newtype<string, 'Subject'>

/** An RFC 3339 DateTime string. */
export type Rfc3339DateTime = newtype.Newtype<string, 'Rfc3339DateTime'>

/** A user/organization in the application. These are the primary owners of a project. */
export interface UserOrOrganization {
    id: UserOrOrganizationId
    name: string
    email: EmailAddress
}

/** Possible states that a project can be in. */
export enum ProjectState {
    created = 'Created',
    new = 'New',
    openInProgress = 'OpenInProgress',
    opened = 'Opened',
    closed = 'Closed',
}

/** Wrapper around a project state value. */
export interface ProjectStateType {
    type: ProjectState
}

/** Common `Project` fields returned by all `Project`-related endpoints.  */
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
    created: Rfc3339DateTime
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

export interface User {
    /* eslint-disable @typescript-eslint/naming-convention */
    pk: Subject
    user_name: string
    user_email: EmailAddress
    organization_id: UserOrOrganizationId
    /* eslint-enable @typescript-eslint/naming-convention */
}

export enum PermissionAction {
    own = 'Own',
    execute = 'Execute',
    edit = 'Edit',
    read = 'Read',
}

export interface UserPermission {
    user: User
    permission: PermissionAction
}

/** Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories. */
export interface BaseAsset {
    title: string
    id: string
    parentId: string
    permissions: UserPermission[] | null
}

export enum AssetType {
    project = 'project',
    file = 'file',
    secret = 'secret',
    directory = 'directory',
}

export interface IdType {
    [AssetType.project]: ProjectId
    [AssetType.file]: FileId
    [AssetType.secret]: SecretId
    [AssetType.directory]: DirectoryId
}

/** Metadata uniquely identifying a directory entry.
 * These can be Projects, Files, Secrets, or other directories. */
export interface Asset<Type extends AssetType = AssetType> extends BaseAsset {
    type: Type
    id: IdType[Type]
}

/** The type returned from the "create directory" endpoint. */
export interface Directory extends Asset<AssetType.directory> {}

// =================
// === Endpoints ===
// =================

/** HTTP request body for the "set username" endpoint. */
export interface CreateUserRequestBody {
    userName: string
    userEmail: EmailAddress
}

/** HTTP request body for the "create directory" endpoint. */
export interface CreateDirectoryRequestBody {
    title: string
    parentId: DirectoryId | null
}

/** HTTP request body for the "create project" endpoint. */
export interface CreateProjectRequestBody {
    projectName: string
    projectTemplateName: string | null
    parentDirectoryId: DirectoryId | null
}

/**
 * HTTP request body for the "project update" endpoint.
 * Only updates of the `projectName` or `ami` are allowed.
 */
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

export interface ListDirectoryRequestParams {
    parentId?: string
}

/** URL query string parameters for the "upload file" endpoint. */
export interface UploadFileRequestParams {
    fileId?: string
    fileName?: string
    parentDirectoryId?: DirectoryId
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

// ===================
// === Type guards ===
// ===================

export function assetIsType<Type extends AssetType>(type: Type) {
    return (asset: Asset): asset is Asset<Type> => asset.type === type
}

// ===============
// === Backend ===
// ===============

/** Interface for sending requests to a backend that manages assets and runs projects. */
export interface Backend {
    readonly platform: platform.Platform

    /** Set the username of the current user. */
    createUser: (body: CreateUserRequestBody) => Promise<UserOrOrganization>
    /** Return user details for the current user. */
    usersMe: () => Promise<UserOrOrganization | null>
    /** Return a list of assets in a directory. */
    listDirectory: (query: ListDirectoryRequestParams) => Promise<Asset[]>
    /** Create a directory. */
    createDirectory: (body: CreateDirectoryRequestBody) => Promise<Directory>
    /** Return a list of projects belonging to the current user. */
    listProjects: () => Promise<ListedProject[]>
    /** Create a project for the current user. */
    createProject: (body: CreateProjectRequestBody) => Promise<CreatedProject>
    /** Close the project identified by the given project ID. */
    closeProject: (projectId: ProjectId) => Promise<void>
    /** Return project details for the specified project ID. */
    getProjectDetails: (projectId: ProjectId) => Promise<Project>
    /** Set a project to an open state. */
    openProject: (projectId: ProjectId, body: OpenProjectRequestBody) => Promise<void>
    projectUpdate: (projectId: ProjectId, body: ProjectUpdateRequestBody) => Promise<UpdatedProject>
    /** Delete a project. */
    deleteProject: (projectId: ProjectId) => Promise<void>
    /** Return project memory, processor and storage usage. */
    checkResources: (projectId: ProjectId) => Promise<ResourceUsage>
    /** Return a list of files accessible by the current user. */
    listFiles: () => Promise<File[]>
    /** Upload a file. */
    uploadFile: (params: UploadFileRequestParams, body: Blob) => Promise<FileInfo>
    /** Delete a file. */
    deleteFile: (fileId: FileId) => Promise<void>
    /** Create a secret environment variable. */
    createSecret: (body: CreateSecretRequestBody) => Promise<SecretAndInfo>
    /** Return a secret environment variable. */
    getSecret: (secretId: SecretId) => Promise<Secret>
    /** Return the secret environment variables accessible by the user. */
    listSecrets: () => Promise<SecretInfo[]>
    /** Delete a secret environment variable. */
    deleteSecret: (secretId: SecretId) => Promise<void>
    /** Create a file tag or project tag. */
    createTag: (body: CreateTagRequestBody) => Promise<TagInfo>
    /** Return file tags or project tags accessible by the user. */
    listTags: (params: ListTagsRequestParams) => Promise<Tag[]>
    /** Delete a file tag or project tag. */
    deleteTag: (tagId: TagId) => Promise<void>
    /** Return a list of backend or IDE versions. */
    listVersions: (params: ListVersionsRequestParams) => Promise<[Version, ...Version[]]>
}
