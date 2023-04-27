/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link Backend} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import * as config from '../config'
import * as http from '../http'
import * as loggerProvider from '../providers/logger'
import * as newtype from '../newtype'

// =================
// === Constants ===
// =================

/** HTTP status indicating that the request was successful. */
const STATUS_OK = 200

/** Default HTTP body for an "open project" request. */
const DEFAULT_OPEN_PROJECT_BODY: OpenProjectRequestBody = {
    forceCreate: false,
}

/** Relative HTTP path to the "set username" endpoint of the Cloud backend API. */
const CREATE_USER_PATH = 'users'
/** Relative HTTP path to the "get user" endpoint of the Cloud backend API. */
const USERS_ME_PATH = 'users/me'
/** Relative HTTP path to the "list directory" endpoint of the Cloud backend API. */
const LIST_DIRECTORY_PATH = 'directories'
/** Relative HTTP path to the "create directory" endpoint of the Cloud backend API. */
const CREATE_DIRECTORY_PATH = 'directories'
/** Relative HTTP path to the "list projects" endpoint of the Cloud backend API. */
const LIST_PROJECTS_PATH = 'projects'
/** Relative HTTP path to the "create project" endpoint of the Cloud backend API. */
const CREATE_PROJECT_PATH = 'projects'
/** Relative HTTP path to the "list files" endpoint of the Cloud backend API. */
const LIST_FILES_PATH = 'files'
/** Relative HTTP path to the "upload file" endpoint of the Cloud backend API. */
const UPLOAD_FILE_PATH = 'files'
/** Relative HTTP path to the "create secret" endpoint of the Cloud backend API. */
const CREATE_SECRET_PATH = 'secrets'
/** Relative HTTP path to the "list secrets" endpoint of the Cloud backend API. */
const LIST_SECRETS_PATH = 'secrets'
/** Relative HTTP path to the "create tag" endpoint of the Cloud backend API. */
const CREATE_TAG_PATH = 'tags'
/** Relative HTTP path to the "list tags" endpoint of the Cloud backend API. */
const LIST_TAGS_PATH = 'tags'
/** Relative HTTP path to the "list versions" endpoint of the Cloud backend API. */
const LIST_VERSIONS_PATH = 'versions'
/** Relative HTTP path to the "close project" endpoint of the Cloud backend API. */
function closeProjectPath(projectId: ProjectId) {
    return `projects/${projectId}/close`
}
/** Relative HTTP path to the "get project details" endpoint of the Cloud backend API. */
function getProjectDetailsPath(projectId: ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "open project" endpoint of the Cloud backend API. */
function openProjectPath(projectId: ProjectId) {
    return `projects/${projectId}/open`
}
/** Relative HTTP path to the "project update" endpoint of the Cloud backend API. */
function projectUpdatePath(projectId: ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "delete project" endpoint of the Cloud backend API. */
function deleteProjectPath(projectId: ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "check resources" endpoint of the Cloud backend API. */
function checkResourcesPath(projectId: ProjectId) {
    return `projects/${projectId}/resources`
}
/** Relative HTTP path to the "delete file" endpoint of the Cloud backend API. */
function deleteFilePath(fileId: FileId) {
    return `files/${fileId}`
}
/** Relative HTTP path to the "get project" endpoint of the Cloud backend API. */
function getSecretPath(secretId: SecretId) {
    return `secrets/${secretId}`
}
/** Relative HTTP path to the "delete secret" endpoint of the Cloud backend API. */
function deleteSecretPath(secretId: SecretId) {
    return `secrets/${secretId}`
}
/** Relative HTTP path to the "delete tag" endpoint of the Cloud backend API. */
function deleteTagPath(tagId: TagId) {
    return `secrets/${tagId}`
}

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
interface BaseAsset {
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

/** HTTP response body for the "list projects" endpoint. */
interface ListDirectoryResponseBody {
    assets: BaseAsset[]
}

/** HTTP response body for the "list projects" endpoint. */
interface ListProjectsResponseBody {
    projects: ListedProjectRaw[]
}

/** HTTP response body for the "list files" endpoint. */
interface ListFilesResponseBody {
    files: File[]
}

/** HTTP response body for the "list secrets" endpoint. */
interface ListSecretsResponseBody {
    secrets: SecretInfo[]
}

/** HTTP response body for the "list tag" endpoint. */
interface ListTagsResponseBody {
    tags: Tag[]
}

/** HTTP response body for the "list versions" endpoint. */
interface ListVersionsResponseBody {
    versions: [Version, ...Version[]]
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

/** Class for sending requests to the Cloud backend API endpoints. */
export class Backend {
    /** Creates a new instance of the {@link Backend} API client.
     *
     * @throws An error if the `Authorization` header is not set on the given `client`. */
    constructor(
        private readonly client: http.Client,
        private readonly logger: loggerProvider.Logger
    ) {
        // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
        // set.
        if (!this.client.defaultHeaders?.has('Authorization')) {
            return this.throw('Authorization header not set.')
        } else {
            return
        }
    }

    throw(message: string): never {
        this.logger.error(message)
        throw new Error(message)
    }

    /** Sets the username of the current user, on the Cloud backend API. */
    async createUser(body: CreateUserRequestBody): Promise<UserOrOrganization> {
        const response = await this.post<UserOrOrganization>(CREATE_USER_PATH, body)
        return await response.json()
    }

    /** Returns organization info for the current user, from the Cloud backend API.
     *
     * @returns `null` if status code 401 or 404 was received. */
    async usersMe(): Promise<UserOrOrganization | null> {
        const response = await this.get<UserOrOrganization>(USERS_ME_PATH)
        if (response.status !== STATUS_OK) {
            return null
        } else {
            return await response.json()
        }
    }

    /** Returns a list of assets in a directory, from the Cloud backend API.
     *
     * @throws An error if status code 401 or 404 was received.
     */
    async listDirectory(query: ListDirectoryRequestParams): Promise<Asset[]> {
        const response = await this.get<ListDirectoryResponseBody>(
            LIST_DIRECTORY_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    ...(query.parentId ? { parent_id: query.parentId } : {}),
                }).toString()
        )
        if (response.status !== STATUS_OK) {
            if (query.parentId) {
                return this.throw(`Unable to list directory with ID '${query.parentId}'.`)
            } else {
                return this.throw('Unable to list root directory.')
            }
        } else {
            return (await response.json()).assets.map(
                // This type assertion is safe; it is only needed to convert `type` to a newtype.
                // eslint-disable-next-line no-restricted-syntax
                asset => ({ ...asset, type: asset.id.match(/^(.+?)-/)?.[1] } as Asset)
            )
        }
    }

    /** Creates a directory, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async createDirectory(body: CreateDirectoryRequestBody): Promise<Directory> {
        const response = await this.post<Directory>(CREATE_DIRECTORY_PATH, body)
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to create directory with name '${body.title}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns a list of projects belonging to the current user, from the Cloud backend API.
     *
     * @throws An error if status code 401 or 404 was received.
     */
    async listProjects(): Promise<ListedProject[]> {
        const response = await this.get<ListProjectsResponseBody>(LIST_PROJECTS_PATH)
        if (response.status !== STATUS_OK) {
            return this.throw('Unable to list projects.')
        } else {
            return (await response.json()).projects.map(project => ({
                ...project,
                jsonAddress:
                    project.address != null
                        ? newtype.asNewtype<Address>(`${project.address}json`)
                        : null,
                binaryAddress:
                    project.address != null
                        ? newtype.asNewtype<Address>(`${project.address}binary`)
                        : null,
            }))
        }
    }

    /** Creates a project for the current user, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async createProject(body: CreateProjectRequestBody): Promise<CreatedProject> {
        const response = await this.post<CreatedProject>(CREATE_PROJECT_PATH, body)
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to create project with name '${body.projectName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Closes the project identified by the given project ID, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async closeProject(projectId: ProjectId): Promise<void> {
        const response = await this.post(closeProjectPath(projectId), {})
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to close project with ID '${projectId}'.`)
        } else {
            return
        }
    }

    /** Returns project details for the specified project ID, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async getProjectDetails(projectId: ProjectId): Promise<Project> {
        const response = await this.get<ProjectRaw>(getProjectDetailsPath(projectId))
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to get details of project with ID '${projectId}'.`)
        } else {
            const project = await response.json()
            return {
                ...project,
                jsonAddress:
                    project.address != null
                        ? newtype.asNewtype<Address>(`${project.address}json`)
                        : null,
                binaryAddress:
                    project.address != null
                        ? newtype.asNewtype<Address>(`${project.address}binary`)
                        : null,
            }
        }
    }

    /** Sets project to an open state, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async openProject(
        projectId: ProjectId,
        body: OpenProjectRequestBody = DEFAULT_OPEN_PROJECT_BODY
    ): Promise<void> {
        const response = await this.post(openProjectPath(projectId), body)
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to open project with ID '${projectId}'.`)
        } else {
            return
        }
    }

    async projectUpdate(
        projectId: ProjectId,
        body: ProjectUpdateRequestBody
    ): Promise<UpdatedProject> {
        const response = await this.put<UpdatedProject>(projectUpdatePath(projectId), body)
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to update project with ID '${projectId}'.`)
        } else {
            return await response.json()
        }
    }

    /** Deletes project, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async deleteProject(projectId: ProjectId): Promise<void> {
        const response = await this.delete(deleteProjectPath(projectId))
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to delete project with ID '${projectId}'.`)
        } else {
            return
        }
    }

    /** Returns project memory, processor and storage usage, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async checkResources(projectId: ProjectId): Promise<ResourceUsage> {
        const response = await this.get<ResourceUsage>(checkResourcesPath(projectId))
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to get resource usage for project with ID '${projectId}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns a list of files accessible by the current user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async listFiles(): Promise<File[]> {
        const response = await this.get<ListFilesResponseBody>(LIST_FILES_PATH)
        if (response.status !== STATUS_OK) {
            return this.throw('Unable to list files.')
        } else {
            return (await response.json()).files
        }
    }

    /** Uploads a file, to the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async uploadFile(params: UploadFileRequestParams, body: Blob): Promise<FileInfo> {
        const response = await this.postBase64<FileInfo>(
            UPLOAD_FILE_PATH +
                '?' +
                new URLSearchParams({
                    /* eslint-disable @typescript-eslint/naming-convention */
                    ...(params.fileName ? { file_name: params.fileName } : {}),
                    ...(params.fileId ? { file_id: params.fileId } : {}),
                    ...(params.parentDirectoryId
                        ? { parent_directory_id: params.parentDirectoryId }
                        : {}),
                    /* eslint-enable @typescript-eslint/naming-convention */
                }).toString(),
            body
        )
        if (response.status !== STATUS_OK) {
            if (params.fileName) {
                return this.throw(`Unable to upload file with name '${params.fileName}'.`)
            } else if (params.fileId) {
                return this.throw(`Unable to upload file with ID '${params.fileId}'.`)
            } else {
                return this.throw('Unable to upload file.')
            }
        } else {
            return await response.json()
        }
    }

    /** Deletes a file, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async deleteFile(fileId: FileId): Promise<void> {
        const response = await this.delete(deleteFilePath(fileId))
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to delete file with ID '${fileId}'.`)
        } else {
            return
        }
    }

    /** Creates a secret environment variable, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async createSecret(body: CreateSecretRequestBody): Promise<SecretAndInfo> {
        const response = await this.post<SecretAndInfo>(CREATE_SECRET_PATH, body)
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to create secret with name '${body.secretName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns a secret environment variable, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async getSecret(secretId: SecretId): Promise<Secret> {
        const response = await this.get<Secret>(getSecretPath(secretId))
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to get secret with ID '${secretId}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns the secret environment variables accessible by the user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async listSecrets(): Promise<SecretInfo[]> {
        const response = await this.get<ListSecretsResponseBody>(LIST_SECRETS_PATH)
        if (response.status !== STATUS_OK) {
            return this.throw('Unable to list secrets.')
        } else {
            return (await response.json()).secrets
        }
    }

    /** Deletes a secret environment variable, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async deleteSecret(secretId: SecretId): Promise<void> {
        const response = await this.delete(deleteSecretPath(secretId))
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to delete secret with ID '${secretId}'.`)
        } else {
            return
        }
    }

    /** Creates a file tag or project tag, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async createTag(body: CreateTagRequestBody): Promise<TagInfo> {
        const response = await this.post<TagInfo>(CREATE_TAG_PATH, {
            /* eslint-disable @typescript-eslint/naming-convention */
            tag_name: body.name,
            tag_value: body.value,
            object_type: body.objectType,
            object_id: body.objectId,
            /* eslint-enable @typescript-eslint/naming-convention */
        })
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to create create tag with name '${body.name}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns file tags or project tags accessible by the user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async listTags(params: ListTagsRequestParams): Promise<Tag[]> {
        const response = await this.get<ListTagsResponseBody>(
            LIST_TAGS_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    tag_type: params.tagType,
                }).toString()
        )
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to list tags of type '${params.tagType}'.`)
        } else {
            return (await response.json()).tags
        }
    }

    /** Deletes a secret environment variable, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async deleteTag(tagId: TagId): Promise<void> {
        const response = await this.delete(deleteTagPath(tagId))
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to delete tag with ID '${tagId}'.`)
        } else {
            return
        }
    }

    /** Returns list of backend or IDE versions, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async listVersions(params: ListVersionsRequestParams): Promise<[Version, ...Version[]]> {
        const response = await this.get<ListVersionsResponseBody>(
            LIST_VERSIONS_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    version_type: params.versionType,
                    default: String(params.default),
                }).toString()
        )
        if (response.status !== STATUS_OK) {
            return this.throw(`Unable to list versions of type '${params.versionType}'.`)
        } else {
            return (await response.json()).versions
        }
    }

    /** Sends an HTTP GET request to the given path. */
    private get<T = void>(path: string) {
        return this.client.get<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
    }

    /** Sends a JSON HTTP POST request to the given path. */
    private post<T = void>(path: string, payload: object) {
        return this.client.post<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
    }

    /** Sends a binary HTTP POST request to the given path. */
    private postBase64<T = void>(path: string, payload: Blob) {
        return this.client.postBase64<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
    }

    /** Sends a JSON HTTP PUT request to the given path. */
    private put<T = void>(path: string, payload: object) {
        return this.client.put<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
    }

    /** Sends an HTTP DELETE request to the given path. */
    private delete<T = void>(path: string) {
        return this.client.delete<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
    }
}

// =====================
// === createBackend ===
// =====================

/** Shorthand method for creating a new instance of the backend API, along with the necessary
 * headers. */
/* TODO [NP]: https://github.com/enso-org/cloud-v2/issues/343
 * This is a hack to quickly create the backend in the format we want, until we get the provider
 * working. This should be removed entirely in favour of creating the backend once and using it from
 * the context. */
export function createBackend(accessToken: string, logger: loggerProvider.Logger): Backend {
    const headers = new Headers()
    headers.append('Authorization', `Bearer ${accessToken}`)
    const client = new http.Client(headers)
    return new Backend(client, logger)
}
