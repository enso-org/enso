/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as backend from './backend'
import * as config from '../config'
import * as errorModule from '../error'
import * as http from '../http'
import * as loggerProvider from '../providers/logger'

// =================
// === Constants ===
// =================

/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_FIRST = 200
/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_LAST = 299
/** HTTP status indicating that the server encountered a fatal exception. */
const STATUS_SERVER_ERROR = 500

/** Default HTTP body for an "open project" request. */
const DEFAULT_OPEN_PROJECT_BODY: backend.OpenProjectRequestBody = {
    forceCreate: false,
}

// ========================
// === Helper functions ===
// ========================

/** Returns true if and only if a response has a success HTTP status code (200-299). */
function responseIsSuccessful(response: Response) {
    return response.status >= STATUS_SUCCESS_FIRST && response.status <= STATUS_SUCCESS_LAST
}

// =============
// === Paths ===
// =============

/** Relative HTTP path to the "list users" endpoint of the Cloud backend API. */
const LIST_USERS_PATH = 'users'
/** Relative HTTP path to the "set username" endpoint of the Cloud backend API. */
const CREATE_USER_PATH = 'users'
/** Relative HTTP path to the "invite user" endpoint of the Cloud backend API. */
const INVITE_USER_PATH = 'users/invite'
/** Relative HTTP path to the "create permission" endpoint of the Cloud backend API. */
const CREATE_PERMISSION_PATH = 'permissions'
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
/** Relative HTTP path to the "update directory" endpoint of the Cloud backend API. */
function updateDirectoryPath(directoryId: backend.DirectoryId) {
    return `directories/${directoryId}`
}
/** Relative HTTP path to the "delete directory" endpoint of the Cloud backend API. */
function deleteDirectoryPath(directoryId: backend.DirectoryId) {
    return `directories/${directoryId}`
}
/** Relative HTTP path to the "close project" endpoint of the Cloud backend API. */
function closeProjectPath(projectId: backend.ProjectId) {
    return `projects/${projectId}/close`
}
/** Relative HTTP path to the "get project details" endpoint of the Cloud backend API. */
function getProjectDetailsPath(projectId: backend.ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "open project" endpoint of the Cloud backend API. */
function openProjectPath(projectId: backend.ProjectId) {
    return `projects/${projectId}/open`
}
/** Relative HTTP path to the "project update" endpoint of the Cloud backend API. */
function projectUpdatePath(projectId: backend.ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "delete project" endpoint of the Cloud backend API. */
function deleteProjectPath(projectId: backend.ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "check resources" endpoint of the Cloud backend API. */
function checkResourcesPath(projectId: backend.ProjectId) {
    return `projects/${projectId}/resources`
}
/** Relative HTTP path to the "delete file" endpoint of the Cloud backend API. */
function deleteFilePath(fileId: backend.FileId) {
    return `files/${fileId}`
}
/** Relative HTTP path to the "get project" endpoint of the Cloud backend API. */
function getSecretPath(secretId: backend.SecretId) {
    return `secrets/${secretId}`
}
/** Relative HTTP path to the "delete secret" endpoint of the Cloud backend API. */
function deleteSecretPath(secretId: backend.SecretId) {
    return `secrets/${secretId}`
}
/** Relative HTTP path to the "delete tag" endpoint of the Cloud backend API. */
function deleteTagPath(tagId: backend.TagId) {
    return `secrets/${tagId}`
}

// =============
// === Types ===
// =============

/** HTTP response body for the "list users" endpoint. */
interface ListUsersResponseBody {
    users: backend.SimpleUser[]
}

/** HTTP response body for the "list projects" endpoint. */
interface ListDirectoryResponseBody {
    assets: backend.AnyAsset[]
}

/** HTTP response body for the "list projects" endpoint. */
interface ListProjectsResponseBody {
    projects: backend.ListedProjectRaw[]
}

/** HTTP response body for the "list files" endpoint. */
interface ListFilesResponseBody {
    files: backend.File[]
}

/** HTTP response body for the "list secrets" endpoint. */
interface ListSecretsResponseBody {
    secrets: backend.SecretInfo[]
}

/** HTTP response body for the "list tag" endpoint. */
interface ListTagsResponseBody {
    tags: backend.Tag[]
}

/** HTTP response body for the "list versions" endpoint. */
interface ListVersionsResponseBody {
    versions: [backend.Version, ...backend.Version[]]
}

// =====================
// === RemoteBackend ===
// =====================

/** Class for sending requests to the Cloud backend API endpoints. */
export class RemoteBackend extends backend.Backend {
    readonly type = backend.BackendType.remote

    /** Create a new instance of the {@link RemoteBackend} API client.
     *
     * @throws An error if the `Authorization` header is not set on the given `client`. */
    constructor(
        private readonly client: http.Client,
        private readonly logger: loggerProvider.Logger
    ) {
        super()
        // All of our API endpoints are authenticated, so we expect the `Authorization` header to be
        // set.
        if (!this.client.defaultHeaders.has('Authorization')) {
            return this.throw('Authorization header not set.')
        } else {
            if (IS_DEV_MODE) {
                // @ts-expect-error This exists only for debugging purposes. It does not have types
                // because it MUST NOT be used in this codebase.
                window.remoteBackend = this
            }
            return
        }
    }

    /** Log an error message and throws an {@link Error} with the specified message.
     * @throws {Error} Always. */
    throw(message: string): never {
        this.logger.error(message)
        throw new Error(message)
    }

    /** Return the root directory id for the given user. */
    override rootDirectoryId(user: backend.UserOrOrganization | null): backend.DirectoryId {
        return backend.DirectoryId(
            // `user` is only null when the user is offline, in which case the remote backend cannot
            // be accessed anyway.
            user != null ? user.id.replace(/^organization-/, `${backend.AssetType.directory}-`) : ''
        )
    }

    /** Return a list of all users in the same organization. */
    async listUsers(): Promise<backend.SimpleUser[]> {
        const response = await this.get<ListUsersResponseBody>(LIST_USERS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to list users in the organization.`)
        } else {
            return (await response.json()).users
        }
    }

    /** Set the username and parent organization of the current user. */
    async createUser(body: backend.CreateUserRequestBody): Promise<backend.UserOrOrganization> {
        const response = await this.post<backend.UserOrOrganization>(CREATE_USER_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw('Unable to create user.')
        } else {
            return await response.json()
        }
    }

    /** Invite a new user to the organization by email. */
    async inviteUser(body: backend.InviteUserRequestBody): Promise<void> {
        const response = await this.post(INVITE_USER_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to invite user '${body.userEmail}'.`)
        } else {
            return
        }
    }

    /** Adds a permission for a specific user on a specific asset. */
    async createPermission(body: backend.CreatePermissionRequestBody): Promise<void> {
        const response = await this.post<backend.UserOrOrganization>(CREATE_PERMISSION_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to set permissions.`)
        } else {
            return
        }
    }

    /** Return organization info for the current user.
     *
     * @returns `null` if a non-successful status code (not 200-299) was received. */
    async usersMe(): Promise<backend.UserOrOrganization | null> {
        const response = await this.get<backend.UserOrOrganization>(USERS_ME_PATH)
        if (!responseIsSuccessful(response)) {
            return null
        } else {
            return await response.json()
        }
    }

    /** Return a list of assets in a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listDirectory(
        query: backend.ListDirectoryRequestParams,
        title: string | null
    ): Promise<backend.AnyAsset[]> {
        const response = await this.get<ListDirectoryResponseBody>(
            LIST_DIRECTORY_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    ...(query.parentId != null ? { parent_id: query.parentId } : {}),
                }).toString()
        )
        if (!responseIsSuccessful(response)) {
            if (response.status === STATUS_SERVER_ERROR) {
                // The directory is probably empty.
                return []
            } else if (query.parentId != null) {
                return this.throw(
                    `Unable to list directory ${
                        title != null ? `'${title}'` : `with ID '${query.parentId}'`
                    }.`
                )
            } else {
                return this.throw('Unable to list root directory.')
            }
        } else {
            return (await response.json()).assets
                .map(
                    asset =>
                        // This type assertion is safe; it is only needed to convert `type` to a
                        // newtype.
                        // eslint-disable-next-line no-restricted-syntax
                        ({ ...asset, type: asset.id.match(/^(.+?)-/)?.[1] } as backend.AnyAsset)
                )
                .map(asset =>
                    asset.type === backend.AssetType.project &&
                    asset.projectState.type === backend.ProjectState.opened
                        ? { ...asset, projectState: { type: backend.ProjectState.openInProgress } }
                        : asset
                )
                .map(asset => ({
                    ...asset,
                    permissions: (asset.permissions ?? []).sort(backend.compareUserPermissions),
                }))
        }
    }

    /** Create a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createDirectory(
        body: backend.CreateDirectoryRequestBody
    ): Promise<backend.CreatedDirectory> {
        const response = await this.post<backend.CreatedDirectory>(CREATE_DIRECTORY_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to create directory with name '${body.title}'.`)
        } else {
            return await response.json()
        }
    }

    /** Change the name of a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async updateDirectory(
        directoryId: backend.DirectoryId,
        body: backend.UpdateDirectoryRequestBody,
        title: string | null
    ) {
        const response = await this.put<backend.UpdatedDirectory>(
            updateDirectoryPath(directoryId),
            body
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to update directory ${
                    title != null ? `'${title}'` : `with ID '${directoryId}'`
                }.`
            )
        } else {
            return await response.json()
        }
    }

    /** Change the name of a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteDirectory(directoryId: backend.DirectoryId, title: string | null) {
        const response = await this.delete(deleteDirectoryPath(directoryId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to delete directory ${
                    title != null ? `'${title}'` : `with ID '${directoryId}'`
                }.`
            )
        } else {
            return
        }
    }

    /** Return a list of projects belonging to the current user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listProjects(): Promise<backend.ListedProject[]> {
        const response = await this.get<ListProjectsResponseBody>(LIST_PROJECTS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Unable to list projects.')
        } else {
            return (await response.json()).projects.map(project => ({
                ...project,
                jsonAddress:
                    project.address != null ? backend.Address(`${project.address}json`) : null,
                binaryAddress:
                    project.address != null ? backend.Address(`${project.address}binary`) : null,
            }))
        }
    }

    /** Create a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createProject(body: backend.CreateProjectRequestBody): Promise<backend.CreatedProject> {
        const response = await this.post<backend.CreatedProject>(CREATE_PROJECT_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to create project with name '${body.projectName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Close a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async closeProject(projectId: backend.ProjectId, title: string | null): Promise<void> {
        const response = await this.post(closeProjectPath(projectId), {})
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to close project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }.`
            )
        } else {
            return
        }
    }

    /** Return details for a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async getProjectDetails(
        projectId: backend.ProjectId,
        title: string | null
    ): Promise<backend.Project> {
        const response = await this.get<backend.ProjectRaw>(getProjectDetailsPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to get details of project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }.`
            )
        } else {
            const project = await response.json()
            const ideVersion =
                project.ide_version ??
                (
                    await this.listVersions({
                        versionType: backend.VersionType.ide,
                        default: true,
                    })
                )[0]?.number
            if (ideVersion == null) {
                return this.throw('No IDE version found')
            } else {
                return {
                    ...project,
                    ideVersion,
                    engineVersion: project.engine_version,
                    jsonAddress:
                        project.address != null ? backend.Address(`${project.address}json`) : null,
                    binaryAddress:
                        project.address != null
                            ? backend.Address(`${project.address}binary`)
                            : null,
                }
            }
        }
    }

    /** Prepare a project for execution.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async openProject(
        projectId: backend.ProjectId,
        body: backend.OpenProjectRequestBody | null,
        title: string | null
    ): Promise<void> {
        const response = await this.post(
            openProjectPath(projectId),
            body ?? DEFAULT_OPEN_PROJECT_BODY
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}.`
            )
        } else {
            return
        }
    }

    /** Update the name or AMI of a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async projectUpdate(
        projectId: backend.ProjectId,
        body: backend.ProjectUpdateRequestBody,
        title: string | null
    ): Promise<backend.UpdatedProject> {
        const response = await this.put<backend.UpdatedProject>(projectUpdatePath(projectId), body)
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to update project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }.`
            )
        } else {
            return await response.json()
        }
    }

    /** Delete a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteProject(projectId: backend.ProjectId, title: string | null): Promise<void> {
        const response = await this.delete(deleteProjectPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to delete project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }.`
            )
        } else {
            return
        }
    }

    /** Return the resource usage of a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async checkResources(
        projectId: backend.ProjectId,
        title: string | null
    ): Promise<backend.ResourceUsage> {
        const response = await this.get<backend.ResourceUsage>(checkResourcesPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to get resource usage for project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }.`
            )
        } else {
            return await response.json()
        }
    }

    /** Return a list of files accessible by the current user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listFiles(): Promise<backend.File[]> {
        const response = await this.get<ListFilesResponseBody>(LIST_FILES_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Unable to list files.')
        } else {
            return (await response.json()).files
        }
    }

    /** Upload a file.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async uploadFile(
        params: backend.UploadFileRequestParams,
        body: Blob
    ): Promise<backend.FileInfo> {
        const response = await this.postBinary<backend.FileInfo>(
            UPLOAD_FILE_PATH +
                '?' +
                new URLSearchParams({
                    /* eslint-disable @typescript-eslint/naming-convention */
                    ...(params.fileName != null ? { file_name: params.fileName } : {}),
                    ...(params.fileId != null ? { file_id: params.fileId } : {}),
                    ...(params.parentDirectoryId
                        ? { parent_directory_id: params.parentDirectoryId }
                        : {}),
                    /* eslint-enable @typescript-eslint/naming-convention */
                }).toString(),
            body
        )
        if (!responseIsSuccessful(response)) {
            let suffix = '.'
            try {
                const error = errorModule.tryGetError<unknown>(await response.json())
                if (error != null) {
                    suffix = `: ${error}`
                }
            } catch {
                // Ignored.
            }
            if (params.fileName != null) {
                return this.throw(`Could not upload file with name '${params.fileName}'${suffix}`)
            } else if (params.fileId != null) {
                return this.throw(`Could not upload file with ID '${params.fileId}'${suffix}`)
            } else {
                return this.throw(`Could not upload file${suffix}`)
            }
        } else {
            return await response.json()
        }
    }

    /** Delete a file.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteFile(fileId: backend.FileId, title: string | null): Promise<void> {
        const response = await this.delete(deleteFilePath(fileId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to delete file ${title != null ? `'${title}'` : `with ID '${fileId}'`}.`
            )
        } else {
            return
        }
    }

    /** Create a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createSecret(body: backend.CreateSecretRequestBody): Promise<backend.SecretAndInfo> {
        const response = await this.post<backend.SecretAndInfo>(CREATE_SECRET_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to create secret with name '${body.secretName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Return a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async getSecret(secretId: backend.SecretId, title: string | null): Promise<backend.Secret> {
        const response = await this.get<backend.Secret>(getSecretPath(secretId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to get secret ${title != null ? `'${title}'` : `with ID '${secretId}'`}.`
            )
        } else {
            return await response.json()
        }
    }

    /** Return the secret environment variables accessible by the user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listSecrets(): Promise<backend.SecretInfo[]> {
        const response = await this.get<ListSecretsResponseBody>(LIST_SECRETS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Unable to list secrets.')
        } else {
            return (await response.json()).secrets
        }
    }

    /** Delete a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteSecret(secretId: backend.SecretId, title: string | null): Promise<void> {
        const response = await this.delete(deleteSecretPath(secretId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to delete secret ${title != null ? `'${title}'` : `with ID '${secretId}'`}.`
            )
        } else {
            return
        }
    }

    /** Create a file tag or project tag.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createTag(body: backend.CreateTagRequestBody): Promise<backend.TagInfo> {
        const response = await this.post<backend.TagInfo>(CREATE_TAG_PATH, {
            /* eslint-disable @typescript-eslint/naming-convention */
            tag_name: body.name,
            tag_value: body.value,
            object_type: body.objectType,
            object_id: body.objectId,
            /* eslint-enable @typescript-eslint/naming-convention */
        })
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to create create tag with name '${body.name}'.`)
        } else {
            return await response.json()
        }
    }

    /** Return file tags or project tags accessible by the user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listTags(params: backend.ListTagsRequestParams): Promise<backend.Tag[]> {
        const response = await this.get<ListTagsResponseBody>(
            LIST_TAGS_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    tag_type: params.tagType,
                }).toString()
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to list tags of type '${params.tagType}'.`)
        } else {
            return (await response.json()).tags
        }
    }

    /** Delete a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteTag(tagId: backend.TagId): Promise<void> {
        const response = await this.delete(deleteTagPath(tagId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to delete tag with ID '${tagId}'.`)
        } else {
            return
        }
    }

    /** Return list of backend or IDE versions.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listVersions(params: backend.ListVersionsRequestParams): Promise<backend.Version[]> {
        const response = await this.get<ListVersionsResponseBody>(
            LIST_VERSIONS_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    version_type: params.versionType,
                    default: String(params.default),
                }).toString()
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to list versions of type '${params.versionType}'.`)
        } else {
            return (await response.json()).versions
        }
    }

    /** Send an HTTP GET request to the given path. */
    private get<T = void>(path: string) {
        return this.client.get<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
    }

    /** Send a JSON HTTP POST request to the given path. */
    private post<T = void>(path: string, payload: object) {
        return this.client.post<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
    }

    /** Send a binary HTTP POST request to the given path. */
    private postBinary<T = void>(path: string, payload: Blob) {
        return this.client.postBinary<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
    }

    /** Send a JSON HTTP PUT request to the given path. */
    private put<T = void>(path: string, payload: object) {
        return this.client.put<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
    }

    /** Send an HTTP DELETE request to the given path. */
    private delete<T = void>(path: string) {
        return this.client.delete<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`)
    }
}
