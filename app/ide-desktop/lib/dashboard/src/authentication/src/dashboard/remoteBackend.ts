/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to an API endpoint. The
 * functions are asynchronous and return a `Promise` that resolves to the response from the API. */
import * as backend from './backend'
import * as config from '../config'
import * as http from '../http'
import * as loggerProvider from '../providers/logger'
import * as newtype from '../newtype'
import * as platformModule from '../platform'

// =================
// === Constants ===
// =================

/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_FIRST = 200
/** HTTP status indicating that the request was successful. */
const STATUS_SUCCESS_LAST = 299

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

/** HTTP response body for the "list projects" endpoint. */
interface ListDirectoryResponseBody {
    assets: backend.BaseAsset[]
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
export class RemoteBackend implements backend.Backend {
    readonly platform = platformModule.Platform.cloud

    /** Creates a new instance of the {@link RemoteBackend} API client.
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
    async createUser(body: backend.CreateUserRequestBody): Promise<backend.UserOrOrganization> {
        const response = await this.post<backend.UserOrOrganization>(CREATE_USER_PATH, body)
        return await response.json()
    }

    /** Returns organization info for the current user, from the Cloud backend API.
     *
     * @returns `null` if status code 401 or 404 was received. */
    async usersMe(): Promise<backend.UserOrOrganization | null> {
        const response = await this.get<backend.UserOrOrganization>(USERS_ME_PATH)
        if (!responseIsSuccessful(response)) {
            return null
        } else {
            return await response.json()
        }
    }

    /** Returns a list of assets in a directory, from the Cloud backend API.
     *
     * @throws An error if status code 401 or 404 was received.
     */
    async listDirectory(query: backend.ListDirectoryRequestParams): Promise<backend.Asset[]> {
        const response = await this.get<ListDirectoryResponseBody>(
            LIST_DIRECTORY_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    ...(query.parentId ? { parent_id: query.parentId } : {}),
                }).toString()
        )
        if (!responseIsSuccessful(response)) {
            if (query.parentId) {
                return this.throw(`Unable to list directory with ID '${query.parentId}'.`)
            } else {
                return this.throw('Unable to list root directory.')
            }
        } else {
            return (await response.json()).assets.map(
                // This type assertion is safe; it is only needed to convert `type` to a newtype.
                // eslint-disable-next-line no-restricted-syntax
                asset => ({ ...asset, type: asset.id.match(/^(.+?)-/)?.[1] } as backend.Asset)
            )
        }
    }

    /** Creates a directory, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async createDirectory(body: backend.CreateDirectoryRequestBody): Promise<backend.Directory> {
        const response = await this.post<backend.Directory>(CREATE_DIRECTORY_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to create directory with name '${body.title}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns a list of projects belonging to the current user, from the Cloud backend API.
     *
     * @throws An error if status code 401 or 404 was received.
     */
    async listProjects(): Promise<backend.ListedProject[]> {
        const response = await this.get<ListProjectsResponseBody>(LIST_PROJECTS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Unable to list projects.')
        } else {
            return (await response.json()).projects.map(project => ({
                ...project,
                jsonAddress:
                    project.address != null
                        ? newtype.asNewtype<backend.Address>(`${project.address}json`)
                        : null,
                binaryAddress:
                    project.address != null
                        ? newtype.asNewtype<backend.Address>(`${project.address}binary`)
                        : null,
            }))
        }
    }

    /** Creates a project for the current user, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async createProject(body: backend.CreateProjectRequestBody): Promise<backend.CreatedProject> {
        const response = await this.post<backend.CreatedProject>(CREATE_PROJECT_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to create project with name '${body.projectName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Closes the project identified by the given project ID, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async closeProject(projectId: backend.ProjectId): Promise<void> {
        const response = await this.post(closeProjectPath(projectId), {})
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to close project with ID '${projectId}'.`)
        } else {
            return
        }
    }

    /** Returns project details for the specified project ID, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async getProjectDetails(projectId: backend.ProjectId): Promise<backend.Project> {
        const response = await this.get<backend.ProjectRaw>(getProjectDetailsPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to get details of project with ID '${projectId}'.`)
        } else {
            const project = await response.json()
            return {
                ...project,
                jsonAddress:
                    project.address != null
                        ? newtype.asNewtype<backend.Address>(`${project.address}json`)
                        : null,
                binaryAddress:
                    project.address != null
                        ? newtype.asNewtype<backend.Address>(`${project.address}binary`)
                        : null,
            }
        }
    }

    /** Sets project to an open state, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async openProject(
        projectId: backend.ProjectId,
        body: backend.OpenProjectRequestBody = DEFAULT_OPEN_PROJECT_BODY
    ): Promise<void> {
        const response = await this.post(openProjectPath(projectId), body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to open project with ID '${projectId}'.`)
        } else {
            return
        }
    }

    async projectUpdate(
        projectId: backend.ProjectId,
        body: backend.ProjectUpdateRequestBody
    ): Promise<backend.UpdatedProject> {
        const response = await this.put<backend.UpdatedProject>(projectUpdatePath(projectId), body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to update project with ID '${projectId}'.`)
        } else {
            return await response.json()
        }
    }

    /** Deletes project, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async deleteProject(projectId: backend.ProjectId): Promise<void> {
        const response = await this.delete(deleteProjectPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to delete project with ID '${projectId}'.`)
        } else {
            return
        }
    }

    /** Returns project memory, processor and storage usage, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async checkResources(projectId: backend.ProjectId): Promise<backend.ResourceUsage> {
        const response = await this.get<backend.ResourceUsage>(checkResourcesPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to get resource usage for project with ID '${projectId}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns a list of files accessible by the current user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async listFiles(): Promise<backend.File[]> {
        const response = await this.get<ListFilesResponseBody>(LIST_FILES_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Unable to list files.')
        } else {
            return (await response.json()).files
        }
    }

    /** Uploads a file, to the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async uploadFile(
        params: backend.UploadFileRequestParams,
        body: Blob
    ): Promise<backend.FileInfo> {
        const response = await this.postBase64<backend.FileInfo>(
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
        if (!responseIsSuccessful(response)) {
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
    async deleteFile(fileId: backend.FileId): Promise<void> {
        const response = await this.delete(deleteFilePath(fileId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to delete file with ID '${fileId}'.`)
        } else {
            return
        }
    }

    /** Creates a secret environment variable, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async createSecret(body: backend.CreateSecretRequestBody): Promise<backend.SecretAndInfo> {
        const response = await this.post<backend.SecretAndInfo>(CREATE_SECRET_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to create secret with name '${body.secretName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns a secret environment variable, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async getSecret(secretId: backend.SecretId): Promise<backend.Secret> {
        const response = await this.get<backend.Secret>(getSecretPath(secretId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to get secret with ID '${secretId}'.`)
        } else {
            return await response.json()
        }
    }

    /** Returns the secret environment variables accessible by the user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async listSecrets(): Promise<backend.SecretInfo[]> {
        const response = await this.get<ListSecretsResponseBody>(LIST_SECRETS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Unable to list secrets.')
        } else {
            return (await response.json()).secrets
        }
    }

    /** Deletes a secret environment variable, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async deleteSecret(secretId: backend.SecretId): Promise<void> {
        const response = await this.delete(deleteSecretPath(secretId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to delete secret with ID '${secretId}'.`)
        } else {
            return
        }
    }

    /** Creates a file tag or project tag, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
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

    /** Returns file tags or project tags accessible by the user, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
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

    /** Deletes a secret environment variable, on the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async deleteTag(tagId: backend.TagId): Promise<void> {
        const response = await this.delete(deleteTagPath(tagId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Unable to delete tag with ID '${tagId}'.`)
        } else {
            return
        }
    }

    /** Returns list of backend or IDE versions, from the Cloud backend API.
     *
     * @throws An error if a 401 or 404 status code was received. */
    async listVersions(
        params: backend.ListVersionsRequestParams
    ): Promise<[backend.Version, ...backend.Version[]]> {
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
