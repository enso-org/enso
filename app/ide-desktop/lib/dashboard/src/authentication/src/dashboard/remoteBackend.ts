/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as backendModule from './backend'
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

/** The number of milliseconds in one day. */
const ONE_DAY_MS = 86_400_000

/** Default HTTP body for an "open project" request. */
const DEFAULT_OPEN_PROJECT_BODY: backendModule.OpenProjectRequestBody = {
    forceCreate: false,
    executeAsync: false,
}

// ============================
// === responseIsSuccessful ===
// ============================

/** Whether a response has a success HTTP status code (200-299). */
function responseIsSuccessful(response: Response) {
    return response.status >= STATUS_SUCCESS_FIRST && response.status <= STATUS_SUCCESS_LAST
}

// ===============================
// === waitUntilProjectIsReady ===
// ===============================

/** The interval between requests checking whether the IDE is ready. */
const CHECK_STATUS_INTERVAL_MS = 5000

/** Return a {@link Promise} that resolves only when a project is ready to open. */
export async function waitUntilProjectIsReady(
    backend: backendModule.Backend,
    item: backendModule.ProjectAsset,
    abortController: AbortController = new AbortController()
) {
    let project = await backend.getProjectDetails(item.id, item.title)
    if (!backendModule.DOES_PROJECT_STATE_INDICATE_VM_EXISTS[project.state.type]) {
        await backend.openProject(item.id, null, item.title)
    }
    let nextCheckTimestamp = 0
    while (
        !abortController.signal.aborted &&
        project.state.type !== backendModule.ProjectState.opened
    ) {
        await new Promise<void>(resolve => {
            const delayMs = nextCheckTimestamp - Number(new Date())
            setTimeout(resolve, Math.max(0, delayMs))
        })
        nextCheckTimestamp = Number(new Date()) + CHECK_STATUS_INTERVAL_MS
        project = await backend.getProjectDetails(item.id, item.title)
    }
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
/** Relative HTTP path to the "undo delete asset" endpoint of the Cloud backend API. */
const UNDO_DELETE_ASSET_PATH = 'assets'
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
/** Relative HTTP path to the "delete asset" endpoint of the Cloud backend API. */
function deleteAssetPath(assetId: backendModule.AssetId) {
    return `assets/${assetId}`
}
/** Relative HTTP path to the "update directory" endpoint of the Cloud backend API. */
function updateDirectoryPath(directoryId: backendModule.DirectoryId) {
    return `directories/${directoryId}`
}
/** Relative HTTP path to the "close project" endpoint of the Cloud backend API. */
function closeProjectPath(projectId: backendModule.ProjectId) {
    return `projects/${projectId}/close`
}
/** Relative HTTP path to the "get project details" endpoint of the Cloud backend API. */
function getProjectDetailsPath(projectId: backendModule.ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "open project" endpoint of the Cloud backend API. */
function openProjectPath(projectId: backendModule.ProjectId) {
    return `projects/${projectId}/open`
}
/** Relative HTTP path to the "project update" endpoint of the Cloud backend API. */
function projectUpdatePath(projectId: backendModule.ProjectId) {
    return `projects/${projectId}`
}
/** Relative HTTP path to the "check resources" endpoint of the Cloud backend API. */
function checkResourcesPath(projectId: backendModule.ProjectId) {
    return `projects/${projectId}/resources`
}
/** Relative HTTP path to the "get project" endpoint of the Cloud backend API. */
function getSecretPath(secretId: backendModule.SecretId) {
    return `secrets/${secretId}`
}
/** Relative HTTP path to the "delete tag" endpoint of the Cloud backend API. */
function deleteTagPath(tagId: backendModule.TagId) {
    return `secrets/${tagId}`
}

// =============
// === Types ===
// =============

/** HTTP response body for the "list users" endpoint. */
interface ListUsersResponseBody {
    users: backendModule.SimpleUser[]
}

/** HTTP response body for the "list projects" endpoint. */
interface ListDirectoryResponseBody {
    assets: backendModule.AnyAsset[]
}

/** HTTP response body for the "list projects" endpoint. */
interface ListProjectsResponseBody {
    projects: backendModule.ListedProjectRaw[]
}

/** HTTP response body for the "list files" endpoint. */
interface ListFilesResponseBody {
    files: backendModule.File[]
}

/** HTTP response body for the "list secrets" endpoint. */
interface ListSecretsResponseBody {
    secrets: backendModule.SecretInfo[]
}

/** HTTP response body for the "list tag" endpoint. */
interface ListTagsResponseBody {
    tags: backendModule.Tag[]
}

/** HTTP response body for the "list versions" endpoint. */
interface ListVersionsResponseBody {
    versions: [backendModule.Version, ...backendModule.Version[]]
}

// =====================
// === RemoteBackend ===
// =====================

/** Information for a cached default version. */
interface DefaultVersionInfo {
    version: backendModule.VersionNumber
    lastUpdatedEpochMs: number
}

/** Class for sending requests to the Cloud backend API endpoints. */
export class RemoteBackend extends backendModule.Backend {
    readonly type = backendModule.BackendType.remote
    protected defaultVersions: Partial<Record<backendModule.VersionType, DefaultVersionInfo>> = {}

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
    override rootDirectoryId(
        user: backendModule.UserOrOrganization | null
    ): backendModule.DirectoryId {
        return backendModule.DirectoryId(
            // `user` is only null when the user is offline, in which case the remote backend cannot
            // be accessed anyway.
            user != null
                ? user.id.replace(/^organization-/, `${backendModule.AssetType.directory}-`)
                : ''
        )
    }

    /** Return a list of all users in the same organization. */
    override async listUsers(): Promise<backendModule.SimpleUser[]> {
        const response = await this.get<ListUsersResponseBody>(LIST_USERS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not list users in the organization.`)
        } else {
            return (await response.json()).users
        }
    }

    /** Set the username and parent organization of the current user. */
    override async createUser(
        body: backendModule.CreateUserRequestBody
    ): Promise<backendModule.UserOrOrganization> {
        const response = await this.post<backendModule.UserOrOrganization>(CREATE_USER_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw('Could not create user.')
        } else {
            return await response.json()
        }
    }

    /** Invite a new user to the organization by email. */
    override async inviteUser(body: backendModule.InviteUserRequestBody): Promise<void> {
        const response = await this.post(INVITE_USER_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not invite user '${body.userEmail}'.`)
        } else {
            return
        }
    }

    /** Adds a permission for a specific user on a specific asset. */
    override async createPermission(
        body: backendModule.CreatePermissionRequestBody
    ): Promise<void> {
        const response = await this.post<backendModule.UserOrOrganization>(
            CREATE_PERMISSION_PATH,
            body
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not set permissions.`)
        } else {
            return
        }
    }

    /** Return organization info for the current user.
     *
     * @returns `null` if a non-successful status code (not 200-299) was received. */
    override async usersMe(): Promise<backendModule.UserOrOrganization | null> {
        const response = await this.get<backendModule.UserOrOrganization>(USERS_ME_PATH)
        if (!responseIsSuccessful(response)) {
            return null
        } else {
            return await response.json()
        }
    }

    /** Return a list of assets in a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async listDirectory(
        query: backendModule.ListDirectoryRequestParams,
        title: string | null
    ): Promise<backendModule.AnyAsset[]> {
        const response = await this.get<ListDirectoryResponseBody>(
            LIST_DIRECTORY_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    ...(query.parentId != null ? { parent_id: query.parentId } : {}),
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    ...(query.filterBy != null ? { filter_by: query.filterBy } : {}),
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    ...(query.recentProjects ? { recent_projects: String(true) } : {}),
                }).toString()
        )
        if (!responseIsSuccessful(response)) {
            if (response.status === STATUS_SERVER_ERROR) {
                // The directory is probably empty.
                return []
            } else if (query.parentId != null) {
                return this.throw(
                    `Could not list folder ${
                        title != null ? `'${title}'` : `with ID '${query.parentId}'`
                    }.`
                )
            } else {
                return this.throw('Could not list root folder.')
            }
        } else {
            return (await response.json()).assets
                .map(
                    asset =>
                        // This type assertion is safe; it is only needed to convert `type` to a
                        // newtype.
                        // eslint-disable-next-line no-restricted-syntax
                        ({
                            ...asset,
                            type: asset.id.match(/^(.+?)-/)?.[1],
                        }) as backendModule.AnyAsset
                )
                .map(asset => ({
                    ...asset,
                    permissions: [...(asset.permissions ?? [])].sort(
                        backendModule.compareUserPermissions
                    ),
                }))
        }
    }

    /** Create a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async createDirectory(
        body: backendModule.CreateDirectoryRequestBody
    ): Promise<backendModule.CreatedDirectory> {
        const response = await this.post<backendModule.CreatedDirectory>(
            CREATE_DIRECTORY_PATH,
            body
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not create folder with name '${body.title}'.`)
        } else {
            return await response.json()
        }
    }

    /** Change the name of a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async updateDirectory(
        directoryId: backendModule.DirectoryId,
        body: backendModule.UpdateDirectoryRequestBody,
        title: string | null
    ) {
        const response = await this.put<backendModule.UpdatedDirectory>(
            updateDirectoryPath(directoryId),
            body
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Could not update folder ${
                    title != null ? `'${title}'` : `with ID '${directoryId}'`
                }.`
            )
        } else {
            return await response.json()
        }
    }

    /** Delete an arbitrary asset.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async deleteAsset(assetId: backendModule.AssetId, title: string | null) {
        const response = await this.delete(deleteAssetPath(assetId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to delete ${title != null ? `'${title}'` : `asset with ID '${assetId}'`}.`
            )
        } else {
            return
        }
    }

    /** Restore an arbitrary asset from the trash.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async undoDeleteAsset(
        assetId: backendModule.AssetId,
        title: string | null
    ): Promise<void> {
        const response = await this.patch(UNDO_DELETE_ASSET_PATH, { assetId })
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Unable to restore ${
                    title != null ? `'${title}'` : `asset with ID '${assetId}'`
                } from Trash.`
            )
        } else {
            return
        }
    }

    /** Return a list of projects belonging to the current user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async listProjects(): Promise<backendModule.ListedProject[]> {
        const response = await this.get<ListProjectsResponseBody>(LIST_PROJECTS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Could not list projects.')
        } else {
            return (await response.json()).projects.map(project => ({
                ...project,
                jsonAddress:
                    project.address != null
                        ? backendModule.Address(`${project.address}json`)
                        : null,
                binaryAddress:
                    project.address != null
                        ? backendModule.Address(`${project.address}binary`)
                        : null,
            }))
        }
    }

    /** Create a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async createProject(
        body: backendModule.CreateProjectRequestBody
    ): Promise<backendModule.CreatedProject> {
        const response = await this.post<backendModule.CreatedProject>(CREATE_PROJECT_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not create project with name '${body.projectName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Close a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async closeProject(
        projectId: backendModule.ProjectId,
        title: string | null
    ): Promise<void> {
        const response = await this.post(closeProjectPath(projectId), {})
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Could not close project ${
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
    override async getProjectDetails(
        projectId: backendModule.ProjectId,
        title: string | null
    ): Promise<backendModule.Project> {
        const response = await this.get<backendModule.ProjectRaw>(getProjectDetailsPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Could not get details of project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }.`
            )
        } else {
            const project = await response.json()
            const ideVersion =
                project.ide_version ?? (await this.getDefaultVersion(backendModule.VersionType.ide))
            return {
                ...project,
                ideVersion,
                engineVersion: project.engine_version,
                jsonAddress:
                    project.address != null
                        ? backendModule.Address(`${project.address}json`)
                        : null,
                binaryAddress:
                    project.address != null
                        ? backendModule.Address(`${project.address}binary`)
                        : null,
            }
        }
    }

    /** Prepare a project for execution.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async openProject(
        projectId: backendModule.ProjectId,
        body: backendModule.OpenProjectRequestBody | null,
        title: string | null
    ): Promise<void> {
        const response = await this.post(
            openProjectPath(projectId),
            body ?? DEFAULT_OPEN_PROJECT_BODY
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Could not open project ${title != null ? `'${title}'` : `with ID '${projectId}'`}.`
            )
        } else {
            return
        }
    }

    /** Update the name or AMI of a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async projectUpdate(
        projectId: backendModule.ProjectId,
        body: backendModule.ProjectUpdateRequestBody,
        title: string | null
    ): Promise<backendModule.UpdatedProject> {
        const response = await this.put<backendModule.UpdatedProject>(
            projectUpdatePath(projectId),
            body
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Could not update project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }.`
            )
        } else {
            return await response.json()
        }
    }

    /** Return the resource usage of a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async checkResources(
        projectId: backendModule.ProjectId,
        title: string | null
    ): Promise<backendModule.ResourceUsage> {
        const response = await this.get<backendModule.ResourceUsage>(checkResourcesPath(projectId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Could not get resource usage for project ${
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
    override async listFiles(): Promise<backendModule.File[]> {
        const response = await this.get<ListFilesResponseBody>(LIST_FILES_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Could not list files.')
        } else {
            return (await response.json()).files
        }
    }

    /** Upload a file.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async uploadFile(
        params: backendModule.UploadFileRequestParams,
        body: Blob
    ): Promise<backendModule.FileInfo> {
        const response = await this.postBinary<backendModule.FileInfo>(
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

    /** Create a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async createSecret(
        body: backendModule.CreateSecretRequestBody
    ): Promise<backendModule.SecretAndInfo> {
        const response = await this.post<backendModule.SecretAndInfo>(CREATE_SECRET_PATH, body)
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not create secret with name '${body.secretName}'.`)
        } else {
            return await response.json()
        }
    }

    /** Return a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async getSecret(
        secretId: backendModule.SecretId,
        title: string | null
    ): Promise<backendModule.Secret> {
        const response = await this.get<backendModule.Secret>(getSecretPath(secretId))
        if (!responseIsSuccessful(response)) {
            return this.throw(
                `Could not get secret ${title != null ? `'${title}'` : `with ID '${secretId}'`}.`
            )
        } else {
            return await response.json()
        }
    }

    /** Return the secret environment variables accessible by the user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async listSecrets(): Promise<backendModule.SecretInfo[]> {
        const response = await this.get<ListSecretsResponseBody>(LIST_SECRETS_PATH)
        if (!responseIsSuccessful(response)) {
            return this.throw('Could not list secrets.')
        } else {
            return (await response.json()).secrets
        }
    }

    /** Create a file tag or project tag.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async createTag(
        body: backendModule.CreateTagRequestBody
    ): Promise<backendModule.TagInfo> {
        const response = await this.post<backendModule.TagInfo>(CREATE_TAG_PATH, {
            /* eslint-disable @typescript-eslint/naming-convention */
            tag_name: body.name,
            tag_value: body.value,
            object_type: body.objectType,
            object_id: body.objectId,
            /* eslint-enable @typescript-eslint/naming-convention */
        })
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not create create tag with name '${body.name}'.`)
        } else {
            return await response.json()
        }
    }

    /** Return file tags or project tags accessible by the user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async listTags(
        params: backendModule.ListTagsRequestParams
    ): Promise<backendModule.Tag[]> {
        const response = await this.get<ListTagsResponseBody>(
            LIST_TAGS_PATH +
                '?' +
                new URLSearchParams({
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    tag_type: params.tagType,
                }).toString()
        )
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not list tags of type '${params.tagType}'.`)
        } else {
            return (await response.json()).tags
        }
    }

    /** Delete a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async deleteTag(tagId: backendModule.TagId): Promise<void> {
        const response = await this.delete(deleteTagPath(tagId))
        if (!responseIsSuccessful(response)) {
            return this.throw(`Could not delete tag with ID '${tagId}'.`)
        } else {
            return
        }
    }

    /** Return a list of backend or IDE versions.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    override async listVersions(
        params: backendModule.ListVersionsRequestParams
    ): Promise<backendModule.Version[]> {
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
            return this.throw(`Could not list versions of type '${params.versionType}'.`)
        } else {
            return (await response.json()).versions
        }
    }

    /** Get the default version given the type of version (IDE or backend). */
    protected async getDefaultVersion(versionType: backendModule.VersionType) {
        const cached = this.defaultVersions[versionType]
        const nowEpochMs = Number(new Date())
        if (cached != null && nowEpochMs - cached.lastUpdatedEpochMs < ONE_DAY_MS) {
            return cached.version
        } else {
            const version = (
                await this.listVersions({
                    versionType,
                    default: true,
                })
            )[0]?.number
            if (version == null) {
                return this.throw(`No default ${versionType} version found.`)
            } else {
                const info: DefaultVersionInfo = {
                    version,
                    lastUpdatedEpochMs: nowEpochMs,
                }
                this.defaultVersions[versionType] = info
                return info.version
            }
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

    /** Send a JSON HTTP PATCH request to the given path. */
    private patch<T = void>(path: string, payload: object) {
        return this.client.patch<T>(`${config.ACTIVE_CONFIG.apiUrl}/${path}`, payload)
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
