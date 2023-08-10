/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as backend from '../../../../src/authentication/src/dashboard/backend'
import * as config from '../../../../src/authentication/src/config'
import * as errorModule from '../../../../src/authentication/src/error'
import * as http from '../../../../src/authentication/src/http'
import * as loggerProvider from '../../../../src/authentication/src/providers/logger'

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

    /** Return a list of all users in the same organization. */
    async listUsers(): Promise<backend.SimpleUser[]> {
        return await Promise.resolve([])
    }

    /** Set the username and parent organization of the current user. */
    async createUser(body: backend.CreateUserRequestBody): Promise<backend.UserOrOrganization> {
        return await Promise.resolve({
            email: body.userEmail,
            name: body.userName,
            id: backend.UserOrOrganizationId('id'),
            isEnabled: true,
        })
    }

    /** Invite a new user to the organization by email. */
    async inviteUser(): Promise<void> {
        await Promise.resolve()
    }

    /** Adds a permission for a specific user on a specific asset. */
    async createPermission(): Promise<void> {
        await Promise.resolve()
    }

    /** Return organization info for the current user.
     *
     * @returns `null` if a non-successful status code (not 200-299) was received. */
    async usersMe(): Promise<backend.UserOrOrganization | null> {
        return await Promise.resolve({
            email: backend.EmailAddress('email@example.com'),
            name: 'user name',
            id: backend.UserOrOrganizationId('id'),
            isEnabled: true,
        })
    }

    /** Return a list of assets in a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listDirectory(): Promise<backend.AnyAsset[]> {
        return await Promise.resolve([])
    }

    /** Create a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createDirectory(
        body: backend.CreateDirectoryRequestBody
    ): Promise<backend.CreatedDirectory> {
        return await Promise.resolve({
            id: backend.DirectoryId('id'),
            parentId: body.parentId ?? backend.DirectoryId('parent id'),
            title: body.title,
        })
    }

    /** Change the name of a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async updateDirectory(
        directoryId: backend.DirectoryId,
        body: backend.UpdateDirectoryRequestBody
    ): Promise<backend.UpdatedDirectory> {
        return await Promise.resolve({
            id: directoryId,
            parentId: backend.DirectoryId('parent id'),
            title: body.title,
        })
    }

    /** Change the name of a directory.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteDirectory() {
        await Promise.resolve()
    }

    /** Return a list of projects belonging to the current user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listProjects(): Promise<backend.ListedProject[]> {
        return await Promise.resolve([])
    }

    /** Create a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createProject(body: backend.CreateProjectRequestBody): Promise<backend.CreatedProject> {
        return await Promise.resolve({})
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
            return {
                ...project,
                jsonAddress:
                    project.address != null ? backend.Address(`${project.address}json`) : null,
                binaryAddress:
                    project.address != null ? backend.Address(`${project.address}binary`) : null,
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
