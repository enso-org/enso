/** @file Module containing the API client for the Cloud backend API.
 *
 * Each exported function in the {@link RemoteBackend} in this module corresponds to
 * an API endpoint. The functions are asynchronous and return a {@link Promise} that resolves to
 * the response from the API. */
import * as backend from '../../../../src/authentication/src/dashboard/backend'
import * as config from '../../../../src/authentication/src/config'
import * as dateTime from '../../../../src/authentication/src/dashboard/dateTime'
import * as http from '../../../../src/authentication/src/http'
import * as loggerProvider from '../../../../src/authentication/src/providers/logger'

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
            throw new Error('Authorization header not set.')
        } else {
            if (IS_DEV_MODE) {
                // @ts-expect-error This exists only for debugging purposes. It does not have types
                // because it MUST NOT be used in this codebase.
                window.remoteBackend = this
            }
            return
        }
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
        return await Promise.resolve<backend.CreatedProject>({
            name: body.projectName,
            organizationId: '',
            packageName: '',
            projectId: backend.ProjectId(''),
            state: { type: backend.ProjectState.closed },
        })
    }

    /** Close a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async closeProject(): Promise<void> {
        await Promise.resolve()
    }

    /** Return details for a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async getProjectDetails(
        projectId: backend.ProjectId,
        title: string | null
    ): Promise<backend.Project> {
        return await Promise.resolve<backend.Project>({
            name: title ?? '',
            packageName: 'Project_root',
            projectId,
            engineVersion: null,
            ideVersion: { lifecycle: backend.VersionLifecycle.development, value: '2023.2.1-dev' },
            binaryAddress: null,
            jsonAddress: null,
            organizationId: backend.UserOrOrganizationId(''),
            state: { type: backend.ProjectState.closed },
        })
    }

    /** Prepare a project for execution.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async openProject(): Promise<void> {
        await Promise.resolve()
    }

    /** Update the name or AMI of a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async projectUpdate(
        projectId: backend.ProjectId,
        _body: backend.ProjectUpdateRequestBody,
        title: string | null
    ): Promise<backend.UpdatedProject> {
        return await Promise.resolve<backend.UpdatedProject>({
            name: title ?? '',
            organizationId: '',
            projectId,
            engineVersion: null,
            ideVersion: null,
            ami: null,
        })
    }

    /** Delete a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteProject(): Promise<void> {
        await Promise.resolve()
    }

    /** Return the resource usage of a project.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async checkResources(): Promise<backend.ResourceUsage> {
        return await Promise.resolve<backend.ResourceUsage>({
            cpu: 0,
            memory: 0,
            storage: 0,
        })
    }

    /** Return a list of files accessible by the current user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listFiles(): Promise<backend.File[]> {
        return await Promise.resolve([])
    }

    /** Upload a file.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async uploadFile(): Promise<backend.FileInfo> {
        await Promise.resolve()
        throw new Error('Not implemented.')
    }

    /** Delete a file.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteFile(): Promise<void> {
        await Promise.resolve()
    }

    /** Create a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createSecret(body: backend.CreateSecretRequestBody): Promise<backend.SecretAndInfo> {
        return await Promise.resolve<backend.SecretAndInfo>({
            id: backend.SecretId(''),
            name: body.secretName,
            value: body.secretValue,
        })
    }

    /** Return a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async getSecret(secretId: backend.SecretId): Promise<backend.Secret> {
        return await Promise.resolve<backend.Secret>({
            id: secretId,
            value: '',
        })
    }

    /** Return the secret environment variables accessible by the user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listSecrets(): Promise<backend.SecretInfo[]> {
        return await Promise.resolve([])
    }

    /** Delete a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteSecret(): Promise<void> {
        await Promise.resolve()
    }

    /** Create a file tag or project tag.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async createTag(body: backend.CreateTagRequestBody): Promise<backend.TagInfo> {
        return await Promise.resolve<backend.TagInfo>({
            id: backend.TagId(''),
            name: body.name,
            value: body.value,
        })
    }

    /** Return file tags or project tags accessible by the user.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listTags(): Promise<backend.Tag[]> {
        return await Promise.resolve([])
    }

    /** Delete a secret environment variable.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async deleteTag(): Promise<void> {
        await Promise.resolve()
    }

    /** Return list of backend or IDE versions.
     *
     * @throws An error if a non-successful status code (not 200-299) was received. */
    async listVersions(
        params: backend.ListVersionsRequestParams
    ): Promise<[backend.Version, ...backend.Version[]]> {
        return await Promise.resolve<[backend.Version, ...backend.Version[]]>([
            {
                ami: null,
                created: dateTime.toRfc3339(new Date()),
                number: { lifecycle: backend.VersionLifecycle.development, value: '2023.2.1-dev' },
                // eslint-disable-next-line @typescript-eslint/naming-convention
                version_type: params.versionType,
            },
        ])
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
