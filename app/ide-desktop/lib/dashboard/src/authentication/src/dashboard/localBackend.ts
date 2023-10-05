/** @file Module containing the API client for the local backend API.
 *
 * Each exported function in the {@link LocalBackend} in this module corresponds to an API endpoint.
 * The functions are asynchronous and return a {@link Promise} that resolves to the response from
 * the API. */
import * as backend from './backend'
import * as dateTime from './dateTime'
import * as errorModule from '../error'
import * as projectManager from './projectManager'

// =============================
// === ipWithSocketToAddress ===
// =============================

/** Convert a {@link projectManager.IpWithSocket} to a {@link backend.Address}. */
function ipWithSocketToAddress(ipWithSocket: projectManager.IpWithSocket) {
    return backend.Address(`ws://${ipWithSocket.host}:${ipWithSocket.port}`)
}

// ====================
// === LocalBackend ===
// ====================

/** Class for sending requests to the Project Manager API endpoints.
 * This is used instead of the cloud backend API when managing local projects from the dashboard. */
export class LocalBackend extends backend.Backend {
    static currentlyOpeningProjectId: backend.ProjectId | null = null
    static currentlyOpenProjects = new Map<projectManager.ProjectId, projectManager.OpenProject>()
    readonly type = backend.BackendType.local
    private readonly projectManager: projectManager.ProjectManager

    /** Create a {@link LocalBackend}. */
    constructor(projectManagerUrl: string | null) {
        super()
        this.projectManager = projectManager.ProjectManager.default(projectManagerUrl)
        if (IS_DEV_MODE) {
            // @ts-expect-error This exists only for debugging purposes. It does not have types
            // because it MUST NOT be used in this codebase.
            window.localBackend = this
        }
    }

    /** Return the root directory id for the given user. */
    override rootDirectoryId(): backend.DirectoryId {
        return backend.DirectoryId('')
    }
    /** Return a list of assets in a directory.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async listDirectory(): Promise<backend.AnyAsset[]> {
        const result = await this.projectManager.listProjects({})
        return result.projects.map(project => ({
            type: backend.AssetType.project,
            id: project.id,
            title: project.name,
            modifiedAt: project.lastOpened ?? project.created,
            parentId: backend.DirectoryId(''),
            permissions: [],
            projectState: {
                type: LocalBackend.currentlyOpenProjects.has(project.id)
                    ? backend.ProjectState.opened
                    : project.id === LocalBackend.currentlyOpeningProjectId
                    ? backend.ProjectState.openInProgress
                    : backend.ProjectState.closed,
                // eslint-disable-next-line @typescript-eslint/naming-convention
                volume_id: '',
            },
        }))
    }

    /** Return a list of projects belonging to the current user.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async listProjects(): Promise<backend.ListedProject[]> {
        const result = await this.projectManager.listProjects({})
        return result.projects.map(project => ({
            name: project.name,
            organizationId: '',
            projectId: project.id,
            packageName: project.name,
            state: {
                type: backend.ProjectState.closed,
                // eslint-disable-next-line @typescript-eslint/naming-convention
                volume_id: '',
            },
            jsonAddress: null,
            binaryAddress: null,
        }))
    }

    /** Create a project.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async createProject(
        body: backend.CreateProjectRequestBody
    ): Promise<backend.CreatedProject> {
        const project = await this.projectManager.createProject({
            name: projectManager.ProjectName(body.projectName),
            ...(body.projectTemplateName != null
                ? { projectTemplate: body.projectTemplateName }
                : {}),
            missingComponentAction: projectManager.MissingComponentAction.install,
        })
        return {
            name: body.projectName,
            organizationId: '',
            projectId: project.projectId,
            packageName: body.projectName,
            state: {
                type: backend.ProjectState.closed,
                // eslint-disable-next-line @typescript-eslint/naming-convention
                volume_id: '',
            },
        }
    }

    /** Close the project identified by the given project ID.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async closeProject(projectId: backend.ProjectId, title: string | null): Promise<void> {
        if (LocalBackend.currentlyOpeningProjectId === projectId) {
            LocalBackend.currentlyOpeningProjectId = null
        }
        LocalBackend.currentlyOpenProjects.delete(projectId)
        try {
            await this.projectManager.closeProject({ projectId })
            return
        } catch (error) {
            throw new Error(
                `Could not close project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }: ${errorModule.tryGetMessage(error) ?? 'unknown error'}.`
            )
        }
    }

    /** Close the project identified by the given project ID.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async getProjectDetails(
        projectId: backend.ProjectId,
        title: string | null
    ): Promise<backend.Project> {
        const cachedProject = LocalBackend.currentlyOpenProjects.get(projectId)
        if (cachedProject == null) {
            const result = await this.projectManager.listProjects({})
            const project = result.projects.find(listedProject => listedProject.id === projectId)
            if (project == null) {
                throw new Error(
                    `Could not get details of project ${
                        title != null ? `'${title}'` : `with ID '${projectId}'`
                    }.`
                )
            } else {
                const version =
                    project.engineVersion == null
                        ? null
                        : {
                              lifecycle: backend.detectVersionLifecycle(project.engineVersion),
                              value: project.engineVersion,
                          }
                return {
                    name: project.name,
                    engineVersion: version,
                    ideVersion: version,
                    jsonAddress: null,
                    binaryAddress: null,
                    organizationId: '',
                    packageName: project.name,
                    projectId,
                    state: {
                        type:
                            projectId === LocalBackend.currentlyOpeningProjectId
                                ? backend.ProjectState.openInProgress
                                : project.lastOpened != null
                                ? backend.ProjectState.closed
                                : backend.ProjectState.created,
                        // eslint-disable-next-line @typescript-eslint/naming-convention
                        volume_id: '',
                    },
                }
            }
        } else {
            return {
                name: cachedProject.projectName,
                engineVersion: {
                    lifecycle: backend.detectVersionLifecycle(cachedProject.engineVersion),
                    value: cachedProject.engineVersion,
                },
                ideVersion: {
                    lifecycle: backend.detectVersionLifecycle(cachedProject.engineVersion),
                    value: cachedProject.engineVersion,
                },
                jsonAddress: ipWithSocketToAddress(cachedProject.languageServerJsonAddress),
                binaryAddress: ipWithSocketToAddress(cachedProject.languageServerBinaryAddress),
                organizationId: '',
                packageName: cachedProject.projectName,
                projectId,
                state: {
                    type: backend.ProjectState.opened,
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    volume_id: '',
                },
            }
        }
    }

    /** Prepare a project for execution.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async openProject(
        projectId: backend.ProjectId,
        _body: backend.OpenProjectRequestBody | null,
        title: string | null
    ): Promise<void> {
        LocalBackend.currentlyOpeningProjectId = projectId
        if (!LocalBackend.currentlyOpenProjects.has(projectId)) {
            try {
                const project = await this.projectManager.openProject({
                    projectId,
                    missingComponentAction: projectManager.MissingComponentAction.install,
                })
                LocalBackend.currentlyOpenProjects.set(projectId, project)
                return
            } catch (error) {
                throw new Error(
                    `Could not open project ${
                        title != null ? `'${title}'` : `with ID '${projectId}'`
                    }: ${errorModule.tryGetMessage(error) ?? 'unknown error'}.`
                )
            } finally {
                LocalBackend.currentlyOpeningProjectId = null
            }
        }
    }

    /** Change the name of a project.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async projectUpdate(
        projectId: backend.ProjectId,
        body: backend.ProjectUpdateRequestBody
    ): Promise<backend.UpdatedProject> {
        if (body.ami != null) {
            throw new Error('Cannot change project AMI on local backend.')
        } else {
            if (body.projectName != null) {
                await this.projectManager.renameProject({
                    projectId,
                    name: projectManager.ProjectName(body.projectName),
                })
            }
            const result = await this.projectManager.listProjects({})
            const project = result.projects.find(listedProject => listedProject.id === projectId)
            const version =
                project?.engineVersion == null
                    ? null
                    : {
                          lifecycle: backend.detectVersionLifecycle(project.engineVersion),
                          value: project.engineVersion,
                      }
            if (project == null) {
                throw new Error(`The project ID '${projectId}' is invalid.`)
            } else {
                return {
                    ami: null,
                    engineVersion: version,
                    ideVersion: version,
                    name: project.name,
                    organizationId: '',
                    projectId,
                }
            }
        }
    }

    /** Delete an arbitrary asset.
     *
     * @throws An error if the JSON-RPC call fails. */
    override async deleteAsset(assetId: backend.AssetId, title: string | null): Promise<void> {
        // This is SAFE, as the only asset type on the local backend is projects.
        // eslint-disable-next-line no-restricted-syntax
        const projectId = assetId as backend.ProjectId
        if (LocalBackend.currentlyOpeningProjectId === projectId) {
            LocalBackend.currentlyOpeningProjectId = null
        }
        LocalBackend.currentlyOpenProjects.delete(projectId)
        try {
            await this.projectManager.deleteProject({ projectId })
            return
        } catch (error) {
            throw new Error(
                `Could not delete project ${
                    title != null ? `'${title}'` : `with ID '${projectId}'`
                }: ${errorModule.tryGetMessage(error) ?? 'unknown error'}.`
            )
        }
    }

    /** Return a list of engine versions. */
    override async listVersions(params: backend.ListVersionsRequestParams) {
        const engineVersions = await this.projectManager.listAvailableEngineVersions()
        const engineVersionToVersion = (
            version: projectManager.EngineVersion
        ): backend.Version => ({
            ami: null,
            created: dateTime.toRfc3339(new Date()),
            number: {
                value: version.version,
                lifecycle: backend.detectVersionLifecycle(version.version),
            },
            // The names come from a third-party API and cannot be changed.
            // eslint-disable-next-line @typescript-eslint/naming-convention
            version_type: params.versionType,
        })
        return engineVersions.versions.map(engineVersionToVersion)
    }

    // === Endpoints that intentionally do not work on the Local Backend ===

    /** @throws An error stating that the operation is intentionally unavailable on the local
     * backend. */
    invalidOperation(): never {
        throw new Error('Cannot manage users, folders, files, and secrets on the local backend.')
    }

    /** Do nothing. This function should never need to be called. */
    override undoDeleteAsset(): Promise<void> {
        return this.invalidOperation()
    }

    /** Return an empty array. This function should never need to be called. */
    override listUsers() {
        return Promise.resolve([])
    }

    /** Invalid operation. */
    override createUser() {
        return this.invalidOperation()
    }

    /** Do nothing. This function should never need to be called. */
    override inviteUser() {
        return Promise.resolve()
    }

    /** Do nothing. This function should never need to be called. */
    override createPermission() {
        return Promise.resolve()
    }

    /** Return `null`. This function should never need to be called. */
    override usersMe() {
        return Promise.resolve(null)
    }

    /** Invalid operation. */
    override createDirectory() {
        return this.invalidOperation()
    }

    /** Invalid operation. */
    override updateDirectory() {
        return this.invalidOperation()
    }

    /** Invalid operation. */
    override checkResources() {
        return this.invalidOperation()
    }

    /** Return an empty array. This function should never need to be called. */
    override listFiles() {
        return Promise.resolve([])
    }

    /** Invalid operation. While project bundles can be uploaded to the Project Manager,
     * they are not uploaded as file assets, and hence do not return a {@link backend.FileInfo}. */
    override uploadFile() {
        return this.invalidOperation()
    }

    /** Invalid operation. */
    override createSecret() {
        return this.invalidOperation()
    }

    /** Invalid operation. */
    override getSecret() {
        return this.invalidOperation()
    }

    /** Return an empty array. This function should never need to be called. */
    override listSecrets() {
        return Promise.resolve([])
    }

    /** Invalid operation. */
    override createTag() {
        return this.invalidOperation()
    }

    /** Return an empty array. This function should never need to be called. */
    override listTags() {
        return Promise.resolve([])
    }

    /** Do nothing. This function should never need to be called. */
    override deleteTag() {
        return Promise.resolve()
    }
}
