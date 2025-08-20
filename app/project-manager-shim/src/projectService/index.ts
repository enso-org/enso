/**
 * @file TypeScript implementation of the ProjectService.
 * This module provides project management functionality including creating, deleting,
 * renaming, opening, closing, and duplicating projects.
 */

import * as crypto from 'node:crypto'
import { type Runner, EnsoRunner } from './ensoRunner'

import { UUID } from 'enso-common/src/services/Backend'

// ==================
// === Data Types ===
// ==================

export interface Project {
  readonly id: UUID
  readonly name: string
  readonly namespace: string
  readonly kind: 'UserProject'
  readonly created: string // ISO DateTime
  readonly edition?: string // Raw edition string
  readonly jvmModeEnabled?: boolean
  readonly path: string // Absolute file path
  readonly lastOpened?: string // ISO DateTime
  readonly directoryCreationTime?: string // File timestamp
}

export interface RunningLanguageServerInfo {
  readonly engineVersion: string // SemVer format
  readonly sockets: LanguageServerSockets
  readonly projectName: string
  readonly projectNormalizedName: string
  readonly projectNamespace: string
}

export interface LanguageServerSockets {
  readonly jsonSocket: Socket
  readonly secureJsonSocket?: Socket
  readonly binarySocket: Socket
  readonly secureBinarySocket?: Socket
}

export interface Socket {
  readonly host: string
  readonly port: number
}

export interface CloudParams {
  readonly cloudProjectDirectoryPath: string
  readonly cloudProjectId: string
  readonly cloudProjectSessionId: string
}

export interface ProjectMetadata {
  readonly name: string
  readonly namespace: string
  readonly id: string // UUID
  readonly created: string // ISO DateTime
  readonly lastOpened?: string // ISO DateTime
}

/** Parameters for the "create project" endpoint. */
export interface CreateProjectParams {
  readonly name: string
  readonly projectTemplate?: string
  readonly version?: string
  readonly projectsDirectory?: string
}

/** The return value of the "create project" endpoint. */
export interface CreateProject {
  readonly projectId: UUID
  readonly projectName: string
  readonly projectPath: string
  readonly projectNormalizedName: string
}

// ===================
// === Error Types ===
// ===================

export abstract class ProjectServiceFailure extends Error {
  constructor(message: string) {
    super(message)
    this.name = this.constructor.name
  }
}

export class ValidationFailure extends ProjectServiceFailure {}
export class ProjectExists extends ProjectServiceFailure {}
export class ProjectNotFound extends ProjectServiceFailure {}
export class DataStoreFailure extends ProjectServiceFailure {}
export class ProjectCreateFailed extends ProjectServiceFailure {}
export class ProjectOpenFailed extends ProjectServiceFailure {}
export class ProjectCloseFailed extends ProjectServiceFailure {}
export class CannotRemoveOpenProject extends ProjectServiceFailure {}
export class CannotRemoveClosingProject extends ProjectServiceFailure {}
export class ProjectOperationTimeout extends ProjectServiceFailure {}
export class ProjectNotOpen extends ProjectServiceFailure {}
export class ProjectOpenByOtherPeers extends ProjectServiceFailure {}
export class LanguageServerFailure extends ProjectServiceFailure {}

// =========================
// === Helper Interfaces ===
// =========================

interface ProjectRepository {
  exists(name: string): Promise<boolean>
  findPathForNewProject(moduleName: string): Promise<string>
  update(project: Project): Promise<void>
  delete(projectId: string): Promise<void>
  moveToTrash(projectId: string): Promise<boolean>
  rename(projectId: string, name: string): Promise<void>
  findById(projectId: string): Promise<Project | null>
  find(predicate: (project: Project) => boolean): Promise<Project[]>
  getAll(): Promise<Project[]>
  moveProject(projectId: string, newName: string): Promise<string>
  copyProject(project: Project, newName: string, newMetadata: ProjectMetadata): Promise<Project>
  getPackageName(projectId: string): Promise<string>
  getPackageNamespace(projectId: string): Promise<string>
  tryLoadProject(directory: string): Promise<Project | null>
}

interface LanguageServerGateway {
  isRunning(projectId: string): Promise<[boolean, boolean]>
  start(
    progressTracker: any,
    clientId: string,
    project: Project,
    version: string,
    extraEnv: Array<[string, string]>,
  ): Promise<LanguageServerSockets>
  stop(clientId: string, projectId: string): Promise<void>
  registerShutdownHook(projectId: string, command: any): Promise<void>
  renameProject(
    projectId: string,
    namespace: string,
    oldPackage: string,
    newPackage: string,
  ): Promise<void>
}

// =======================
// === ProjectService ====
// =======================

export class ProjectService {
  private readonly DEFAULT_NAMESPACE = 'local'

  constructor(
    private readonly projectRepository: ProjectRepository,
    private readonly runner: Runner,
    private readonly logger: Console = console,
  ) {}

  static getInstance(): ProjectService {
    throw new Error('Unimplemented ProjectService.getInstance()')
  }

  /**
   * Creates a new user project with the specified configuration.
   */
  async createProject(
    projectName: string,
    engineVersion?: string,
    projectTemplate?: string,
    projectsDirectory?: string,
  ): Promise<Project> {
    // Step 1: Generate Project ID
    const projectId = this.generateUUID()

    // Step 2: Log Creation
    this.logger.debug(
      `Creating project [${projectName}, ${projectId}, ${projectTemplate}, ${projectsDirectory}].`,
    )

    // Step 3: Get Repository
    const repo = await this.getProjectRepository(projectsDirectory)

    // Step 4: Name Resolution - ensure unique name
    const actualName = await this.getNameForNewProject(projectName, repo)
    this.logger.info(`Created project with actual name [${actualName}].`)

    // Step 5: Validation
    await this.validateProjectName(actualName)
    await this.checkIfNameExists(actualName, repo)

    // Step 6: Normalize Module Name
    const moduleName = this.normalizeProjectName(actualName)

    // Step 7: Find Path for new project
    const projectPath = await repo.findPathForNewProject(moduleName)

    // Step 8: Create Project Object
    const creationTime = new Date().toISOString()
    const project: Project = {
      id: projectId,
      name: actualName,
      namespace: this.DEFAULT_NAMESPACE,
      kind: 'UserProject',
      created: creationTime,
      path: projectPath,
    }

    this.logger.debug(
      `Found a path [${projectPath}] for a new project [${actualName}, ${projectId}].`,
    )

    // Step 9: Create Project Structure
    await this.runner.createProject(
      projectPath,
      actualName,
      engineVersion,
      projectTemplate,
    )

    this.logger.debug(
      `Project [${projectId}] structure created with [${projectPath}, ${actualName}, ${moduleName}].`,
    )

    // Step 10: Update Repository
    await repo.update(project)
    this.logger.debug(`Project [${projectId}] updated in repository.`)

    // Step 11: Return created project
    this.logger.info(`Project created [${JSON.stringify(project)}].`)
    return project
  }

  // ========================
  // === Helper Functions ===
  // ========================

  private generateUUID(): UUID {
    return UUID(crypto.randomUUID())
  }

  private async getProjectRepository(projectsDirectory?: string): Promise<ProjectRepository> {
    // TODO: Implement ProjectRepositoryFactory logic
    // For now, return the injected repository
    return this.projectRepository
  }

  private async getNameForNewProject(
    projectName: string,
    repo: ProjectRepository,
  ): Promise<string> {
    const mkName = (name: string, suffix: number): string => `${name}_${suffix}`

    const findAvailableName = async (name: string, suffix: number): Promise<string> => {
      const newName = mkName(name, suffix)
      const exists = await repo.exists(newName)
      if (exists) {
        return findAvailableName(name, suffix + 1)
      }
      return newName
    }

    const exists = await repo.exists(projectName)
    if (exists) {
      return findAvailableName(projectName, 1)
    }
    return projectName
  }

  private async validateProjectName(name: string): Promise<void> {
    if (name.trim().length === 0) {
      throw new ValidationFailure('Project name cannot be empty.')
    }
    this.logger.debug(`Project name [${name}] validated.`)
  }

  private async checkIfNameExists(name: string, repo: ProjectRepository): Promise<void> {
    const exists = await repo.exists(name)
    if (exists) {
      throw new ProjectExists(`Project with name '${name}' already exists.`)
    }
    this.logger.debug(`Checked if the project name [${name}] exists in repository.`)
  }

  private normalizeProjectName(name: string): string {
    // TODO: Implement proper name normalization logic from NameValidation.normalizeName
    // For now, basic normalization:
    // - Replace spaces and special characters with underscores
    // - Convert to lowercase
    // - Ensure starts with letter or underscore
    let normalized = name
      .toLowerCase()
      .replace(/[^a-z0-9_]/g, '_')
      .replace(/^[0-9]/, '_$&') // Prefix numbers with underscore

    // Remove consecutive underscores
    normalized = normalized.replace(/_+/g, '_')

    // Remove trailing underscores
    normalized = normalized.replace(/_+$/, '')

    return normalized || 'Project'
  }

  async deleteUserProject(projectId: string, projectsDirectory?: string): Promise<void> {
    // TODO: Implement deleteUserProject
    throw new Error('deleteUserProject not implemented yet')
  }

  async renameProject(
    projectId: string,
    newName: string,
    projectsDirectory?: string,
  ): Promise<void> {
    // TODO: Implement renameProject
    throw new Error('renameProject not implemented yet')
  }

  async openProject(
    progressTracker: any,
    clientId: string,
    projectId: string,
    cloud?: CloudParams,
    projectsDirectory?: string,
  ): Promise<RunningLanguageServerInfo> {
    // TODO: Implement openProject
    throw new Error('openProject not implemented yet')
  }

  async closeProject(clientId: string, projectId: string): Promise<void> {
    // TODO: Implement closeProject
    throw new Error('closeProject not implemented yet')
  }

  async duplicateUserProject(projectId: string, projectsDirectory?: string): Promise<Project> {
    // TODO: Implement duplicateUserProject
    throw new Error('duplicateUserProject not implemented yet')
  }
}
