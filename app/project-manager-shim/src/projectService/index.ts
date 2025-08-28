/**
 * @file TypeScript implementation of the ProjectService.
 * This module provides project management functionality including creating, deleting,
 * renaming, opening, closing, and duplicating projects.
 */

import * as crypto from 'node:crypto'

import { UUID } from 'enso-common/src/services/Backend'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { Path } from 'enso-common/src/utilities/file'
import {
  type LanguageServerSockets,
  type Runner,
  type Socket,
  EnsoRunner,
  findEnsoExecutable,
} from './ensoRunner.js'
import * as nameValidation from './nameValidation.js'
import { type Project, type ProjectRepository, ProjectFileRepository } from './projectRepository.js'

// ==================
// === Data Types ===
// ==================

export interface RunningLanguageServerInfo {
  readonly sockets: LanguageServerSockets
  readonly projectName: string
  readonly projectNormalizedName: string
  readonly projectNamespace: string
}

export interface CloudParams {
  readonly cloudProjectDirectoryPath: Path
  readonly cloudProjectId: string
  readonly cloudProjectSessionId: string
}

/** Parameters for the "create project" endpoint. */
export interface CreateProjectParams {
  readonly name: string
  readonly projectTemplate?: string
  readonly version?: string
  readonly projectsDirectory?: Path
}

/** The return value of the "create project" endpoint. */
export interface CreateProject {
  readonly projectId: UUID
  readonly projectName: string
  readonly projectNormalizedName: string
  readonly projectPath: Path
}

/** The return value of the "open project" endpoint. */
export interface OpenProject {
  readonly languageServerJsonAddress: Socket
  readonly languageServerBinaryAddress: Socket
  readonly projectName: string
  readonly projectNormalizedName: string
  readonly projectNamespace: string
}

// =======================
// === ProjectService ====
// =======================

/** Service for managing Enso projects. */
export class ProjectService {
  private static readonly DEFAULT_NAMESPACE = 'local'

  /** Creates a new ProjectService with the specified runner. */
  constructor(
    private readonly runner: Runner,
    private readonly logger: Console = console,
  ) {}

  /** Creates a default ProjectService using the Enso executable found in the environment. */
  static default(): ProjectService {
    const ensoPath = findEnsoExecutable()
    if (!ensoPath) {
      throw new Error('Enso executable not found')
    }
    const runner = new EnsoRunner(ensoPath)
    return new ProjectService(runner)
  }

  /**
   * Creates a new user project with the specified configuration.
   */
  async createProject(
    projectName: string,
    projectsDirectory: Path,
    engineVersion?: string,
    projectTemplate?: string,
  ): Promise<CreateProject> {
    const projectId = this.generateUUID()
    const repo = this.getProjectRepository(projectsDirectory)

    // Ensure unique name
    const actualName = await this.getNameForNewProject(projectName, repo)

    // Validation
    await this.validateProjectName(actualName)
    await this.checkIfNameExists(actualName, repo)

    // Normalize project name
    const normalizedName = nameValidation.normalizedName(actualName)

    // Find path for new project
    const projectPath = await repo.findPathForNewProject(normalizedName)
    const creationTime = toRfc3339(new Date())
    const project: Project = {
      id: projectId,
      name: actualName,
      namespace: ProjectService.DEFAULT_NAMESPACE,
      kind: 'UserProject',
      created: creationTime,
      path: projectPath,
    }

    // Create project structure
    await this.runner.createProject(projectPath, actualName, engineVersion, projectTemplate)

    // Update metadata
    await repo.update(project)

    this.logger.debug('Created project', projectPath)

    return {
      projectId,
      projectName: actualName,
      projectNormalizedName: normalizedName,
      projectPath,
    }
  }

  /** Deletes a user project. */
  async deleteProject(_projectId: string, _projectsDirectory?: Path): Promise<void> {
    // TODO: Implement deleteProject
    throw new Error('deleteProject not implemented yet')
  }

  /** Renames a project. */
  async renameProject(
    _projectId: string,
    _newName: string,
    _projectsDirectory?: Path,
  ): Promise<void> {
    // TODO: Implement renameProject
    throw new Error('renameProject not implemented yet')
  }

  /** Opens a project and starts its language server. */
  async openProject(
    projectId: string,
    projectsDirectory: Path,
    cloud?: CloudParams,
  ): Promise<OpenProject> {
    this.logger.debug('Opening project', projectId)

    // Get the project repository
    const repo = this.getProjectRepository(projectsDirectory)

    // Get the project from the repository
    const project = await repo.findById(projectId)
    if (!project) {
      throw new Error(`Project not found: ${projectId}`)
    }

    // Update the lastOpened timestamp
    const openTime = toRfc3339(new Date())
    const updatedProject = { ...project, lastOpened: openTime }
    await repo.update(updatedProject)

    // Prepare cloud environment variables if provided
    const extraEnv: Array<[string, string]> = []
    if (cloud) {
      extraEnv.push(['ENSO_CLOUD_PROJECT_DIRECTORY_PATH', cloud.cloudProjectDirectoryPath])
      extraEnv.push(['ENSO_CLOUD_PROJECT_ID', cloud.cloudProjectId])
      extraEnv.push(['ENSO_CLOUD_PROJECT_SESSION_ID', cloud.cloudProjectSessionId])
    }

    // Start the language server
    const sockets = await this.runner.openProject(
      project.path,
      projectId,
      project.name,
      extraEnv.length > 0 ? extraEnv : undefined,
    )

    // Return the OpenProject response
    return {
      languageServerJsonAddress: sockets.jsonSocket,
      languageServerBinaryAddress: sockets.binarySocket,
      projectName: project.name,
      projectNormalizedName: nameValidation.normalizedName(project.name),
      projectNamespace: project.namespace,
    }
  }

  /** Closes a project and stops its language server. */
  async closeProject(projectId: string): Promise<void> {
    this.logger.debug('Closing project', projectId)
    await this.runner.closeProject(projectId)
  }

  /** Duplicates a user project. */
  async duplicateUserProject(_projectId: string, _projectsDirectory?: Path): Promise<Project> {
    // TODO: Implement duplicateUserProject
    throw new Error('duplicateUserProject not implemented yet')
  }

  // ========================
  // === Helper Functions ===
  // ========================

  private generateUUID(): UUID {
    return UUID(crypto.randomUUID())
  }

  private getProjectRepository(projectsDirectory: Path): ProjectRepository {
    return new ProjectFileRepository(projectsDirectory)
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
      throw new Error('Project name cannot be empty.')
    }
  }

  private async checkIfNameExists(name: string, repo: ProjectRepository): Promise<void> {
    const exists = await repo.exists(name)
    if (exists) {
      throw new Error(`Project with name '${name}' already exists.`)
    }
  }
}
