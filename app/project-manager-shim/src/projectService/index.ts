/**
 * @file TypeScript implementation of the ProjectService.
 * This module provides project management functionality including creating, deleting,
 * renaming, opening, closing, and duplicating projects.
 */

import * as crypto from 'node:crypto'
import * as path from 'node:path'

import { UUID } from 'enso-common/src/services/Backend'
import { type Runner, EnsoRunner, findEnsoPath } from './ensoRunner.js'
import * as nameValidation from './nameValidation.js'
import { type Project, type ProjectRepository, ProjectFileRepository } from './projectRepository.js'

// ==================
// === Data Types ===
// ==================

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

// =======================
// === ProjectService ====
// =======================

export class ProjectService {
  private static readonly DEFAULT_NAMESPACE = 'local'

  constructor(
    private readonly runner: Runner,
    private readonly logger: Console = console,
  ) {}

  static default(): ProjectService {
    const ensoPath = findEnsoPath('.')
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
    projectsDirectory: string,
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

    const creationTime = new Date().toISOString()
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

  // ========================
  // === Helper Functions ===
  // ========================

  private generateUUID(): UUID {
    return UUID(crypto.randomUUID())
  }

  private getProjectRepository(projectsDirectory: string): ProjectRepository {
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
