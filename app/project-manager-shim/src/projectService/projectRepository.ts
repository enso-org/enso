import { toRfc3339, type Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import * as crypto from 'node:crypto'
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import trash from 'trash'
import * as yaml from 'yaml'
import * as nameValidation from './nameValidation.js'
import { Path, type UUID } from './types.js'

export interface Project {
  readonly id: UUID
  readonly name: string
  readonly namespace: string
  readonly kind: 'UserProject'
  readonly created: Rfc3339DateTime
  readonly edition?: string
  readonly jvmModeEnabled?: boolean
  readonly path: Path // Absolute file path
  readonly lastOpened?: Rfc3339DateTime
  readonly directoryCreationTime?: Rfc3339DateTime
}

export interface ProjectMetadata {
  readonly name: string
  readonly namespace: string
  readonly id: UUID
  readonly created: Rfc3339DateTime
  readonly lastOpened?: Rfc3339DateTime
}

export interface ProjectRepository {
  exists(name: string): Promise<boolean>
  findPathForNewProject(normalizedName: string): Promise<Path>
  update(project: Project): Promise<void>
  delete(path: Path): Promise<void>
  moveToTrash(path: Path): Promise<void>
  rename(projectId: UUID, name: string): Promise<void>
  renameProjectDirectory(oldPath: Path, newNormalizedName: string): Promise<Path>
  findById(projectId: UUID): Promise<Project | null>
  find(predicate: (project: Project) => boolean): Promise<readonly Project[]>
  getAll(): Promise<readonly Project[]>
  moveProject(projectId: UUID, newName: string): Promise<Path>
  copyProject(project: Project, newName: string, newMetadata: ProjectMetadata): Promise<Project>
  getPackageName(projectId: UUID): Promise<string>
  getPackageNamespace(projectId: UUID): Promise<string>
  tryLoadProject(directory: Path): Promise<Project | null>
}

const PACKAGE_METADATA_RELATIVE_PATH = 'package.yaml'
const PROJECT_METADATA_RELATIVE_PATH = '.enso/project.json'

interface PackageYaml {
  name?: string
  normalizedName?: string
  namespace?: string
  edition?: string
  jvmModeEnabled?: boolean
}

interface ProjectJson {
  id?: string
  kind?: string
  created?: Rfc3339DateTime
  lastOpened?: Rfc3339DateTime | null
}

/** File-based implementation of ProjectRepository. */
export class ProjectFileRepository implements ProjectRepository {
  /** Creates a new ProjectFileRepository with the specified projects directory. */
  constructor(private readonly projectsPath: Path) {}

  /** Checks if a project with the given name exists. */
  async exists(name: string): Promise<boolean> {
    const projects = await this.getAll()
    return projects.some((p) => p.name === name)
  }

  /** Finds an available path for a new project. */
  async findPathForNewProject(projectName: string): Promise<Path> {
    const normalizedName = nameValidation.normalizedName(projectName)
    return this.findTargetPath(normalizedName)
  }

  /** Updates project metadata. */
  async update(project: Project): Promise<void> {
    const metadataPath = path.join(project.path, PROJECT_METADATA_RELATIVE_PATH)
    const metadata: ProjectJson = {
      id: project.id,
      kind: project.kind,
      created: project.created,
      lastOpened: project.lastOpened ?? null,
    }
    await fs.mkdir(path.dirname(metadataPath), { recursive: true })
    await fs.writeFile(metadataPath, JSON.stringify(metadata, null, 2))
  }

  /** Deletes a path. */
  async delete(path: Path): Promise<void> {
    await fs.rm(path, { recursive: true, force: true })
  }

  /** Moves a path to system trash. */
  async moveToTrash(path: Path): Promise<void> {
    await trash(path)
  }

  /** Renames a project. */
  async rename(projectId: UUID, name: string): Promise<void> {
    const project = await this.findById(projectId)
    if (!project) {
      throw new Error(`Project '${projectId}' not found`)
    }
    await this.renamePackage(project.path, name)
  }

  /** Renames the project directory on disk. */
  async renameProjectDirectory(oldPath: Path, newNormalizedName: string): Promise<Path> {
    const newPath = await this.findTargetPath(newNormalizedName)
    await fs.rename(oldPath, newPath)
    return newPath
  }

  /** Finds a project by ID. */
  async findById(projectId: UUID): Promise<Project | null> {
    const projects = await this.getAll()
    return projects.find((p) => p.id === projectId) ?? null
  }

  /** Finds projects matching a predicate. */
  async find(predicate: (project: Project) => boolean): Promise<readonly Project[]> {
    const projects = await this.getAll()
    return projects.filter(predicate)
  }

  /** Gets all projects. */
  async getAll(): Promise<readonly Project[]> {
    try {
      const entries = await fs.readdir(this.projectsPath, { withFileTypes: true })
      const directories = entries.filter((e) => e.isDirectory() && !e.name.startsWith('.'))

      const projects: readonly (Project | null)[] = await Promise.all(
        directories.map((dir) => this.tryLoadProject(Path(path.join(this.projectsPath, dir.name)))),
      )

      const validProjects = projects.filter((p) => p !== null)
      return this.resolveClashingIds(validProjects)
    } catch (error) {
      if ((error as any).code === 'ENOENT') {
        return []
      }
      throw error
    }
  }

  /** Moves a project to a new location. */
  async moveProject(projectId: UUID, newName: string): Promise<Path> {
    const project = await this.findById(projectId)
    if (!project) {
      throw new Error(`Project '${projectId}' not found`)
    }

    const normalizedName = nameValidation.normalizedName(newName)
    const targetPath = await this.findTargetPath(normalizedName)
    await fs.rename(project.path, targetPath)
    return targetPath
  }

  /** Copies a project with a new name and metadata. */
  async copyProject(
    project: Project,
    newName: string,
    newMetadata: ProjectMetadata,
  ): Promise<Project> {
    const normalizedName = nameValidation.normalizedName(newName)
    const targetPath = await this.findTargetPath(normalizedName)
    // Copy directory
    await fs.cp(project.path, targetPath, { recursive: true })
    // Update metadata
    const metadataPath = path.join(targetPath, PROJECT_METADATA_RELATIVE_PATH)
    const metadata: ProjectJson = {
      id: newMetadata.id,
      kind: 'UserProject',
      created: newMetadata.created,
      lastOpened: newMetadata.lastOpened ?? null,
    }
    await fs.mkdir(path.dirname(metadataPath), { recursive: true })
    await fs.writeFile(metadataPath, JSON.stringify(metadata, null, 2))
    // Update package name
    await this.renamePackage(targetPath, newName)

    const newProject = await this.tryLoadProject(targetPath)
    if (!newProject) {
      throw new Error('Failed to load copied project')
    }
    return newProject
  }

  /** Gets the package name for a project. */
  async getPackageName(projectId: UUID): Promise<string> {
    const project = await this.findById(projectId)
    if (!project) {
      throw new Error(`Project '${projectId}' not found`)
    }

    const packagePath = path.join(project.path, PACKAGE_METADATA_RELATIVE_PATH)
    const content = await fs.readFile(packagePath, 'utf-8')
    const pkg = yaml.parse(content) as PackageYaml
    return pkg.name ?? ''
  }

  /** Gets the package namespace for a project. */
  async getPackageNamespace(projectId: UUID): Promise<string> {
    const project = await this.findById(projectId)
    if (!project) {
      throw new Error(`Project '${projectId}' not found`)
    }

    const packagePath = path.join(project.path, PACKAGE_METADATA_RELATIVE_PATH)
    const content = await fs.readFile(packagePath, 'utf-8')
    const pkg = yaml.parse(content) as PackageYaml
    return pkg.namespace ?? 'local'
  }

  /** Attempts to load a project from a directory. */
  async tryLoadProject(directory: Path): Promise<Project | null> {
    try {
      const packagePath = path.join(directory, PACKAGE_METADATA_RELATIVE_PATH)
      const metadataPath = path.join(directory, PROJECT_METADATA_RELATIVE_PATH)

      // Load package.yaml
      const packageContent = await fs.readFile(packagePath, 'utf-8')
      const pkg = yaml.parse(packageContent) as PackageYaml

      if (!pkg.name) {
        return null
      }

      // Load or create project metadata
      let metadata: ProjectJson
      try {
        const metadataContent = await fs.readFile(metadataPath, 'utf-8')
        metadata = JSON.parse(metadataContent)
      } catch {
        // Create new metadata if it doesn't exist
        metadata = {
          id: crypto.randomUUID(),
          kind: 'UserProject',
          created: toRfc3339(new Date()),
          lastOpened: null,
        }
        await fs.mkdir(path.dirname(metadataPath), { recursive: true })
        await fs.writeFile(metadataPath, JSON.stringify(metadata, null, 2))
      }

      if (!metadata.id || !metadata.created) {
        return null
      }

      const stats = await fs.stat(directory)
      return {
        id: metadata.id as UUID,
        name: pkg.name,
        namespace: pkg.namespace ?? 'local',
        kind: 'UserProject',
        created: metadata.created,
        path: directory,
        directoryCreationTime: toRfc3339(stats.birthtime),
        ...(pkg.edition ? { edition: pkg.edition } : {}),
        ...(pkg.jvmModeEnabled ? { jvmModeEnabled: pkg.jvmModeEnabled } : {}),
        ...(metadata.lastOpened ? { lastOpened: metadata.lastOpened } : {}),
      }
    } catch {
      return null
    }
  }

  private async renamePackage(projectPath: Path, newName: string): Promise<void> {
    const packagePath = path.join(projectPath, PACKAGE_METADATA_RELATIVE_PATH)
    const content = await fs.readFile(packagePath, 'utf-8')
    const pkg = yaml.parse(content) as PackageYaml
    pkg.name = newName
    delete pkg.normalizedName
    await fs.writeFile(packagePath, yaml.stringify(pkg))
  }

  private async findTargetPath(moduleName: string): Promise<Path> {
    let suffix = 0
    while (true) {
      const candidatePath = path.join(
        this.projectsPath,
        moduleName + (suffix === 0 ? '' : `_${suffix}`),
      )
      try {
        await fs.access(candidatePath)
        suffix += 1
      } catch {
        return Path(candidatePath)
      }
    }
  }

  private async resolveClashingIds(projects: Project[]): Promise<Project[]> {
    const idGroups = new Map<string, Project[]>()

    for (const project of projects) {
      const group = idGroups.get(project.id) ?? []
      group.push(project)
      idGroups.set(project.id, group)
    }

    const result: Project[] = []

    for (const group of idGroups.values()) {
      if (group.length === 1) {
        result.push(group[0]!)
      } else {
        // Sort by directory creation time, keep oldest
        group.sort((a, b) => {
          const timeA = a.directoryCreationTime ? new Date(a.directoryCreationTime).getTime() : 0
          const timeB = b.directoryCreationTime ? new Date(b.directoryCreationTime).getTime() : 0
          return timeA - timeB
        })

        result.push(group[0]!)

        // Assign new IDs to clashing projects
        for (let i = 1; i < group.length; i++) {
          const project = group[i]!
          const newId = crypto.randomUUID() as UUID
          const updatedProject = { ...project, id: newId }
          await this.update(updatedProject)
          result.push(updatedProject)
        }
      }
    }

    return result
  }
}
