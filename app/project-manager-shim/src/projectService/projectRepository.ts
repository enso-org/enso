import { UUID } from 'enso-common/src/services/Backend'

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

export interface ProjectMetadata {
  readonly name: string
  readonly namespace: string
  readonly id: UUID
  readonly created: string // ISO DateTime
  readonly lastOpened?: string // ISO DateTime
}

export interface ProjectRepository {
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

