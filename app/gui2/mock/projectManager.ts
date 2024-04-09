declare const projectIdBrand: unique symbol
/** An ID of a project. */
export type ProjectId = string & { [projectIdBrand]: never }
declare const projectNameBrand: unique symbol
/** A name of a project. */
export type ProjectName = string & { [projectNameBrand]: never }
declare const utcDateTimeBrand: unique symbol
/** A UTC date and time. */
export type UTCDateTime = string & { [utcDateTimeBrand]: never }

/** A value specifying the hostname and port of a socket. */
export interface IpWithSocket {
  host: string
  port: number
}

export interface OpenProject {
  engineVersion: string
  languageServerJsonAddress: IpWithSocket
  languageServerBinaryAddress: IpWithSocket
  projectName: ProjectName
  projectNormalizedName: string
  projectNamespace: string
}

/** Details for a project. */
export interface ProjectMetadata {
  name: ProjectName
  namespace: string
  id: ProjectId
  engineVersion: string | null
  created: UTCDateTime
  lastOpened: UTCDateTime | null
}

export const projects = new Map<string, ProjectMetadata>()
const openProjects = new Set<string>()

export const methods = {
  async 'project/open'(id) {
    openProjects.add(id)
    const project = projects.get(id)
    if (!project) throw new Error(`Cannot find project with ID ${id}.`)
    return {
      projectName: project.name,
      projectNormalizedName: project.name,
      projectNamespace: project.namespace,
      languageServerJsonAddress: { host: '127.0.0.1', port: 30000 },
      languageServerBinaryAddress: { host: '127.0.0.1', port: 30001 },
      engineVersion: '',
    } satisfies OpenProject
  },
  async 'project/close'(id) {
    openProjects.delete(id)
    return {}
  },
  async 'project/list'(numberOfProjects) {
    const projectsList = Array.from(projects.values())
    return {
      projects: numberOfProjects != null ? projectsList.slice(0, numberOfProjects) : projectsList,
    }
  },
} satisfies Record<string, (...params: any[]) => Promise<unknown>>
