/** @file Constants `` */
import { BackendType, type DirectoryId, type ProjectId } from '#/services/Backend'
import { LocalStorage } from '#/utilities/LocalStorage'
import { createContext } from 'react'
import * as z from 'zod'

export const TAB_TYPES = ['drive', 'settings'] as const

/** Main content of the screen. Only one should be visible at a time. */
export type TabType = (typeof TAB_TYPES)[number]

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly isAssetPanelVisible: boolean
    readonly page: z.infer<typeof PAGES_SCHEMA>
    readonly launchedProjects: z.infer<typeof LAUNCHED_PROJECT_SCHEMA>
  }
}

const PROJECT_ID_SCHEMA = z.custom<ProjectId>(
  (x) => typeof x === 'string' && x.startsWith('project-'),
)
const DIRECTORY_ID_SCHEMA = z.custom<DirectoryId>(
  (x) => typeof x === 'string' && x.startsWith('directory-'),
)
const PROJECT_SCHEMA = z
  .object({
    id: PROJECT_ID_SCHEMA,
    parentId: DIRECTORY_ID_SCHEMA,
    title: z.string(),
    type: z.nativeEnum(BackendType),
    hybrid: z.optional(
      z.object({
        cloudProjectId: PROJECT_ID_SCHEMA,
        parentId: DIRECTORY_ID_SCHEMA,
      }),
    ),
  })
  .readonly()
const LAUNCHED_PROJECT_SCHEMA = z.array(PROJECT_SCHEMA).readonly()

/** Launched project information. */
export type LaunchedProject = z.infer<typeof PROJECT_SCHEMA>
/** Launched project ID. */
export type LaunchedProjectId = ProjectId

LocalStorage.registerKey('launchedProjects', {
  isUserSpecific: true,
  schema: LAUNCHED_PROJECT_SCHEMA,
})

export const PAGES_SCHEMA = z
  .enum(TAB_TYPES)
  .or(
    z.custom<LaunchedProjectId>(
      (value) => typeof value === 'string' && value.startsWith('project-'),
    ),
  )

LocalStorage.registerKey('page', { schema: PAGES_SCHEMA })

/** State contained in a `ProjectsContext`. */
export interface ProjectsContextType {
  readonly setLaunchedProjects: (launchedProjects: readonly LaunchedProject[]) => void
  readonly addLaunchedProject: (project: LaunchedProject) => void
  readonly removeLaunchedProject: (projectId: LaunchedProjectId) => void
  readonly updateLaunchedProjects: (
    update: (projects: readonly LaunchedProject[]) => readonly LaunchedProject[],
  ) => void
  readonly getState: () => {
    readonly launchedProjects: readonly LaunchedProject[]
    readonly page: LaunchedProjectId | TabType
  }
  readonly setPage: (page: LaunchedProjectId | TabType) => void
}

export const ProjectsContext = createContext<ProjectsContextType | null>(null)
export const PageContext = createContext<LaunchedProjectId | TabType | null>(null)
export const LaunchedProjectsContext = createContext<readonly LaunchedProject[] | null>(null)
