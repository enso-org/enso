/** @file Local storage keys for the list of opened projects. */
import * as z from 'zod'

import { BackendType, type DirectoryId, type ProjectId } from 'enso-common/src/services/Backend'

import { defineLocalStorageKey } from '#/providers/LocalStorageProvider'

/** Main content of the screen. Only one should be visible at a time. */
export const TAB_TYPES = ['drive', 'settings'] as const

/** Main content of the screen. Only one should be visible at a time. */
export type TabType = (typeof TAB_TYPES)[number]

const PROJECT_SCHEMA = z
  .object({
    id: z.custom<ProjectId>((x) => typeof x === 'string' && x.startsWith('project-')),
    parentId: z.custom<DirectoryId>((x) => typeof x === 'string' && x.startsWith('directory-')),
    title: z.string(),
    type: z.nativeEnum(BackendType),
  })
  .readonly()
const LAUNCHED_PROJECT_SCHEMA = z.array(PROJECT_SCHEMA).readonly()

/** Launched project information. */
export type LaunchedProject = z.infer<typeof PROJECT_SCHEMA>
/** Launched project ID. */
export type LaunchedProjectId = ProjectId

export const { use: useLaunchedProjects, useState: useLaunchedProjectsState } =
  defineLocalStorageKey('launchedProjects', {
    isUserSpecific: true,
    schema: () => LAUNCHED_PROJECT_SCHEMA,
  })

export const PAGES_SCHEMA = z
  .enum(TAB_TYPES)
  .or(
    z.custom<LaunchedProjectId>(
      (value) => typeof value === 'string' && value.startsWith('project-'),
    ),
  )

export const {
  use: usePage,
  validate: validatePage,
  useState: usePageState,
} = defineLocalStorageKey('page', {
  schema: () => PAGES_SCHEMA,
})
