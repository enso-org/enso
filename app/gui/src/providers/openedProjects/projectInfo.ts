import type {
  DirectoryId,
  EnsoPath,
  ProjectId,
  ProjectSessionId,
} from 'enso-common/src/services/Backend'
import * as z from 'zod'

declare module '#/utilities/LocalStorage' {
  interface LocalStorageData {
    readonly openedTabs: RunningProjectInfo[]
  }
}

const PROJECT_ID_SCHEMA = z.custom<ProjectId>(
  (x) => typeof x === 'string' && x.startsWith('project-'),
)
const PROJECT_SESSION_ID_SCHEMA = z.custom<ProjectSessionId>(
  (x) => typeof x === 'string' && x.startsWith('projectsession-'),
)
const DIRECTORY_ID_SCHEMA = z.custom<DirectoryId>(
  (x) => typeof x === 'string' && x.startsWith('directory-'),
)
const ENSO_PATH_SCHEMA = z.custom<EnsoPath>((x) => typeof x === 'string')
const PROJECT_INFO_SCHEMA = z.object({
  id: PROJECT_ID_SCHEMA,
  parentId: DIRECTORY_ID_SCHEMA,
  title: z.string(),
  ensoPath: ENSO_PATH_SCHEMA,
  mode: z.enum(['local', 'cloud', 'hybrid']),
})

const RUNNING_NATIVE_PROJECT_INFO_SCHEMA = PROJECT_INFO_SCHEMA.extend({
  mode: z.enum(['local', 'cloud']),
})

const RUNNING_HYBRID_PROJECT_INFO_SCHEMA = PROJECT_INFO_SCHEMA.extend({
  mode: z.literal('hybrid'),
  runningId: PROJECT_ID_SCHEMA,
  hybridSessionId: PROJECT_SESSION_ID_SCHEMA,
  localParentId: DIRECTORY_ID_SCHEMA,
  synced: z.boolean().optional(),
})
export const RUNNING_PROJECT_INFO_SCHEMA = z.discriminatedUnion('mode', [
  RUNNING_NATIVE_PROJECT_INFO_SCHEMA,
  RUNNING_HYBRID_PROJECT_INFO_SCHEMA,
])

export type ProjectInfo = z.infer<typeof PROJECT_INFO_SCHEMA>
export type RunningProjectInfo = z.infer<typeof RUNNING_PROJECT_INFO_SCHEMA>
export type RunMode = ProjectInfo['mode']
