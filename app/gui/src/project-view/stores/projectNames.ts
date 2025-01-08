import { createContextStore } from '@/providers'
import { parseAbsoluteProjectPath, ProjectPath } from '@/util/projectPath'
import { normalizeQualifiedName, qnJoin } from '@/util/qualifiedName'
import { type ToValue } from '@/util/reactivity'
import { computed, ref, toValue } from 'vue'
import { type Identifier, type QualifiedName } from 'ydoc-shared/ast'

export type ProjectNameStore = ReturnType<typeof useProjectNames>

/** Manages the state of the project's name. */
function makeProjectNameStore(
  namespace: ToValue<string | undefined> = 'local',
  initialName: string = 'Mock_Project',
  displayName: ToValue<string> = 'Mock_Project',
) {
  const ns = computed(() => {
    if (import.meta.env.PROD && namespace == null) {
      console.warn(
        'Unknown project\'s namespace. Assuming "local", however it likely won\'t work in cloud',
      )
    }
    return (toValue(namespace) ?? 'local') as Identifier
  })
  const synchronizedName = ref(initialName as Identifier)
  const pendingName = ref<Identifier>()

  const inboundProject = computed(() => qnJoin(ns.value, synchronizedName.value))
  const outboundProject = computed(() =>
    pendingName.value ? qnJoin(ns.value, pendingName.value) : inboundProject.value,
  )

  function parseProjectPath(path: QualifiedName): ProjectPath {
    const parsed = parseAbsoluteProjectPath(path)
    return parsed.project === inboundProject.value ?
        ProjectPath.create(undefined, parsed.path)
      : parsed
  }

  function printProjectPath(path: ProjectPath): QualifiedName {
    return normalizeQualifiedName(printProjectPathDenormalized(path))
  }

  function printProjectPathDenormalized(path: ProjectPath): QualifiedName {
    const project = path.project ?? outboundProject.value
    return path.path ? qnJoin(project, path.path) : project
  }

  return {
    parseProjectPath,
    printProjectPath,
    printProjectPathDenormalized,
    onProjectRenameRequested: (newName: Identifier) => {
      pendingName.value = newName
    },
    onProjectRenamed: (oldName: string, newName: string) => {
      if ((oldName as Identifier) === synchronizedName.value) {
        synchronizedName.value = newName as Identifier
        pendingName.value = undefined
      }
    },
    displayName: computed(() => toValue(displayName)),
  }
}

export const mockProjectNameStore = makeProjectNameStore

export const [provideProjectNames, useProjectNames] = createContextStore(
  'projectNames',
  makeProjectNameStore,
)
