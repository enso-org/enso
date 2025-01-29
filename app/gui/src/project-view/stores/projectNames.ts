import { createContextStore } from '@/providers'
import { Ok, Result } from '@/util/data/result'
import { parseAbsoluteProjectPath, ProjectPath } from '@/util/projectPath'
import { normalizeQualifiedName, qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import { type ToValue } from '@/util/reactivity'
import { computed, readonly, ref, toRef, toValue } from 'vue'
import { type Identifier, type QualifiedName } from 'ydoc-shared/ast'

export type ProjectNameStore = ReturnType<typeof injectProjectNames>

/** Manages the state of the project's name. */
function useProjectNameStore(
  namespace: ToValue<string | undefined>,
  initialName: string,
  displayName: ToValue<string>,
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

  /**
   * Interpret a qualified name as a project path. A project path abstracts the project name, and remains valid if the
   * current project is renamed.
   *
   * To ensure that QNs are interpreted correctly during and after project renames, this should be applied to data
   * from the backend as it is received.
   */
  function parseProjectPath(path: QualifiedName): Result<ProjectPath> {
    const parsed = parseAbsoluteProjectPath(path)
    if (!parsed.ok) return parsed
    return parsed.value.project === inboundProject.value ?
        Ok(ProjectPath.create(undefined, parsed.value.path))
      : parsed
  }

  /**
   * Interpret a string as a project path.
   *
   * Same as {@link parseProjectPath}, but the path is also checked for being an actual Qualified Name.
   */
  function parseProjectPathRaw(path: string): Result<ProjectPath> {
    const qn = tryQualifiedName(path)
    if (!qn.ok) return qn
    return parseProjectPath(qn.value)
  }

  /**
   * Serialize the path, with any project's `Main` segment elided. This is appropriate for values that will be displayed
   * to the user or written into source code.
   */
  function printProjectPath(path: ProjectPath): QualifiedName {
    return normalizeQualifiedName(serializeUnnormalized(path))
  }

  /**
   * Serialize the path, including the `Main` segment if applicable. This is appropriate when the backend will be the
   * direct consumer of the result, e.g. when serializing a `StackItem` to send to the language server.
   */
  function serializeProjectPathForBackend(path: ProjectPath): QualifiedName {
    return serializeUnnormalized(path)
  }

  function serializeUnnormalized(path: ProjectPath): QualifiedName {
    const project = path.project ?? outboundProject.value
    return path.path ? qnJoin(project, path.path) : project
  }

  return {
    parseProjectPath,
    parseProjectPathRaw,
    printProjectPath,
    serializeProjectPathForBackend,
    onProjectRenameRequested: (newName: Identifier) => {
      pendingName.value = newName
    },
    onProjectRenamed: (oldName: string, newName: string) => {
      if ((oldName as Identifier) === synchronizedName.value) {
        synchronizedName.value = newName as Identifier
        pendingName.value = undefined
      }
    },
    displayName: readonly(toRef(displayName)),
  }
}

/** Creates a project name store for use in tests. */
export function mockProjectNameStore(
  namespace: ToValue<string | undefined> = 'local',
  initialName: string = 'Mock_Project',
  displayName: ToValue<string> = 'Mock Project',
) {
  return useProjectNameStore(namespace, initialName, displayName)
}

export const [provideProjectNames, injectProjectNames] = createContextStore(
  'projectNames',
  useProjectNameStore,
)
