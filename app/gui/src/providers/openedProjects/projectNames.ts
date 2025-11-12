import { parseAbsoluteProjectPath, ProjectPath } from '@/util/projectPath'
import { normalizeQualifiedName, qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import { Ok, type Result } from 'enso-common/src/utilities/data/result'
import { normalizeName } from 'enso-common/src/utilities/nameValidation'
import { computed, ref, toValue } from 'vue'
import type { Identifier, QualifiedName } from 'ydoc-shared/ast'

export type ProjectNameStore = ReturnType<typeof createProjectNameStore>

/** Manages the state of the project's name. */
export function createProjectNameStore({
  projectNamespace,
  projectInitialName,
  projectDisplayedName,
}: {
  projectNamespace: ToValue<string | undefined>
  projectInitialName: string
  projectDisplayedName: ToValue<string>
}) {
  const ns = computed(() => {
    if (import.meta.env.PROD && projectNamespace == null) {
      console.warn(
        'Unknown project\'s namespace. Assuming "local", however it likely won\'t work in cloud',
      )
    }
    return (toValue(projectNamespace) ?? 'local') as Identifier
  })
  const synchronizedName = ref(projectInitialName as Identifier)
  const pendingName = ref<string>()

  const displayedName = computed(() => pendingName.value ?? toValue(projectDisplayedName))

  const inboundProject = computed(() => qnJoin(ns.value, synchronizedName.value))
  const outboundProject = computed(() =>
    pendingName.value ?
      qnJoin(ns.value, normalizeName(pendingName.value) as QualifiedName)
    : inboundProject.value,
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

  function serializeUnnormalized(path: ProjectPath): QualifiedName
  function serializeUnnormalized<Nullish extends null | undefined>(
    path: ProjectPath | Nullish,
  ): QualifiedName | Nullish
  function serializeUnnormalized<Nullish extends null | undefined>(
    path: ProjectPath | Nullish,
  ): QualifiedName | Nullish {
    if (path == null) return path
    const project = path.project ?? outboundProject.value
    return path.path ? qnJoin(project, path.path) : project
  }

  return {
    parseProjectPath,
    parseProjectPathRaw,
    printProjectPath,
    /**
     * Serialize the path, including the `Main` segment if applicable. This is appropriate when the
     * backend will be the direct consumer of the result, e.g. when serializing a `StackItem` to
     * send to the language server.
     */
    serializeProjectPathForBackend: serializeUnnormalized,
    onProjectRenameRequested: (newName: string) => {
      pendingName.value = newName
    },
    onProjectRenameFailed: () => {
      pendingName.value = undefined
    },
    onProjectRenamed: (oldName: string, newName: string) => {
      if ((oldName as Identifier) === synchronizedName.value) {
        synchronizedName.value = newName as Identifier
        pendingName.value = undefined
      }
    },
    displayName: displayedName,
  }
}

/** Creates a project name store for use in tests. */
export function mockProjectNameStore(
  namespace: ToValue<string | undefined> = 'local',
  initialName: string = 'Mock_Project',
  displayName: ToValue<string> = 'Mock Project',
) {
  return createProjectNameStore({
    projectNamespace: namespace,
    projectInitialName: initialName,
    projectDisplayedName: displayName,
  })
}
