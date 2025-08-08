import type { MethodPointer } from '@/util/methodPointer'
import type { ToValue } from '@/util/reactivity'
import type { ProjectId } from 'enso-common/src/services/Backend'
import { computed, type Ref, toRef, toValue } from 'vue'
import type { Opt } from 'ydoc-shared/util/data/opt'

export interface DocumentViewIdOptions {
  /** The project to which the method belongs. */
  projectId: ToValue<Opt<ProjectId>>
  /** The method whose documentation is being viewed. */
  methodPointer: ToValue<Opt<MethodPointer>>
  /** Distinguishes between different views of the same method. */
  view: ToValue<string>
}

/** Returns a value that uniquely identifies a view of a method's documentation. */
export function useDocumentViewId(
  options: DocumentViewIdOptions,
): Readonly<Ref<string | undefined>> {
  const methodPointer = toRef(options.methodPointer)
  return computed((): string | undefined =>
    methodDocumentViewId({
      project: toValue(options.projectId),
      method: methodPointer.value ? methodPointerKey(methodPointer.value) : '$.main',
      view: toValue(options.view),
    }),
  )
}

function methodDocumentViewId({
  project,
  method,
  view,
}: {
  project: Opt<string>
  method: Opt<string>
  view: string
}): string | undefined {
  if (project == null || method == null) return
  return JSON.stringify({ project, method, view })
}

function methodPointerKey(methodPointer: MethodPointer): string
function methodPointerKey(methodPointer: MethodPointer | undefined): string | undefined {
  if (!methodPointer) return
  const { definedOnType, name } = methodPointer
  return definedOnType.append(name).key() satisfies string
}
