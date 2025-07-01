import { ProjectId } from '#/services/Backend'
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { ToValue } from '@/util/reactivity'
import { toValue } from 'vue'

export interface CapturedResourceContext {
  project: ProjectId | undefined
  basePathSegments: string[] | undefined
}

export type ResourceContext = {
  [K in keyof CapturedResourceContext]: ToValue<CapturedResourceContext[K]>
}

/** Capture all context closures at current point.  */
export function captureResourceContext(context: ResourceContext): CapturedResourceContext {
  return {
    project: toValue(context.project),
    basePathSegments: toValue(context.basePathSegments),
  }
}

/** Wrap captured context into a lazy context, so that it can be used in APIs expecting lazy variant. */
export function capturedContextAsLazy(context: CapturedResourceContext): ResourceContext {
  return {
    project: () => context.project,
    basePathSegments: () => context.basePathSegments,
  }
}

/** Assemble resource context from current Vue's context. */
export function useAmbientContext(): ResourceContext {
  const currentProject = useCurrentProject(true)
  return {
    project: () => currentProject?.id.value ?? undefined,
    basePathSegments: () => {
      if (!currentProject) return

      const fileName = currentProject.storesRefs.store.value?.observedFileName
      if (fileName) return ['src', ...fileName.split('/')]
    },
  }
}
