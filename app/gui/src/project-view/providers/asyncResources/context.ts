import { ProjectId } from '#/services/Backend'
import { injectCurrentProject } from '$/components/WithCurrentProject.vue'

export interface LazyResourceContext {
  project: () => ProjectId | undefined
  basePathSegments: () => string[] | undefined
}

export type CapturedResourceContext = {
  [K in keyof LazyResourceContext]: ReturnType<LazyResourceContext[K]>
}

/** Capture all context closures at current point.  */
export function captureResourceContext(context: LazyResourceContext): CapturedResourceContext {
  return {
    project: context.project(),
    basePathSegments: context.basePathSegments(),
  }
}

/** Wrap captured context into a lazy context, so that it can be used in APIs expecting lazy variant. */
export function capturedContextAsLazy(context: CapturedResourceContext): LazyResourceContext {
  return {
    project: () => context.project,
    basePathSegments: () => context.basePathSegments,
  }
}

/** Assemble resource context from current Vue's context. */
export function useAmbientContext(): LazyResourceContext {
  const currentProject = injectCurrentProject(true)
  return {
    project: () => currentProject?.id.value ?? undefined,
    basePathSegments: () => {
      if (!currentProject) return

      const fileName = currentProject.storesRefs.store.value?.observedFileName
      if (fileName) return ['src', ...fileName.split('/')]
    },
  }
}
