import { ProjectId } from '#/services/Backend'
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { ToValue } from '@/util/reactivity'
import { toValue } from 'vue'

/**
 * Data reprsenting contextual information that can influence the resolution process of a resource URL.
 * Not every type of a resource URL requires any particular context information to be fully resolved,
 * but we can only know that during or after the resolution. The resolution process will access any
 * context values at the latest possible time in order to avoid capturing any unnecessary dependencies.
 */
export type ResourceContext = {
  [K in keyof ResourceContextSnapshot]: ToValue<ResourceContextSnapshot[K]>
}

/**
 * A non-reactive snapshot of {@link ResourceContext} with all context values fully resolved. Snapshotting
 * is used when we want to ensure that an asynchronous task gets to use the same context between await points.
 */
export interface ResourceContextSnapshot {
  project: ProjectId | undefined
  basePathSegments: string[] | undefined
}

/** Capture a snapshot of all context values at current point. */
export function captureResourceContext(context: ResourceContext): ResourceContextSnapshot {
  return {
    project: toValue(context.project),
    basePathSegments: toValue(context.basePathSegments),
  }
}

/**
 * Assemble resource context based on `currentProject` structure present in Vue's context.
 */
export function useCurrentProjectResourceContext(): ResourceContext {
  const currentProject = useCurrentProject(true)
  return {
    project: () => currentProject?.id.value ?? undefined,
    basePathSegments: () => {
      if (!currentProject) return
      const openedProjectStore = currentProject.storesRefs.store.value
      // When project is not opened, we assume that all image access is relative to main module.
      if (!openedProjectStore) return ['src', 'Main.enso']

      const fileName = openedProjectStore.observedFileName
      if (fileName) return ['src', ...fileName.split('/')]
    },
  }
}
