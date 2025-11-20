import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { useRightPanelData } from '$/providers/rightPanel'
import type { ToValue } from '@/util/reactivity'
import type { Asset, ProjectId } from 'enso-common/src/services/Backend'
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
  asset: Asset | undefined
  basePathSegments: string[] | undefined
}

/** Capture a snapshot of all context values at current point. */
export function captureResourceContext(context: ResourceContext): ResourceContextSnapshot {
  return {
    project: toValue(context.project),
    asset: toValue(context.asset),
    basePathSegments: toValue(context.basePathSegments),
  }
}

/**
 * Assemble resource context based on available project information in Vue's context.
 *
 * It will check `currentProject` from `WithCurrentProject` component first, and then `focusedAsset` in container.
 */
export function useCurrentProjectResourceContext(): ResourceContext {
  const currentProject = useCurrentProject(true)
  if (currentProject != null) {
    return {
      project: () => currentProject.store.value.id,
      asset: undefined,
      basePathSegments: () => {
        const fileName = currentProject.store.value.observedFileName
        if (fileName) return ['src', ...fileName.split('/')]
      },
    }
  }
  const rightPanel = useRightPanelData(true)
  return {
    project: () => rightPanel?.focusedProject,
    asset: () => rightPanel?.focusedAsset,
    // We display documentation of `main` function, so image access is relative to the main module.
    basePathSegments: ['src', 'Main.enso'],
  }
}
