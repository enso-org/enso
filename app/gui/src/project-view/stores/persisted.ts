import { useSyncLocalStorage } from '@/composables/syncLocalStorage'
import { createContextStore } from '@/providers'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { injectVisibility } from '@/providers/visibility'
import { Vec2 } from '@/util/data/vec2'
import { encodeMethodPointer, type MethodPointer } from '@/util/methodPointer'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { until } from '@vueuse/core'
import { Ok, type Result } from 'enso-common/src/utilities/data/result'
import { encoding } from 'lib0'
import { computed, toValue } from 'vue'
import { type GraphStore } from '../../providers/openedProjects/graph'

export type PersistedStore = ReturnType<typeof usePersisted>

export const [providePersisted, usePersisted] = createContextStore(
  'persisted',
  (
    projectId: ToValue<string>,
    graphStore: GraphStore,
    graphNavigator: GraphNavigator,
    onRestore: () => void,
  ) => {
    /**
     * JSON serializable representation of graph state saved in localStorage. The names of fields here
     * are kept relatively short, because it will be common to store hundreds of them within one big
     * JSON object, and serialize it quite often whenever the state is modified. Shorter keys end up
     * costing less localStorage space and slightly reduce serialization overhead.
     */
    interface GraphStoredState {
      /** Navigator position X */
      x?: number | undefined
      /** Navigator position Y */
      y?: number | undefined
      /** Navigator scale */
      s?: number | undefined
    }

    const visible = injectVisibility()
    const visibleAreasReady = computed(() => {
      const nodesCount = graphStore.db.nodeIdToNode.size
      const visibleNodeAreas = graphStore.visibleNodeAreas
      return nodesCount > 0 && visibleNodeAreas.length == nodesCount
    })

    // Client graph state needs to be stored separately for:
    // - each project
    // - each function within the project
    function encodeKey(enc: encoding.Encoder, methodPointer: Result<MethodPointer>) {
      encoding.writeVarString(enc, toValue(projectId))
      if (methodPointer.ok) encodeMethodPointer(enc, methodPointer.value)
    }

    const storageOps = useSyncLocalStorage<GraphStoredState>({
      storageKey: 'enso-graph-state',
      mapKeyEncoder: (enc) => encodeKey(enc, graphStore.currentMethod.pointer),
      debounce: 200,
      captureState() {
        return {
          x: graphNavigator.targetLeftTop.x,
          y: graphNavigator.targetLeftTop.y,
          s: graphNavigator.targetScale,
        } satisfies GraphStoredState
      },
      async restoreState(restored, abort) {
        if (restored) {
          const pos = new Vec2(restored.x ?? 0, restored.y ?? 0)
          const scale = restored.s ?? 1
          graphNavigator.setPosAndScale(pos, scale)
        } else {
          await until(visibleAreasReady).toBe(true)
          await until(visible).toBe(true)
          if (!abort.aborted) onRestore()
        }
      },
    })

    function handleModifiedMethodPointer(
      oldMethodPointer: MethodPointer,
      newMethodPointer: MethodPointer,
    ) {
      storageOps.moveToNewKey(
        (enc) => encodeKey(enc, Ok(oldMethodPointer)),
        (enc) => encodeKey(enc, Ok(newMethodPointer)),
      )
    }

    return proxyRefs({
      handleModifiedMethodPointer,
    })
  },
)
