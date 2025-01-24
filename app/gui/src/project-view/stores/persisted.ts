import { useSyncLocalStorage } from '@/composables/syncLocalStorage'
import { createContextStore } from '@/providers'
import { GraphNavigator } from '@/providers/graphNavigator'
import { injectVisibility } from '@/providers/visibility'
import { Ok, Result } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import { encodeMethodPointer, MethodPointer } from '@/util/methodPointer'
import { ToValue } from '@/util/reactivity'
import { until } from '@vueuse/core'
import { encoding } from 'lib0'
import { computed, proxyRefs, ref, toValue } from 'vue'
import { GraphStore } from './graph'

export type PersistedStore = ReturnType<typeof usePersisted>

export const [providePersisted, usePersisted] = createContextStore(
  'persisted',
  (
    projectId: ToValue<string>,
    graphStore: GraphStore,
    graphNavigator: GraphNavigator,
    onRestore: () => void,
  ) => {
    const graphRightDock = ref<boolean>()
    const graphRightDockTab = ref<string>()
    const graphRightDockWidth = ref<number>()

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
      /** Whether or not the documentation panel is open. */
      doc?: boolean | undefined
      /** The selected tab in the right-side panel. */
      rtab?: string | undefined
      /** Width of the right dock. */
      rwidth?: number | undefined
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
      mapKeyEncoder: (enc) => encodeKey(enc, graphStore.currentMethodPointer),
      debounce: 200,
      captureState() {
        return {
          x: graphNavigator.targetCenter.x,
          y: graphNavigator.targetCenter.y,
          s: graphNavigator.targetScale,
          doc: graphRightDock.value,
          rtab: graphRightDockTab.value,
          rwidth: graphRightDockWidth.value ?? undefined,
        } satisfies GraphStoredState
      },
      async restoreState(restored, abort) {
        if (restored) {
          const pos = new Vec2(restored.x ?? 0, restored.y ?? 0)
          const scale = restored.s ?? 1
          graphNavigator.setCenterAndScale(pos, scale)
          graphRightDock.value = restored.doc ?? undefined
          graphRightDockTab.value = restored.rtab ?? undefined
          graphRightDockWidth.value = restored.rwidth ?? undefined
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
      graphRightDock,
      graphRightDockTab,
      graphRightDockWidth,
      handleModifiedMethodPointer,
    })
  },
)
