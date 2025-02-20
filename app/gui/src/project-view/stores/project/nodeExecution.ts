import { createContextStore } from '@/providers'
import { NodeId } from '@/stores/graph'
import { type ProjectStore } from '@/stores/project'
import { computed, reactive, ref } from 'vue'
import { ExecutionEnvironment } from 'ydoc-shared/languageServerTypes'
import { ExternalId } from 'ydoc-shared/yjsModel'

/** Allows to recompute certain expressions (usually nodes). */
export const [provideNodeExecution, useNodeExecution] = createContextStore(
  'nodeExecution',
  (projectStore: ProjectStore) => {
    const recomputationInProgress = reactive(new Set<ExternalId>())
    const globalRecomputationInProgress = ref(false)

    /** Recompute all expressions using provided environment. */
    function recomputeAll(environment?: ExecutionEnvironment) {
      projectStore.executionContext.recompute('all', environment)
      globalRecomputationInProgress.value = true
      whenExecutionFinished(() => {
        globalRecomputationInProgress.value = false
      })
    }

    /** Recompute a specific node and its using provided environment. */
    function recomputeOnce(id: ExternalId, environment: ExecutionEnvironment) {
      // We don’t need to pass `invalidatedIds` when providing per-expression configs.
      const invalidatedIds = undefined
      const expressionConfigs = [{ expressionId: id, environment }]
      projectStore.executionContext.recompute(invalidatedIds, environment, expressionConfigs)
      recomputationInProgress.add(id)
      whenExecutionFinished(() => {
        recomputationInProgress.delete(id)
      })
    }

    function isBeingRecomputed(id: NodeId) {
      return computed(() => globalRecomputationInProgress.value || recomputationInProgress.has(id))
    }

    function whenExecutionFinished(f: () => void) {
      projectStore.executionContext.once('executionComplete', f)
      projectStore.executionContext.once('executionFailed', f)
    }

    return { recomputeOnce, isBeingRecomputed, recomputeAll }
  },
)
