import type { ExecutionContext } from '@/stores/project'
import type { ComputedValueRegistry, ExpressionInfo } from '@/stores/project/computedValueRegistry'
import type { VisualizationDataRegistry } from '@/stores/project/visualizationDataRegistry'
import * as random from 'lib0/random'
import type { ExpressionUpdate } from 'shared/languageServerTypes'
import type { ExprId, Uuid } from 'shared/yjsModel'
import { computed, shallowReadonly } from 'vue'

export class DataflowErrorRegistry {
  private visualizationIds = new Map<ExprId, Uuid>()

  constructor(
    computedValueRegistry: ComputedValueRegistry,
    private executionContext: ExecutionContext,
    private visualizationDataRegistry: VisualizationDataRegistry,
  ) {
    computedValueRegistry.on('update', this.onUpdate.bind(this))
  }

  private onUpdate(
    update: ExpressionUpdate,
    oldInfo: ExpressionInfo | undefined,
    newInfo: ExpressionInfo,
  ) {
    if (
      (!oldInfo || oldInfo.payload.type !== 'DataflowError') &&
      newInfo.payload.type === 'DataflowError'
    ) {
      const id = random.uuidv4() as Uuid
      this.visualizationIds.set(update.expressionId, id)
      this.executionContext?.setVisualization(id, {
        expressionId: update.expressionId,
        visualizationModule: 'Standard.Visualization.Preprocessor',
        expression: {
          module: 'Standard.Visualization.Preprocessor',
          definedOnType: 'Standard.Visualization.Preprocessor',
          name: 'error_preprocessor',
        },
      })
    } else if (
      oldInfo?.payload.type === 'DataflowError' &&
      newInfo.payload.type !== 'DataflowError'
    ) {
      const id = this.visualizationIds.get(update.expressionId)
      id && this.executionContext?.setVisualization(id, null)
    }
  }

  get(exprId: ExprId) {
    const id = this.visualizationIds.get(exprId)
    if (!id) return
    return shallowReadonly(
      computed<{ kind: 'Dataflow'; message: string } | undefined>(() => {
        const json = this.visualizationDataRegistry?.getRawData(id)
        return json != null ? JSON.parse(json) : undefined
      }),
    )
  }
}
