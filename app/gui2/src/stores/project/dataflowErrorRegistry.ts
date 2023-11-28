import type { ExecutionContext } from '@/stores/project'
import type { ComputedValueRegistry, ExpressionInfo } from '@/stores/project/computedValueRegistry'
import * as random from 'lib0/random'
import type { ExpressionUpdate } from 'shared/languageServerTypes'
import type { ExprId, Uuid } from 'shared/yjsModel'

export class DataflowErrorRegistry {
  visualizationIds: ReadonlyMap<ExprId, Uuid> = new Map<ExprId, Uuid>()

  private get dataflowErrorVisualizationIds_() {
    return this.visualizationIds as Map<ExprId, Uuid>
  }

  constructor(
    computedValueRegistry: ComputedValueRegistry,
    private executionContext: ExecutionContext,
  ) {
    computedValueRegistry.on('update', this.onUpdate.bind(this))
  }

  onUpdate(update: ExpressionUpdate, oldInfo: ExpressionInfo | undefined, newInfo: ExpressionInfo) {
    if (
      (!oldInfo || oldInfo.payload.type !== 'DataflowError') &&
      newInfo.payload.type === 'DataflowError'
    ) {
      const id = random.uuidv4() as Uuid
      this.dataflowErrorVisualizationIds_.set(update.expressionId, id)
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
      const id = this.dataflowErrorVisualizationIds_.get(update.expressionId)
      id && this.executionContext?.setVisualization(id, null)
    }
  }
}
