import type { ExecutionContext } from '@/stores/project'
import { Err, Ok, type Result } from '@/util/result'
import { OutboundPayload, VisualizationUpdate } from 'shared/binaryProtocol'
import type { DataServer } from 'shared/dataServer'
import type {
  Diagnostic,
  ExpressionId,
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
  Uuid,
} from 'shared/languageServerTypes'
import { reactive } from 'vue'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
}

/** This class holds the computed values that have been received from the language server. */
export class VisualizationDataRegistry {
  /** This map stores only keys representing attached visualization. The responses for
   * executeExpression are handled by project store's `executeExpression` method. */
  private visualizationValues: Map<Uuid, Result<string> | null>
  private dataServer: Promise<DataServer>
  private executionContext: ExecutionContext
  private reconfiguredHandler = this.visualizationsConfigured.bind(this)
  private dataHandler = this.visualizationUpdate.bind(this)
  private errorHandler = this.visualizationError.bind(this)

  constructor(executionContext: ExecutionContext, dataServer: Promise<DataServer>) {
    this.executionContext = executionContext
    this.dataServer = dataServer
    this.visualizationValues = reactive(new Map())

    this.executionContext.on('visualizationsConfigured', this.reconfiguredHandler)
    this.dataServer.then((data) => {
      data.on(`${OutboundPayload.VISUALIZATION_UPDATE}`, this.dataHandler)
    })
    this.executionContext.on('visualizationEvaluationFailed', this.errorHandler)
  }

  private visualizationsConfigured(uuids: Set<Uuid>) {
    for (const key of this.visualizationValues.keys()) {
      if (!uuids.has(key)) {
        this.visualizationValues.delete(key)
      }
    }
    for (const id of uuids) {
      if (!this.visualizationValues.has(id)) {
        this.visualizationValues.set(id, null)
      }
    }
  }

  private visualizationUpdate(update: VisualizationUpdate, uuid: Uuid | null) {
    if (uuid && this.visualizationValues.has(uuid)) {
      const newData = update.dataString()
      const current = this.visualizationValues.get(uuid)
      if (newData == null && current != null) {
        this.visualizationValues.set(uuid, null)
      }
      if (newData != null && (!current?.ok || current?.value != newData)) {
        this.visualizationValues.set(uuid, Ok(newData))
      }
    }
  }

  private visualizationError(
    visId: Uuid,
    _exprId: ExpressionId,
    message: string,
    _diagnostic: Diagnostic,
  ) {
    if (this.visualizationValues.has(visId)) {
      const current = this.visualizationValues.get(visId)
      if (current == null || current.ok || current.error.payload !== message) {
        this.visualizationValues.set(visId, Err(message))
      }
    }
  }

  getRawData(visualizationId: Uuid): Result<string> | null {
    return this.visualizationValues.get(visualizationId) ?? null
  }

  destroy() {
    this.executionContext.off('visualizationsConfigured', this.reconfiguredHandler)
    this.dataServer.then((data) => {
      data.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, this.dataHandler)
    })
    this.executionContext.off('visualizationEvaluationFailed', this.errorHandler)
  }
}
