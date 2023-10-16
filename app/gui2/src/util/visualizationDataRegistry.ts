import type { ExecutionContext } from '@/stores/project.ts'
import { OutboundPayload, VisualizationUpdate } from 'shared/binaryProtocol.ts'
import type { DataServer } from 'shared/dataServer.ts'
import type {
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
  Uuid,
} from 'shared/languageServerTypes.ts'
import { reactive } from 'vue'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
}

/** This class holds the computed values that have been received from the language server. */
export class VisualizationDataRegistry {
  private visualizationValues: Map<Uuid, string | null>
  private dataServer: Promise<DataServer>
  private executionContext: ExecutionContext
  private _reconfiguredHandler = this.visualizationsConfigured.bind(this)
  private _dataHandler = this.visualizationUpdate.bind(this)

  constructor(executionContext: ExecutionContext, dataServer: Promise<DataServer>) {
    this.executionContext = executionContext
    this.dataServer = dataServer
    this.visualizationValues = reactive(new Map())

    this.executionContext.on('visualizationsConfigured', this._reconfiguredHandler)
    this.dataServer.then((data) => {
      data.on(`${OutboundPayload.VISUALIZATION_UPDATE}`, this._dataHandler)
    })
  }

  private visualizationsConfigured(uuids: Set<Uuid>) {
    for (const key of this.visualizationValues.keys()) {
      if (!uuids.has(key)) {
        this.visualizationValues.delete(key)
      }
    }
  }

  private visualizationUpdate(update: VisualizationUpdate, uuid: Uuid | null) {
    if (uuid) {
      const newData = update.dataString()
      const current = this.visualizationValues.get(uuid)
      if (current !== newData) {
        this.visualizationValues.set(uuid, newData)
      }
    }
  }

  getRawData(visualizationId: Uuid): string | null {
    return this.visualizationValues.get(visualizationId) ?? null
  }

  destroy() {
    this.executionContext.off('visualizationsConfigured', this._reconfiguredHandler)
    this.dataServer.then((data) => {
      data.off(`${OutboundPayload.VISUALIZATION_UPDATE}`, this._dataHandler)
    })
  }
}
