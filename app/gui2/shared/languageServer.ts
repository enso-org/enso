import { Client } from '@open-rpc/client-js'

import * as map from 'lib0/map'
import * as set from 'lib0/set'
import { SHA3 } from 'sha3'
import { Emitter } from './event'
import type {
  Checksum,
  ContextId,
  ExecutionEnvironment,
  ExpressionId,
  FileEdit,
  Notifications,
  Path,
  RegisterOptions,
  StackItem,
  VisualizationConfiguration,
  response,
} from './lsTypes'
import type { Uuid } from './yjsModel'

export class LanguageServer extends Emitter<Notifications> {
  client: Client
  handlers: Map<string, Set<(...params: any[]) => void>>

  constructor(client: Client) {
    super()
    this.client = client
    this.handlers = new Map()

    client.onNotification((notification) => {
      this.emit(notification.method as keyof Notifications, [notification.params])
    })
  }

  addEventListener<K extends keyof Notifications>(
    type: K,
    listener: (params: Notifications[K]) => void,
  ) {
    const listeners = map.setIfUndefined(this.handlers, type as string, set.create)
    listeners.add(listener)
  }

  removeEventListener<K extends keyof Notifications>(
    type: K,
    listener: (params: Notifications[K]) => void,
  ) {
    const listeners = this.handlers.get(type as string)
    if (listeners) {
      listeners.delete(listener)
    }
  }

  private request<T>(method: string, params: object): Promise<T> {
    return this.client.request({ method, params })
  }

  acquireCapability(method: string, registerOptions: RegisterOptions): Promise<void> {
    return this.request('capability/acquire', { method, registerOptions })
  }

  acquireReceivesTreeUpdates(path: Path): Promise<void> {
    return this.acquireCapability('file/receivesTreeUpdates', { path })
  }

  initProtocolConnection(clientId: Uuid): Promise<response.InitProtocolConnection> {
    return this.request('session/initProtocolConnection', { clientId })
  }

  openTextFile(path: Path): Promise<response.OpenTextFile> {
    return this.request('text/openFile', { path })
  }

  closeTextFile(path: Path): Promise<void> {
    return this.request('text/closeFile', { path })
  }

  saveTextFile(path: Path, currentVersion: Checksum): Promise<void> {
    return this.request('text/save', { path, currentVersion })
  }

  applyEdit(edit: FileEdit, execute: boolean): Promise<void> {
    return this.request('text/applyEdit', { edit, execute })
  }

  listFiles(path: Path): Promise<response.FileList> {
    return this.request('file/list', { path })
  }

  createExecutionContext(contextId?: ContextId): Promise<response.ExecutionContext> {
    return this.request('executionContext/create', { contextId })
  }

  destroyExecutionContext(contextId: ContextId): Promise<void> {
    return this.request('executionContext/destroy', { contextId })
  }

  forkExecutionContext(contextId: ContextId): Promise<response.ExecutionContext> {
    return this.request('executionContext/fork', { contextId })
  }

  pushExecutionContext(contextId: ContextId, stackItem: StackItem): Promise<void> {
    return this.request('executionContext/push', { contextId, stackItem })
  }

  popExecutionContext(contextId: ContextId): Promise<void> {
    return this.request('executionContext/pop', { contextId })
  }

  recomputeExecutionContext(
    contextId: ContextId,
    invalidatedExpressions?: 'all' | string[],
    executionEnvironment?: ExecutionEnvironment,
  ): Promise<void> {
    return this.request('executionContext/recompute', {
      contextId,
      invalidatedExpressions,
      executionEnvironment,
    })
  }

  interruptExecutionContext(contextId: ContextId): Promise<void> {
    return this.request('executionContext/interrupt', { contextId })
  }

  setExecutionEnvironment(
    contextId: ContextId,
    executionEnvironment?: ExecutionEnvironment,
  ): Promise<void> {
    return this.request('executionContext/setExecutionEnvironment', {
      contextId,
      executionEnvironment,
    })
  }

  executeExpression(
    visualizationId: Uuid,
    expressionId: ExpressionId,
    visualizationConfig: VisualizationConfiguration,
  ): Promise<void> {
    return this.request('executionContext/interrupt', {
      visualizationId,
      expressionId,
      visualizationConfig,
    })
  }

  attachVisualization(
    visualizationId: Uuid,
    expressionId: ExpressionId,
    visualizationConfig: VisualizationConfiguration,
  ): Promise<void> {
    return this.request('executionContext/attachVisualization', {
      visualizationId,
      expressionId,
      visualizationConfig,
    })
  }

  detachVisualization(
    visualizationId: Uuid,
    expressionId: ExpressionId,
    executionContextId: ContextId,
  ): Promise<void> {
    return this.request('executionContext/detachVisualization', {
      visualizationId,
      expressionId,
      executionContextId,
    })
  }

  modifyVisualization(
    visualizationId: Uuid,
    visualizationConfig: VisualizationConfiguration,
  ): Promise<void> {
    return this.request('executionContext/modifyVisualization', {
      visualizationId,
      visualizationConfig,
    })
  }

  dispose() {
    this.client.close()
  }
}

export function computeTextChecksum(text: string): Checksum {
  return new SHA3(224).update(text).digest('hex') as Checksum
}
