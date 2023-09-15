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

/** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md) */
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

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#capabilityacquire) */
  acquireCapability(method: string, registerOptions: RegisterOptions): Promise<void> {
    return this.request('capability/acquire', { method, registerOptions })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filereceivestreeupdates) */
  acquireReceivesTreeUpdates(path: Path): Promise<void> {
    return this.acquireCapability('file/receivesTreeUpdates', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#sessioninitprotocolconnection) */
  initProtocolConnection(clientId: Uuid): Promise<response.InitProtocolConnection> {
    return this.request('session/initProtocolConnection', { clientId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textopenfile) */
  openTextFile(path: Path): Promise<response.OpenTextFile> {
    return this.request('text/openFile', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textclosefile) */
  closeTextFile(path: Path): Promise<void> {
    return this.request('text/closeFile', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textsave) */
  saveTextFile(path: Path, currentVersion: Checksum): Promise<void> {
    return this.request('text/save', { path, currentVersion })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textapplyedit) */
  applyEdit(edit: FileEdit, execute: boolean): Promise<void> {
    return this.request('text/applyEdit', { edit, execute })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filelist) */
  listFiles(path: Path): Promise<response.FileList> {
    return this.request('file/list', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextcreate) */
  createExecutionContext(contextId?: ContextId): Promise<response.ExecutionContext> {
    return this.request('executionContext/create', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextdestroy) */
  destroyExecutionContext(contextId: ContextId): Promise<void> {
    return this.request('executionContext/destroy', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextfork) */
  forkExecutionContext(contextId: ContextId): Promise<response.ExecutionContext> {
    return this.request('executionContext/fork', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextpush) */
  pushExecutionContextItem(contextId: ContextId, stackItem: StackItem): Promise<void> {
    return this.request('executionContext/push', { contextId, stackItem })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextpop) */
  popExecutionContextItem(contextId: ContextId): Promise<void> {
    return this.request('executionContext/pop', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextrecompute) */
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

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextinterrupt) */
  interruptExecutionContext(contextId: ContextId): Promise<void> {
    return this.request('executionContext/interrupt', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextsetexecutionenvironment) */
  setExecutionEnvironment(
    contextId: ContextId,
    executionEnvironment?: ExecutionEnvironment,
  ): Promise<void> {
    return this.request('executionContext/setExecutionEnvironment', {
      contextId,
      executionEnvironment,
    })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextexecuteexpression) */
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

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextattachvisualization) */
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

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextdetachvisualization) */
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

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextmodifyvisualization) */
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
