import { sha3_224 as SHA3 } from '@noble/hashes/sha3'
import { bytesToHex } from '@noble/hashes/utils'
import { Client, RequestManager } from '@open-rpc/client-js'
import debug from 'debug'
import { ObservableV2 } from 'lib0/observable'
import { uuidv4 } from 'lib0/random'
import { z } from 'zod'
import { walkFs } from './languageServer/files'
import type {
  Checksum,
  ContentRoot,
  ContextId,
  Event,
  ExecutionEnvironment,
  ExpressionId,
  FileEdit,
  FileSystemObject,
  IdMapTriple,
  IdMapTuple,
  Notifications,
  Path,
  RegisterOptions,
  response,
  StackItem,
  TextFileContents,
  VisualizationConfiguration,
} from './languageServerTypes'
import { Err, Ok, type Result } from './util/data/result'
import { AbortScope, exponentialBackoff, ReconnectingWebSocketTransport } from './util/net'
import type { Uuid } from './yjsModel'

const debugLog = debug('ydoc-shared:languageServer')

const RPC_TIMEOUT_MS = 15000

export enum ErrorCode {
  ACCESS_DENIED = 100,
  FILE_SYSTEM_ERROR = 1000,
  CONTENT_ROOT_NOT_FOUND = 1001,
  FILE_NOT_FOUND = 1003,
  FILE_EXISTS = 1004,
  OPERATION_TIMEOUT = 1005,
  NOT_DIRECTORY = 1006,
  NOT_FILE = 1007,
  CANNOT_OVERWRITE = 1008,
  READ_OUT_OF_BOUNDS = 1009,
  CANNOT_DECODE = 1010,
  STACK_ITEM_NOT_FOUND = 2001,
  CONTEXT_NOT_FOUND = 2002,
  EMPTY_STACK = 2003,
  INVALID_STACK_ITEM = 2004,
  MODULE_NOT_FOUND = 2005,
  VISUALIZATION_NOT_FOUND = 2006,
  VISUALIZATION_EXPRESSION_ERROR = 2007,
  FILE_NOT_OPENED = 3001,
  TEXT_EDIT_VALIDATION_ERROR = 3002,
  INVALID_VERSION = 3003,
  WRITE_DENIED = 3004,
  CAPABILITY_NOT_ACQUIRED = 5001,
  SESSION_NOT_INITIALIZED = 6001,
  SESSION_ALREADY_INITIALIZED = 6002,
  RESOURCES_INITIALIZATION_ERROR = 6003,
  SUGGESTION_DATABASE_ERROR = 7001,
  PROJECT_NOT_FOUND = 7002,
  MODULE_NAME_NOT_RESOLVED = 7003,
  SUGGESTION_NOT_FOUND = 7004,
  EDITION_NOT_FOUND = 8001,
  LIBRARY_ALREADY_EXISTS = 8002,
  LIBRARY_REPOSITORY_AUTHENTICATION_ERROR = 8003,
  LIBRARY_PUBLISH_ERROR = 8004,
  LIBRARY_UPLOAD_ERROR = 8005,
  LIBRARY_DOWNLOAD_ERROR = 8006,
  LOCAL_LIBRARY_NOT_FOUND = 8007,
  LIBRARY_NOT_RESOLVED = 8008,
  INVALID_LIBRARY_NAME = 8009,
  DEPENDENCY_DISCOVERY_ERROR = 8010,
  INVALID_SEMVER_VERSION = 8011,
  EXPRESSION_NOT_FOUND = 9001,
  FAILED_TO_APPLY_EDITS = 9002,
  REFACTORING_NOT_SUPPORTED = 9003,
}

const RemoteRpcErrorSchema = z.object({
  code: z.nativeEnum(ErrorCode),
  message: z.string(),
  data: z.optional(z.any()),
})
type RemoteRpcErrorParsed = z.infer<typeof RemoteRpcErrorSchema>

/** Payload for a {@linnk LsRpcError}. */
export class RemoteRpcError {
  code: ErrorCode
  message: string
  data?: any
  /** Create a {@link RemoteRpcError}. */
  constructor(error: RemoteRpcErrorParsed) {
    this.code = error.code
    this.message = error.message
    this.data = error.data
  }
}

/** An error executing a request from the {@link LanguageServer}. */
export class LsRpcError {
  cause: RemoteRpcError | Error | string
  request: string
  params: object
  /** Create an {@link LsRpcError}. */
  constructor(cause: RemoteRpcError | Error | string, request: string, params: object) {
    this.cause = cause
    this.request = request
    this.params = params
  }

  /** Get a human-readable string representation of this error. */
  toString() {
    return `Language Server request '${this.request}' failed: ${this.cause instanceof RemoteRpcError ? this.cause.message : this.cause}`
  }
}

export type LsRpcResult<T> = Result<T, LsRpcError>

export type TransportEvents = {
  'transport/closed': () => void
  'transport/connected': () => void
}

/**
 * This client implements the [Language Server Protocol](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md)
 *
 * It also handles the initialization (and re-initialization on every reconnect); each method
 * representing a remote call (except the `initProtocolConnection` obviously) waits for
 * initialization before sending the request.
 */
export class LanguageServer extends ObservableV2<Notifications & TransportEvents> {
  client: Client
  /**
   * This promise is resolved once the LS protocol is initialized. When connection is lost, this
   * field becomes again an unresolved promise until reconnected and reinitialized.
   */
  initialized: Promise<LsRpcResult<response.InitProtocolConnection>>
  private clientScope: AbortScope = new AbortScope()
  private initializationScheduled = false
  private shouldReconnect = true
  private retainCount = 1
  debug = false

  /** Create a {@link LanguageServer}. */
  constructor(
    private clientID: Uuid,
    private transport: ReconnectingWebSocketTransport,
  ) {
    super()
    this.initialized = this.scheduleInitializationAfterConnect()
    const requestManager = new RequestManager([transport])
    this.client = new Client(requestManager)
    this.client.onNotification(notification => {
      this.emit(notification.method as keyof Notifications, [notification.params])
    })
    this.client.onError(error => {
      console.error('Unexpected Language Server connection error:', error)
    })
    transport.on('error', error => {
      if (this.shouldReconnect) {
        console.error('Language Server transport error:', error)
      }
    })
    const onTransportClosed = () => {
      if (!this.shouldReconnect) {
        if (!this.isDisposed) {
          this.dispose()
        }
        return
      }
      this.emit('transport/closed', [])
      debugLog('Language Server: WebSocket closed')
      this.scheduleInitializationAfterConnect()
    }
    transport.on('close', onTransportClosed)
    this.clientScope.onAbort(() => {
      this.transport.off('close', onTransportClosed)
      this.transport.close()
    })
  }

  private scheduleInitializationAfterConnect() {
    if (this.initializationScheduled) return this.initialized
    this.initializationScheduled = true
    this.initialized = new Promise(resolve => {
      this.transport.on(
        'open',
        () => {
          this.emit('transport/connected', [])
          this.initializationScheduled = false
          exponentialBackoff(() => this.initProtocolConnection(this.clientID), {
            onBeforeRetry: (error, _, delay) => {
              console.warn(
                `Failed to initialize Language Server connection, retrying after ${delay}ms...\n`,
                error,
              )
            },
          }).then(result => {
            if (!result.ok) {
              result.error.log('Error initializing Language Server RPC')
            }
            resolve(result)
          })
        },
        { once: true },
      )
    })
    return this.initialized
  }

  /** Whether this {@link LanguageServer} has been disposed and therefore is no longer usable. */
  get isDisposed() {
    return this.retainCount === 0
  }

  /** The {@link ContentRoot}s of this {@link LanguageServer}. */
  get contentRoots(): Promise<ContentRoot[]> {
    return this.initialized.then(result => (result.ok ? result.value.contentRoots : []))
  }

  /** Reconnect the underlying network transport. */
  reconnect() {
    this.transport.reconnect()
  }

  // The "magic bag of holding" generic that is only present in the return type is UNSOUND.
  // However, it is SAFE, as the return type of the API is statically known.
  private async request<T>(
    method: string,
    params: object,
    waitForInit = true,
  ): Promise<LsRpcResult<T>> {
    if (this.isDisposed) {
      return Err(new LsRpcError('LanguageServer disposed', method, params))
    }
    const uuid = uuidv4()
    const now = performance.now()
    try {
      debugLog('LS [%s] %s:\n%j', uuid, method, params)
      if (waitForInit) {
        const initResult = await this.initialized
        if (!initResult.ok) return initResult
      }
      return Ok(await this.client.request({ method, params }, RPC_TIMEOUT_MS))
    } catch (error) {
      const remoteError = RemoteRpcErrorSchema.safeParse(error)
      if (remoteError.success) {
        return Err(new LsRpcError(new RemoteRpcError(remoteError.data), method, params))
      } else if (error instanceof Error) {
        return Err(new LsRpcError(error, method, params))
      } else throw error
    } finally {
      debugLog('LS [%s] %s took %dms', uuid, method, performance.now() - now)
    }
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#capabilityacquire) */
  acquireCapability(method: string, registerOptions: RegisterOptions): Promise<LsRpcResult<void>> {
    return this.request('capability/acquire', { method, registerOptions })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filereceivestreeupdates) */
  acquireReceivesTreeUpdates(path: Path): Promise<LsRpcResult<void>> {
    return this.acquireCapability('file/receivesTreeUpdates', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextcanmodify) */
  acquireExecutionContextCanModify(contextId: ContextId): Promise<LsRpcResult<void>> {
    return this.acquireCapability('executionContext/canModify', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#sessioninitprotocolconnection) */
  initProtocolConnection(clientId: Uuid): Promise<LsRpcResult<response.InitProtocolConnection>> {
    return this.request('session/initProtocolConnection', { clientId }, false)
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textopenfile) */
  openTextFile(path: Path): Promise<LsRpcResult<response.OpenTextFile>> {
    return this.request<response.OpenTextFile>('text/openFile', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textclosefile) */
  closeTextFile(path: Path): Promise<LsRpcResult<void>> {
    return this.request('text/closeFile', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textsave) */
  saveTextFile(path: Path, currentVersion: Checksum): Promise<LsRpcResult<void>> {
    return this.request('text/save', { path, currentVersion })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#textapplyedit) */
  applyEdit(
    edit: FileEdit,
    execute: boolean,
    idMap?: readonly IdMapTriple[] | readonly IdMapTuple[],
  ): Promise<LsRpcResult<void>> {
    return this.request('text/applyEdit', { edit, execute, idMap })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filewrite) */
  writeFile(path: Path, contents: TextFileContents): Promise<LsRpcResult<void>> {
    return this.request('file/write', { path, contents })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#fileread) */
  readFile(path: Path): Promise<LsRpcResult<response.FileContents>> {
    return this.request('file/read', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filecreate) */
  createFile(object: FileSystemObject): Promise<LsRpcResult<void>> {
    return this.request('file/create', { object })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filedelete) */
  deleteFile(path: Path): Promise<LsRpcResult<void>> {
    return this.request('file/delete', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filecopy) */
  copyFile(from: Path, to: Path): Promise<LsRpcResult<void>> {
    return this.request('file/copy', { from, to })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filemove) */
  moveFile(from: Path, to: Path): Promise<LsRpcResult<void>> {
    return this.request('file/move', { from, to })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#fileexists) */
  fileExists(path: Path): Promise<LsRpcResult<response.FileExists>> {
    return this.request('file/exists', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filetree) */
  fileTree(path: Path, depth?: number): Promise<LsRpcResult<response.FileTree>> {
    return this.request('file/tree', { path, depth })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filelist) */
  listFiles(path: Path): Promise<LsRpcResult<response.FileList>> {
    return this.request('file/list', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#fileinfo) */
  fileInfo(path: Path): Promise<LsRpcResult<response.FileInfo>> {
    return this.request('file/info', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filechecksum) */
  fileChecksum(path: Path): Promise<LsRpcResult<response.FileChecksum>> {
    return this.request('file/checksum', { path })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcsinit) */
  vcsInit(root: Path): Promise<LsRpcResult<void>> {
    return this.request('vcs/init', { root })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcssave) */
  vcsSave(root: Path, name?: string): Promise<LsRpcResult<response.VCSCommit>> {
    return this.request('vcs/save', { root, name })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcsstatus) */
  vcsStatus(root: Path): Promise<LsRpcResult<response.VCSStatus>> {
    return this.request('vcs/status', { root })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcsrestore) */
  vcsRestore(root: Path, commitId?: string): Promise<LsRpcResult<response.VCSChanges>> {
    return this.request('vcs/restore', { root, commitId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#vcslist) */
  vcsList(root: Path, limit?: number): Promise<LsRpcResult<response.VCSSaves>> {
    return this.request('vcs/list', { root, limit })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextcreate) */
  createExecutionContext(contextId?: ContextId): Promise<LsRpcResult<response.ExecutionContext>> {
    return this.request('executionContext/create', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextdestroy) */
  destroyExecutionContext(contextId: ContextId): Promise<LsRpcResult<void>> {
    return this.request('executionContext/destroy', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextfork) */
  forkExecutionContext(contextId: ContextId): Promise<LsRpcResult<response.ExecutionContext>> {
    return this.request('executionContext/fork', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextpush) */
  pushExecutionContextItem(contextId: ContextId, stackItem: StackItem): Promise<LsRpcResult<void>> {
    return this.request('executionContext/push', { contextId, stackItem })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextpop) */
  popExecutionContextItem(contextId: ContextId): Promise<LsRpcResult<void>> {
    return this.request('executionContext/pop', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextrecompute) */
  recomputeExecutionContext(
    contextId: ContextId,
    invalidatedExpressions?: 'all' | string[],
    executionEnvironment?: ExecutionEnvironment,
  ): Promise<LsRpcResult<void>> {
    return this.request('executionContext/recompute', {
      contextId,
      invalidatedExpressions,
      executionEnvironment,
    })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextinterrupt) */
  interruptExecutionContext(contextId: ContextId): Promise<LsRpcResult<void>> {
    return this.request('executionContext/interrupt', { contextId })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextsetexecutionenvironment) */
  setExecutionEnvironment(
    contextId: ContextId,
    executionEnvironment?: ExecutionEnvironment,
  ): Promise<LsRpcResult<void>> {
    return this.request('executionContext/setExecutionEnvironment', {
      contextId,
      executionEnvironment,
    })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextexecuteexpression) */
  executeExpression(
    executionContextId: Uuid,
    visualizationId: Uuid,
    expressionId: ExpressionId,
    expression: string,
  ): Promise<LsRpcResult<void>> {
    return this.request('executionContext/executeExpression', {
      executionContextId,
      visualizationId,
      expressionId,
      expression,
    })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextattachvisualization) */
  attachVisualization(
    visualizationId: Uuid,
    expressionId: ExpressionId,
    visualizationConfig: VisualizationConfiguration,
  ): Promise<LsRpcResult<void>> {
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
    contextId: ContextId,
  ): Promise<LsRpcResult<void>> {
    return this.request('executionContext/detachVisualization', {
      visualizationId,
      expressionId,
      contextId,
    })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#executioncontextmodifyvisualization) */
  modifyVisualization(
    visualizationId: Uuid,
    visualizationConfig: VisualizationConfiguration,
  ): Promise<LsRpcResult<void>> {
    return this.request('executionContext/modifyVisualization', {
      visualizationId,
      visualizationConfig,
    })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#searchgetsuggestionsdatabase) */
  getSuggestionsDatabase(): Promise<LsRpcResult<response.GetSuggestionsDatabase>> {
    return this.request('search/getSuggestionsDatabase', {})
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#runtimegetcomponentgroups) */
  getComponentGroups(): Promise<LsRpcResult<response.GetComponentGroups>> {
    return this.request('runtime/getComponentGroups', {})
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#refactoringrenamesymbol) */
  renameSymbol(
    module: string,
    expressionId: ExpressionId,
    newName: string,
  ): Promise<LsRpcResult<response.RenameSymbol>> {
    return this.request('refactoring/renameSymbol', { module, expressionId, newName })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#refactoringrenameproject) */
  renameProject(namespace: string, oldName: string, newName: string): Promise<LsRpcResult<void>> {
    return this.request('refactoring/renameProject', { namespace, oldName, newName })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#profilingstart) */
  profilingStart(memorySnapshot?: boolean): Promise<LsRpcResult<void>> {
    return this.request('profiling/start', { memorySnapshot })
  }

  /** [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#profilingstop) */
  profilingStop(): Promise<LsRpcResult<void>> {
    return this.request('profiling/stop', {})
  }

  /** @deprecated `ai/completion_v2` should be used instead. */
  aiCompletion(prompt: string, stopSequence: string): Promise<LsRpcResult<response.AICompletion>> {
    return this.request('ai/completion', { prompt, stopSequence })
  }

  /**
   * A helper function to subscribe to file updates.
   * Please use `ls.on('file/event')` directly if the initial `'Added'` notifications are not
   * needed.
   */
  watchFiles(rootId: Uuid, segments: string[], callback: (event: Event<'file/event'>) => void) {
    let running = true
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    const self = this
    return {
      promise: (async () => {
        self.on('file/event', callback)
        const updatesAcquired = await exponentialBackoff(async () =>
          running ? self.acquireReceivesTreeUpdates({ rootId, segments }) : Ok(),
        )
        if (!updatesAcquired) return updatesAcquired
        return await walkFs(self, { rootId, segments }, (type, path) => {
          if (!running) return
          if (type !== 'File') return
          if (
            path.segments.length < segments.length ||
            segments.some((segment, i) => segment !== path.segments[i])
          )
            return
          callback({
            path: { rootId: path.rootId, segments: path.segments.slice(segments.length) },
            kind: 'Added',
          })
        })
      })(),
      unsubscribe() {
        running = false
        self.off('file/event', callback)
      },
    }
  }

  /**
   * Increment the reference count of this {@link LanguageServer}, preventing it from being disposed
   * until it is {@link LanguageServer['release']}d.
   */
  retain() {
    if (this.isDisposed) {
      throw new Error('Trying to retain already disposed Language Server.')
    }
    this.retainCount += 1
  }

  /** Disable automatic reconnection of the underlying network transport. */
  stopReconnecting() {
    this.shouldReconnect = false
  }

  /**
   * Decrement the reference count of this {@link LanguageServer},
   * disposing it if there are no longer any references to this {@link LanguageServer}.
   */
  release() {
    if (this.retainCount > 0) {
      this.retainCount -= 1
      // Equivalent to `this.isDisposed`, but written out explicitly here to avoid confusion,
      // since technically here it is not yet disposed.
      if (this.retainCount === 0) {
        this.dispose()
      }
    } else {
      throw new Error('Released already disposed Language Server.')
    }
  }

  /** Like `release()`, but unconditionally disposes the AbortScope. */
  private dispose() {
    this.retainCount = 0
    this.clientScope.dispose('Language Server released')
  }
}

/** Compute the SHA3 checksum of the given text. */
export function computeTextChecksum(text: string): Checksum {
  return bytesToHex(SHA3.create().update(text).digest()) as Checksum
}
