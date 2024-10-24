import createDebug from 'debug'
import { Base64 } from 'js-base64'
import * as json from 'lib0/json'
import * as map from 'lib0/map'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import * as Ast from 'ydoc-shared/ast'
import { astCount } from 'ydoc-shared/ast'
import { EnsoFileParts, combineFileParts, splitFileContents } from 'ydoc-shared/ensoFile'
import { LanguageServer, computeTextChecksum } from 'ydoc-shared/languageServer'
import {
  Checksum,
  FileEdit,
  FileEventKind,
  Path,
  TextEdit,
  response,
} from 'ydoc-shared/languageServerTypes'
import { assertNever } from 'ydoc-shared/util/assert'
import { Err, Ok, Result, withContext } from 'ydoc-shared/util/data/result'
import {
  AbortScope,
  ReconnectingWebSocketTransport,
  exponentialBackoff,
  printingCallbacks,
} from 'ydoc-shared/util/net'
import {
  DistributedProject,
  ExternalId,
  IdMap,
  ModuleDoc,
  visMetadataEquals,
  type Uuid,
} from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'
import {
  applyDiffAsTextEdits,
  applyDocumentUpdates,
  prettyPrintDiff,
  translateVisualizationFromFile,
} from './edits'
import * as fileFormat from './fileFormat'
import { deserializeIdMap, idMapToArray, serializeIdMap } from './serialization'
import { WSSharedDoc } from './ydoc'

const SOURCE_DIR = 'src'
const EXTENSION = '.enso'

const debugLog = createDebug('ydoc-server:session')

/** TODO: Add docs */
export class LanguageServerSession {
  clientId: Uuid
  indexDoc: WSSharedDoc
  docs: Map<string, WSSharedDoc>
  retainCount: number
  url: string
  ls: LanguageServer
  connection: response.InitProtocolConnection | undefined
  model: DistributedProject
  projectRootId: Uuid | null
  authoritativeModules: Map<string, ModulePersistence>
  clientScope: AbortScope

  static DEBUG = false

  /** Create a {@link LanguageServerSession}. */
  constructor(url: string) {
    this.clientScope = new AbortScope()
    this.clientId = random.uuidv4() as Uuid
    this.docs = new Map()
    this.retainCount = 0
    this.url = url
    console.log('new session with', url)
    this.indexDoc = new WSSharedDoc()
    this.docs.set('index', this.indexDoc)
    this.model = new DistributedProject(this.indexDoc.doc)
    this.projectRootId = null
    this.authoritativeModules = new Map()

    this.indexDoc.doc.on('subdocs', (subdocs: { loaded: Set<Y.Doc> }) => {
      for (const doc of subdocs.loaded) {
        const name = this.model.findModuleByDocId(doc.guid)
        if (!name) continue
        const persistence = this.authoritativeModules.get(name)
        if (!persistence) continue
      }
    })
    this.ls = new LanguageServer(this.clientId, new ReconnectingWebSocketTransport(this.url))
    this.clientScope.onAbort(() => this.ls.release())
    this.setupClient()
  }

  static sessions = new Map<string, LanguageServerSession>()

  /** Get a {@link LanguageServerSession} by its URL. */
  static get(url: string): LanguageServerSession {
    const session = map.setIfUndefined(
      LanguageServerSession.sessions,
      url,
      () => new LanguageServerSession(url),
    )
    session.retain()
    return session
  }

  private restartClient() {
    this.ls.reconnect()
    return exponentialBackoff(() => this.readInitialState())
  }

  private setupClient() {
    this.ls.on('file/event', async event => {
      debugLog('file/event %O', event)
      const result = await this.handleFileEvent(event)
      if (!result.ok) this.restartClient()
    })
    const fileModified = async (event: { path: Path }) => {
      const path = event.path.segments.join('/')
      const result = await exponentialBackoff(
        async () => this.tryGetExistingModuleModel(event.path)?.reload() ?? Ok(),
        printingCallbacks(`reloaded file '${path}'`, `reload file '${path}'`),
      )
      if (!result.ok) this.restartClient()
    }
    this.ls.on('text/fileModifiedOnDisk', fileModified)
    this.ls.on('text/didChange', event => {
      for (const edit of event.edits) {
        fileModified(edit)
      }
    })
    exponentialBackoff(
      () => this.readInitialState(),
      printingCallbacks('read initial state', 'read initial state'),
    ).then(result => {
      if (!result.ok) {
        result.error.log('Could not read initial state')
        exponentialBackoff(
          async () => this.restartClient(),
          printingCallbacks('restarted RPC client', 'restart RPC client'),
        )
      }
    })
  }

  private handleFileEvent(event: { path: Path; kind: FileEventKind }): Promise<Result<void>> {
    return withContext(
      () => 'Handling file/event',
      async () => {
        const path = event.path.segments.join('/')
        switch (event.kind) {
          case 'Added': {
            if (isSourceFile(event.path)) {
              const fileInfo = await this.ls.fileInfo(event.path)
              if (!fileInfo.ok) return fileInfo
              if (fileInfo.value.attributes.kind.type == 'File') {
                return await exponentialBackoff(
                  () => this.getModuleModel(event.path).open(),
                  printingCallbacks(`opened new file '${path}'`, `open new file '${path}'`),
                )
              }
            }
            break
          }
          case 'Modified': {
            return await exponentialBackoff(
              () => this.tryGetExistingModuleModel(event.path)?.reload() ?? Promise.resolve(Ok()),
              printingCallbacks(`reloaded file '${path}'`, `reload file '${path}'`),
            )
          }
        }
        return Ok()
      },
    )
  }

  private assertProjectRoot(): asserts this is { projectRootId: Uuid } {
    if (this.projectRootId == null) throw new Error('Missing project root')
  }

  private async readInitialState(): Promise<Result<void>> {
    return await withContext(
      () => 'When reading initial state',
      async () => {
        let moduleOpenPromises: Promise<Result<void>>[] = []
        const projectRoot = (await this.ls.contentRoots).find(root => root.type === 'Project')
        if (!projectRoot) return Err('Missing project root')
        this.projectRootId = projectRoot.id
        const aquireResult = await this.ls.acquireReceivesTreeUpdates({
          rootId: this.projectRootId,
          segments: [],
        })
        if (!aquireResult.ok) return aquireResult
        const files = await this.scanSourceFiles()
        if (!files.ok) return files
        moduleOpenPromises = this.indexDoc.doc.transact(
          () =>
            files.value.map(file =>
              this.getModuleModel(pushPathSegment(file.path, file.name)).open(),
            ),
          this,
        )
        const results = await Promise.all(moduleOpenPromises)
        return results.find(res => !res.ok) ?? Ok()
      },
    )
  }

  /** TODO: Add docs */
  async scanSourceFiles() {
    this.assertProjectRoot()
    const sourceDir: Path = { rootId: this.projectRootId, segments: [SOURCE_DIR] }
    const srcModules = await this.ls.listFiles(sourceDir)
    if (!srcModules.ok) return srcModules
    return Ok(
      srcModules.value.paths.filter(file => file.type === 'File' && file.name.endsWith(EXTENSION)),
    )
  }

  /** TODO: Add docs */
  tryGetExistingModuleModel(path: Path): ModulePersistence | undefined {
    const name = pathToModuleName(path)
    return this.authoritativeModules.get(name)
  }

  /** TODO: Add docs */
  getModuleModel(path: Path): ModulePersistence {
    const name = pathToModuleName(path)
    return map.setIfUndefined(this.authoritativeModules, name, () => {
      const wsDoc = new WSSharedDoc()
      this.docs.set(wsDoc.doc.guid, wsDoc)
      this.model.createUnloadedModule(name, wsDoc.doc)
      const mod = new ModulePersistence(this.ls, path, wsDoc.doc)
      mod.once('removed', () => {
        const index = this.model.findModuleByDocId(wsDoc.doc.guid)
        this.docs.delete(wsDoc.doc.guid)
        this.authoritativeModules.delete(name)
        if (index != null) this.model.deleteModule(index)
      })
      return mod
    })
  }

  /** TODO: Add docs */
  retain() {
    this.retainCount += 1
  }

  /** TODO: Add docs */
  async release(): Promise<void> {
    this.retainCount -= 1
    if (this.retainCount !== 0) return
    const modules = this.authoritativeModules.values()
    const moduleDisposePromises = Array.from(modules, mod => mod.dispose())
    this.authoritativeModules.clear()
    this.model.doc.destroy()
    this.clientScope.dispose('LangueServerSession disposed.')
    LanguageServerSession.sessions.delete(this.url)
    await Promise.all(moduleDisposePromises)
  }

  /** Get a YDoc by its id. */
  getYDoc(guid: string): WSSharedDoc | undefined {
    return this.docs.get(guid)
  }
}

function isSourceFile(path: Path): boolean {
  return (
    (path.segments[0] === SOURCE_DIR &&
      path.segments[path.segments.length - 1]?.endsWith(EXTENSION)) ??
    false
  )
}

function pathToModuleName(path: Path): string {
  if (path.segments[0] === SOURCE_DIR) return path.segments.slice(1).join('/')
  else return '//' + path.segments.join('/')
}

function pushPathSegment(path: Path, segment: string): Path {
  return { rootId: path.rootId, segments: [...path.segments, segment] }
}

enum LsSyncState {
  Closed,
  Opening,
  Synchronized,
  WritingFile,
  WriteError,
  CapabilityError,
  Reloading,
  Closing,
  Disposed,
}

enum LsAction {
  Open,
  Close,
  Reload,
}

class ModulePersistence extends ObservableV2<{ removed: () => void }> {
  ls: LanguageServer
  path: Path
  doc: ModuleDoc = new ModuleDoc(new Y.Doc())
  readonly state: LsSyncState = LsSyncState.Closed
  readonly lastAction = Promise.resolve()
  updateToApply: Uint8Array | null = null
  syncedCode: string | null = null
  syncedIdMap: string | null = null
  syncedMetaJson: string | null = null
  syncedContent: string | null = null
  syncedVersion: Checksum | null = null
  syncedMeta: fileFormat.Metadata = fileFormat.tryParseMetadataOrFallback(null)
  queuedAction: LsAction | null = null
  cleanup = () => {}

  constructor(ls: LanguageServer, path: Path, sharedDoc: Y.Doc) {
    super()
    this.ls = ls
    this.path = path

    const onRemoteUpdate = this.queueRemoteUpdate.bind(this)
    const onLocalUpdate = (update: Uint8Array, origin: unknown) => {
      if (origin === 'file') Y.applyUpdate(sharedDoc, update, this)
    }
    const onFileModified = this.handleFileModified.bind(this)
    const onFileRemoved = this.handleFileRemoved.bind(this)
    this.doc.ydoc.on('update', onLocalUpdate)
    sharedDoc.on('update', onRemoteUpdate)
    this.ls.on('text/fileModifiedOnDisk', onFileModified)
    this.ls.on('file/rootRemoved', onFileRemoved)
    this.cleanup = () => {
      this.doc.ydoc.off('update', onLocalUpdate)
      sharedDoc.off('update', onRemoteUpdate)
      this.ls.off('text/fileModifiedOnDisk', onFileModified)
      this.ls.off('file/rootRemoved', onFileRemoved)
    }
  }

  private inState(...states: LsSyncState[]): boolean {
    return states.includes(this.state)
  }

  private setState(state: LsSyncState) {
    if (this.state !== LsSyncState.Disposed) {
      debugLog('State change: %o -> %o', LsSyncState[this.state], LsSyncState[state])
      // @ts-expect-error This is SAFE. `this.state` is only `readonly` to ensure that
      // this is the only place where it is mutated.
      this.state = state
      if (state === LsSyncState.Synchronized) this.trySyncRemoveUpdates()
    } else {
      throw new Error('LsSync disposed')
    }
  }

  private setLastAction<T>(lastAction: Promise<T>) {
    // @ts-expect-error This is SAFE. `this.lastAction` is only `readonly` to ensure that
    // this is the only place where it is mutated.
    this.lastAction = lastAction.then(
      () => {},
      () => {},
    )
    return lastAction
  }

  /**
   * Set the current state to the given state while the callback is running.
   * Set the current state back to {@link LsSyncState.Synchronized} when the callback finishes.
   */
  private async withState(state: LsSyncState, callback: () => void | Promise<void>): Promise<void>
  private async withState(
    state: LsSyncState,
    callback: () => Result<void> | Promise<Result<void>>,
  ): Promise<Result<void>>
  private async withState(
    state: LsSyncState,
    callback: () => void | Promise<void> | Result<void> | Promise<Result<void>>,
  ): Promise<Result<void> | void> {
    this.setState(state)
    const result = await callback()
    if (result && !result.ok) return result
    this.setState(LsSyncState.Synchronized)
    if (result) return result
  }

  async open(): Promise<Result<void>> {
    return await withContext(
      () => `When opening module ${this.path}`,
      async () => {
        this.queuedAction = LsAction.Open
        switch (this.state) {
          case LsSyncState.Disposed:
          case LsSyncState.WritingFile:
          case LsSyncState.Synchronized:
          case LsSyncState.WriteError:
          case LsSyncState.Reloading: {
            return Ok()
          }
          case LsSyncState.Closing: {
            await this.lastAction
            if (this.queuedAction === LsAction.Open) return await this.open()
            return Ok()
          }
          case LsSyncState.Opening: {
            await this.lastAction
            return Ok()
          }
          case LsSyncState.Closed: {
            return await this.withState(LsSyncState.Opening, async () => {
              const promise = this.ls.openTextFile(this.path)
              this.setLastAction(promise.then(res => !res.ok && this.setState(LsSyncState.Closed)))
              const result = await promise
              if (!result.ok) return result
              if (!result.value.writeCapability) {
                this.setState(LsSyncState.CapabilityError)
                return Err(
                  `Could not acquire write capability for module '${this.path.segments.join('/')}'`,
                )
              }
              this.syncFileContents(result.value.content, result.value.currentVersion)
              return Ok()
            })
          }
          case LsSyncState.CapabilityError:
            return this.reload()
          default: {
            assertNever(this.state)
          }
        }
      },
    )
  }

  handleFileRemoved() {
    if (this.inState(LsSyncState.Closed)) return
    this.close()
  }

  handleFileModified() {
    if (this.inState(LsSyncState.Closed)) return
  }

  queueRemoteUpdate(update: Uint8Array, origin: unknown) {
    if (origin === this) return
    if (this.updateToApply != null) {
      this.updateToApply = Y.mergeUpdates([this.updateToApply, update])
    } else {
      this.updateToApply = update
    }
    this.trySyncRemoveUpdates()
  }

  trySyncRemoveUpdates() {
    if (this.updateToApply == null) return
    // apply updates to the ls-representation doc if we are already in sync with the LS.
    if (!this.inState(LsSyncState.Synchronized)) return
    const update = this.updateToApply
    this.updateToApply = null

    const syncModule = new Ast.MutableModule(this.doc.ydoc)
    const moduleUpdate = syncModule.applyUpdate(update, 'remote')
    if (moduleUpdate && this.syncedContent) {
      const synced = splitFileContents(this.syncedContent)
      const { newCode, newIdMap, newMetadata } = applyDocumentUpdates(
        this.doc,
        synced,
        moduleUpdate,
      )
      this.sendLsUpdate(synced, newCode, newIdMap, newMetadata)
    }
  }

  private static getIdMapToPersist(
    idMap: IdMap | undefined,
    metadata: fileFormat.IdeMetadata['node'],
  ): IdMap | undefined {
    if (idMap === undefined) {
      return
    } else {
      const entriesIntersection = idMap.entries().filter(([, id]) => id in metadata)
      return new IdMap(entriesIntersection)
    }
  }

  private static encodeCodeSnapshot(code: string): string {
    return Base64.encode(code)
  }

  private static decodeCodeSnapshot(snapshot: string): string {
    return Base64.decode(snapshot)
  }

  private sendLsUpdate(
    synced: EnsoFileParts,
    newCode: string | undefined,
    newIdMap: IdMap | undefined,
    newMetadata: fileFormat.IdeMetadata['node'] | undefined,
  ) {
    if (this.syncedContent == null || this.syncedVersion == null) return

    const newSnapshot = newCode && {
      snapshot: ModulePersistence.encodeCodeSnapshot(newCode),
    }
    const newMetadataJson =
      newMetadata &&
      json.stringify({
        ...this.syncedMeta,
        ide: {
          ...this.syncedMeta.ide,
          ...newSnapshot,
          node: newMetadata,
        },
      })
    const idMapToPersist =
      (newIdMap || newMetadata) &&
      ModulePersistence.getIdMapToPersist(newIdMap, newMetadata ?? this.syncedMeta.ide.node)
    const newIdMapToPersistJson = idMapToPersist && serializeIdMap(idMapToPersist)
    const code = newCode ?? synced.code
    const newContent = combineFileParts({
      code,
      idMapJson: newIdMapToPersistJson ?? synced.idMapJson ?? '[]',
      metadataJson: newMetadataJson ?? synced.metadataJson ?? '{}',
    })

    const edits: TextEdit[] = []
    if (newCode) edits.push(...applyDiffAsTextEdits(0, synced.code, newCode))
    if (newIdMap || newMetadata || newSnapshot) {
      const oldMetaContent = this.syncedContent.slice(synced.code.length)
      const metaContent = newContent.slice(code.length)
      const metaStartLine = (code.match(/\n/g) ?? []).length
      edits.push(...applyDiffAsTextEdits(metaStartLine, oldMetaContent, metaContent))
    }

    const newVersion = computeTextChecksum(newContent)

    if (debugLog.enabled) {
      debugLog(' === changes === ')
      debugLog('number of edits: %d', edits.length)
      if (edits.length > 0) {
        debugLog('version: %s -> %s', this.syncedVersion, newVersion)
        debugLog('Content diff:\n%s', prettyPrintDiff(this.syncedContent, newContent))
      }
      debugLog(' =============== ')
    }

    this.setState(LsSyncState.WritingFile)

    const execute = newCode != null || newIdMap != null
    const edit: FileEdit = { path: this.path, edits, oldVersion: this.syncedVersion, newVersion }
    const apply = this.ls.applyEdit(edit, execute, newIdMap && idMapToArray(newIdMap))
    const handleError = (error: unknown) => {
      console.error('Could not apply edit:', error)
      // Try to recover by reloading the file.
      // Drop the attempted updates, since applying them have failed.
      this.setState(LsSyncState.WriteError)
      this.syncedContent = null
      this.syncedVersion = null
      this.syncedCode = null
      this.syncedIdMap = null
      this.syncedMetaJson = null
      return this.reload()
    }
    const promise = apply.then(result => {
      if (!result.ok) return handleError(result.error)
      this.syncedContent = newContent
      this.syncedVersion = newVersion
      if (newMetadata) this.syncedMeta.ide.node = newMetadata
      if (newCode) this.syncedCode = newCode
      if (newIdMapToPersistJson) this.syncedIdMap = newIdMapToPersistJson
      if (newMetadataJson) this.syncedMetaJson = newMetadataJson
      this.setState(LsSyncState.Synchronized)
    }, handleError)
    this.setLastAction(promise)
    return promise
  }

  private syncFileContents(content: string, version: Checksum) {
    const contentsReceived = splitFileContents(content)
    let unsyncedIdMap: IdMap | undefined
    this.doc.ydoc.transact(() => {
      const { code, idMapJson, metadataJson } = contentsReceived
      const metadata = fileFormat.tryParseMetadataOrFallback(metadataJson)
      const nodeMeta = Object.entries(metadata.ide.node)

      let parsedSpans
      let parsedIdMap
      const syncModule = new Ast.MutableModule(this.doc.ydoc)
      if (code !== this.syncedCode) {
        const syncRoot = syncModule.root()
        if (syncRoot) {
          const edit = syncModule.edit()
          edit.getVersion(syncRoot).syncToCode(code)
          const editedRoot = edit.root()
          if (editedRoot instanceof Ast.BodyBlock) Ast.repair(editedRoot, edit)
          syncModule.applyEdit(edit)
        } else {
          const metadataSnapshot = metadata.ide.snapshot
          const snapshotCode =
            metadataSnapshot && ModulePersistence.decodeCodeSnapshot(metadataSnapshot)
          if (metadataSnapshot && idMapJson && snapshotCode && snapshotCode !== code) {
            // When the received code does not match the saved code snapshot, it means that
            // the code was externally edited. In this case we try to fix the spans by running
            // the `syncToCode` on the saved code snapshot.
            const { root, spans } = Ast.parseModuleWithSpans(snapshotCode, syncModule)
            syncModule.syncRoot(root)
            parsedIdMap = deserializeIdMap(idMapJson)

            const edit = syncModule.edit()
            Ast.setExternalIds(edit, spans, parsedIdMap)
            edit.getVersion(root).syncToCode(code)
            syncModule.applyEdit(edit)
          } else {
            const { root, spans } = Ast.parseModuleWithSpans(code, syncModule)
            syncModule.syncRoot(root)
            parsedSpans = spans
          }
        }
      }
      const astRoot = syncModule.root()
      if (!astRoot) return
      if ((code !== this.syncedCode || idMapJson !== this.syncedIdMap) && idMapJson) {
        const spans = parsedSpans ?? Ast.print(astRoot).info
        if (idMapJson !== this.syncedIdMap && parsedIdMap === undefined) {
          const idMap = deserializeIdMap(idMapJson)
          const idsAssigned = Ast.setExternalIds(syncModule, spans, idMap)
          const numberOfAsts = astCount(astRoot)
          const idsNotSetByMap = numberOfAsts - idsAssigned
          if (idsNotSetByMap > 0) {
            if (code !== this.syncedCode) {
              unsyncedIdMap = Ast.spanMapToIdMap(spans)
            } else {
              console.warn(
                `The LS sent an IdMap-only edit that is missing ${idsNotSetByMap} of our expected ASTs.`,
              )
            }
          }
        } else {
          // If only code was externally changed, treat it as a text edit: the old idmap should
          // be replaced with updated AST.
          unsyncedIdMap = Ast.spanMapToIdMap(spans)
        }
      }
      if (
        (code !== this.syncedCode ||
          idMapJson !== this.syncedIdMap ||
          metadataJson !== this.syncedMetaJson) &&
        nodeMeta.length !== 0
      ) {
        const externalIdToAst = new Map<ExternalId, Ast.Ast>()
        astRoot.visitRecursiveAst(ast => {
          if (!externalIdToAst.has(ast.externalId)) externalIdToAst.set(ast.externalId, ast)
        })
        const missing = new Set<string>()
        for (const [id, meta] of nodeMeta) {
          if (typeof id !== 'string') continue
          const ast = externalIdToAst.get(id as ExternalId)
          if (!ast) {
            missing.add(id)
            continue
          }
          const metadata = syncModule.getVersion(ast).mutableNodeMetadata()
          const oldPos = metadata.get('position')
          const newPos = { x: meta.position.vector[0], y: -meta.position.vector[1] }
          if (oldPos?.x !== newPos.x || oldPos?.y !== newPos.y) metadata.set('position', newPos)
          const oldVis = metadata.get('visualization')
          const newVis = meta.visualization && translateVisualizationFromFile(meta.visualization)
          if (!visMetadataEquals(newVis, oldVis)) metadata.set('visualization', newVis)
          const oldColorOverride = metadata.get('colorOverride')
          const newColorOverride = meta.colorOverride
          if (oldColorOverride !== newColorOverride) metadata.set('colorOverride', newColorOverride)
        }
      }

      this.syncedCode = code
      this.syncedIdMap = unsyncedIdMap ? null : idMapJson
      this.syncedContent = content
      this.syncedVersion = version
      this.syncedMeta = metadata
      this.syncedMetaJson = metadataJson
    }, 'file')
    if (unsyncedIdMap)
      this.sendLsUpdate(
        contentsReceived,
        this.syncedCode ?? undefined,
        unsyncedIdMap,
        this.syncedMeta?.ide?.node,
      )
  }

  async close() {
    this.queuedAction = LsAction.Close
    switch (this.state) {
      case LsSyncState.Disposed:
      case LsSyncState.Closed: {
        return
      }
      case LsSyncState.Closing: {
        await this.lastAction
        return
      }
      case LsSyncState.Opening:
      case LsSyncState.WritingFile:
      case LsSyncState.Reloading: {
        await this.lastAction
        if (this.queuedAction === LsAction.Close) {
          await this.close()
        }
        return
      }
      case LsSyncState.CapabilityError:
      case LsSyncState.WriteError:
      case LsSyncState.Synchronized: {
        this.setState(LsSyncState.Closing)
        const promise = this.ls.closeTextFile(this.path)
        const state = this.state
        this.setLastAction(promise.catch(() => this.setState(state)))
        await promise
        this.setState(LsSyncState.Closed)
        return
      }
      default: {
        assertNever(this.state)
      }
    }
  }

  async reload(): Promise<Result<void>> {
    return await withContext(
      () => `When reloading module ${this.path}`,
      async () => {
        this.queuedAction = LsAction.Reload
        switch (this.state) {
          case LsSyncState.Opening:
          case LsSyncState.Disposed:
          case LsSyncState.Closed:
          case LsSyncState.Closing: {
            return Ok()
          }
          case LsSyncState.Reloading: {
            await this.lastAction
            return Ok()
          }
          case LsSyncState.WritingFile: {
            await this.lastAction
            if (this.queuedAction === LsAction.Reload) return await this.reload()
            return Ok()
          }
          case LsSyncState.Synchronized: {
            return this.withState(LsSyncState.Reloading, async () => {
              const promise = this.ls.readFile(this.path)
              this.setLastAction(promise)
              const contents = await promise
              if (!contents.ok) return contents
              const checksum = computeTextChecksum(contents.value.contents)
              this.syncFileContents(contents.value.contents, checksum)
              return Ok()
            })
          }
          case LsSyncState.CapabilityError:
          case LsSyncState.WriteError: {
            return this.withState(LsSyncState.Reloading, async () => {
              const path = this.path.segments.join('/')
              const reloading = this.ls.closeTextFile(this.path).then(async closing => {
                if (!closing.ok) closing.error.log('Could not close file after write error:')
                return exponentialBackoff(
                  async () => {
                    const result = await this.ls.openTextFile(this.path)
                    if (!result.ok) return result
                    if (!result.value.writeCapability) {
                      return Err(
                        `Could not acquire write capability for module '${this.path.segments.join(
                          '/',
                        )}'`,
                      )
                    }
                    return result
                  },
                  printingCallbacks(
                    `opened file '${path}' for writing`,
                    `open file '${path}' for writing`,
                  ),
                )
              })

              this.setLastAction(reloading)
              const result = await reloading
              if (!result.ok) return result
              this.syncFileContents(result.value.content, result.value.currentVersion)
              return Ok()
            })
          }
          default: {
            assertNever(this.state)
          }
        }
      },
    )
  }

  async dispose(): Promise<void> {
    this.cleanup()
    const alreadyClosed = this.inState(LsSyncState.Closing, LsSyncState.Closed)
    this.setState(LsSyncState.Disposed)
    if (alreadyClosed) return Promise.resolve()
    const closing = await this.ls.closeTextFile(this.path)
    if (!closing.ok) {
      closing.error.log(`Closing text file ${this.path}`)
    }
  }
}
