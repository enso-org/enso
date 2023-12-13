import { Client, RequestManager, WebSocketTransport } from '@open-rpc/client-js'
import { simpleDiffString } from 'lib0/diff'
import * as map from 'lib0/map'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import * as Y from 'yjs'
import { LanguageServer, computeTextChecksum } from '../shared/languageServer'
import { Checksum, Path } from '../shared/languageServerTypes'
import { exponentialBackoff, onBeforeRetry } from '../shared/retry'
import {
  DistributedProject,
  ExprId,
  IdMap,
  ModuleDoc,
  type NodeMetadata,
  type Uuid,
} from '../shared/yjsModel'
import {
  applyDocumentUpdates,
  preParseContent,
  prettyPrintDiff,
  translateVisualizationFromFile,
} from './edits'
import * as fileFormat from './fileFormat'
import { WSSharedDoc } from './ydoc'

const SOURCE_DIR = 'src'
const EXTENSION = '.enso'

const DEBUG_LOG_SYNC = false

function createOpenRPCClient(url: string) {
  const transport = new WebSocketTransport(url)
  const requestManager = new RequestManager([transport])
  transport.connection.on('error', (error) =>
    console.error('Language Server transport error:', error),
  )
  return new Client(requestManager)
}

export class LanguageServerSession {
  clientId: Uuid
  indexDoc: WSSharedDoc
  docs: Map<string, WSSharedDoc>
  retainCount: number
  url: string
  client!: Client
  ls!: LanguageServer
  model: DistributedProject
  projectRootId: Uuid | null
  authoritativeModules: Map<string, ModulePersistence>

  constructor(url: string) {
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
    this.restartClient()
  }

  static sessions = new Map<string, LanguageServerSession>()
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
    // `this.client` WILL be undefined on the first call.
    this.client?.close()
    this.client = createOpenRPCClient(this.url)
    // `this.ls` WILL be undefined on the first call.
    this.ls?.destroy()
    this.ls = new LanguageServer(this.client)
    this.ls.on('file/event', async (event) => {
      if (DEBUG_LOG_SYNC) {
        console.log('file/event', event)
      }
      try {
        switch (event.kind) {
          case 'Added': {
            if (isSourceFile(event.path)) {
              const fileInfo = await this.ls.fileInfo(event.path)
              if (fileInfo.attributes.kind.type == 'File') {
                await exponentialBackoff(() => this.getModuleModel(event.path).open(), {
                  onBeforeRetry: onBeforeRetry('Could not open new file'),
                  onSuccess(retryCount) {
                    if (retryCount === 0) return
                    console.info(
                      `Successfully opened new file '${event.path.segments.join(
                        '/',
                      )}' after ${retryCount} failures.`,
                    )
                  },
                })
              }
            }
            break
          }
          case 'Modified': {
            await exponentialBackoff(
              async () => this.tryGetExistingModuleModel(event.path)?.reload(),
              {
                onBeforeRetry: onBeforeRetry('Could not reload file'),
                onSuccess(retryCount) {
                  if (retryCount === 0) return
                  console.info(
                    `Successfully reloaded file '${event.path.segments.join(
                      '/',
                    )}' after ${retryCount} failures.`,
                  )
                },
              },
            )
            break
          }
        }
      } catch {
        this.restartClient()
      }
    })
    this.ls.on('text/fileModifiedOnDisk', async (event) => {
      try {
        await exponentialBackoff(async () => this.tryGetExistingModuleModel(event.path)?.reload(), {
          onBeforeRetry: onBeforeRetry('Could not reload file'),
          onSuccess(retryCount) {
            if (retryCount === 0) return
            console.info(
              `Successfully reloaded file '${event.path.segments.join(
                '/',
              )}' after ${retryCount} failures.`,
            )
          },
        })
      } catch {
        this.restartClient()
      }
    })
    exponentialBackoff(() => this.readInitialState(), {
      onBeforeRetry: onBeforeRetry('Could not read initial state'),
      onSuccess(retryCount) {
        if (retryCount === 0) return
        console.info(`Successfully read initial state after ${retryCount} failures.`)
      },
    }).catch((error) => {
      console.error('Could not read initial state.')
      console.error(error)
      exponentialBackoff(async () => this.restartClient(), {
        onBeforeRetry: onBeforeRetry('Could not restart RPC client'),
        onSuccess(retryCount) {
          if (retryCount === 0) return
          console.info(`Successfully restarted RPC client after ${retryCount} failures.`)
        },
      })
    })
  }

  private assertProjectRoot(): asserts this is { projectRootId: Uuid } {
    if (this.projectRootId == null) throw new Error('Missing project root')
  }

  private async readInitialState() {
    try {
      const connection = await this.ls.initProtocolConnection(this.clientId)
      const projectRoot = connection.contentRoots.find((root) => root.type === 'Project')
      if (!projectRoot) throw new Error('Missing project root')
      this.projectRootId = projectRoot.id
      await this.ls.acquireReceivesTreeUpdates({ rootId: this.projectRootId, segments: [] })
      const files = await this.scanSourceFiles()
      const moduleOpenPromises = this.indexDoc.doc.transact(
        () =>
          files.map((file) => this.getModuleModel(pushPathSegment(file.path, file.name)).open()),
        this,
      )
      await Promise.all(moduleOpenPromises)
    } catch (error) {
      console.error('LS initialization failed.')
      throw error
    }
    console.log('LS connection initialized.')
  }

  async scanSourceFiles() {
    this.assertProjectRoot()
    const sourceDir: Path = { rootId: this.projectRootId, segments: [SOURCE_DIR] }
    const srcModules = await this.ls.listFiles(sourceDir)
    return srcModules.paths.filter((file) => file.type === 'File' && file.name.endsWith(EXTENSION))
  }

  tryGetExistingModuleModel(path: Path): ModulePersistence | undefined {
    const name = pathToModuleName(path)
    return this.authoritativeModules.get(name)
  }

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

  retain() {
    this.retainCount += 1
  }

  async release(): Promise<void> {
    this.retainCount -= 1
    if (this.retainCount !== 0) return
    const modules = this.authoritativeModules.values()
    const moduleDisposePromises = Array.from(modules, (mod) => mod.dispose())
    this.authoritativeModules.clear()
    this.model.doc.destroy()
    this.ls.dispose()
    LanguageServerSession.sessions.delete(this.url)
    await Promise.all(moduleDisposePromises)
  }

  getYDoc(guid: string): WSSharedDoc | undefined {
    return this.docs.get(guid)
  }
}

function isSourceFile(path: Path): boolean {
  return (
    path.segments[0] === SOURCE_DIR && path.segments[path.segments.length - 1].endsWith(EXTENSION)
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
  state: LsSyncState = LsSyncState.Closed
  lastAction = Promise.resolve()
  updateToApply: Uint8Array | null = null
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
      if (DEBUG_LOG_SYNC) {
        console.debug('State change:', LsSyncState[this.state], '->', LsSyncState[state])
      }
      this.state = state
      if (state === LsSyncState.Synchronized) {
        this.trySyncRemoveUpdates()
      }
    } else {
      throw new Error('LsSync disposed')
    }
  }

  /** Set the current state to the given state while the callback is running.
   * Set the current state back to {@link LsSyncState.Synchronized} when the callback finishes. */
  private async withState(state: LsSyncState, callback: () => void | Promise<void>) {
    this.setState(state)
    await callback()
    this.setState(LsSyncState.Synchronized)
  }

  async open() {
    this.queuedAction = LsAction.Open
    switch (this.state) {
      case LsSyncState.Disposed:
      case LsSyncState.WritingFile:
      case LsSyncState.Synchronized:
      case LsSyncState.WriteError:
      case LsSyncState.Reloading: {
        return
      }
      case LsSyncState.Closing: {
        await this.lastAction
        if (this.queuedAction === LsAction.Open) await this.open()
        return
      }
      case LsSyncState.Opening: {
        await this.lastAction
        return
      }
      case LsSyncState.Closed: {
        await this.withState(LsSyncState.Opening, async () => {
          const promise = this.ls.openTextFile(this.path)
          this.lastAction = promise.then()
          const result = await promise
          if (!result.writeCapability) {
            console.error('Could not acquire write capability for module:', this.path)
            throw new Error(
              `Could not acquire write capability for module '${this.path.segments.join('/')}'`,
            )
          }
          this.syncFileContents(result.content, result.currentVersion)
        })
        return
      }
      default: {
        this.state satisfies never
        return
      }
    }
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

    let contentDelta: Y.YTextEvent['delta'] | null = null
    let idMapKeys: Y.YMapEvent<Uint8Array>['keys'] | null = null
    let metadataKeys: Y.YMapEvent<NodeMetadata>['keys'] | null = null
    const observeContent = (event: Y.YTextEvent) => (contentDelta = event.delta)
    const observeIdMap = (event: Y.YMapEvent<Uint8Array>) => (idMapKeys = event.keys)
    const observeMetadata = (event: Y.YMapEvent<NodeMetadata>) => (metadataKeys = event.keys)

    this.doc.contents.observe(observeContent)
    this.doc.idMap.observe(observeIdMap)
    this.doc.metadata.observe(observeMetadata)
    Y.applyUpdate(this.doc.ydoc, update, 'remote')
    this.doc.contents.unobserve(observeContent)
    this.doc.idMap.unobserve(observeIdMap)
    this.doc.metadata.unobserve(observeMetadata)
    this.writeSyncedEvents(contentDelta, idMapKeys, metadataKeys)
  }

  private writeSyncedEvents(
    contentDelta: Y.YTextEvent['delta'] | null,
    idMapKeys: Y.YMapEvent<Uint8Array>['keys'] | null,
    metadataKeys: Y.YMapEvent<NodeMetadata>['keys'] | null,
  ) {
    if (this.syncedContent == null || this.syncedVersion == null) return
    if (contentDelta == null && idMapKeys == null && metadataKeys == null) return

    const { edits, newContent, newMetadata } = applyDocumentUpdates(
      this.doc,
      this.syncedMeta,
      this.syncedContent,
      contentDelta,
      idMapKeys,
      metadataKeys,
    )

    const newVersion = computeTextChecksum(newContent)

    if (DEBUG_LOG_SYNC) {
      console.debug(' === changes === ')
      console.debug('number of edits:', edits.length)
      console.debug('metadata:', metadataKeys)
      console.debug('content:', contentDelta)
      console.debug('idMap:', idMapKeys)
      if (edits.length > 0) {
        console.debug('version:', this.syncedVersion, '->', newVersion)
        console.debug('Content diff:')
        console.debug(prettyPrintDiff(this.syncedContent, newContent))
      }
      console.debug(' =============== ')
    }
    if (edits.length === 0) {
      if (newVersion !== this.syncedVersion) {
        console.error('Version mismatch:', this.syncedVersion, '->', newVersion)
      }
      return
    }

    this.setState(LsSyncState.WritingFile)

    const execute = contentDelta != null || idMapKeys != null
    const apply = this.ls.applyEdit(
      {
        path: this.path,
        edits,
        oldVersion: this.syncedVersion,
        newVersion,
      },
      execute,
    )
    return (this.lastAction = apply.then(
      () => {
        this.syncedContent = newContent
        this.syncedVersion = newVersion
        this.syncedMeta = newMetadata
        this.setState(LsSyncState.Synchronized)
      },
      (error) => {
        console.error('Could not apply edit:', error)

        // Try to recover by reloading the file. Drop the attempted updates, since applying them
        // have failed.
        this.setState(LsSyncState.WriteError)
        this.syncedContent = null
        this.syncedVersion = null
        return this.reload()
      },
    ))
  }

  private syncFileContents(content: string, version: Checksum) {
    this.doc.ydoc.transact(() => {
      const { code, idMapJson, metadataJson } = preParseContent(content)
      const idMapMeta = fileFormat.tryParseIdMapOrFallback(idMapJson)
      const metadata = fileFormat.tryParseMetadataOrFallback(metadataJson)
      const nodeMeta = metadata.ide.node

      const idMap = new IdMap(this.doc.idMap, this.doc.contents)
      for (const [{ index, size }, id] of idMapMeta) {
        const start = index.value
        const end = index.value + size.value
        const range: [number, number] = [start, end]
        if (typeof start !== 'number' || typeof end !== 'number') {
          console.error(`Invalid range for id ${id}:`, range)
          continue
        }
        idMap.insertKnownId(range, id as ExprId)
      }

      const keysToDelete = new Set(this.doc.metadata.keys())
      for (const [id, meta] of Object.entries(nodeMeta)) {
        if (typeof id !== 'string') continue
        const formattedMeta: NodeMetadata = {
          x: meta.position.vector[0],
          y: meta.position.vector[1],
          vis: (meta.visualization && translateVisualizationFromFile(meta.visualization)) ?? null,
        }
        keysToDelete.delete(id)
        this.doc.metadata.set(id, formattedMeta)
      }
      for (const id of keysToDelete) {
        this.doc.metadata.delete(id)
      }
      this.syncedContent = content
      this.syncedVersion = version
      this.syncedMeta = metadata

      const codeDiff = simpleDiffString(this.doc.contents.toString(), code)
      this.doc.contents.delete(codeDiff.index, codeDiff.remove)
      this.doc.contents.insert(codeDiff.index, codeDiff.insert)
      idMap.finishAndSynchronize()
    }, 'file')
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
      case LsSyncState.WriteError:
      case LsSyncState.Synchronized: {
        this.setState(LsSyncState.Closing)
        const closing = (this.lastAction = this.ls.closeTextFile(this.path))
        await closing
        this.setState(LsSyncState.Closed)
        return
      }
      default: {
        this.state satisfies never
        return
      }
    }
  }

  async reload() {
    this.queuedAction = LsAction.Reload
    switch (this.state) {
      case LsSyncState.Opening:
      case LsSyncState.Disposed:
      case LsSyncState.Closed:
      case LsSyncState.Closing: {
        return
      }
      case LsSyncState.Reloading: {
        await this.lastAction
        return
      }
      case LsSyncState.WritingFile: {
        await this.lastAction
        if (this.queuedAction === LsAction.Reload) await this.reload()
        return
      }
      case LsSyncState.Synchronized: {
        this.withState(LsSyncState.Reloading, async () => {
          const promise = Promise.all([
            this.ls.readFile(this.path),
            this.ls.fileChecksum(this.path),
          ])
          this.lastAction = promise.then()
          const [contents, checksum] = await promise
          this.syncFileContents(contents.contents, checksum.checksum)
        })
        return
      }
      case LsSyncState.WriteError: {
        this.withState(LsSyncState.Reloading, async () => {
          const reloading = this.ls
            .closeTextFile(this.path)
            .catch((error) => {
              console.error('Could not close file after write error:', error)
            })
            .then(
              () =>
                exponentialBackoff(
                  async () => {
                    const result = await this.ls.openTextFile(this.path)
                    if (!result.writeCapability) {
                      const message = `Could not acquire write capability for module '${this.path.segments.join(
                        '/',
                      )}'`
                      console.error(message)
                      throw new Error(message)
                    }
                    return result
                  },
                  {
                    onBeforeRetry: onBeforeRetry('Could not open file for writing'),
                    onSuccess(retryCount) {
                      if (retryCount === 0) return
                      console.info(
                        `Successfully opened file for writing after ${retryCount} failures.`,
                      )
                    },
                  },
                ),
              (error) => {
                console.error('Could not reopen file after write error.')
                console.error(error)
                // This error is unrecoverable.
                throw error
              },
            )
          this.lastAction = reloading.then()
          const result = await reloading
          this.syncFileContents(result.content, result.currentVersion)
        })
        return
      }
      default: {
        this.state satisfies never
        return
      }
    }
  }

  async dispose(): Promise<void> {
    this.cleanup()
    const alreadyClosed = this.inState(LsSyncState.Closing, LsSyncState.Closed)
    this.setState(LsSyncState.Disposed)
    if (alreadyClosed) return Promise.resolve()
    return this.ls.closeTextFile(this.path)
  }
}
