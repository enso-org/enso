import { Client, RequestManager, WebSocketTransport } from '@open-rpc/client-js'
import { simpleDiffString } from 'lib0/diff'
import * as map from 'lib0/map'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import * as Y from 'yjs'
import { LanguageServer, computeTextChecksum } from '../shared/languageServer'
import { Checksum, Path } from '../shared/languageServerTypes'
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

const sessions = new Map<string, LanguageServerSession>()

const DEBUG_LOG_SYNC = false

type Events = {
  error: (error: Error) => void
}

export class LanguageServerSession extends ObservableV2<Events> {
  clientId: Uuid
  indexDoc: WSSharedDoc
  docs: Map<string, WSSharedDoc>
  retainCount: number
  url: string
  client: Client
  ls: LanguageServer
  model: DistributedProject
  projectRootId: Uuid | null
  authoritativeModules: Map<string, ModulePersistence>

  constructor(url: string) {
    super()
    this.clientId = random.uuidv4() as Uuid
    this.docs = new Map()
    this.retainCount = 0
    this.url = url
    const transport = new WebSocketTransport(url)
    const requestManager = new RequestManager([transport])
    this.client = new Client(requestManager)
    console.log('new session with', url)
    transport.connection.on('error', (error) => this.emit('error', [error]))
    this.ls = new LanguageServer(this.client)
    this.indexDoc = new WSSharedDoc()
    this.docs.set('index', this.indexDoc)
    this.model = new DistributedProject(this.indexDoc.doc)
    this.projectRootId = null
    this.authoritativeModules = new Map()

    this.indexDoc.doc.on('subdocs', (subdocs: { loaded: Set<Y.Doc> }) => {
      for (const doc of subdocs.loaded) {
        const name = this.model.findModuleByDocId(doc.guid)
        if (name == null) continue
        const persistence = this.authoritativeModules.get(name)
        if (persistence == null) continue
      }
    })

    this.ls.on('file/event', async (event) => {
      if (DEBUG_LOG_SYNC) {
        console.log('file/event', event)
      }
      switch (event.kind) {
        case 'Added':
          if (isSourceFile(event.path)) {
            const fileInfo = await this.ls.fileInfo(event.path)
            if (fileInfo.attributes.kind.type == 'File') {
              this.getModuleModel(event.path).open()
            }
          }
          break
        case 'Modified':
          this.getModuleModelIfExists(event.path)?.reload()
          break
      }
    })

    this.ls.on('text/fileModifiedOnDisk', async (event) => {
      this.getModuleModelIfExists(event.path)?.reload()
    })

    this.readInitialState()
  }

  private assertProjectRoot(): asserts this is { projectRootId: Uuid } {
    if (this.projectRootId == null) throw new Error('Missing project root')
  }

  private async readInitialState() {
    try {
      const { contentRoots } = await this.ls.initProtocolConnection(this.clientId)
      const projectRoot = contentRoots.find((root) => root.type === 'Project') ?? null
      if (projectRoot == null) throw new Error('Missing project root')
      this.projectRootId = projectRoot.id
      await this.ls.acquireReceivesTreeUpdates({ rootId: this.projectRootId, segments: [] })
      const srcFiles = await this.scanSrcFiles()
      await Promise.all(
        this.indexDoc.doc.transact(() => {
          return srcFiles.map((file) =>
            this.getModuleModel(pushPathSegment(file.path, file.name)).open(),
          )
        }, this),
      )
    } catch (error) {
      console.error('LS Initialization failed:', error)
      if (error instanceof Error) {
        this.emit('error', [error])
      }
      return
    }
    console.log('LS connection initialized.')
  }

  async scanSrcFiles() {
    this.assertProjectRoot()
    const srcModules = await this.ls.listFiles({ rootId: this.projectRootId, segments: ['src'] })
    return srcModules.paths.filter((file) => file.type === 'File' && file.name.endsWith('.enso'))
  }

  getModuleModelIfExists(path: Path): ModulePersistence | null {
    const name = pathToModuleName(path)
    return this.authoritativeModules.get(name) ?? null
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
        if (index != null) {
          this.model.deleteModule(index)
        }
      })
      return mod
    })
  }

  static get(url: string): LanguageServerSession {
    const session = map.setIfUndefined(sessions, url, () => new LanguageServerSession(url))
    session.retain()
    return session
  }

  retain() {
    this.retainCount++
  }

  release(): Promise<void> {
    this.retainCount--
    if (this.retainCount === 0) {
      const closing = Promise.all(
        Array.from(this.authoritativeModules.values(), (mod) => mod.dispose()),
      ).then(() => {})
      this.authoritativeModules.clear()
      this.model.doc.destroy()
      this.ls.dispose()
      sessions.delete(this.url)
      return closing
    }
    return Promise.resolve()
  }

  getYDoc(guid: string): WSSharedDoc | undefined {
    return this.docs.get(guid)
  }
}

const isSourceFile = (path: Path): boolean => {
  return path.segments[0] === 'src' && path.segments[path.segments.length - 1].endsWith('.enso')
}

const pathToModuleName = (path: Path): string => {
  if (path.segments[0] === 'src') {
    return path.segments.slice(1).join('/')
  } else {
    return '//' + path.segments.join('/')
  }
}

const pushPathSegment = (path: Path, segment: string): Path => {
  return {
    rootId: path.rootId,
    segments: [...path.segments, segment],
  }
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
      if (origin === 'file') {
        Y.applyUpdate(sharedDoc, update, this)
      }
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

  async open() {
    this.queuedAction = LsAction.Open
    switch (this.state) {
      case LsSyncState.Disposed:
      case LsSyncState.WritingFile:
      case LsSyncState.Synchronized:
      case LsSyncState.WriteError:
      case LsSyncState.Reloading:
        return
      case LsSyncState.Closing:
        await this.lastAction
        if (this.queuedAction === LsAction.Open) {
          await this.open()
        }
        return
      case LsSyncState.Opening:
        await this.lastAction
        return
      case LsSyncState.Closed:
        {
          this.changeState(LsSyncState.Opening)
          const opening = this.ls.openTextFile(this.path)
          this.lastAction = opening.then()
          const result = await opening
          this.syncFileContents(result.content, result.currentVersion)
          this.changeState(LsSyncState.Synchronized)
        }
        return
      default: {
        const _: never = this.state
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
      console.log(' === changes === ')
      console.log('number of edits:', edits.length)
      console.log('metadata:', metadataKeys)
      console.log('content:', contentDelta)
      console.log('idMap:', idMapKeys)
      if (edits.length > 0) {
        console.log('version:', this.syncedVersion, '->', newVersion)
        console.log('Content diff:')
        console.log(prettyPrintDiff(this.syncedContent, newContent))
      }
      console.log(' =============== ')
    }
    if (edits.length === 0) {
      if (newVersion !== this.syncedVersion) {
        console.error('Version mismatch:', this.syncedVersion, '->', newVersion)
      }
      return
    }

    this.changeState(LsSyncState.WritingFile)

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
        this.changeState(LsSyncState.Synchronized)
      },
      (e) => {
        console.error('Failed to apply edit:', e)

        // Try to recover by reloading the file. Drop the attempted updates, since applying them
        // have failed.
        this.changeState(LsSyncState.WriteError)
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
        const range = [index.value, index.value + size.value]
        if (typeof range[0] !== 'number' || typeof range[1] !== 'number') {
          console.error(`Invalid range for id ${id}:`, range)
          continue
        }
        idMap.insertKnownId([index.value, index.value + size.value], id as ExprId)
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
      case LsSyncState.Closed:
        return
      case LsSyncState.Closing:
        await this.lastAction
        return
      case LsSyncState.Opening:
      case LsSyncState.WritingFile:
      case LsSyncState.Reloading:
        await this.lastAction
        if (this.queuedAction === LsAction.Close) {
          await this.close()
        }
        return
      case LsSyncState.WriteError:
      case LsSyncState.Synchronized: {
        this.changeState(LsSyncState.Closing)
        const closing = (this.lastAction = this.ls.closeTextFile(this.path))
        await closing
        this.changeState(LsSyncState.Closed)
        return
      }
      default: {
        const _: never = this.state
      }
    }
  }

  async reload() {
    this.queuedAction = LsAction.Reload
    switch (this.state) {
      case LsSyncState.Opening:
      case LsSyncState.Disposed:
      case LsSyncState.Closed:
      case LsSyncState.Closing:
        return
      case LsSyncState.Reloading:
        await this.lastAction
        return
      case LsSyncState.WritingFile:
        await this.lastAction
        if (this.queuedAction === LsAction.Reload) {
          await this.reload()
        }
        return
      case LsSyncState.Synchronized:
      case LsSyncState.WriteError: {
        this.changeState(LsSyncState.Reloading)
        const reloading = this.ls.closeTextFile(this.path).then(() => {
          return this.ls.openTextFile(this.path)
        })
        this.lastAction = reloading.then()
        const result = await reloading
        this.syncFileContents(result.content, result.currentVersion)
        this.changeState(LsSyncState.Synchronized)
        return
      }
      default: {
        const _: never = this.state
      }
    }
  }

  private inState(...states: LsSyncState[]): boolean {
    return states.includes(this.state)
  }

  private changeState(state: LsSyncState) {
    if (this.state !== LsSyncState.Disposed) {
      if (DEBUG_LOG_SYNC) {
        console.log('State change:', LsSyncState[this.state], '->', LsSyncState[state])
      }
      this.state = state
      if (state === LsSyncState.Synchronized) {
        this.trySyncRemoveUpdates()
      }
    } else {
      throw new Error('LsSync disposed')
    }
  }

  dispose(): Promise<void> {
    this.cleanup()
    const alreadyClosed = this.inState(LsSyncState.Closing, LsSyncState.Closed)
    this.changeState(LsSyncState.Disposed)
    if (!alreadyClosed) {
      return this.ls.closeTextFile(this.path).then()
    }
    return Promise.resolve()
  }
}
