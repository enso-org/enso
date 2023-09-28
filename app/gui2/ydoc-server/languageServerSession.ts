import { Client, RequestManager, WebSocketTransport } from '@open-rpc/client-js'
import * as map from 'lib0/map'
import { createMutex } from 'lib0/mutex'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import * as Y from 'yjs'
import { LanguageServer } from '../shared/languageServer'
import { Checksum, Path, response } from '../shared/languageServerTypes'
import { DistributedModule, DistributedProject, NodeMetadata, Uuid } from '../shared/yjsModel'
import { WSSharedDoc } from './ydoc'

const sessions = new Map<string, LanguageServerSession>()

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
    transport.connection.on('error', (error) => this.emit('error', [error]))
    this.ls = new LanguageServer(this.client)
    this.indexDoc = new WSSharedDoc()
    this.docs.set('index', this.indexDoc)
    this.model = new DistributedProject(this.indexDoc.doc)
    this.projectRootId = null
    this.readInitialState()
    this.authoritativeModules = new Map()

    this.indexDoc.doc.on('subdocs', (subdocs: { loaded: Set<Y.Doc> }) => {
      for (const doc of subdocs.loaded) {
        const name = this.model.findModuleByDocId(doc.guid)
        if (name == null) continue
        const persistence = this.authoritativeModules.get(name)
        if (persistence == null) continue
        persistence.load()
      }
    })

    this.model.modules.observe((event, transaction) => {
      if (transaction.origin === this) return
      const changes = event.changes
      console.log('index change', changes.delta)

      // let index = 0
      // const indicesToDelete: number[] = []
      // for (const op of delta) {
      //   if (op.insert != null) {
      //     const mod = this.model.openUnloadedModule(index)
      //     if (mod == null || !this.isAuthoritative(mod)) {
      //       console.log(`index ${index} not authoritative, deleting`)
      //       indicesToDelete.push(index - indicesToDelete.length)
      //     }
      //     index++
      //   } else if (op.delete != null) {
      //     console.error('TODO: Module delete')
      //   } else if (op.retain != null) {
      //     index += op.retain
      //   }
      // }
      // this.indexDoc.doc.transact(() => {
      //   for (const index of indicesToDelete) {
      //     this.model.modules.delete(index)
      //   }
      // }, this)

      // console.log('index update', this.model.moduleNames())
    })

    this.ls.addEventListener('file/event', (event) => {
      console.log('file/event', event)
    })
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
          return srcFiles.map((file) => this.getModuleModel(pushPathSegment(file.path, file.name)))
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

  getModuleModel(path: Path): ModulePersistence {
    const name = pathToModuleName(path)
    return map.setIfUndefined(this.authoritativeModules, name, () => {
      const wsDoc = new WSSharedDoc()
      this.docs.set(wsDoc.doc.guid, wsDoc)
      const model = this.model.createUnloadedModule(name, wsDoc.doc)
      const mod = new ModulePersistence(model, path, this.ls)
      mod.once('removed', () => {
        const index = this.model.findModuleByDocId(wsDoc.doc.guid)
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

  release() {
    this.retainCount--
    if (this.retainCount === 0) {
      this.model.doc.destroy()
      this.ls.dispose()
      sessions.delete(this.url)
    }
  }

  getYDoc(guid: string): WSSharedDoc | undefined {
    return this.docs.get(guid)
  }
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

type Mutex = ReturnType<typeof createMutex>

class ModulePersistence extends ObservableV2<{ removed: () => void }> {
  ls: LanguageServer
  model: DistributedModule
  path: Path
  currentVersion: Checksum | null
  mux: Mutex

  constructor(model: DistributedModule, path: Path, ls: LanguageServer) {
    super()
    this.ls = ls
    this.model = model
    this.path = path
    this.currentVersion = null
    this.mux = createMutex()
  }

  async load() {
    let loaded: response.OpenTextFile
    try {
      loaded = await this.ls.openTextFile(this.path)
    } catch (_) {
      return this.dispose()
    }

    this.model.doc.transact(() => {
      let code = loaded.content
      const idMap = this.model.getIdMap()
      try {
        const metaTag = '#### METADATA ####'
        const splitPoint = loaded.content.lastIndexOf(metaTag)
        if (splitPoint < 0) throw new Error('Metadata not found')
        code = loaded.content.slice(0, splitPoint)
        const metadataString = loaded.content.slice(splitPoint + metaTag.length)
        const metaLines = metadataString.trim().split('\n')
        const idMapMeta = JSON.parse(metaLines[0])
        const ideMeta = JSON.parse(metaLines[1])?.ide
        const nodeMeta = ideMeta?.node

        for (const [{ index, size }, id] of idMapMeta) {
          const range = [index.value, index.value + size.value]
          if (typeof range[0] !== 'number' || typeof range[1] !== 'number') {
            console.error(`Invalid range for id ${id}:`, range)
            continue
          }
          idMap.insertKnownId([index.value, index.value + size.value], id)
        }

        for (const [id, rawMeta] of Object.entries(nodeMeta ?? {})) {
          if (typeof id !== 'string') continue
          const meta = rawMeta as any
          const formattedMeta: NodeMetadata = {
            x: meta?.position?.vector?.[0] ?? 0,
            y: meta?.position?.vector?.[1] ?? 0,
            vis: meta?.visualization ?? undefined,
          }
          this.model.metadata.set(id, formattedMeta)
        }
      } catch (e) {
        console.log('Metadata parse failed:', e)
      }

      this.currentVersion = loaded.currentVersion
      this.model.contents.delete(0, this.model.contents.length)
      this.model.contents.insert(0, code)
      idMap.finishAndSynchronize()
    }, this)
  }

  /** A file was removed on the LS side. */
  dispose() {
    this.model.doc.destroy()
    this.emit('removed', [])
  }

  handleLsUpdate() {
    this.mux(() => {})
  }

  handleDocUpdate() {
    this.mux(() => {})
  }
}
