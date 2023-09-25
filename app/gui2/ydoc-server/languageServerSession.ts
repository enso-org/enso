import { Client, RequestManager, WebSocketTransport } from '@open-rpc/client-js'
import diff from 'fast-diff'
import * as json from 'lib0/json'
import * as map from 'lib0/map'
import * as random from 'lib0/random'
import * as Y from 'yjs'
import { Emitter } from '../shared/event'
import { LanguageServer, computeTextChecksum } from '../shared/languageServer'
import { Checksum, Path, TextEdit, response } from '../shared/languageServerTypes'
import {
  DistributedModule,
  DistributedProject,
  NodeMetadata,
  Uuid,
  decodeRange,
} from '../shared/yjsModel'
import { WSSharedDoc } from './ydoc'

const sessions = new Map<string, LanguageServerSession>()

type Events = {
  error: [error: Error]
}

export class LanguageServerSession extends Emitter<Events> {
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

const META_TAG = '#### METADATA ####'

type IdMapEntry = [{ index: { value: number }; size: { value: number } }, string]
interface IdeMetadata {
  node: {
    [id: string]: {
      position?: { vector: [number, number] }
      visualization?: VisMetadata | null
    }
  }
  import: { [id: string]: {} }
}

interface VisMetadata {
  name: { content: { content: string } }
  project: string
}

function catchError(fn: () => void) {
  try {
    fn()
  } catch (e) {
    console.error(e)
  }
}

class ModulePersistence extends Emitter<{ removed: [] }> {
  ls: LanguageServer
  model: DistributedModule
  path: Path
  knownContent: string
  knownMeta: { ide: IdeMetadata }
  currentVersion: Checksum | null

  constructor(model: DistributedModule, path: Path, ls: LanguageServer) {
    super()
    this.ls = ls
    this.model = model
    this.path = path
    this.currentVersion = null
    this.knownContent = ''
    this.knownMeta = { ide: { node: {}, import: {} } }

    this.model.contents.observe((change, tx) => this.handleContentUpdate(change, tx))
    this.model.idMap.observe((change, tx) => this.handleIdMapUpdate(change, tx))
    this.model.metadata.observe((change, tx) => this.handleMetaUpdate(change, tx))
    this.model.doc.on('update', (update, origin) =>
      catchError(() => this.handleDocUpdate(update, origin)),
    )
  }
  async load() {
    let loaded: response.OpenTextFile
    try {
      loaded = await this.ls.openTextFile(this.path)
    } catch (_) {
      return this.dispose()
    }

    this.model.doc.transact(() => {
      const idMap = this.model.getIdMap()
      const preParsed = preParseContent(loaded.content)
      let idMapMeta
      try {
        idMapMeta = tryParseOrFallback(preParsed.idMapJson, [], 'Id map parse failed:')
        const metadata = tryParseOrFallback(
          preParsed.metadataJson,
          this.knownMeta,
          'Metadata parse failed:',
        )
        if (metadata != null && typeof metadata === 'object') {
          metadata.ide ??= this.knownMeta.ide
          metadata.ide.node ??= this.knownMeta.ide.node
          metadata.ide.import ??= this.knownMeta.ide.import
          this.knownMeta = metadata as typeof this.knownMeta
        }
      } catch (e) {
        console.log('Metadata parse failed:', e)
        return
      }

      const code = preParsed.code
      const nodeMeta = this.knownMeta.ide.node

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

      this.knownContent = loaded.content
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
    if (this.currentVersion == null) return
  }

  contentDelta: Y.YTextEvent['delta'] = []
  metaUpdated = false
  idMapUpdated = false

  handleContentUpdate(change: Y.YTextEvent, tx: Y.Transaction) {
    if (tx.origin === this) return
    this.contentDelta = change.delta
  }

  handleMetaUpdate(change: Y.YMapEvent<NodeMetadata>, tx: Y.Transaction): void {
    if (tx.origin === this) return
    this.metaUpdated = true
    for (const [key, op] of change.keys) {
      switch (op.action) {
        case 'delete':
          delete this.knownMeta.ide.node[key]
          break
        case 'add':
        case 'update': {
          const updatedMeta = this.model.metadata.get(key)
          const oldMeta = this.knownMeta.ide.node[key] ?? {}
          if (updatedMeta == null) continue
          this.knownMeta.ide.node[key] = {
            ...oldMeta,
            position: {
              vector: [updatedMeta.x, updatedMeta.y],
            },
          }
          break
        }
      }
    }
  }

  handleIdMapUpdate(change: Y.YMapEvent<Uint8Array>, tx: Y.Transaction): void {
    if (tx.origin === this) return
    this.idMapUpdated = true
  }

  handleDocUpdate(_update: Uint8Array, origin: any) {
    if (origin === this) return
    if (this.currentVersion == null) return
    const oldVersion = this.currentVersion
    const execute = this.contentDelta.length > 0 || this.idMapUpdated

    const changed = this.metaUpdated || this.idMapUpdated || this.contentDelta.values.length > 0
    if (!changed) return

    const known = preParseContent(this.knownContent)
    const allEdits: TextEdit[] = []

    let updatedCode = known.code
    if (this.contentDelta.length > 0) {
      const { code, edits } = convertDeltaToTextEdits(known.code, this.contentDelta)
      printEdits('code', edits)
      updatedCode = code
      allEdits.push(...edits)
      this.contentDelta = []
    }

    const idMapJson = json.stringify(idMapToArray(this.model.idMap))
    const metadataJson = json.stringify(this.knownMeta)

    const metaStartLine = (updatedCode.match(/\n/g) ?? []).length
    const oldMeta = this.knownContent.slice(known.code.length)
    const newMeta = [META_TAG, idMapJson, metadataJson].join('\n')

    const metaEdits = applyDiffAsTextEdits(metaStartLine, oldMeta, newMeta)
    if (metaEdits.length > 0) {
      printEdits('meta', metaEdits)
      allEdits.push(...metaEdits)
    }

    const newContent = updatedCode + newMeta
    const newVersion = computeTextChecksum(newContent)
    this.knownContent = newContent
    this.currentVersion = newVersion

    if (allEdits.length === 0) {
      console.log('No edits')
      return
    }
    console.log(oldVersion, '->', newVersion)

    this.ls.applyEdit(
      {
        path: this.path,
        edits: allEdits,
        oldVersion,
        newVersion,
      },
      execute,
    )
  }
}

function tryParseOrFallback(jsonString: string, fallback: any, message: string): any {
  try {
    return json.parse(jsonString)
  } catch (e) {
    console.error(message, e)
    return fallback
  }
}

function printEdits(label: string, edits: TextEdit[]) {
  for (const edit of edits) {
    const { start, end } = edit.range
    console.log(
      `[${label}] ${start.line}:${start.character}-${end.line}:${end.character} -> ${edit.text.length}`,
    )
  }
}

function applyDiffAsTextEdits(
  lineOffset: number,
  oldString: string,
  newString: string,
): TextEdit[] {
  const changes = diff(oldString, newString)
  let newIndex = 0
  let lineNum = lineOffset
  let lineStartIdx = 0
  const edits = []
  for (const [op, text] of changes) {
    if (op === 1) {
      const pos = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      edits.push({ range: { start: pos, end: pos }, text })
      const numLineBreaks = (text.match(/\n/g) ?? []).length
      if (numLineBreaks > 0) {
        lineStartIdx = newIndex + text.lastIndexOf('\n') + 1
      }
      newIndex += text.length
    } else if (op === -1) {
      const start = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      const numLineBreaks = (text.match(/\n/g) ?? []).length
      const character =
        numLineBreaks > 0 ? text.lastIndexOf('\n') + 1 : newIndex - lineStartIdx + text.length
      const end = {
        character,
        line: lineNum + numLineBreaks,
      }
      edits.push({ range: { start, end }, text: '' })
    } else if (op === 0) {
      const numLineBreaks = (text.match(/\n/g) ?? []).length
      lineNum += numLineBreaks
      if (numLineBreaks > 0) {
        lineStartIdx = newIndex + text.lastIndexOf('\n') + 1
      }
      newIndex += text.length
    }
  }
  return edits
}

function convertDeltaToTextEdits(
  prevText: string,
  contentDelta: Y.YTextEvent['delta'],
): { code: string; edits: TextEdit[] } {
  const edits = []
  let index = 0
  let newIndex = 0
  let lineNum = 0
  let lineStartIdx = 0
  let code = ''
  console.log('contentDelta', contentDelta)
  for (const op of contentDelta) {
    if (op.insert != null && typeof op.insert === 'string') {
      const pos = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      edits.push({ range: { start: pos, end: pos }, text: op.insert })
      const numLineBreaks = (op.insert.match(/\n/g) ?? []).length
      if (numLineBreaks > 0) {
        lineStartIdx = newIndex + op.insert.lastIndexOf('\n') + 1
      }
      code += op.insert
      newIndex += op.insert.length
    } else if (op.delete != null) {
      const start = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      const deleted = prevText.slice(index, index + op.delete)
      const numLineBreaks = (deleted.match(/\n/g) ?? []).length
      const character =
        numLineBreaks > 0 ? deleted.lastIndexOf('\n') + 1 : newIndex - lineStartIdx + op.delete
      const end = {
        character,
        line: lineNum + numLineBreaks,
      }
      edits.push({ range: { start, end }, text: '' })
      index += op.delete
    } else if (op.retain != null) {
      const retained = prevText.slice(index, index + op.retain)
      const numLineBreaks = (retained.match(/\n/g) ?? []).length
      lineNum += numLineBreaks
      if (numLineBreaks > 0) {
        lineStartIdx = newIndex + retained.lastIndexOf('\n') + 1
      }
      code += retained
      index += op.retain
      newIndex += op.retain
    }
  }
  code += prevText.slice(index)
  return { code, edits }
}

interface PreParsedContent {
  code: string
  idMapJson: string
  metadataJson: string
}

function preParseContent(content: string): PreParsedContent {
  const splitPoint = content.lastIndexOf(META_TAG)
  if (splitPoint < 0) {
    return {
      code: content,
      idMapJson: '',
      metadataJson: '',
    }
  }
  const code = content.slice(0, splitPoint)
  const metadataString = content.slice(splitPoint + META_TAG.length)
  const metaLines = metadataString.trim().split('\n')
  const idMapJson = metaLines[0]
  const metadataJson = metaLines[1]
  return { code, idMapJson, metadataJson }
}

function idMapToArray(map: Y.Map<Uint8Array>): IdMapEntry[] {
  const entries: IdMapEntry[] = []
  const doc = map.doc!
  map.forEach((rangeBuffer, id) => {
    const decoded = decodeRange(rangeBuffer)
    const index = Y.createAbsolutePositionFromRelativePosition(decoded[0], doc)?.index
    const endIndex = Y.createAbsolutePositionFromRelativePosition(decoded[1], doc)?.index
    if (index == null || endIndex == null) return
    const size = endIndex - index
    entries.push([{ index: { value: index }, size: { value: size } }, id])
  })
  entries.sort(idMapCmp)
  return entries
}

function idMapCmp(a: IdMapEntry, b: IdMapEntry) {
  const val1 = a[0]?.index?.value ?? 0
  const val2 = b[0]?.index?.value ?? 0
  if (val1 === val2) {
    const size1 = a[0]?.size.value ?? 0
    const size2 = b[0]?.size.value ?? 0
    return size1 - size2
  }
  return val1 - val2
}

if (import.meta.vitest) {
  const { test, expect, describe } = import.meta.vitest

  describe.only('applyDiffAsTextEdits', () => {
    test.only('no change', () => {
      const edits = applyDiffAsTextEdits(0, 'abcd', 'abcd')
      expect(edits).toStrictEqual([])
    })
    test.only('simple add', () => {
      const before = 'abcd'
      const after = 'abefcd'
      const edits = applyDiffAsTextEdits(1, before, after)
      expect(edits).toStrictEqual([
        {
          range: { end: { character: 2, line: 1 }, start: { character: 2, line: 1 } },
          text: 'ef',
        },
      ])
    })
    test.only('two adds', () => {
      const before = 'abcd'
      const after = 'abefcdxy'
      const edits = applyDiffAsTextEdits(1, before, after)
      expect(edits).toStrictEqual([
        {
          range: { end: { character: 2, line: 1 }, start: { character: 2, line: 1 } },
          text: 'ef',
        },
        {
          range: { end: { character: 6, line: 1 }, start: { character: 6, line: 1 } },
          text: 'xy',
        },
      ])
    })
  })
}
