import { Client, RequestManager, WebSocketTransport } from '@open-rpc/client-js'
import diff from 'fast-diff'
import { simpleDiffString } from 'lib0/diff'
import * as json from 'lib0/json'
import * as map from 'lib0/map'
import { ObservableV2 } from 'lib0/observable'
import * as random from 'lib0/random'
import * as Y from 'yjs'
import { LanguageServer, computeTextChecksum } from '../shared/languageServer'
import { Checksum, Path, TextEdit } from '../shared/languageServerTypes'
import {
  DistributedProject,
  ExprId,
  IdMap,
  ModuleDoc,
  NodeMetadata,
  Uuid,
  decodeRange,
} from '../shared/yjsModel'
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

    this.ls.on('file/event', (event) => {
      if (DEBUG_LOG_SYNC) {
        console.log('file/event', event)
      }
      switch (event.kind) {
        case 'Added':
          this.getModuleModel(event.path).open()
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

    const synced = preParseContent(this.syncedContent)

    let newContent = ''

    const allEdits: TextEdit[] = []
    if (contentDelta && contentDelta.length > 0) {
      const { code, edits } = convertDeltaToTextEdits(synced.code, contentDelta)
      newContent += code
      allEdits.push(...edits)
    } else {
      newContent += synced.code
    }

    newContent += META_TAG + '\n'

    const metaStartLine = (newContent.match(/\n/g) ?? []).length

    if (idMapKeys != null || (contentDelta && contentDelta.length > 0)) {
      const idMapJson = json.stringify(idMapToArray(this.doc.idMap))
      allEdits.push(...applyDiffAsTextEdits(metaStartLine, synced.idMapJson ?? '', idMapJson))
      newContent += idMapJson + '\n'
    } else {
      newContent += synced.idMapJson + '\n'
    }

    const nodeMetadata = this.syncedMeta.ide.node
    if (metadataKeys != null) {
      for (const [key, op] of metadataKeys) {
        switch (op.action) {
          case 'delete':
            delete nodeMetadata[key]
            break
          case 'add':
          case 'update': {
            const updatedMeta = this.doc.metadata.get(key)
            const oldMeta = nodeMetadata[key] ?? {}
            if (updatedMeta == null) continue
            nodeMetadata[key] = {
              ...oldMeta,
              position: {
                vector: [updatedMeta.x, updatedMeta.y],
              },
            }
            break
          }
        }
      }

      const metadataJson = json.stringify(this.syncedMeta)
      allEdits.push(
        ...applyDiffAsTextEdits(metaStartLine + 1, synced.metadataJson ?? '', metadataJson),
      )
      newContent += metadataJson
    } else {
      newContent += synced.metadataJson
    }

    const newVersion = computeTextChecksum(newContent)

    if (DEBUG_LOG_SYNC) {
      console.log(' === changes === ')
      console.log('number of edits:', allEdits.length)
      console.log('metadata:', metadataKeys)
      console.log('content:', contentDelta)
      console.log('idMap:', idMapKeys)
      if (allEdits.length > 0) {
        console.log('version:', this.syncedVersion, '->', newVersion)
        console.log('Content diff:')
        console.log(prettyPrintDiff(this.syncedContent, newContent))
      }
      console.log(' =============== ')
    }
    if (allEdits.length === 0) {
      return
    }

    const execute = (contentDelta && contentDelta.length > 0) || metadataKeys != null

    this.changeState(LsSyncState.WritingFile)
    const apply = this.ls.applyEdit(
      {
        path: this.path,
        edits: allEdits,
        oldVersion: this.syncedVersion,
        newVersion,
      },
      execute,
    )
    return (this.lastAction = apply.then(
      () => {
        this.syncedContent = newContent
        this.syncedVersion = newVersion
        this.syncedMeta.ide.node = nodeMetadata
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
          x: meta?.position?.vector?.[0] ?? 0,
          y: meta?.position?.vector?.[1] ?? 0,
          vis: meta?.visualization ?? undefined,
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
  for (const op of contentDelta) {
    if (op.insert != null && typeof op.insert === 'string') {
      const pos = {
        character: newIndex - lineStartIdx,
        line: lineNum,
      }
      // if the last edit was a delete on the same position, we can merge the insert into it
      const lastEdit = edits[edits.length - 1]
      if (
        lastEdit &&
        lastEdit.text.length === 0 &&
        lastEdit.range.start.line === pos.line &&
        lastEdit.range.start.character === pos.character
      ) {
        lastEdit.text = op.insert
      } else {
        edits.push({ range: { start: pos, end: pos }, text: op.insert })
      }
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
  idMapJson: string | null
  metadataJson: string | null
}

function preParseContent(content: string): PreParsedContent {
  const splitPoint = content.lastIndexOf(META_TAG)
  if (splitPoint < 0) {
    return {
      code: content,
      idMapJson: null,
      metadataJson: null,
    }
  }
  const code = content.slice(0, splitPoint)
  const metadataString = content.slice(splitPoint + META_TAG.length)
  const metaLines = metadataString.trim().split('\n')
  const idMapJson = metaLines[0] ?? null
  const metadataJson = metaLines[1] ?? null
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

  describe('applyDiffAsTextEdits', () => {
    test('no change', () => {
      const edits = applyDiffAsTextEdits(0, 'abcd', 'abcd')
      expect(edits).toStrictEqual([])
    })
    test('simple add', () => {
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
    test('two adds', () => {
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

function prettyPrintDiff(from: string, to: string): string {
  const colReset = '\x1b[0m'
  const colRed = '\x1b[31m'
  const colGreen = '\x1b[32m'

  const diffs = diff(from, to)
  if (diffs.length === 1 && diffs[0]![0] === 0) return 'No changes'
  let content = ''
  for (let i = 0; i < diffs.length; i++) {
    const [op, text] = diffs[i]!
    if (op === 1) {
      content += colGreen + text
    } else if (op === -1) {
      content += colRed + text
    } else if (op === 0) {
      content += colReset
      const numNewlines = (text.match(/\n/g) ?? []).length
      if (numNewlines < 2) {
        content += text
      } else {
        const firstNewline = text.indexOf('\n')
        const lastNewline = text.lastIndexOf('\n')
        const firstLine = text.slice(0, firstNewline + 1)
        const lastLine = text.slice(lastNewline + 1)
        const isFirst = i === 0
        const isLast = i === diffs.length - 1
        if (!isFirst) content += firstLine
        if (!isFirst && !isLast) content += '...\n'
        if (!isLast) content += lastLine
      }
    }
  }
  content += colReset
  return content
}
