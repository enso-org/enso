import * as Y from 'yjs'
import { setIfUndefined } from 'lib0/map'
import { isSome } from '@/util/opt'

export type Uuid = ReturnType<typeof crypto.randomUUID>
declare const brandExprId: unique symbol
export type ExprId = Uuid & { [brandExprId]: never }
export const NULL_EXPR_ID: ExprId = '00000000-0000-0000-0000-000000000000' as ExprId

export interface NodeMetadata {
  x: number
  y: number
  vis?: string
}

const enum ModKeys {
  NAME = 'name',
  DOC = 'doc',
}

type NamedDocMap = Y.Map<Y.Text | Y.Doc>

interface ProjectUpdate {}

export class DistributedProject {
  doc: Y.Doc
  name: Y.Text
  modules: Y.Array<NamedDocMap>

  constructor(doc: Y.Doc) {
    this.doc = doc
    this.name = this.doc.getText('name')
    this.modules = this.doc.getArray('modules')
  }

  moduleNames(): string[] {
    return this.modules.map((map) => map.get(ModKeys.NAME)?.toString() ?? '')
  }

  observe(callback: (update: ProjectUpdate) => void) {
    return this.doc.on('update', callback)
  }

  async findModuleByName(name: string): Promise<number | null> {
    await this.doc.whenLoaded
    const index = this.moduleNames().indexOf(name)
    return index < 0 ? null : index
  }

  async openModule(index: number): Promise<DistributedModule | null> {
    const map = this.modules.get(index)
    const name = map.get(ModKeys.NAME)
    const doc = map.get(ModKeys.DOC)
    if (name instanceof Y.Text && doc instanceof Y.Doc) {
      return await DistributedModule.load(doc, name)
    }
    return null
  }

  async createNewModule(name: string): Promise<DistributedModule> {
    const map = new Y.Map<Y.Text | Y.Doc>()
    const doc = map.set(ModKeys.DOC, new Y.Doc())
    const text = map.set(ModKeys.NAME, new Y.Text(name))
    this.modules.push([map])
    return await DistributedModule.load(doc, text)
  }

  async openOrCreateModule(name: string): Promise<DistributedModule> {
    const index = await this.findModuleByName(name)
    if (index != null) {
      return (await this.openModule(index)) ?? (await this.createNewModule(name))
    } else {
      return await this.createNewModule(name)
    }
  }

  deleteModule(index: number): void {
    this.modules.delete(index)
  }
}

class DistributedModule {
  doc: Y.Doc
  name: Y.Text
  contents: Y.Text
  idMap: Y.Map<[any, any]>
  metadata: Y.Map<NodeMetadata>

  static async load(doc: Y.Doc, name: Y.Text) {
    const module = new DistributedModule(doc, name)
    module.doc.load()
    console.log('Waiting for module to load')
    await module.doc.whenLoaded
    console.log('Module loaded')
    return module
  }

  private constructor(doc: Y.Doc, name: Y.Text) {
    this.doc = doc
    this.name = name
    this.contents = this.doc.getText('contents')
    this.idMap = this.doc.getMap('idMap')
    this.metadata = this.doc.getMap('metadata')
  }

  insertNewNode(offset: number, content: string, meta: NodeMetadata): ExprId {
    const range = [offset, offset + content.length]
    const newId = crypto.randomUUID() as ExprId
    this.doc.transact(() => {
      this.contents.insert(offset, content + '\n')
      const start = Y.createRelativePositionFromTypeIndex(this.contents, range[0])
      const end = Y.createRelativePositionFromTypeIndex(this.contents, range[1])
      const startJson = Y.relativePositionToJSON(start)
      const endJson = Y.relativePositionToJSON(end)
      this.idMap.set(newId, [startJson, endJson])
      this.metadata.set(newId, meta)
    })
    return newId
  }

  replaceExpressionContent(id: ExprId, content: string, range?: ContentRange): void {
    const exprRangeJson = this.idMap.get(id)
    if (exprRangeJson == null) return
    const exprRange = [
      Y.createRelativePositionFromJSON(exprRangeJson[0]),
      Y.createRelativePositionFromJSON(exprRangeJson[1]),
    ]
    const exprStart = Y.createAbsolutePositionFromRelativePosition(exprRange[0], this.doc)?.index
    const exprEnd = Y.createAbsolutePositionFromRelativePosition(exprRange[1], this.doc)?.index
    if (exprStart == null || exprEnd == null) return
    const start = range == null ? exprStart : exprStart + range[0]
    const end = range == null ? exprEnd : exprStart + range[1]
    if (start > end) throw new Error('Invalid range')
    if (start < exprStart || end > exprEnd) throw new Error('Range out of bounds')
    this.doc.transact(() => {
      if (content.length > 0) {
        this.contents.insert(start, content)
      }
      if (start !== end) {
        this.contents.delete(start + content.length, end - start)
      }
    })
  }

  updateNodeMetadata(id: ExprId, meta: Partial<NodeMetadata>): void {
    const existing = this.metadata.get(id) ?? { x: 0, y: 0 }
    this.metadata.set(id, { ...existing, ...meta })
  }

  getIdMap(): IdMap {
    return new IdMap(this.idMap, this.contents)
  }

  dispose(): void {
    this.doc.destroy()
  }
}

export interface RelativeRange {
  start: Y.RelativePosition
  end: Y.RelativePosition
}

/**
 * Accessor for the ID map stored in shared yjs map as relative ranges. Synchronizes the ranges
 * that were accessed during parsing, throws away stale ones. The text contents is used to translate
 * the relative ranges to absolute ranges, but it is not modified.
 */
export class IdMap {
  private contents: Y.Text
  private doc: Y.Doc
  private yMap: Y.Map<[any, any]>
  private rangeToExpr: Map<string, ExprId>
  private accessed: Set<ExprId>
  private finished: boolean

  constructor(yMap: Y.Map<[any, any]>, contents: Y.Text) {
    if (yMap.doc == null) {
      throw new Error('IdMap must be associated with a document')
    }
    this.doc = yMap.doc
    this.contents = contents
    this.yMap = yMap
    this.rangeToExpr = new Map()
    this.accessed = new Set()

    yMap.forEach((range, expr) => {
      if (!isUuid(expr)) return
      const indices = this.modelToIndices(range)
      if (indices == null) return
      this.rangeToExpr.set(IdMap.keyForRange(indices), expr as ExprId)
    })

    this.finished = false
  }

  private static keyForRange(range: [number, number]): string {
    return `${range[0]}:${range[1]}`
  }

  private modelToIndices(range: [any, any]): [number, number] | null {
    const relStart = Y.createRelativePositionFromJSON(range[0])
    const relEnd = Y.createRelativePositionFromJSON(range[1])
    const start = Y.createAbsolutePositionFromRelativePosition(relStart, this.doc)
    const end = Y.createAbsolutePositionFromRelativePosition(relEnd, this.doc)
    if (start == null || end == null) return null
    return [start.index, end.index]
  }

  getOrInsertUniqueId(range: [number, number]): ExprId {
    if (this.finished) {
      throw new Error('IdMap already finished')
    }

    const key = IdMap.keyForRange(range)
    const val = this.rangeToExpr.get(key)
    if (val !== undefined) {
      this.accessed.add(val)
      return val
    } else {
      const newId = crypto.randomUUID() as ExprId
      this.rangeToExpr.set(key, newId)
      this.accessed.add(newId)
      return newId
    }
  }

  accessedSoFar(): ReadonlySet<ExprId> {
    return this.accessed
  }

  /**
   * Finish accessing or modifying ID map. Synchronizes the accessed keys back to the shared map,
   * removes keys that were present previously, but were not accessed.
   *
   * Can be called at most once. After calling this method, the ID map is no longer usable.
   */
  finishAndSynchronize(): void {
    if (this.finished) {
      throw new Error('IdMap already finished')
    }
    this.finished = true

    const doc = this.doc

    doc.transact(() => {
      this.yMap.forEach((currentRange, expr) => {
        // Expressions that were accessed and present in the map are guaranteed to match. There is
        // no mechanism for modifying them, so we don't need to check for equality. We only need to
        // delete the expressions ones that are not used anymore.
        if (!this.accessed.delete(expr as ExprId)) {
          this.yMap.delete(expr)
        }
      })

      this.rangeToExpr.forEach((expr, key) => {
        // For all remaining expressions, we need to write them into the map.
        if (!this.accessed.has(expr)) return
        const range = key.split(':').map((x) => parseInt(x, 10)) as [number, number]
        const start = Y.createRelativePositionFromTypeIndex(this.contents, range[0])
        const end = Y.createRelativePositionFromTypeIndex(this.contents, range[1])
        const startJson = Y.relativePositionToJSON(start)
        const endJson = Y.relativePositionToJSON(end)
        this.yMap.set(expr, [startJson, endJson])
      })
    })
  }
}

const uuidRegex = /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/
export function isUuid(x: any): x is Uuid {
  return typeof x === 'string' && x.length === 36 && uuidRegex.test(x)
}

export type ContentRange = [number, number]

export function rangeEncloses(a: ContentRange, b: ContentRange): boolean {
  return a[0] <= b[0] && a[1] >= b[1]
}

export function rangeIntersects(a: ContentRange, b: ContentRange): boolean {
  return a[0] <= b[1] && a[1] >= b[0]
}
