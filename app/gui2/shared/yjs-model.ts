import * as y from 'yjs'
import { setIfUndefined } from 'lib0/map'
import { isSome, type Opt } from '@/util/opt'

export type Uuid = ReturnType<typeof crypto.randomUUID>
declare const BRAND_ExprId: unique symbol
export type ExprId = Uuid & { [BRAND_ExprId]: never }
export const NULL_EXPR_ID: ExprId = '00000000-0000-0000-0000-000000000000' as ExprId

interface Handle {
  dispose(): void
}

export interface NodeMetadata {
  x: number
  y: number
  vis?: string
}

const enum NamedDocKey {
  NAME = 'name',
  DOC = 'doc',
}

interface NamedDoc {
  name: y.Text
  doc: y.Doc
}

let next_id = 0

type NamedDocMap = y.Map<y.Text | y.Doc>

export class NamedDocArray {
  _name: string
  array: y.Array<NamedDocMap>
  nameToIndex: Map<string, y.RelativePosition[]>

  constructor(doc: y.Doc, name: string) {
    this._name = `${name}#${next_id++}`
    this.array = doc.getArray(name)
    this.nameToIndex = new Map()
    this.array.forEach(this.integrateAddedItem.bind(this))
  }

  private integrateAddedItem(item: NamedDocMap, index: number): void {
    const name = item.get(NamedDocKey.NAME)?.toString()
    if (name == null) return
    const indices = setIfUndefined(this.nameToIndex, name, () => [] as y.RelativePosition[])
    indices.push(y.createRelativePositionFromTypeIndex(this.array, index))
  }

  names(): string[] {
    return this.array.map((item) => item.get(NamedDocKey.NAME)?.toString()).filter(isSome)
  }

  createNew(name: string): NamedDoc {
    const map = new y.Map<y.Text | y.Doc>()
    const yName = map.set(NamedDocKey.NAME, new y.Text(name))
    const doc = map.set(NamedDocKey.DOC, new y.Doc())
    const pos = y.createRelativePositionFromTypeIndex(this.array, this.array.length)
    this.array.push([map])
    return { name: yName, doc }
  }

  open(name: string): NamedDoc | undefined {
    const index = this.names().indexOf(name)
    if (index < 0) return
    const item = this.array.get(index)
    if (item == null) return
    const yName = item.get(NamedDocKey.NAME)
    const doc = item.get(NamedDocKey.DOC)
    if (!(yName instanceof y.Text && doc instanceof y.Doc)) return
    return { name: yName, doc }
  }

  delete(name: string): void {
    const relPos = this.nameToIndex.get(name)
    if (relPos == null) return
    const pos = y.createAbsolutePositionFromRelativePosition(relPos[0], this.array.doc!)
    if (pos == null) return
    this.array.delete(pos.index, 1)
    this.nameToIndex.delete(name)
  }

  dispose(): void {}
}

export class DistributedModel {
  doc: y.Doc
  projects: NamedDocArray

  constructor(doc: y.Doc) {
    this.doc = doc
    this.projects = new NamedDocArray(this.doc, 'projects')
  }

  projectNames(): string[] {
    return this.projects.names()
  }

  async openProject(name: string): Promise<Opt<DistributedProject>> {
    await this.doc.whenLoaded
    const pair = this.projects.open(name)
    if (pair == null) return null
    return await DistributedProject.load(pair)
  }

  async createNewProject(name: string): Promise<DistributedProject> {
    await this.doc.whenLoaded
    const pair = this.projects.createNew(name)
    return await DistributedProject.load(pair)
  }

  async openOrCreateProject(name: string): Promise<DistributedProject> {
    return (await this.openProject(name)) ?? (await this.createNewProject(name))
  }

  deleteProject(name: string): void {
    this.projects.delete(name)
  }

  dispose(): void {
    this.projects.dispose()
  }
}

export class DistributedProject {
  doc: y.Doc
  name: y.Text
  modules: NamedDocArray

  static async load(pair: NamedDoc) {
    const project = new DistributedProject(pair)
    project.doc.load()
    await project.doc.whenLoaded
    return project
  }

  private constructor(pair: NamedDoc) {
    this.doc = pair.doc
    this.name = pair.name
    this.modules = new NamedDocArray(this.doc, 'modules')
  }

  moduleNames(): string[] {
    return this.modules.names()
  }

  async openModule(name: string): Promise<DistributedModule | null> {
    const pair = this.modules.open(name)
    if (pair == null) return null
    return await DistributedModule.load(pair)
  }

  async createNewModule(name: string): Promise<DistributedModule> {
    const pair = this.modules.createNew(name)
    return await DistributedModule.load(pair)
  }

  async openOrCreateModule(name: string): Promise<DistributedModule> {
    return (await this.openModule(name)) ?? (await this.createNewModule(name))
  }

  deleteModule(name: string): void {
    this.modules.delete(name)
  }
}

class DistributedModule {
  name: y.Text
  doc: y.Doc
  contents: y.Text
  idMap: y.Map<[any, any]>
  metadata: y.Map<NodeMetadata>

  static async load(pair: NamedDoc) {
    const module = new DistributedModule(pair)
    module.doc.load()
    await module.doc.whenLoaded
    return module
  }

  private constructor(pair: NamedDoc) {
    this.name = pair.name
    this.doc = pair.doc
    this.contents = this.doc.getText('contents')
    this.idMap = this.doc.getMap('idMap')
    this.metadata = this.doc.getMap('metadata')
  }

  insertNewNode(offset: number, content: string, meta: NodeMetadata): ExprId {
    const range = [offset, offset + content.length]
    const newId = crypto.randomUUID() as ExprId
    this.doc.transact(() => {
      this.contents.insert(offset, content + '\n')
      const start = y.createRelativePositionFromTypeIndex(this.contents, range[0])
      const end = y.createRelativePositionFromTypeIndex(this.contents, range[1])
      const startJson = y.relativePositionToJSON(start)
      const endJson = y.relativePositionToJSON(end)
      this.idMap.set(newId, [startJson, endJson])
      this.metadata.set(newId, meta)
    })
    return newId
  }

  setExpressionContent(id: ExprId, content: string): void {
    const rangeJson = this.idMap.get(id)
    if (rangeJson == null) return
    const range = [
      y.createRelativePositionFromJSON(rangeJson[0]),
      y.createRelativePositionFromJSON(rangeJson[1]),
    ]
    const start = y.createAbsolutePositionFromRelativePosition(range[0], this.doc)?.index
    const end = y.createAbsolutePositionFromRelativePosition(range[1], this.doc)?.index
    const idMapData = this.idMap.toJSON()
    if (start == null || end == null) return
    this.doc.transact(() => {
      this.contents.delete(start, end - start)
      this.contents.insert(start, content)
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
  start: y.RelativePosition
  end: y.RelativePosition
}

/**
 * Accessor for the ID map stored in shared yjs map as relative ranges. Synchronizes the ranges
 * that were accessed during parsing, throws away stale ones. The text contents is used to translate
 * the relative ranges to absolute ranges, but it is not modified.
 */
export class IdMap {
  private contents: y.Text
  private doc: y.Doc
  private yMap: y.Map<[any, any]>
  private rangeToExpr: Map<string, ExprId>
  private accessed: Set<ExprId>
  private finished: boolean

  constructor(yMap: y.Map<[any, any]>, contents: y.Text) {
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
    const relStart = y.createRelativePositionFromJSON(range[0])
    const relEnd = y.createRelativePositionFromJSON(range[1])
    const start = y.createAbsolutePositionFromRelativePosition(relStart, this.doc)
    const end = y.createAbsolutePositionFromRelativePosition(relEnd, this.doc)
    if (start == null || end == null) return null
    return [start.index, end.index]
  }

  getOrInsertUniqueId(range: [number, number]): ExprId {
    if (this.finished) {
      throw new Error('IdMap already finished')
    }
    if (range[0] === 0 && range[1] == 1) {
      debugger
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
        // for all remaining expressions, we need to write them into the map
        if (!this.accessed.has(expr)) return
        const range = key.split(':').map((x) => parseInt(x, 10)) as [number, number]
        const start = y.createRelativePositionFromTypeIndex(this.contents, range[0])
        const end = y.createRelativePositionFromTypeIndex(this.contents, range[1])
        const startJson = y.relativePositionToJSON(start)
        const endJson = y.relativePositionToJSON(end)
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
