import * as object from 'lib0/object'
import * as random from 'lib0/random'
import * as Y from 'yjs'

export type Uuid = `${string}-${string}-${string}-${string}-${string}`
declare const brandExprId: unique symbol
export type ExprId = Uuid & { [brandExprId]: never }

export type VisualizationModule =
  | { kind: 'Builtin' }
  | { kind: 'CurrentProject' }
  | { kind: 'Library'; name: string }

export interface VisualizationIdentifier {
  module: VisualizationModule
  name: string
}

export interface VisualizationMetadata {
  identifier: VisualizationIdentifier | null
  visible: boolean
}

export function visMetadataEquals(
  a: VisualizationMetadata | null | undefined,
  b: VisualizationMetadata | null | undefined,
) {
  return (
    (!a && !b) ||
    (a && b && a.visible === b.visible && visIdentifierEquals(a.identifier, b.identifier))
  )
}

export function visIdentifierEquals(
  a: VisualizationIdentifier | null | undefined,
  b: VisualizationIdentifier | null | undefined,
) {
  return (!a && !b) || (a && b && a.name === b.name && object.equalFlat(a.module, b.module))
}

export type ProjectSetting = string

export class DistributedProject {
  doc: Y.Doc
  name: Y.Text
  modules: Y.Map<Y.Doc>
  settings: Y.Map<ProjectSetting>

  constructor(doc: Y.Doc) {
    this.doc = doc
    this.name = this.doc.getText('name')
    this.modules = this.doc.getMap('modules')
    this.settings = this.doc.getMap('settings')
  }

  moduleNames(): string[] {
    return Array.from(this.modules.keys())
  }

  findModuleByDocId(id: string): string | null {
    for (const [name, doc] of this.modules.entries()) {
      if (doc.guid === id) return name
    }
    return null
  }

  async openModule(name: string): Promise<DistributedModule | null> {
    const doc = this.modules.get(name)
    if (doc == null) return null
    return await DistributedModule.load(doc)
  }

  openUnloadedModule(name: string): DistributedModule | null {
    const doc = this.modules.get(name)
    if (doc == null) return null
    return new DistributedModule(doc)
  }

  createUnloadedModule(name: string, doc: Y.Doc): DistributedModule {
    this.modules.set(name, doc)
    return new DistributedModule(doc)
  }

  createNewModule(name: string): DistributedModule {
    return this.createUnloadedModule(name, new Y.Doc())
  }

  deleteModule(name: string): void {
    this.modules.delete(name)
  }

  dispose(): void {
    this.doc.destroy()
  }
}

export interface NodeMetadata {
  x: number
  y: number
  vis: VisualizationMetadata | null
}

export class ModuleDoc {
  ydoc: Y.Doc
  metadata: Y.Map<NodeMetadata>
  data: Y.Map<any>
  constructor(ydoc: Y.Doc) {
    this.ydoc = ydoc
    this.metadata = ydoc.getMap('metadata')
    this.data = ydoc.getMap('data')
  }

  setIdMap(map: IdMap) {
    const oldMap = new IdMap(this.data.get('idmap') ?? [])
    if (oldMap.isEqual(map)) return
    this.data.set('idmap', map.entries())
  }

  getIdMap(): IdMap {
    const map = this.data.get('idmap')
    return new IdMap(map ?? [])
  }

  setCode(code: string) {
    this.data.set('code', code)
  }

  getCode(): string {
    return this.data.get('code') ?? ''
  }
}

export class DistributedModule {
  doc: ModuleDoc
  undoManager: Y.UndoManager

  static async load(ydoc: Y.Doc): Promise<DistributedModule> {
    ydoc.load()
    await ydoc.whenLoaded
    return new DistributedModule(ydoc)
  }

  constructor(ydoc: Y.Doc) {
    this.doc = new ModuleDoc(ydoc)
    this.undoManager = new Y.UndoManager([this.doc.data, this.doc.metadata])
  }

  transact<T>(fn: () => T): T {
    return this.doc.ydoc.transact(fn, 'local')
  }

  updateNodeMetadata(id: ExprId, meta: Partial<NodeMetadata>): void {
    const existing = this.doc.metadata.get(id) ?? { x: 0, y: 0, vis: null }
    this.transact(() => this.doc.metadata.set(id, { ...existing, ...meta }))
  }

  getNodeMetadata(id: ExprId): NodeMetadata | null {
    return this.doc.metadata.get(id) ?? null
  }

  getIdMap(): IdMap {
    return new IdMap(this.doc.data.get('idmap') ?? [])
  }

  dispose(): void {
    this.doc.ydoc.destroy()
  }
}

export type SourceRange = readonly [start: number, end: number]
export type RelativeRange = readonly [start: number, end: number]

export class IdMap {
  private readonly rangeToExpr: Map<string, ExprId>

  constructor(entries?: [string, ExprId][]) {
    this.rangeToExpr = new Map(entries ?? [])
  }

  static Mock(): IdMap {
    return new IdMap([])
  }

  public static keyForRange(range: SourceRange): string {
    return `${range[0].toString(16)}:${range[1].toString(16)}`
  }

  public static rangeForKey(key: string): SourceRange {
    return key.split(':').map((x) => parseInt(x, 16)) as [number, number]
  }

  insertKnownId(range: SourceRange, id: ExprId) {
    const key = IdMap.keyForRange(range)
    this.rangeToExpr.set(key, id)
  }

  getIfExist(range: SourceRange): ExprId | undefined {
    const key = IdMap.keyForRange(range)
    return this.rangeToExpr.get(key)
  }

  getOrInsertUniqueId(range: SourceRange): ExprId {
    const key = IdMap.keyForRange(range)
    const val = this.rangeToExpr.get(key)
    if (val !== undefined) {
      return val
    } else {
      const newId = random.uuidv4() as ExprId
      this.rangeToExpr.set(key, newId)
      return newId
    }
  }

  entries(): [string, ExprId][] {
    return [...this.rangeToExpr]
  }

  get size(): number {
    return this.rangeToExpr.size
  }

  clear(): void {
    this.rangeToExpr.clear()
  }

  isEqual(other: IdMap): boolean {
    if (other.size !== this.size) return false
    for (const [key, value] of this.rangeToExpr.entries()) {
      const oldValue = other.rangeToExpr.get(key)
      if (oldValue !== value) return false
    }
    return true
  }
}

const uuidRegex = /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/
export function isUuid(x: unknown): x is Uuid {
  return typeof x === 'string' && x.length === 36 && uuidRegex.test(x)
}

/** A range represented as start and end indices. */
export type ContentRange = [start: number, end: number]

export function rangeEquals(a: ContentRange, b: ContentRange): boolean {
  return a[0] == b[0] && a[1] == b[1]
}

export function rangeEncloses(a: ContentRange, b: ContentRange): boolean {
  return a[0] <= b[0] && a[1] >= b[1]
}

export function rangeIntersects(a: ContentRange, b: ContentRange): boolean {
  return a[0] <= b[1] && a[1] >= b[0]
}

/** Whether the given range is before the other range. */
export function rangeIsBefore(a: ContentRange, b: ContentRange): boolean {
  return a[1] <= b[0]
}
