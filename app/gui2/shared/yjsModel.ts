import * as object from 'lib0/object'
import * as random from 'lib0/random'
import * as Y from 'yjs'

export type Uuid = `${string}-${string}-${string}-${string}-${string}`

declare const brandExternalId: unique symbol
/** Identifies an AST node or token. Used in module serialization and communication with the language server. */
export type ExternalId = Uuid & { [brandExternalId]: never }

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

export class ModuleDoc {
  ydoc: Y.Doc
  nodes: Y.Map<any>
  constructor(ydoc: Y.Doc) {
    this.ydoc = ydoc
    this.nodes = ydoc.getMap('nodes')
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
    this.undoManager = new Y.UndoManager([this.doc.nodes])
  }

  transact<T>(fn: () => T): T {
    return this.doc.ydoc.transact(fn, 'local')
  }

  dispose(): void {
    this.doc.ydoc.destroy()
  }
}

export type SourceRange = readonly [start: number, end: number]
declare const brandSourceRangeKey: unique symbol
export type SourceRangeKey = string & { [brandSourceRangeKey]: never }

export function sourceRangeKey(range: SourceRange): SourceRangeKey {
  return `${range[0].toString(16)}:${range[1].toString(16)}` as SourceRangeKey
}
export function sourceRangeFromKey(key: SourceRangeKey): SourceRange {
  return key.split(':').map((x) => parseInt(x, 16)) as [number, number]
}

export class IdMap {
  private readonly rangeToExpr: Map<string, ExternalId>

  constructor(entries?: [string, ExternalId][]) {
    this.rangeToExpr = new Map(entries ?? [])
  }

  static Mock(): IdMap {
    return new IdMap([])
  }

  insertKnownId(range: SourceRange, id: ExternalId) {
    const key = sourceRangeKey(range)
    this.rangeToExpr.set(key, id)
  }

  getIfExist(range: SourceRange): ExternalId | undefined {
    const key = sourceRangeKey(range)
    return this.rangeToExpr.get(key)
  }

  getOrInsertUniqueId(range: SourceRange): ExternalId {
    const key = sourceRangeKey(range)
    const val = this.rangeToExpr.get(key)
    if (val !== undefined) {
      return val
    } else {
      const newId = random.uuidv4() as ExternalId
      this.rangeToExpr.set(key, newId)
      return newId
    }
  }

  entries(): [SourceRangeKey, ExternalId][] {
    return [...this.rangeToExpr] as [SourceRangeKey, ExternalId][]
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

  validate() {
    const uniqueValues = new Set(this.rangeToExpr.values())
    if (uniqueValues.size < this.rangeToExpr.size) {
      console.warn(`Duplicate UUID in IdMap`)
    }
  }

  clone(): IdMap {
    return new IdMap(this.entries())
  }

  // Debugging.
  compare(other: IdMap) {
    console.info(`IdMap.compare -------`)
    const allKeys = new Set<string>()
    for (const key of this.rangeToExpr.keys()) allKeys.add(key)
    for (const key of other.rangeToExpr.keys()) allKeys.add(key)
    for (const key of allKeys) {
      const mine = this.rangeToExpr.get(key)
      const yours = other.rangeToExpr.get(key)
      if (mine !== yours) {
        console.info(`IdMap.compare[${key}]: ${mine} -> ${yours}`)
      }
    }
  }
}

const uuidRegex = /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/
export function isUuid(x: unknown): x is Uuid {
  return typeof x === 'string' && x.length === 36 && uuidRegex.test(x)
}

export function rangeEquals(a: SourceRange, b: SourceRange): boolean {
  return a[0] == b[0] && a[1] == b[1]
}

export function rangeEncloses(a: SourceRange, b: SourceRange): boolean {
  return a[0] <= b[0] && a[1] >= b[1]
}

export function rangeIntersects(a: SourceRange, b: SourceRange): boolean {
  return a[0] <= b[1] && a[1] >= b[0]
}

/** Whether the given range is before the other range. */
export function rangeIsBefore(a: SourceRange, b: SourceRange): boolean {
  return a[1] <= b[0]
}
