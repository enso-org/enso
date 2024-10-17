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
  width: number | null
  height: number | null
}

/** TODO: Add docs */
export function visMetadataEquals(
  a: VisualizationMetadata | null | undefined,
  b: VisualizationMetadata | null | undefined,
) {
  return (
    (!a && !b) ||
    (a &&
      b &&
      a.visible === b.visible &&
      a.width == b.width &&
      a.height == b.height &&
      visIdentifierEquals(a.identifier, b.identifier))
  )
}

/** TODO: Add docs */
export function visIdentifierEquals(
  a: VisualizationIdentifier | null | undefined,
  b: VisualizationIdentifier | null | undefined,
) {
  return (!a && !b) || (a && b && a.name === b.name && object.equalFlat(a.module, b.module))
}

export type ProjectSetting = string

/** TODO: Add docs */
export class DistributedProject {
  doc: Y.Doc
  name: Y.Text
  modules: Y.Map<Y.Doc>
  settings: Y.Map<ProjectSetting>

  /** TODO: Add docs */
  constructor(doc: Y.Doc) {
    this.doc = doc
    this.name = this.doc.getText('name')
    this.modules = this.doc.getMap('modules')
    this.settings = this.doc.getMap('settings')
  }

  /** TODO: Add docs */
  moduleNames(): string[] {
    return Array.from(this.modules.keys())
  }

  /** TODO: Add docs */
  findModuleByDocId(id: string): string | null {
    for (const [name, doc] of this.modules.entries()) {
      if (doc.guid === id) return name
    }
    return null
  }

  /** TODO: Add docs */
  async openModule(name: string): Promise<DistributedModule | null> {
    const doc = this.modules.get(name)
    if (doc == null) return null
    return await DistributedModule.load(doc)
  }

  /** TODO: Add docs */
  openUnloadedModule(name: string): DistributedModule | null {
    const doc = this.modules.get(name)
    if (doc == null) return null
    return new DistributedModule(doc)
  }

  /** TODO: Add docs */
  createUnloadedModule(name: string, doc: Y.Doc): DistributedModule {
    this.modules.set(name, doc)
    return new DistributedModule(doc)
  }

  /** TODO: Add docs */
  createNewModule(name: string): DistributedModule {
    return this.createUnloadedModule(name, new Y.Doc())
  }

  /** TODO: Add docs */
  deleteModule(name: string): void {
    this.modules.delete(name)
  }

  /** TODO: Add docs */
  dispose(): void {
    this.doc.destroy()
  }
}

/** TODO: Add docs */
export class ModuleDoc {
  ydoc: Y.Doc
  nodes: Y.Map<any>
  /** TODO: Add docs */
  constructor(ydoc: Y.Doc) {
    this.ydoc = ydoc
    this.nodes = ydoc.getMap('nodes')
  }
}

/** TODO: Add docs */
export class DistributedModule {
  doc: ModuleDoc
  undoManager: Y.UndoManager

  /** TODO: Add docs */
  static async load(ydoc: Y.Doc): Promise<DistributedModule> {
    ydoc.load()
    await ydoc.whenLoaded
    return new DistributedModule(ydoc)
  }

  /** TODO: Add docs */
  constructor(ydoc: Y.Doc) {
    this.doc = new ModuleDoc(ydoc)
    this.undoManager = new Y.UndoManager([this.doc.nodes])
  }

  /** TODO: Add docs */
  dispose(): void {
    this.doc.ydoc.destroy()
  }
}

export const localUserActionOrigins = ['local:userAction', 'local:userAction:CodeEditor'] as const
export type LocalUserActionOrigin = (typeof localUserActionOrigins)[number]
export type Origin = LocalUserActionOrigin | 'remote' | 'local:autoLayout'
/** Locally-originated changes not otherwise specified. */
export const defaultLocalOrigin: LocalUserActionOrigin = 'local:userAction'
/** TODO: Add docs */
export function isLocalUserActionOrigin(origin: string): origin is LocalUserActionOrigin {
  const localOriginNames: readonly string[] = localUserActionOrigins
  return localOriginNames.includes(origin)
}
/** TODO: Add docs */
export function tryAsOrigin(origin: string): Origin | undefined {
  if (isLocalUserActionOrigin(origin)) return origin
  if (origin === 'local:autoLayout') return origin
  if (origin === 'remote') return origin
}

export type SourceRange = readonly [start: number, end: number]
declare const brandSourceRangeKey: unique symbol
export type SourceRangeKey = string & { [brandSourceRangeKey]: never }

/** TODO: Add docs */
export function sourceRangeKey(range: SourceRange): SourceRangeKey {
  return `${range[0].toString(16)}:${range[1].toString(16)}` as SourceRangeKey
}
/** TODO: Add docs */
export function sourceRangeFromKey(key: SourceRangeKey): SourceRange {
  return key.split(':').map(x => parseInt(x, 16)) as [number, number]
}

/** TODO: Add docs */
export class IdMap {
  private readonly rangeToExpr: Map<string, ExternalId>

  /** TODO: Add docs */
  constructor(entries?: [string, ExternalId][]) {
    this.rangeToExpr = new Map(entries ?? [])
  }

  /** TODO: Add docs */
  static Mock(): IdMap {
    return new IdMap([])
  }

  /** TODO: Add docs */
  insertKnownId(range: SourceRange, id: ExternalId) {
    const key = sourceRangeKey(range)
    this.rangeToExpr.set(key, id)
  }

  /** TODO: Add docs */
  getIfExist(range: SourceRange): ExternalId | undefined {
    const key = sourceRangeKey(range)
    return this.rangeToExpr.get(key)
  }

  /** TODO: Add docs */
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

  /** TODO: Add docs */
  entries(): [SourceRangeKey, ExternalId][] {
    return [...this.rangeToExpr] as [SourceRangeKey, ExternalId][]
  }

  /** TODO: Add docs */
  get size(): number {
    return this.rangeToExpr.size
  }

  /** TODO: Add docs */
  clear(): void {
    this.rangeToExpr.clear()
  }

  /** TODO: Add docs */
  isEqual(other: IdMap): boolean {
    if (other.size !== this.size) return false
    for (const [key, value] of this.rangeToExpr.entries()) {
      const oldValue = other.rangeToExpr.get(key)
      if (oldValue !== value) return false
    }
    return true
  }

  /** TODO: Add docs */
  validate() {
    const uniqueValues = new Set(this.rangeToExpr.values())
    if (uniqueValues.size < this.rangeToExpr.size) {
      console.warn(`Duplicate UUID in IdMap`)
    }
  }

  /** TODO: Add docs */
  clone(): IdMap {
    return new IdMap(this.entries())
  }

  // Debugging.
  /** TODO: Add docs */
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
/** TODO: Add docs */
export function isUuid(x: unknown): x is Uuid {
  return typeof x === 'string' && x.length === 36 && uuidRegex.test(x)
}

/** TODO: Add docs */
export function rangeEquals(a: SourceRange, b: SourceRange): boolean {
  return a[0] == b[0] && a[1] == b[1]
}

/** TODO: Add docs */
export function rangeIncludes(a: SourceRange, b: number): boolean {
  return a[0] <= b && a[1] >= b
}

/** TODO: Add docs */
export function rangeLength(a: SourceRange): number {
  return a[1] - a[0]
}

/** TODO: Add docs */
export function rangeEncloses(a: SourceRange, b: SourceRange): boolean {
  return a[0] <= b[0] && a[1] >= b[1]
}

/** TODO: Add docs */
export function rangeIntersects(a: SourceRange, b: SourceRange): boolean {
  return a[0] <= b[1] && a[1] >= b[0]
}

/** Whether the given range is before the other range. */
export function rangeIsBefore(a: SourceRange, b: SourceRange): boolean {
  return a[1] <= b[0]
}
