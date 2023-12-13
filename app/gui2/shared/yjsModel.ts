import * as decoding from 'lib0/decoding'
import * as encoding from 'lib0/encoding'
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
  contents: Y.Text
  idMap: Y.Map<Uint8Array>
  metadata: Y.Map<NodeMetadata>
  constructor(ydoc: Y.Doc) {
    this.ydoc = ydoc
    this.contents = ydoc.getText('contents')
    this.idMap = ydoc.getMap('idMap')
    this.metadata = ydoc.getMap('metadata')
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
    this.undoManager = new Y.UndoManager([this.doc.contents, this.doc.idMap, this.doc.metadata])
  }

  insertNewNode(
    offset: number,
    pattern: string,
    expression: string,
    meta: NodeMetadata,
    withImport?: { str: string; offset: number },
  ): ExprId {
    // Spaces at the beginning are needed to place the new node in scope of the `main` function with proper indentation.
    const lhs = `    ${pattern} = `
    const content = lhs + expression
    const range = [offset + lhs.length, offset + content.length] as const
    const newId = random.uuidv4() as ExprId
    this.transact(() => {
      if (withImport) {
        this.doc.contents.insert(withImport.offset, withImport.str + '\n')
      }
      this.doc.contents.insert(offset, content + '\n')
      const start = range[0]
      const end = range[1]
      this.doc.idMap.set(newId, encodeRange([start, end]))
      this.doc.metadata.set(newId, meta)
    })
    return newId
  }

  deleteExpression(id: ExprId): void {
    const rangeBuffer = this.doc.idMap.get(id)
    if (rangeBuffer == null) return
    const [relStart, relEnd] = decodeRange(rangeBuffer)
    const start = relStart
    const end = relEnd
    if (start == null || end == null) return
    this.transact(() => {
      this.doc.idMap.delete(id)
      this.doc.metadata.delete(id)
      this.doc.contents.delete(start, end - start)
    })
  }

  replaceExpressionContent(id: ExprId, content: string, range?: ContentRange): void {
    const rangeBuffer = this.doc.idMap.get(id)
    if (rangeBuffer == null) return
    const [relStart, relEnd] = decodeRange(rangeBuffer)
    const exprStart = relStart
    const exprEnd = relEnd
    if (exprStart == null || exprEnd == null) return
    const start = range == null ? exprStart : exprStart + range[0]
    const end = range == null ? exprEnd : exprStart + range[1]
    if (start > end) throw new Error('Invalid range')
    if (start < exprStart || end > exprEnd)
      throw new Error(
        `Range out of bounds. Got [${start}, ${end}], bounds are [${exprStart}, ${exprEnd}]`,
      )
    this.transact(() => {
      if (content.length > 0) {
        this.doc.contents.insert(start, content)
      }
      if (start !== end) {
        this.doc.contents.delete(start + content.length, end - start)
      }
    })
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
    return new IdMap(this.doc.idMap)
  }

  dispose(): void {
    this.doc.ydoc.destroy()
  }
}

export type RelativeRange = [number, number]

export class IdMap {
  private yMap: Y.Map<Uint8Array>
  private rangeToExpr: Map<string, ExprId>

  constructor(yMap: Y.Map<Uint8Array>) {
    this.yMap = yMap
    this.rangeToExpr = new Map()

    yMap.forEach((rangeBuffer, expr) => {
      if (!(isUuid(expr) && rangeBuffer instanceof Uint8Array)) return
      const indices = this.modelToIndices(rangeBuffer)
      if (indices == null) return
      const key = IdMap.keyForRange(indices)
      if (!this.rangeToExpr.has(key)) {
        this.rangeToExpr.set(key, expr as ExprId)
      }
    })
  }

  static Mock(): IdMap {
    const doc = new Y.Doc()
    const map = doc.getMap<Uint8Array>('idMap')
    return new IdMap(map)
  }

  public static keyForRange(range: readonly [number, number]): string {
    return `${range[0].toString(16)}:${range[1].toString(16)}`
  }

  public static rangeForKey(key: string): [number, number] {
    return key.split(':').map((x) => parseInt(x, 16)) as [number, number]
  }

  private modelToIndices(rangeBuffer: Uint8Array): [number, number] | null {
    const [relStart, relEnd] = decodeRange(rangeBuffer)
    const start = relStart
    const end = relEnd
    if (start == null || end == null) return null
    return [start, end]
  }

  insertKnownId(range: [number, number], id: ExprId) {
    const key = IdMap.keyForRange(range)
    this.rangeToExpr.set(key, id)
  }

  getIfExist(range: readonly [number, number]): ExprId | undefined {
    const key = IdMap.keyForRange(range)
    return this.rangeToExpr.get(key)
  }

  getOrInsertUniqueId(range: readonly [number, number]): ExprId {
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

  accessedSoFar(): ReadonlySet<ExprId> {
    return new Set()
  }

  toRawRanges(): Record<string, [number, number]> {
    const ranges: Record<string, [number, number]> = {}
    for (const [key, expr] of this.rangeToExpr.entries()) {
      ranges[expr] = IdMap.rangeForKey(key)
    }
    return ranges
  }

  finishAndSynchronize(): typeof this.yMap {
    return this.yMap
  }
}

function encodeRange(range: RelativeRange): Uint8Array {
  const encoder = encoding.createEncoder()
  const start = range[0]
  const end = range[1]
  encoding.writeUint32(encoder, start)
  encoding.writeUint32(encoder, end)
  return encoding.toUint8Array(encoder)
}

export function decodeRange(buffer: Uint8Array): RelativeRange {
  const decoder = decoding.createDecoder(buffer)
  const start = decoding.readUint32(decoder)
  const end = decoding.readUint32(decoder)
  return [start, end]
}

const uuidRegex = /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/
export function isUuid(x: unknown): x is Uuid {
  return typeof x === 'string' && x.length === 36 && uuidRegex.test(x)
}

/** A range represented as start and end indices. */
export type ContentRange = [number, number]

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

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest
  type RangeTest = { a: ContentRange; b: ContentRange }

  const equalRanges: RangeTest[] = [
    { a: [0, 0], b: [0, 0] },
    { a: [0, 1], b: [0, 1] },
    { a: [-5, 5], b: [-5, 5] },
  ]

  const totalOverlap: RangeTest[] = [
    { a: [0, 1], b: [0, 0] },
    { a: [0, 2], b: [2, 2] },
    { a: [-1, 1], b: [1, 1] },
    { a: [0, 2], b: [0, 1] },
    { a: [-10, 10], b: [-3, 7] },
    { a: [0, 5], b: [1, 2] },
    { a: [3, 5], b: [3, 4] },
  ]

  const reverseTotalOverlap: RangeTest[] = totalOverlap.map(({ a, b }) => ({ a: b, b: a }))

  const noOverlap: RangeTest[] = [
    { a: [0, 1], b: [2, 3] },
    { a: [0, 1], b: [-1, -1] },
    { a: [5, 6], b: [2, 3] },
    { a: [0, 2], b: [-2, -1] },
    { a: [-5, -3], b: [9, 10] },
    { a: [-3, 2], b: [3, 4] },
  ]

  const partialOverlap: RangeTest[] = [
    { a: [0, 3], b: [-1, 1] },
    { a: [0, 1], b: [-1, 0] },
    { a: [0, 0], b: [-1, 0] },
    { a: [0, 2], b: [1, 4] },
    { a: [-8, 0], b: [0, 10] },
  ]

  test.each([...equalRanges, ...totalOverlap])('Range $a should enclose $b', ({ a, b }) =>
    expect(rangeEncloses(a, b)).toBe(true),
  )
  test.each([...noOverlap, ...partialOverlap, ...reverseTotalOverlap])(
    'Range $a should not enclose $b',
    ({ a, b }) => expect(rangeEncloses(a, b)).toBe(false),
  )
  test.each([...equalRanges, ...totalOverlap, ...reverseTotalOverlap, ...partialOverlap])(
    'Range $a should intersect $b',
    ({ a, b }) => expect(rangeIntersects(a, b)).toBe(true),
  )
  test.each([...noOverlap])('Range $a should not intersect $b', ({ a, b }) =>
    expect(rangeIntersects(a, b)).toBe(false),
  )
}
