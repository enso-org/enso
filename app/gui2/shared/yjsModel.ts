import * as decoding from 'lib0/decoding'
import * as encoding from 'lib0/encoding'
import * as object from 'lib0/object'
import * as random from 'lib0/random'
import * as Y from 'yjs'

export type Uuid = `${string}-${string}-${string}-${string}-${string}`
declare const brandExprId: unique symbol
export type ExprId = Uuid & { [brandExprId]: never }
export const NULL_EXPR_ID: ExprId = '00000000-0000-0000-0000-000000000000' as ExprId

export type VisualizationModule =
  | { kind: 'Builtin' }
  | { kind: 'CurrentProject' }
  | { kind: 'Library'; name: string }

export interface VisualizationIdentifier {
  module: VisualizationModule
  name: string
}

export interface VisualizationMetadata extends VisualizationIdentifier {
  visible: boolean
}

export function visMetadataEquals(
  a: VisualizationMetadata | null | undefined,
  b: VisualizationMetadata | null | undefined,
) {
  return (
    (a == null && b == null) ||
    (a != null && b != null && a.visible === b.visible && visIdentifierEquals(a, b))
  )
}

export function visIdentifierEquals(a: VisualizationIdentifier, b: VisualizationIdentifier) {
  return a.name === b.name && object.equalFlat(a.module, b.module)
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

  insertNewNode(offset: number, pattern: string, expression: string, meta: NodeMetadata): ExprId {
    // Spaces at the beginning are needed to place the new node in scope of the `main` function with proper indentation.
    const lhs = `    ${pattern} = `
    const content = lhs + expression
    const range = [offset + lhs.length, offset + content.length] as const
    const newId = random.uuidv4() as ExprId
    this.transact(() => {
      this.doc.contents.insert(offset, content + '\n')
      const start = Y.createRelativePositionFromTypeIndex(this.doc.contents, range[0], -1)
      const end = Y.createRelativePositionFromTypeIndex(this.doc.contents, range[1])
      this.doc.idMap.set(newId, encodeRange([start, end]))
      this.doc.metadata.set(newId, meta)
    })
    return newId
  }

  deleteExpression(id: ExprId): void {
    const rangeBuffer = this.doc.idMap.get(id)
    if (rangeBuffer == null) return
    const [relStart, relEnd] = decodeRange(rangeBuffer)
    const start = Y.createAbsolutePositionFromRelativePosition(relStart, this.doc.ydoc)?.index
    const end = Y.createAbsolutePositionFromRelativePosition(relEnd, this.doc.ydoc)?.index
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
    const exprStart = Y.createAbsolutePositionFromRelativePosition(relStart, this.doc.ydoc)?.index
    const exprEnd = Y.createAbsolutePositionFromRelativePosition(relEnd, this.doc.ydoc)?.index
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
    this.transact(() => {
      this.doc.metadata.set(id, { ...existing, ...meta })
    })
  }

  getIdMap(): IdMap {
    return new IdMap(this.doc.idMap, this.doc.contents)
  }

  dispose(): void {
    this.doc.ydoc.destroy()
  }
}

export type RelativeRange = [Y.RelativePosition, Y.RelativePosition]

/**
 * Accessor for the ID map stored in shared yjs map as relative ranges. Synchronizes the ranges
 * that were accessed during parsing, throws away stale ones. The text contents is used to translate
 * the relative ranges to absolute ranges, but it is not modified.
 */
export class IdMap {
  private contents: Y.Text
  private doc: Y.Doc
  private yMap: Y.Map<Uint8Array>
  private rangeToExpr: Map<string, ExprId>
  private accessed: Set<ExprId>
  private finished: boolean

  constructor(yMap: Y.Map<Uint8Array>, contents: Y.Text) {
    if (yMap.doc == null) {
      throw new Error('IdMap must be associated with a document')
    }
    this.doc = yMap.doc
    this.contents = contents
    this.yMap = yMap
    this.rangeToExpr = new Map()
    this.accessed = new Set()

    yMap.forEach((rangeBuffer, expr) => {
      if (!(isUuid(expr) && rangeBuffer instanceof Uint8Array)) return
      const indices = this.modelToIndices(rangeBuffer)
      if (indices == null) return
      this.rangeToExpr.set(IdMap.keyForRange(indices), expr as ExprId)
    })

    this.finished = false
  }

  public static keyForRange(range: readonly [number, number]): string {
    return `${range[0].toString(16)}:${range[1].toString(16)}`
  }

  public static rangeForKey(key: string): [number, number] {
    return key.split(':').map((x) => parseInt(x, 16)) as [number, number]
  }

  private modelToIndices(rangeBuffer: Uint8Array): [number, number] | null {
    const [relStart, relEnd] = decodeRange(rangeBuffer)
    const start = Y.createAbsolutePositionFromRelativePosition(relStart, this.doc)
    const end = Y.createAbsolutePositionFromRelativePosition(relEnd, this.doc)
    if (start == null || end == null) return null
    return [start.index, end.index]
  }

  insertKnownId(range: [number, number], id: ExprId) {
    if (this.finished) {
      throw new Error('IdMap already finished')
    }

    const key = IdMap.keyForRange(range)
    this.rangeToExpr.set(key, id)
    this.accessed.add(id)
  }

  getIfExist(range: readonly [number, number]): ExprId | undefined {
    const key = IdMap.keyForRange(range)
    return this.rangeToExpr.get(key)
  }

  getOrInsertUniqueId(range: readonly [number, number]): ExprId {
    if (this.finished) {
      throw new Error('IdMap already finished')
    }

    const key = IdMap.keyForRange(range)
    const val = this.rangeToExpr.get(key)
    if (val !== undefined) {
      this.accessed.add(val)
      return val
    } else {
      const newId = random.uuidv4() as ExprId
      this.rangeToExpr.set(key, newId)
      this.accessed.add(newId)
      return newId
    }
  }

  accessedSoFar(): ReadonlySet<ExprId> {
    return this.accessed
  }

  toRawRanges(): Record<string, [number, number]> {
    const ranges: Record<string, [number, number]> = {}
    for (const [key, expr] of this.rangeToExpr.entries()) {
      ranges[expr] = IdMap.rangeForKey(key)
    }
    return ranges
  }

  /**
   * Finish accessing or modifying ID map. Synchronizes the accessed keys back to the shared map,
   * removes keys that were present previously, but were not accessed.
   *
   * Can be called at most once. After calling this method, the ID map is no longer usable.
   */
  finishAndSynchronize(): typeof this.yMap {
    if (this.finished) {
      throw new Error('IdMap already finished')
    }
    this.finished = true

    const doc = this.doc

    doc.transact(() => {
      this.yMap.forEach((_, expr) => {
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
        const range = IdMap.rangeForKey(key)
        const start = Y.createRelativePositionFromTypeIndex(this.contents, range[0], -1)
        const end = Y.createRelativePositionFromTypeIndex(this.contents, range[1])
        const encoded = encodeRange([start, end])
        this.yMap.set(expr, encoded)
      })
    })
    return this.yMap
  }
}

function encodeRange(range: RelativeRange): Uint8Array {
  const encoder = encoding.createEncoder()
  const start = Y.encodeRelativePosition(range[0])
  const end = Y.encodeRelativePosition(range[1])
  encoding.writeUint8(encoder, start.length)
  encoding.writeUint8Array(encoder, start)
  encoding.writeUint8(encoder, end.length)
  encoding.writeUint8Array(encoder, end)
  return encoding.toUint8Array(encoder)
}

export function decodeRange(buffer: Uint8Array): RelativeRange {
  const decoder = decoding.createDecoder(buffer)
  const startLen = decoding.readUint8(decoder)
  const start = decoding.readUint8Array(decoder, startLen)
  const endLen = decoding.readUint8(decoder)
  const end = decoding.readUint8Array(decoder, endLen)
  return [Y.decodeRelativePosition(start), Y.decodeRelativePosition(end)]
}

const uuidRegex = /^[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/
export function isUuid(x: unknown): x is Uuid {
  return typeof x === 'string' && x.length === 36 && uuidRegex.test(x)
}

/** A range represented as start and end indices. */
export type ContentRange = [number, number]

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
