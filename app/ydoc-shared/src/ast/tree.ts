// Declaration-merging is used to implement mixin types in this file.
/* eslint-disable @typescript-eslint/no-unsafe-declaration-merging */
import * as Y from 'yjs'
import { assert, assertDefined, assertEqual, bail } from '../util/assert'
import type { Result } from '../util/data/result'
import { Err, Ok } from '../util/data/result'
import type { SourceRangeEdit } from '../util/data/text'
import { allKeys } from '../util/types'
import type { ExternalId, VisualizationMetadata } from '../yjsModel'
import { visMetadataEquals } from '../yjsModel'
import { docLineToConcrete, functionDocsToConcrete } from './documentation'
import { is_numeric_literal } from './ffi'
import type { SpanMap } from './idMap'
import { newExternalId } from './idMap'
import type { Module } from './mutableModule'
import { MutableModule, ROOT_ID } from './mutableModule'
import { parseExpression, parseStatement } from './parse'
import type { RawConcreteChild } from './print'
import {
  ensureSpaced,
  ensureSpacedOnlyIf,
  ensureUnspaced,
  firstChild,
  preferSpaced,
  preferSpacedIf,
  preferUnspaced,
  printAst,
  printWithSpans,
  spaced,
  unspaced,
  withWhitespace,
} from './print'
import { applyTextEditsToAst, syncToCode } from './syncToCode'
import { escapeTextLiteral } from './text'
import type {
  Identifier,
  IdentifierOrOperatorIdentifier,
  IdentifierOrOperatorIdentifierToken,
  IdentifierToken,
  SyncTokenId,
  TypeOrConstructorIdentifier,
} from './token'
import { Token, TokenType, isIdentifier, isToken, isTokenChild, isTokenId } from './token'

// This is used to ensure immutability of data owned by Yjs, which is important and is difficult to enforce without this
// type-level help. When constructing a new value for a field, it is tempting to `get` the old value, mutate it, and
// `set` the new value. Even in this case, when the data seems about to be overwritten, it must not be mutated because
// Yjs *shares data between documents*. Thus, a mutating update is likely to "work" as far as the edited module is
// concerned, but cause internal inconsistency in the base module.
export type DeepReadonly<T> =
  T extends Builtin ? T
  : T extends FixedMap<infer V> ? FixedMapView<V>
  : T extends Map<infer K, infer V> ? ReadonlyMap<DeepReadonly<K>, DeepReadonly<V>>
  : T extends ReadonlyMap<infer K, infer V> ? ReadonlyMap<DeepReadonly<K>, DeepReadonly<V>>
  : T extends WeakMap<infer K, infer V> ? WeakMap<DeepReadonly<K>, DeepReadonly<V>>
  : T extends Set<infer U> ? ReadonlySet<DeepReadonly<U>>
  : T extends ReadonlySet<infer U> ? ReadonlySet<DeepReadonly<U>>
  : T extends WeakSet<infer U> ? WeakSet<DeepReadonly<U>>
  : T extends Promise<infer U> ? Promise<DeepReadonly<U>>
  : T extends object ? { readonly [K in keyof T]: DeepReadonly<T[K]> }
  : Readonly<T>
type Primitive = string | number | boolean | bigint | symbol | undefined | null
type AnyFunction = () => void
type Builtin = Primitive | AnyFunction | Date | Error | RegExp
// Note that typescript doesn't consider this assignable to `DeepReadonly<T>`, so the intersection type can be useful.
type DeepReadonlyExceptY<T> =
  T extends Builtin ? T
  : // Allow mutation
  T extends Y.Map<unknown> ? T
  : T extends Y.Text ? T
  : // Same as DeepReadonly
  T extends FixedMap<unknown> ? T
  : T extends Map<infer K, infer V> ? ReadonlyMap<DeepReadonly<K>, DeepReadonlyExceptY<V>>
  : T extends ReadonlyMap<infer K, infer V> ? ReadonlyMap<DeepReadonly<K>, DeepReadonlyExceptY<V>>
  : T extends WeakMap<infer K, infer V> ? WeakMap<DeepReadonly<K>, DeepReadonlyExceptY<V>>
  : T extends Set<infer U> ? ReadonlySet<DeepReadonly<U>>
  : T extends ReadonlySet<infer U> ? ReadonlySet<DeepReadonly<U>>
  : T extends WeakSet<infer U> ? WeakSet<DeepReadonly<U>>
  : T extends Promise<infer U> ? Promise<DeepReadonly<U>>
  : T extends object ? { readonly [K in keyof T]: DeepReadonlyExceptY<T[K]> }
  : Readonly<T>

declare const brandAstId: unique symbol
export type AstId = string & { [brandAstId]: never }

declare const brandOwned: unique symbol
/**
 * Used to mark references required to be unique.
 *
 *  Note that the typesystem cannot stop you from copying an `Owned`,
 *  but that is an easy mistake to see (because it occurs locally).
 *
 *  We can at least require *obtaining* an `Owned`,
 *  which statically prevents the otherwise most likely usage errors when rearranging ASTs.
 */
export type Owned<T = MutableAst> = T & { [brandOwned]: never }
/** @internal */
export function asOwned<T>(t: T): Owned<T> {
  return t as Owned<T>
}

export type NodeChild<T> = { whitespace: string | undefined; node: T }
export type RawNodeChild = NodeChild<AstId> | NodeChild<SyncTokenId>

/** @internal */
export function parentId(ast: Ast): AstId | undefined {
  return ast.fields.get('parent')
}

/** @internal */
export interface MetadataFields {
  externalId: ExternalId
  widget: Y.Map<unknown>
}
export interface NodeMetadataFields {
  position?: { x: number; y: number } | undefined
  visualization?: VisualizationMetadata | undefined
  colorOverride?: string | undefined
}
const nodeMetadataKeys = allKeys<NodeMetadataFields>({
  position: null,
  visualization: null,
  colorOverride: null,
})
export type NodeMetadata = FixedMapView<NodeMetadataFields & MetadataFields>
export type MutableNodeMetadata = FixedMap<NodeMetadataFields & MetadataFields>

export const astTypes = [
  'App',
  'Assignment',
  'BodyBlock',
  'ExpressionStatement',
  'FunctionDef',
  'Generic',
  'Group',
  'Ident',
  'Import',
  'Invalid',
  'NegationApp',
  'NumericLiteral',
  'OprApp',
  'PropertyAccess',
  'TextLiteral',
  'UnaryOprApp',
  'AutoscopedIdentifier',
  'Vector',
  'Wildcard',
] as const
export type AstType = (typeof astTypes)[number]

/** @internal */
interface RawAstFields {
  id: AstId
  type: AstType
  parent: AstId | undefined
  metadata: FixedMap<MetadataFields>
}
export interface AstFields extends RawAstFields, LegalFieldContent {}
const astFieldKeys = allKeys<RawAstFields>({
  id: null,
  type: null,
  parent: null,
  metadata: null,
})

/**
 * Base class for "first class" AST types. The kinds of AST data include:
 * - Subtypes of `Ast`, including {@link Expression} and {@link Statement}. These are high-level types representing
 *   meaningful substructures of the AST; they have unique IDs, can be mutated, and support many common operations.
 * - {@link Token}s: All AST types are ultimately composed of tokens representing their constituent code.
 * - Structured field data: Types organizing {@link Token}s into structures representing parts of {@link Ast}s. Some
 *   structured field data includes Y.Js types, such as the metadata maps; field data is otherwise immutable, and is
 *   get/set through accessors of the `Ast` types.
 */
export abstract class Ast {
  readonly module: Module
  /** @internal */
  readonly fields: FixedMapView<AstFields>

  /**
   * Return a stable unique identifier for this AST object. This can be used to retrieve the object from the module, or
   * from any module derived from this module (e.g. by calling {@link MutableModule.edit}).
   */
  get id(): AstId {
    return this.fields.get('id')
  }

  /**
   * Get an id used to communication with engine.
   *
   * This id has two purposes:
   * - It is used in communication with Engine (regarding visualization, execution context frames, etc.)
   * - This is used as key files metadata (keeping e.g. node's positions)
   *
   * ## Id vs. externalId
   *
   * It subtly differs in meaning from standard id: while the latter keeps identity from ydoc conflict resolution
   * perspective, this id rather try to keep what is the node or widget identity from user perspective. The best
   * example showing this difference is adding a new argument, like changing  `foo = bar` to `foo = bar a`:
   * - `id` should be kept on `Main.bar` expression, and `Main.bar a` gets new id, so any concurrent edit on `bar`
   *   will be handled properly
   * - `externalId` should be moved from `bar` to `bar a` expression, as this is still the same node (so its
   *   position and other properties should be kept)
   */
  get externalId(): ExternalId {
    const id = this.fields.get('metadata').get('externalId')
    assert(id != null)
    return id
  }

  /** Returns whether this type can be an expression. */
  isAllowedInExpressionContext(): boolean {
    return false
  }

  /** Returns whether this type can be a statement. */
  isAllowedInStatementContext(): boolean {
    return false
  }

  /** Type predicate for {@link Expression}. */
  isExpression(): this is Expression {
    return this.isAllowedInExpressionContext()
  }

  /** Type predicate for {@link Statement}. */
  isStatement(): this is Statement {
    return this.isAllowedInStatementContext()
  }

  /** Returns a view of the metadata applicable to a graph node (e.g. its position) associated with this AST object. */
  get nodeMetadata(): NodeMetadata {
    const metadata = this.fields.get('metadata')
    return metadata as FixedMapView<NodeMetadataFields & MetadataFields>
  }

  /** Get metadata of all widgets assigned to this node. WARNING: The returned value must not be mutated. */
  widgetsMetadata(): ReadonlyMap<string, DeepReadonly<unknown>> {
    return this.fields.get('metadata').get('widget')
  }

  /** Get metadata of given widget assigned to this node. WARNING: The returned value must not be mutated. */
  widgetMetadata(widgetKey: string): DeepReadonly<unknown> | undefined {
    return this.fields.get('metadata').get('widget').get(widgetKey)
  }

  /** Returns a JSON-compatible object containing all metadata properties. */
  serializeMetadata(): MetadataFields & NodeMetadataFields {
    return this.fields.get('metadata').toJSON() as any
  }

  /** TODO: Add docs */
  get typeName(): AstType {
    return this.fields.get('type')
  }

  /** Return whether `this` and `other` are the same object, possibly in different modules. */
  is<T extends Ast>(other: T): boolean {
    return this.id === other.id
  }

  /** Return source code representing this node. */
  code(): string {
    return printWithSpans(this).code
  }

  /** TODO: Add docs */
  visitRecursive(visit: (ast: Ast) => void | boolean): void {
    if (visit(this) === false) return
    for (const child of this.children()) {
      if (!isToken(child)) child.visitRecursive(visit)
    }
  }

  /** TODO: Add docs */
  printSubtree(
    info: SpanMap,
    offset: number,
    parentIndent: string | null,
    verbatim?: boolean,
  ): string {
    return printAst(this, info, offset, parentIndent, verbatim)
  }

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): IterableIterator<Ast | Token> {
    for (const child of this.concreteChildren({ verbatim: false, indent: '' })) {
      if (isTokenId(child.node)) {
        yield this.module.getToken(child.node)
      } else {
        const node = this.module.get(child.node)
        if (node) yield node
      }
    }
  }

  /** TODO: Add docs */
  get parentId(): AstId | undefined {
    const parentId = this.fields.get('parent')
    if (parentId !== ROOT_ID) return parentId
  }

  /** TODO: Add docs */
  parent(): Ast | undefined {
    return this.module.get(this.parentId)
  }

  ////////////////////

  protected constructor(module: Module, fields: FixedMapView<AstFields>) {
    this.module = module
    this.fields = fields
  }

  /**
   * Returns child subtrees, including information about the whitespace between them.
   * @internal
   */
  abstract concreteChildren(printContext: PrintContext): IterableIterator<RawConcreteChild>
}
export interface MutableAst {}
/** TODO: Add docs */
export abstract class MutableAst extends Ast {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields>

  /** TODO: Add docs */
  setExternalId(id: ExternalId) {
    this.fields.get('metadata').set('externalId', id)
  }

  /** Set the widget's new metadata. */
  setWidgetMetadata(widgetKey: string, widgetMetadata: unknown) {
    this.fields.get('metadata').get('widget').set(widgetKey, widgetMetadata)
  }

  /** Get map of all widget's metadata. */
  mutableWidgetsMetadata() {
    return this.fields.get('metadata').get('widget')
  }

  /** Returns a writable view of the {@link nodeMetadata}. */
  mutableNodeMetadata(): MutableNodeMetadata {
    const metadata = this.fields.get('metadata')
    return metadata as FixedMap<NodeMetadataFields & MetadataFields>
  }

  /** Overwrites the provided fields of the {@link nodeMetadata}. */
  setNodeMetadata(nodeMeta: NodeMetadataFields) {
    const metadata = this.fields.get('metadata') as unknown as Map<string, unknown>
    for (const [key, value] of Object.entries(nodeMeta)) {
      if (!nodeMetadataKeys.has(key)) continue
      if (value === undefined) {
        metadata.delete(key)
      } else {
        metadata.set(key, value)
      }
    }
  }

  /** Modify the parent of this node to refer to a new object instead. Return the object, which now has no parent. */
  replace<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const parentId = this.fields.get('parent')
    if (parentId) {
      const parent = this.module.get(parentId)
      parent.replaceChild(this.id, replacement)
      this.fields.set('parent', undefined)
    }
    return asOwned(this)
  }

  /**
   * Change the value of the object referred to by the `target` ID. (The initial ID of `replacement` will be ignored.)
   *  Returns the old value, with a new (unreferenced) ID.
   */
  replaceValue<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const replacement_ = this.module.copyIfForeign(replacement)
    const old = this.replace(replacement_)
    replacement_.fields.set('metadata', old.fields.get('metadata').clone())
    old.setExternalId(newExternalId())
    return old
  }

  /** TODO: Add docs */
  replaceValueChecked<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const parentId = this.fields.get('parent')
    assertDefined(parentId)
    return this.replaceValue(replacement)
  }

  /**
   * Replace the parent of this object with a reference to a new placeholder object.
   * Returns the object, now parentless, and the placeholder.
   */
  takeToReplace(): Removed<this> {
    if (parentId(this)) {
      const placeholder = Wildcard.new(this.module)
      const node = this.replace(placeholder)
      return { node, placeholder }
    } else {
      return { node: asOwned(this), placeholder: undefined }
    }
  }

  /**
   * Replace the parent of this object with a reference to a new placeholder object.
   * Returns the object, now parentless.
   */
  take(): Owned<this> {
    return this.replace(Wildcard.new(this.module))
  }

  /** TODO: Add docs */
  takeIfParented(): Owned<typeof this> {
    const parent = parentId(this)
    if (parent) {
      const parentAst = this.module.get(parent)
      const placeholder = Wildcard.new(this.module)
      parentAst.replaceChild(this.id, placeholder)
      this.fields.set('parent', undefined)
    }
    return asOwned(this)
  }

  /**
   * Replace the value assigned to the given ID with a placeholder.
   *  Returns the removed value, with a new unreferenced ID.
   */
  takeValue(): Removed<typeof this> {
    const placeholder = Wildcard.new(this.module)
    const node = this.replaceValue(placeholder)
    return { node, placeholder }
  }

  /**
   * Take this node from the tree, and replace it with the result of applying the given function to it.
   *
   *  Note that this is a modification of the *parent* node. Any `Ast` objects or `AstId`s that pointed to the old value
   *  will still point to the old value.
   */
  update<T extends MutableAst>(f: (x: Owned<typeof this>) => Owned<T>): T {
    const taken = this.takeToReplace()
    assertDefined(taken.placeholder, 'To replace an `Ast`, it must have a parent.')
    const replacement = f(taken.node)
    taken.placeholder.replace(replacement)
    return replacement
  }

  /**
   * Take this node from the tree, and replace it with the result of applying the given function to it; transfer the
   *  metadata from this node to the replacement.
   *
   *  Note that this is a modification of the *parent* node. Any `Ast` objects or `AstId`s that pointed to the old value
   *  will still point to the old value.
   */
  updateValue<T extends MutableAst>(f: (x: Owned<typeof this>) => Owned<T>): T {
    const taken = this.takeValue()
    assertDefined(taken.placeholder, 'To replace an `Ast`, it must have a parent.')
    const replacement = f(taken.node)
    taken.placeholder.replaceValue(replacement)
    return replacement
  }

  /** TODO: Add docs */
  mutableParent(): MutableAst | undefined {
    const parentId = this.fields.get('parent')
    if (parentId === 'ROOT_ID') return
    return this.module.get(parentId)
  }

  /** Modify this tree to represent the given code, while minimizing changes from the current set of `Ast`s. */
  syncToCode(code: string, metadataSource?: Module) {
    syncToCode(this, code, metadataSource)
  }

  /** Update the AST according to changes to its corresponding source code. */
  applyTextEdits(textEdits: SourceRangeEdit[], metadataSource?: Module) {
    applyTextEditsToAst(this, textEdits, metadataSource ?? this.module)
  }

  ///////////////////

  /** @internal */
  importReferences(module: Module) {
    if (module === this.module) return
    for (const child of this.concreteChildren({ verbatim: false, indent: '' })) {
      if (!isTokenId(child.node)) {
        const childInForeignModule = module.get(child.node)
        assert(childInForeignModule !== undefined)
        const importedChild = this.module.copy(childInForeignModule)
        importedChild.fields.set('parent', undefined)
        this.replaceChild(child.node, asOwned(importedChild))
      }
    }
  }

  /** @internal */
  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    const replacementId = this.claimChild(replacement)
    const changes = rewriteRefs(this, (id) => (id === target ? replacementId : undefined))
    assertEqual(changes, 1)
  }

  /** @internal */
  claimChild<T extends MutableAst>(child: Owned<T>): AstId
  /** TODO: Add docs */
  claimChild<T extends MutableAst>(child: Owned<T> | undefined): AstId | undefined
  /** TODO: Add docs */
  claimChild<T extends MutableAst>(child: Owned<T> | undefined): AstId | undefined {
    return child ? claimChild(this.module, child, this.id) : undefined
  }
}

/** Values that may be found in fields of `Ast` subtypes. */
type FieldData<T extends TreeRefs = RawRefs> =
  | NonArrayFieldData<T>
  | NonArrayFieldData<T>[]
  | (T['ast'] | T['token'])[]

// Logically `FieldData<T>[]` could be a type of `FieldData`, but the type needs to be non-recursive so that it can be
// used with `DeepReadonly`.
type NonArrayFieldData<T extends TreeRefs> =
  | T['ast']
  | T['token']
  | undefined
  | StructuralField<T>
  | string

/** Objects that do not directly contain `AstId`s or `SyncTokenId`s, but may have `NodeChild` fields. */
type StructuralField<T extends TreeRefs = RawRefs> =
  | MultiSegmentAppSegment<T>
  | Line<T>
  | OpenCloseTokens<T>
  | NameSpecification<T>
  | TextElement<T>
  | ArgumentDefinition<T>
  | VectorElement<T>
  | TypeSignature<T>
  | SignatureLine<T>
  | FunctionAnnotation<T>
  | AnnotationLine<T>
  | DocComment<T>
  | DocLine<T>
  | Y.Text

/** Type whose fields are all suitable for storage as `Ast` fields. */
interface FieldObject<T extends TreeRefs> {
  [field: string]: FieldData<T>
}

/** Returns the fields of an `Ast` subtype that are not part of `AstFields`. */
function* fieldDataEntries<Fields>(map: FixedMapView<Fields>) {
  for (const entry of map.entries()) {
    // All fields that are not from `AstFields` are `FieldData`.
    if (!astFieldKeys.has(entry[0])) yield entry as [string, DeepReadonly<FieldData>]
  }
}

function idRewriter(
  f: (id: AstId) => AstId | undefined,
): (field: DeepReadonly<FieldData>) => FieldData | undefined {
  return (field: DeepReadonly<FieldData>) => {
    if (typeof field !== 'object') return
    assert(field !== null)
    if (!('node' in field)) return
    if (isTokenId(field.node)) return
    const newId = f(field.node)
    if (!newId) return
    return { whitespace: field.whitespace, node: newId }
  }
}

/**
 * Apply the given function to each `AstId` in the fields of `ast`. For each value that it returns an output, that
 *  output will be substituted for the input ID.
 */
export function rewriteRefs(ast: MutableAst, f: (id: AstId) => AstId | undefined) {
  let fieldsChanged = 0
  for (const [key, value] of fieldDataEntries(ast.fields)) {
    const newValue = rewriteFieldRefs(value, idRewriter(f))
    if (newValue !== undefined) {
      ast.fields.set(key as any, newValue)
      fieldsChanged += 1
    }
  }
  return fieldsChanged
}

/**
 * Copy all fields except the `Ast` base fields from `ast2` to `ast1`. A reference-rewriting function will be applied
 *  to `AstId`s in copied fields; see {@link rewriteRefs}.
 */
export function syncFields(ast1: MutableAst, ast2: Ast, f: (id: AstId) => AstId | undefined) {
  for (const [key, value] of fieldDataEntries(ast2.fields)) {
    if (value instanceof Y.Text) {
      const ast1Value = ast1.fields.get(key as any)
      assert(ast1Value instanceof Y.Text)
      syncYText(ast1Value, value)
      continue
    }
    const newValue = mapRefs(value, idRewriter(f))
    if (!fieldEqual(ast1.fields.get(key as any), newValue)) ast1.fields.set(key as any, newValue)
  }
}

function syncYText(target: Y.Text, source: Y.Text) {
  const sourceString = source.toJSON()
  if (target.toJSON() !== sourceString) {
    target.delete(0, target.length)
    target.insert(0, sourceString)
  }
}

/** TODO: Add docs */
export function syncNodeMetadata(target: MutableNodeMetadata, source: NodeMetadata) {
  const oldPos = target.get('position')
  const newPos = source.get('position')
  if (oldPos?.x !== newPos?.x || oldPos?.y !== newPos?.y) target.set('position', newPos)
  const newVis = source.get('visualization')
  if (!visMetadataEquals(target.get('visualization'), newVis)) target.set('visualization', newVis)
}

function rewriteFieldRefs<T extends TreeRefs, U extends TreeRefs>(
  field: DeepReadonly<FieldData<T>>,
  f: (t: DeepReadonly<FieldData<T>>) => FieldData<U> | undefined,
): FieldData<U> {
  const newValue = f(field)
  if (newValue) return newValue
  if (typeof field !== 'object') return
  if (field === null) return
  if (field instanceof Y.Text) return
  // `Array.isArray` doesn't work with `DeepReadonly`, but we just need a narrowing that distinguishes it from all
  // `StructuralField` types.
  if ('forEach' in field) {
    const newValues = new Map<number, FieldData<U>>()
    field.forEach((subfield, i) => {
      const newValue = rewriteFieldRefs(subfield, f)
      if (newValue !== undefined) newValues.set(i, newValue)
    })
    if (newValues.size) return Array.from(field, (oldValue, i) => newValues.get(i) ?? oldValue)
  } else {
    const fieldObject = field satisfies DeepReadonly<StructuralField>
    const newValues = new Map<string, FieldData<U>>()
    for (const [key, value] of Object.entries(fieldObject)) {
      const newValue = rewriteFieldRefs(value, f)
      if (newValue !== undefined) newValues.set(key, newValue)
    }
    if (newValues.size)
      return Object.fromEntries(
        Object.entries(fieldObject).map(([key, oldValue]) => [key, newValues.get(key) ?? oldValue]),
      )
  }
}

type MapRef<T extends TreeRefs, U extends TreeRefs> = (t: FieldData<T>) => FieldData<U> | undefined

// This operation can transform any `FieldData` type parameterized by some `TreeRefs` into the same type parameterized
// by another `TreeRefs`, but it is not possible to express that generalization to TypeScript as such.
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: ImportFields<T>,
  f: MapRef<T, U>,
): ImportFields<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: TextToken<T>,
  f: MapRef<T, U>,
): TextToken<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: TextElement<T>,
  f: MapRef<T, U>,
): TextElement<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: ArgumentDefinition<T>,
  f: MapRef<T, U>,
): ArgumentDefinition<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: VectorElement<T>,
  f: MapRef<T, U>,
): VectorElement<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: DocLine<T>,
  f: MapRef<T, U>,
): DocLine<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: AnnotationLine<T>,
  f: MapRef<T, U>,
): AnnotationLine<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: SignatureLine<T>,
  f: MapRef<T, U>,
): SignatureLine<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: FieldData<T>,
  f: MapRef<T, U>,
): FieldData<U>
function mapRefs<T extends TreeRefs, U extends TreeRefs>(
  field: FieldData<T>,
  f: MapRef<T, U>,
): FieldData<U> {
  return rewriteFieldRefs(field, f) ?? field
}

function fieldEqual(field1: FieldData, field2: FieldData): boolean {
  if (typeof field1 !== 'object') return field1 === field2
  if (typeof field2 !== 'object') return false
  if ('node' in field1 && 'node' in field2) {
    if (field1['whitespace'] !== field2['whitespace']) return false
    if (isTokenId(field1.node) && isTokenId(field2.node))
      return Token.equal(field1.node, field2.node)
    else return field1.node === field2.node
  } else if ('node' in field1 || 'node' in field2) {
    return false
  } else if (Array.isArray(field1) && Array.isArray(field2)) {
    return (
      field1.length === field2.length && field1.every((value1, i) => fieldEqual(value1, field2[i]))
    )
  } else if (Array.isArray(field1) || Array.isArray(field2)) {
    return false
  } else {
    const fieldObject1 = field1 satisfies StructuralField
    const fieldObject2 = field2 satisfies StructuralField
    const keys = new Set<string>()
    for (const key of Object.keys(fieldObject1)) keys.add(key)
    for (const key of Object.keys(fieldObject2)) keys.add(key)
    for (const key of keys)
      if (!fieldEqual((fieldObject1 as any)[key], (fieldObject2 as any)[key])) return false
    return true
  }
}

function applyMixins(derivedCtor: any, constructors: any[]) {
  constructors.forEach((baseCtor) => {
    Object.getOwnPropertyNames(baseCtor.prototype).forEach((name) => {
      Object.defineProperty(
        derivedCtor.prototype,
        name,
        Object.getOwnPropertyDescriptor(baseCtor.prototype, name) || Object.create(null),
      )
    })
  })
}

abstract class BaseStatement extends Ast {
  override isAllowedInStatementContext(): true {
    return true
  }
  override isAllowedInExpressionContext() {
    return false
  }
}
/**
 * A statement, i.e. the contents of a line which is either at the top level of a module (a module declaration), or
 * within a body block (a function body statement).
 */
export interface Statement extends BaseStatement {
  /** If this statement type supports plain-text documentation, return a value synchronized with its content. */
  mutableDocumentationText?: () => Y.Text
}
abstract class BaseMutableStatement extends MutableAst implements Statement {
  override isAllowedInStatementContext(): true {
    return true
  }
  override isAllowedInExpressionContext() {
    return false
  }
}
/** A mutable {@link Statement}. */
export interface MutableStatement extends BaseMutableStatement {
  /** If this statement type supports plain-text documentation, return a value synchronized with its content. */
  mutableDocumentationText?: () => Y.Text
}

abstract class BaseExpression extends Ast {
  override isAllowedInStatementContext() {
    return false
  }
  override isAllowedInExpressionContext(): true {
    return true
  }
}
/** An expression, pattern, or type-expression. */
export interface Expression extends BaseExpression {}
abstract class BaseMutableExpression extends MutableAst implements Expression {
  override isAllowedInStatementContext() {
    return false
  }
  override isAllowedInExpressionContext(): true {
    return true
  }
}
/** A mutable {@link Expression}. */
export interface MutableExpression extends BaseMutableExpression {}

function toStatement(
  ast: Owned<MutableStatement> | Owned<MutableExpression>,
  edit?: MutableModule,
) {
  return ast.isAllowedInStatementContext() ? ast : ExpressionStatement.new(ast as any, { edit })
}

interface AppFields {
  function: NodeChild<AstId>
  parens: OpenCloseTokens | undefined
  nameSpecification: NameSpecification | undefined
  argument: NodeChild<AstId>
}
interface OpenCloseTokens<T extends TreeRefs = RawRefs> {
  open: T['token']
  close: T['token']
}
interface NameSpecification<T extends TreeRefs = RawRefs> {
  name: T['token']
  equals: T['token']
}
/** TODO: Add docs */
export class App extends BaseExpression {
  declare fields: FixedMap<AstFields & AppFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & AppFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableApp> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableApp) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    func: NodeChild<Owned<MutableExpression>>,
    parens: OpenCloseTokens | undefined,
    nameSpecification: NameSpecification | undefined,
    argument: NodeChild<Owned<MutableExpression>>,
  ) {
    const base = module.baseObject('App')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      function: concreteChild(module, func, id_),
      parens,
      nameSpecification,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableApp(module, fields))
  }

  /** TODO: Add docs */
  static new(
    module: MutableModule,
    func: Owned<MutableExpression>,
    argumentName: StrictIdentLike | undefined,
    argument: Owned<MutableExpression>,
  ) {
    return App.concrete(
      module,
      autospaced(func),
      undefined,
      nameSpecification(argumentName),
      autospaced(argument),
    )
  }

  /** TODO: Add docs */
  static positional(
    func: Owned<MutableExpression>,
    argument: Owned<MutableExpression>,
    module?: MutableModule,
  ): Owned<MutableApp> {
    return App.new(module ?? MutableModule.Transient(), func, undefined, argument)
  }

  /** Build a tree of 0-or-more applications applying all given arguments to the given function. */
  static PositionalSequence(
    func: Owned<MutableExpression>,
    args: Owned<MutableExpression>[],
  ): Owned<MutableExpression> {
    return args.reduce<Owned<MutableExpression>>(
      (expression, argument) => App.new(func.module, expression, undefined, argument),
      func,
    )
  }

  /** TODO: Add docs */
  get function(): Expression {
    return this.module.get(this.fields.get('function').node) as Expression
  }
  /** TODO: Add docs */
  get argumentName(): Token | undefined {
    return this.module.getToken(this.fields.get('nameSpecification')?.name.node)
  }
  /** TODO: Add docs */
  get argument(): Expression {
    return this.module.get(this.fields.get('argument').node) as Expression
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const { function: function_, parens, nameSpecification, argument } = getAll(this.fields)
    yield firstChild(function_)
    const useParens = !!(parens && (nameSpecification || verbatim))
    const spacedEquals = useParens && !!nameSpecification?.equals.whitespace
    if (useParens) yield ensureSpaced(parens.open, verbatim)
    if (nameSpecification) {
      yield useParens ?
        preferUnspaced(nameSpecification.name)
      : ensureSpaced(nameSpecification.name, verbatim)
      yield ensureSpacedOnlyIf(nameSpecification.equals, spacedEquals, verbatim)
    }
    // Some syntax trees, including many error conditions, involve unspaced applications.
    // If a parsed input lacked a space before the argument, reproduce it as-is.
    const verbatimArgument = !nameSpecification
    yield ensureSpacedOnlyIf(argument, !nameSpecification || spacedEquals, verbatimArgument)
    if (useParens) yield preferUnspaced(parens.close)
  }
}
/** TODO: Add docs */
export class MutableApp extends App implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & AppFields>

  setFunction(value: Owned<MutableExpression>) {
    setNode(this.fields, 'function', this.claimChild(value))
  }
  setArgumentName(name: StrictIdentLike | undefined) {
    this.fields.set('nameSpecification', nameSpecification(name))
  }
  setArgument(value: Owned<MutableExpression>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }
}
export interface MutableApp extends App, MutableExpression {
  get function(): MutableExpression
  get argument(): MutableExpression
}
applyMixins(MutableApp, [MutableAst])

interface UnaryOprAppFields {
  operator: NodeChild<SyncTokenId>
  argument: NodeChild<AstId> | undefined
}
/** TODO: Add docs */
export class UnaryOprApp extends BaseExpression {
  declare fields: FixedMapView<AstFields & UnaryOprAppFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & UnaryOprAppFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableUnaryOprApp> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableUnaryOprApp) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    operator: NodeChild<Token>,
    argument: NodeChild<Owned<MutableExpression>> | undefined,
  ) {
    const base = module.baseObject('UnaryOprApp')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableUnaryOprApp(module, fields))
  }

  /** TODO: Add docs */
  static new(
    module: MutableModule,
    operator: Token,
    argument: Owned<MutableExpression> | undefined,
  ) {
    return this.concrete(module, unspaced(operator), argument ? autospaced(argument) : undefined)
  }

  /** TODO: Add docs */
  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  /** TODO: Add docs */
  get argument(): Expression | undefined {
    return this.module.get(this.fields.get('argument')?.node) as Expression | undefined
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const { operator, argument } = getAll(this.fields)
    yield firstChild(operator)
    if (argument) yield ensureUnspaced(argument, verbatim)
  }
}
/** TODO: Add docs */
export class MutableUnaryOprApp extends UnaryOprApp implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & UnaryOprAppFields>

  setOperator(value: Token) {
    this.fields.set('operator', unspaced(value))
  }
  setArgument<T extends MutableExpression>(argument: Owned<T> | undefined) {
    setNode(this.fields, 'argument', this.claimChild(argument))
  }
}
export interface MutableUnaryOprApp extends UnaryOprApp, MutableExpression {
  get argument(): MutableExpression | undefined
}
applyMixins(MutableUnaryOprApp, [MutableAst])

interface AutoscopedIdentifierFields {
  operator: NodeChild<SyncTokenId>
  identifier: NodeChild<SyncTokenId>
}
/** TODO: Add docs */
export class AutoscopedIdentifier extends BaseExpression {
  declare fields: FixedMapView<AstFields & AutoscopedIdentifierFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & AutoscopedIdentifierFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  get identifier(): Token {
    return this.module.getToken(this.fields.get('identifier').node)
  }

  /** TODO: Add docs */
  static tryParse(
    source: string,
    module?: MutableModule,
  ): Owned<MutableAutoscopedIdentifier> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableAutoscopedIdentifier) return parsed
  }

  /** TODO: Add docs */
  static concrete(module: MutableModule, operator: NodeChild<Token>, identifier: NodeChild<Token>) {
    const base = module.baseObject('AutoscopedIdentifier')
    const fields = composeFieldData(base, {
      operator,
      identifier,
    })
    return asOwned(new MutableAutoscopedIdentifier(module, fields))
  }

  /** TODO: Add docs */
  static new(
    identifier: TypeOrConstructorIdentifier,
    module?: MutableModule,
  ): Owned<MutableAutoscopedIdentifier> {
    const module_ = module || MutableModule.Transient()
    const operator = Token.new('..')
    const ident = Token.new(identifier, TokenType.Ident)
    return this.concrete(module_, unspaced(operator), unspaced(ident))
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const { operator, identifier } = getAll(this.fields)
    yield firstChild(operator)
    yield ensureUnspaced(identifier, verbatim)
  }
}
/** TODO: Add docs */
export class MutableAutoscopedIdentifier extends AutoscopedIdentifier implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & AutoscopedIdentifierFields>

  setIdentifier(value: TypeOrConstructorIdentifier) {
    const token = Token.new(value, TokenType.Ident)
    this.fields.set('identifier', unspaced(token))
  }
}
export interface MutableAutoscopedIdentifier extends AutoscopedIdentifier, MutableExpression {}
applyMixins(MutableAutoscopedIdentifier, [MutableAst])

interface NegationAppFields {
  operator: NodeChild<SyncTokenId>
  argument: NodeChild<AstId>
}
/** TODO: Add docs */
export class NegationApp extends BaseExpression {
  declare fields: FixedMapView<AstFields & NegationAppFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & NegationAppFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableNegationApp> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableNegationApp) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    operator: NodeChild<Token>,
    argument: NodeChild<Owned<MutableExpression>>,
  ) {
    const base = module.baseObject('NegationApp')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableNegationApp(module, fields))
  }

  /** TODO: Add docs */
  static new(module: MutableModule, argument: Owned<MutableExpression>) {
    const minus = Token.new('-', TokenType.Operator)
    return this.concrete(module, unspaced(minus), unspaced(argument))
  }

  /** TODO: Add docs */
  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  /** TODO: Add docs */
  get argument(): Expression {
    return this.module.get(this.fields.get('argument').node) as Expression
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const { operator, argument } = getAll(this.fields)
    yield firstChild(operator)
    if (argument) yield ensureUnspaced(argument, verbatim)
  }
}
/** TODO: Add docs */
export class MutableNegationApp extends NegationApp implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & NegationAppFields>

  setArgument<T extends MutableExpression>(value: Owned<T>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }
}
export interface MutableNegationApp extends NegationApp, MutableExpression {
  get argument(): MutableExpression
}
applyMixins(MutableNegationApp, [MutableAst])

interface OprAppFields {
  lhs: NodeChild<AstId> | undefined
  operators: NodeChild<SyncTokenId>[]
  rhs: NodeChild<AstId> | undefined
}
/** TODO: Add docs */
export class OprApp extends BaseExpression {
  declare fields: FixedMapView<AstFields & OprAppFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & OprAppFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableOprApp> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableOprApp) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned<MutableExpression>> | undefined,
    operators: NodeChild<Token>[],
    rhs: NodeChild<Owned<MutableExpression>> | undefined,
  ) {
    const base = module.baseObject('OprApp')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      lhs: concreteChild(module, lhs, id_),
      operators,
      rhs: concreteChild(module, rhs, id_),
    })
    return asOwned(new MutableOprApp(module, fields))
  }

  /** TODO: Add docs */
  static new(
    module: MutableModule,
    lhs: Owned<MutableExpression> | undefined,
    operator: Token | string,
    rhs: Owned<MutableExpression> | undefined,
  ) {
    const operatorToken =
      operator instanceof Token ? operator : Token.new(operator, TokenType.Operator)
    return OprApp.concrete(module, unspaced(lhs), [autospaced(operatorToken)], autospaced(rhs))
  }

  /** TODO: Add docs */
  get lhs(): Expression | undefined {
    return this.module.get(this.fields.get('lhs')?.node) as Expression | undefined
  }
  /** TODO: Add docs */
  get operator(): Result<Token, NodeChild<Token>[]> {
    const operators = this.fields.get('operators')
    const operators_ = operators.map((child) => ({
      ...child,
      node: this.module.getToken(child.node),
    }))
    const [opr] = operators_
    return opr ? Ok(opr.node) : Err(operators_)
  }
  /** TODO: Add docs */
  get rhs(): Expression | undefined {
    return this.module.get(this.fields.get('rhs')?.node) as Expression | undefined
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const { lhs, operators, rhs } = getAll(this.fields)
    if (lhs) yield firstChild(lhs)
    const [operator0, ...extraOperators] = operators
    assertDefined(operator0)
    const spaced = ((lhs && operator0.whitespace) ?? rhs?.whitespace ?? ' ') !== ''
    const isAsymmetricallySpaceableOperator =
      operator0.node.code_ === '->' || operator0.node.code_ === '=' || operator0.node.code_ === ','
    const applySpacing =
      isAsymmetricallySpaceableOperator ?
        (token: RawNodeChild) => preferSpacedIf(token, spaced)
      : (token: RawNodeChild) => ensureSpacedOnlyIf(token, spaced, verbatim)
    yield lhs ? applySpacing(operator0) : firstChild(operator0)
    const extrasSpaced = (extraOperators[0]?.whitespace ?? ' ') !== ''
    for (const op of extraOperators) yield ensureSpacedOnlyIf(op, extrasSpaced, verbatim)
    if (rhs) yield applySpacing(rhs)
  }
}
/** TODO: Add docs */
export class MutableOprApp extends OprApp implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & OprAppFields>

  setLhs<T extends MutableExpression>(value: Owned<T>) {
    setNode(this.fields, 'lhs', this.claimChild(value))
  }
  setOperator(value: Token) {
    this.fields.set('operators', [unspaced(value)])
  }
  setRhs<T extends MutableExpression>(value: Owned<T>) {
    setNode(this.fields, 'rhs', this.claimChild(value))
  }
}
export interface MutableOprApp extends OprApp, MutableExpression {
  get lhs(): MutableExpression | undefined
  get rhs(): MutableExpression | undefined
}
applyMixins(MutableOprApp, [MutableAst])

interface PropertyAccessFields {
  lhs: NodeChild<AstId> | undefined
  operator: NodeChild<SyncTokenId>
  rhs: NodeChild<AstId>
}
/** TODO: Add docs */
export class PropertyAccess extends BaseExpression {
  declare fields: FixedMapView<AstFields & PropertyAccessFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & PropertyAccessFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(
    source: string,
    module?: MutableModule,
  ): Owned<MutablePropertyAccess> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutablePropertyAccess) return parsed
  }

  /** TODO: Add docs */
  static new(
    module: MutableModule,
    lhs: Owned<MutableExpression>,
    rhs: IdentLike,
    style?: { spaced?: boolean },
  ) {
    const dot = Token.new('.', TokenType.Operator)
    const whitespace = style?.spaced ? ' ' : ''
    return this.concrete(
      module,
      unspaced(lhs),
      { whitespace, node: dot },
      { whitespace, node: Ident.newAllowingOperators(module, toIdent(rhs)) },
    )
  }

  /** TODO: Add docs */
  static Sequence(
    segments: [StrictIdentLike, ...StrictIdentLike[]],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent>
  /** TODO: Add docs */
  static Sequence(
    segments: [StrictIdentLike, ...StrictIdentLike[], IdentLike],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent>
  /** TODO: Add docs */
  static Sequence(
    segments: IdentLike[],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined
  /** TODO: Add docs */
  static Sequence(
    segments: IdentLike[],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined {
    let path: Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined
    let operatorInNonFinalSegment = false
    segments.forEach((s, i) => {
      const t = toIdent(s)
      if (i !== segments.length - 1 && !isIdentifier(t.code())) operatorInNonFinalSegment = true
      path = path ? this.new(module, path, t) : Ident.newAllowingOperators(module, t)
    })
    if (!operatorInNonFinalSegment) return path
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned<MutableExpression>> | undefined,
    operator: NodeChild<Token>,
    rhs: NodeChild<Owned<MutableIdent>>,
  ) {
    const base = module.baseObject('PropertyAccess')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      lhs: concreteChild(module, lhs, id_),
      operator,
      rhs: concreteChild(module, rhs, id_),
    })
    return asOwned(new MutablePropertyAccess(module, fields))
  }

  /** TODO: Add docs */
  get lhs(): Expression | undefined {
    return this.module.get(this.fields.get('lhs')?.node) as Expression | undefined
  }
  /** TODO: Add docs */
  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  /** TODO: Add docs */
  get rhs(): IdentifierOrOperatorIdentifierToken {
    const ast = this.module.get(this.fields.get('rhs').node)
    assert(ast instanceof Ident)
    return ast.token as IdentifierOrOperatorIdentifierToken
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const { lhs, operator, rhs } = getAll(this.fields)
    if (lhs) yield firstChild(lhs)
    const spaced = ((lhs && operator.whitespace) ?? rhs.whitespace ?? '') !== ''
    yield lhs ? ensureSpacedOnlyIf(operator, spaced, verbatim) : firstChild(operator)
    yield ensureSpacedOnlyIf(rhs, spaced, verbatim)
  }
}
/** TODO: Add docs */
export class MutablePropertyAccess extends PropertyAccess implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & PropertyAccessFields>

  setLhs<T extends MutableExpression>(value: Owned<T> | undefined) {
    setNode(this.fields, 'lhs', this.claimChild(value))
  }
  setRhs(ident: IdentLike) {
    const node = this.claimChild(Ident.newAllowingOperators(this.module, ident))
    const old = this.fields.get('rhs')
    this.fields.set('rhs', old ? { ...old, node } : unspaced(node))
  }
}
export interface MutablePropertyAccess extends PropertyAccess, MutableExpression {
  get lhs(): MutableExpression | undefined
}
applyMixins(MutablePropertyAccess, [MutableAst])

interface GenericFields {
  children: RawNodeChild[]
}
/** TODO: Add docs */
export class Generic extends Ast implements Expression, Statement {
  /** See {@link Ast.isAllowedInStatementContext}. */
  override isAllowedInStatementContext(): true {
    return true
  }
  /** See {@link Ast.isAllowedInExpressionContext}. */
  override isAllowedInExpressionContext(): true {
    return true
  }
  declare fields: FixedMapView<AstFields & GenericFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & GenericFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static concrete(module: MutableModule, children: (NodeChild<Owned> | NodeChild<Token>)[]) {
    const base = module.baseObject('Generic')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      children: children.map((child) => concreteChild(module, child, id_)),
    })
    return asOwned(new MutableGeneric(module, fields))
  }

  /** TODO: Add docs */
  concreteChildren(_printContext: PrintContext): IterableIterator<RawConcreteChild> {
    // This type is only created by the parser, so its children are always concrete.
    return this.fields.get('children')[Symbol.iterator]() as IterableIterator<RawConcreteChild>
  }
}
/** TODO: Add docs */
export class MutableGeneric extends Generic implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & GenericFields>
}
export interface MutableGeneric extends Generic, MutableAst {
  isAllowedInStatementContext(): true
  isAllowedInExpressionContext(): true
}
applyMixins(MutableGeneric, [MutableAst])

interface MultiSegmentAppSegment<T extends TreeRefs = RawRefs> {
  header: T['token']
  body: T['ast'] | undefined
}
function multiSegmentAppSegment<T extends MutableExpression>(
  header: string,
  body: Owned<T>,
): MultiSegmentAppSegment<OwnedRefs>
function multiSegmentAppSegment<T extends MutableExpression>(
  header: string,
  body: Owned<T> | undefined,
): MultiSegmentAppSegment<OwnedRefs> | undefined
function multiSegmentAppSegment<T extends MutableExpression>(
  header: string,
  body: Owned<T> | undefined,
): MultiSegmentAppSegment<OwnedRefs> | undefined {
  return {
    header: autospaced(Token.new(header, TokenType.Ident)),
    body: spaced(body ? (body as any) : undefined),
  }
}

function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: MultiSegmentAppSegment<OwnedRefs> | undefined,
  parent: AstId,
): MultiSegmentAppSegment | undefined {
  if (!msas) return undefined
  return {
    ...msas,
    body: concreteChild(module, msas.body, parent),
  }
}
interface ImportFields<T extends TreeRefs = RawRefs> extends FieldObject<T> {
  polyglot: MultiSegmentAppSegment<T> | undefined
  from: MultiSegmentAppSegment<T> | undefined
  import: MultiSegmentAppSegment<T>
  all: T['token'] | undefined
  as: MultiSegmentAppSegment<T> | undefined
  hiding: MultiSegmentAppSegment<T> | undefined
}

/** TODO: Add docs */
export class Import extends BaseStatement {
  declare fields: FixedMapView<AstFields & ImportFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & ImportFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableImport> | undefined {
    const parsed = parseStatement(source, module)
    if (parsed instanceof MutableImport) return parsed
  }

  /** TODO: Add docs */
  get polyglot(): Expression | undefined {
    return this.module.get(this.fields.get('polyglot')?.body?.node) as Expression | undefined
  }
  /** TODO: Add docs */
  get from(): Expression | undefined {
    return this.module.get(this.fields.get('from')?.body?.node) as Expression | undefined
  }
  /** TODO: Add docs */
  get import_(): Expression | undefined {
    return this.module.get(this.fields.get('import').body?.node) as Expression | undefined
  }
  /** TODO: Add docs */
  get all(): Token | undefined {
    return this.module.getToken(this.fields.get('all')?.node)
  }
  /** TODO: Add docs */
  get as(): Expression | undefined {
    return this.module.get(this.fields.get('as')?.body?.node) as Expression | undefined
  }
  /** TODO: Add docs */
  get hiding(): Expression | undefined {
    return this.module.get(this.fields.get('hiding')?.body?.node) as Expression | undefined
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    polyglot: MultiSegmentAppSegment<OwnedRefs> | undefined,
    from: MultiSegmentAppSegment<OwnedRefs> | undefined,
    import_: MultiSegmentAppSegment<OwnedRefs>,
    all: NodeChild<Token> | undefined,
    as: MultiSegmentAppSegment<OwnedRefs> | undefined,
    hiding: MultiSegmentAppSegment<OwnedRefs> | undefined,
  ) {
    const base = module.baseObject('Import')
    const id_ = base.get('id')
    const ownedFields: ImportFields<OwnedRefs> = {
      polyglot,
      from,
      import: import_,
      all,
      as,
      hiding,
    }
    const rawFields = mapRefs(ownedFields, ownedToRaw(module, id_))
    const fields = composeFieldData(base, rawFields)
    return asOwned(new MutableImport(module, fields))
  }

  /** TODO: Add docs */
  static Qualified(path: IdentLike[], module: MutableModule): Owned<MutableImport> | undefined {
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    return MutableImport.concrete(
      module,
      undefined,
      undefined,
      multiSegmentAppSegment('import', path_),
      undefined,
      undefined,
      undefined,
    )
  }

  /** TODO: Add docs */
  static Unqualified(
    path: IdentLike[],
    name: IdentLike,
    module: MutableModule,
  ): Owned<MutableImport> | undefined {
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    const name_ = Ident.newAllowingOperators(module, name)
    return MutableImport.concrete(
      module,
      undefined,
      multiSegmentAppSegment('from', path_),
      multiSegmentAppSegment('import', name_),
      undefined,
      undefined,
      undefined,
    )
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    let isFirst = true
    function* segment(segment: MultiSegmentAppSegment) {
      yield isFirst ? firstChild(segment.header) : ensureSpaced(segment.header, verbatim)
      if (segment.body) yield ensureSpaced(segment.body, verbatim)
      isFirst = false
    }
    const { polyglot, from, import: import_, all, as, hiding } = getAll(this.fields)
    if (polyglot) yield* segment(polyglot)
    if (from) yield* segment(from)
    if (import_) yield* segment(import_)
    assert(!isFirst) // There's always a `from` or `import`
    if (all) yield ensureSpaced(all, verbatim)
    if (as) yield* segment(as)
    if (hiding) yield* segment(hiding)
  }
}
/** TODO: Add docs */
export class MutableImport extends Import implements MutableStatement {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & ImportFields>

  private toRaw(msas: MultiSegmentAppSegment<OwnedRefs>): MultiSegmentAppSegment
  private toRaw(
    msas: MultiSegmentAppSegment<OwnedRefs> | undefined,
  ): MultiSegmentAppSegment | undefined
  private toRaw(
    msas: MultiSegmentAppSegment<OwnedRefs> | undefined,
  ): MultiSegmentAppSegment | undefined {
    return multiSegmentAppSegmentToRaw(this.module, msas, this.id)
  }

  setPolyglot<T extends MutableExpression>(value: Owned<T> | undefined) {
    this.fields.set(
      'polyglot',
      value ? this.toRaw(multiSegmentAppSegment('polyglot', value)) : undefined,
    )
  }
  setFrom<T extends MutableExpression>(value: Owned<T> | undefined) {
    this.fields.set('from', value ? this.toRaw(multiSegmentAppSegment('from', value)) : value)
  }
  setImport<T extends MutableExpression>(value: Owned<T>) {
    this.fields.set('import', this.toRaw(multiSegmentAppSegment('import', value)))
  }
  setAll(value: Token | undefined) {
    this.fields.set('all', spaced(value))
  }
  setAs<T extends MutableExpression>(value: Owned<T> | undefined) {
    this.fields.set('as', this.toRaw(multiSegmentAppSegment('as', value)))
  }
  setHiding<T extends MutableExpression>(value: Owned<T> | undefined) {
    this.fields.set('hiding', this.toRaw(multiSegmentAppSegment('hiding', value)))
  }
}
export interface MutableImport extends Import, MutableStatement {
  get polyglot(): MutableExpression | undefined
  get from(): MutableExpression | undefined
  get import_(): MutableExpression | undefined
  get as(): MutableExpression | undefined
  get hiding(): MutableExpression | undefined
}
applyMixins(MutableImport, [MutableAst])

interface TreeRefs {
  token: any
  ast: any
  expression: any
  statement: any
}
type RefMap<T extends TreeRefs, U extends TreeRefs> = (
  field: FieldData<T>,
) => FieldData<U> | undefined
interface RawRefs extends TreeRefs {
  token: NodeChild<SyncTokenId>
  ast: NodeChild<AstId>
  expression: NodeChild<AstId>
  statement: NodeChild<AstId>
}
export interface ConcreteRefs extends TreeRefs {
  token: NodeChild<Token>
  ast: NodeChild<Ast>
  expression: NodeChild<Expression>
  statement: NodeChild<Statement>
}
interface MutableRefs extends ConcreteRefs {
  token: NodeChild<Token>
  ast: NodeChild<MutableAst>
  expression: NodeChild<MutableExpression>
  statement: NodeChild<MutableStatement>
}
export interface OwnedRefs extends MutableRefs {
  token: NodeChild<Token>
  ast: NodeChild<Owned>
  expression: NodeChild<Owned<MutableExpression>>
  statement: NodeChild<Owned<MutableStatement>>
}
function ownedToRaw(module: MutableModule, parentId: AstId): RefMap<OwnedRefs, RawRefs> {
  return (child: FieldData<OwnedRefs>) => {
    if (typeof child !== 'object') return
    if (!('node' in child)) return
    if (isToken(child.node)) return
    return { ...child, node: claimChild(module, child.node, parentId) }
  }
}
function rawToConcrete(module: Module): RefMap<RawRefs, ConcreteRefs> {
  return (child: FieldData) => {
    if (typeof child !== 'object') return
    if (!('node' in child)) return
    if (isTokenId(child.node)) return { ...child, node: module.getToken(child.node) }
    else return { ...child, node: module.get(child.node) }
  }
}

function concreteToOwned(module: MutableModule): RefMap<ConcreteRefs, OwnedRefs> {
  return (child: FieldData<ConcreteRefs>) => {
    if (typeof child !== 'object') return
    if (!('node' in child)) return
    if (isTokenChild(child)) return child
    else return { ...child, node: module.copy(child.node) }
  }
}

export interface TextToken<T extends TreeRefs = RawRefs> {
  type: 'token'
  readonly token: T['token']
  readonly interpreted?: string | undefined
}
export interface TextSplice<T extends TreeRefs = RawRefs> {
  type: 'splice'
  readonly open: T['token']
  readonly expression: T['ast'] | undefined
  readonly close: T['token']
}

export type TextElement<T extends TreeRefs = RawRefs> = TextToken<T> | TextSplice<T>

function textElementValue(element: TextElement<ConcreteRefs>): string {
  switch (element.type) {
    case 'token': {
      if (element.interpreted != null) return element.interpreted
      // The logical newline is not necessarily the same as the concrete token, e.g. the token could be a CRLF.
      if (element.token.node.tokenType_ === TokenType.Newline) return '\n'
      // The token is an invalid escape-sequence or a text-section; return it verbatim.
      return element.token.node.code()
    }
    case 'splice': {
      let s = ''
      s += element.open.node.code()
      if (element.expression) {
        s += element.expression.whitespace ?? ''
        s += element.expression.node.code()
      }
      s += element.close.whitespace ?? ''
      s += element.close.node.code()
      return s
    }
  }
}

function rawTextElementValue(raw: TextElement, module: Module): string {
  return textElementValue(mapRefs(raw, rawToConcrete(module)))
}
function uninterpolatedText(elements: DeepReadonly<TextElement[]>, module: Module): string {
  return elements.reduce((s, e) => s + rawTextElementValue(e, module), '')
}
function docTextToConcrete(
  docLine: DeepReadonly<DocLine> | undefined,
  docText: DeepReadonly<Y.Text>,
  indent: string | null,
  module: Module,
): IterableIterator<RawConcreteChild> | undefined {
  const docLineText = docLineToText(docLine, module) ?? ''
  const docTextValue = docText.toJSON()
  if (docLine && docTextValue === docLineText) {
    return docLineToConcrete(docLine, indent || '')
  } else if (docTextValue) {
    const fromText = elementsToDocLine(textToUninterpolatedElements(docTextValue))
    return docLineToConcrete(fromText, indent || '')
  }
}

function fieldRawChildren(field: DeepReadonly<FieldData>) {
  const children = new Array<RawNodeChild>()
  rewriteFieldRefs(field, (subfield: FieldData) => {
    if (typeof subfield === 'object' && 'node' in subfield) children.push(subfield)
  })
  return children
}

interface TextLiteralFields {
  open: NodeChild<SyncTokenId> | undefined
  newline: NodeChild<SyncTokenId> | undefined
  elements: TextElement[]
  close: NodeChild<SyncTokenId> | undefined
}
/** TODO: Add docs */
export class TextLiteral extends BaseExpression {
  declare fields: FixedMapView<AstFields & TextLiteralFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & TextLiteralFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableTextLiteral> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableTextLiteral) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    newline: NodeChild<Token> | undefined,
    elements: TextElement<OwnedRefs>[],
    close: NodeChild<Token> | undefined,
  ) {
    const base = module.baseObject('TextLiteral')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      open,
      newline,
      elements: elements.map((e) => mapRefs(e, ownedToRaw(module, id_))),
      close,
    })
    return asOwned(new MutableTextLiteral(module, fields))
  }

  /** TODO: Add docs */
  static new(rawText: string, module?: MutableModule): Owned<MutableTextLiteral> {
    const escaped = escapeTextLiteral(rawText)
    const parsed = parseExpression(`'${escaped}'`, module)
    if (!(parsed instanceof MutableTextLiteral)) {
      console.error(`Failed to escape string for interpolated text`, rawText, escaped, parsed)
      const safeText = rawText.replaceAll(/[^-+A-Za-z0-9_. ]/g, '')
      return TextLiteral.new(safeText, module)
    }
    return parsed
  }

  /**
   * Return the literal value of the string with all escape sequences applied, but without
   * evaluating any interpolated expressions.
   */
  get rawTextContent(): string {
    return uninterpolatedText(this.fields.get('elements'), this.module)
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim, indent }: PrintContext): IterableIterator<RawConcreteChild> {
    const { open, newline, elements, close } = getAll(this.fields)
    if (open) yield firstChild(open)
    const rawChildren = new Array<RawNodeChild>()
    if (newline) rawChildren.push(newline)
    rawChildren.push(...fieldRawChildren(elements))
    let nextTokenSpacing: 'unspaced' | 'maybe-spaced' | 'indented' = 'unspaced'
    let blockIndent: string | undefined = undefined
    for (const raw of rawChildren) {
      const tokenType = isToken(raw.node) ? raw.node.tokenType_ : null
      const specialToken =
        tokenType === TokenType.Newline ? 'newline'
        : tokenType === TokenType.OpenSymbol || tokenType === TokenType.CloseSymbol ?
          'splice-delimiter'
        : null
      if (specialToken === 'newline') {
        yield preferUnspaced(raw)
      } else if (nextTokenSpacing === 'indented') {
        const whitespace: string =
          blockIndent ??
          (raw.whitespace && raw.whitespace.length > (indent || '').length ?
            raw.whitespace
          : undefined) ??
          (indent || '') + '    '
        blockIndent = whitespace
        yield withWhitespace(raw, whitespace)
      } else if (specialToken === 'splice-delimiter' || nextTokenSpacing === 'maybe-spaced') {
        yield preferUnspaced(raw)
      } else {
        yield ensureUnspaced(raw, verbatim)
      }
      nextTokenSpacing =
        specialToken === 'newline' ? 'indented'
        : specialToken === 'splice-delimiter' ? 'maybe-spaced'
        : 'unspaced'
    }
    if (close) yield ensureUnspaced(close, verbatim)
  }

  /** TODO: Add docs */
  boundaryTokenCode(): string | undefined {
    return (this.open || this.close)?.code()
  }

  /** TODO: Add docs */
  isInterpolated(): boolean {
    const token = this.boundaryTokenCode()
    return token === "'" || token === "'''"
  }

  /** TODO: Add docs */
  get open(): Token | undefined {
    return this.module.getToken(this.fields.get('open')?.node)
  }

  /** TODO: Add docs */
  get close(): Token | undefined {
    return this.module.getToken(this.fields.get('close')?.node)
  }

  /** TODO: Add docs */
  get elements(): TextElement<ConcreteRefs>[] {
    return this.fields.get('elements').map((e) => mapRefs(e, rawToConcrete(this.module)))
  }
}
/** TODO: Add docs */
export class MutableTextLiteral extends TextLiteral implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & TextLiteralFields>

  setBoundaries(code: string) {
    this.fields.set('open', unspaced(Token.new(code)))
    this.fields.set('close', unspaced(Token.new(code)))
  }

  setElements(elements: TextElement<OwnedRefs>[]) {
    this.fields.set(
      'elements',
      elements.map((e) => mapRefs(e, ownedToRaw(this.module, this.id))),
    )
  }

  /**
   * Set literal value of the string. The code representation of assigned text will be automatically
   * transformed to use escape sequences when necessary.
   */
  setRawTextContent(rawText: string) {
    let boundary = this.boundaryTokenCode()
    const isInterpolated = this.isInterpolated()
    const mustBecomeInterpolated = !isInterpolated && (!boundary || rawText.match(/["\n\r]/))
    if (mustBecomeInterpolated) {
      boundary = "'"
      this.setBoundaries(boundary)
    }
    const literalContents =
      isInterpolated || mustBecomeInterpolated ? escapeTextLiteral(rawText) : rawText
    const parsed = parseExpression(`${boundary}${literalContents}${boundary}`)
    assert(parsed instanceof TextLiteral)
    const elements = parsed.elements.map((e) => mapRefs(e, concreteToOwned(this.module)))
    this.setElements(elements)
  }
}
export interface MutableTextLiteral extends TextLiteral, MutableExpression {}
applyMixins(MutableTextLiteral, [MutableAst])

interface ExpressionStatementFields {
  docLine: DocLine | undefined
  docText: Y.Text
  expression: NodeChild<AstId>
}
/** TODO: Add docs */
export class ExpressionStatement extends BaseStatement {
  declare fields: FixedMapView<AstFields & ExpressionStatementFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & ExpressionStatementFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(
    source: string,
    module?: MutableModule,
  ): Owned<MutableExpressionStatement> | undefined {
    const parsed = parseStatement(source, module)
    if (parsed instanceof MutableExpressionStatement) return parsed
  }

  /** TODO: Add docs */
  static new(
    expression: Owned<MutableExpression>,
    options: {
      documentation?: string | undefined
      edit?: MutableModule | undefined
    } = {},
  ) {
    return this.concrete(
      options.edit ?? MutableModule.Transient(),
      options.documentation != null ?
        elementsToDocLine(textToUninterpolatedElements(options.documentation))
      : undefined,
      autospaced(expression),
    )
  }

  /** TODO: Add docs */
  static documented(documentation: string, expression: Owned<MutableExpression>) {
    return this.new(expression, { documentation, edit: expression.module })
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    docLine: DocLine<OwnedRefs> | undefined,
    expression: NodeChild<Owned<MutableExpression>>,
  ) {
    const base = module.baseObject('ExpressionStatement')
    const id_ = base.get('id')
    const rawDocLine = docLine && mapRefs(docLine, ownedToRaw(module, id_))
    const fields = composeFieldData(base, {
      docLine: rawDocLine,
      docText: new Y.Text(docLineToText(rawDocLine, module) ?? ''),
      expression: concreteChild(module, expression, id_),
    })
    return asOwned(new MutableExpressionStatement(module, fields))
  }

  /** TODO: Add docs */
  get expression(): Expression {
    return this.module.get(this.fields.get('expression').node) as Expression
  }

  /**
   * Returns the documentation text for editing. The returned object may be used to edit the documentation directly,
   * without creating an edit module to explicitly commit it.
   */
  mutableDocumentationText(): Y.Text {
    return (this.fields as MutableExpressionStatement['fields']).get('docText')
  }

  /** TODO: Add docs */
  *concreteChildren({ indent }: PrintContext): IterableIterator<RawConcreteChild> {
    const { docLine, docText, expression } = getAll(this.fields)
    const docTokens = docTextToConcrete(docLine, docText, indent, this.module)
    const prevLine = !!docTokens
    if (docTokens) yield* docTokens
    yield prevLine ? { whitespace: indent || '', node: expression.node } : firstChild(expression)
  }
}
function docLineToText(
  docLine: DeepReadonly<DocLine> | undefined,
  module: Module,
): string | undefined {
  if (docLine == null) return
  const raw = uninterpolatedText(docLine.docs.elements, module)
  return raw.startsWith(' ') ? raw.slice(1) : raw
}
/** TODO: Add docs */
export class MutableExpressionStatement extends ExpressionStatement implements MutableStatement {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & ExpressionStatementFields>

  setExpression<T extends MutableExpression>(value: Owned<T>) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }
}
export interface MutableExpressionStatement extends ExpressionStatement, MutableAst {
  isAllowedInStatementContext(): true
  isAllowedInExpressionContext(): boolean
  get expression(): MutableExpression
}
applyMixins(MutableExpressionStatement, [MutableAst])

function textToUninterpolatedElements(text: string): TextToken<OwnedRefs>[] {
  text = text && ' ' + text
  const elements = new Array<TextToken<OwnedRefs>>()
  text.split('\n').forEach((line, i) => {
    if (i)
      elements.push({
        type: 'token',
        token: unspaced(Token.new('\n', TokenType.Newline)),
      })
    elements.push({
      type: 'token',
      token: autospaced(Token.new(line, TokenType.TextSection)),
    })
  })
  return elements
}

function elementsToDocLine(elements: TextToken<OwnedRefs>[]): DocLine<OwnedRefs> {
  return {
    docs: {
      open: unspaced(Token.new('##', TokenType.TextStart)),
      elements,
    },
    newlines: [unspaced(Token.new('\n', TokenType.Newline))],
  }
}

interface InvalidFields {
  expression: NodeChild<AstId>
}
/** TODO: Add docs */
export class Invalid extends Ast implements Statement, Expression {
  /** See {@link Ast.isAllowedInStatementContext}. */
  override isAllowedInStatementContext(): true {
    return true
  }
  /** See {@link Ast.isAllowedInExpressionContext}. */
  override isAllowedInExpressionContext(): true {
    return true
  }
  declare fields: FixedMapView<AstFields & InvalidFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & InvalidFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static concrete(module: MutableModule, expression: NodeChild<Owned>) {
    const base = module.baseObject('Invalid')
    return asOwned(new MutableInvalid(module, invalidFields(module, base, expression)))
  }

  /** TODO: Add docs */
  get expression(): Ast {
    return this.module.get(this.fields.get('expression').node)
  }

  /** TODO: Add docs */
  *concreteChildren(_printContext: PrintContext): IterableIterator<RawConcreteChild> {
    yield firstChild(this.fields.get('expression'))
  }

  /** TODO: Add docs */
  override printSubtree(
    info: SpanMap,
    offset: number,
    parentIndent: string | null,
    _verbatim?: boolean,
  ): string {
    return super.printSubtree(info, offset, parentIndent, true)
  }
}
/** TODO: Add docs */
export function invalidFields(
  module: MutableModule,
  base: FixedMap<AstFields>,
  expression: NodeChild<Owned>,
): FixedMap<AstFields & InvalidFields> {
  const id_ = base.get('id')
  return composeFieldData(base, { expression: concreteChild(module, expression, id_) })
}
/** TODO: Add docs */
export class MutableInvalid extends Invalid implements MutableStatement, MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & InvalidFields>
}
export interface MutableInvalid extends Invalid, MutableAst {
  isAllowedInStatementContext(): true
  isAllowedInExpressionContext(): true
  /**
   * The `expression` getter is intentionally not narrowed to provide mutable access:
   * It makes more sense to `.replace` the `Invalid` node.
   */
}
applyMixins(MutableInvalid, [MutableAst])

interface GroupFields {
  open: NodeChild<SyncTokenId> | undefined
  expression: NodeChild<AstId> | undefined
  close: NodeChild<SyncTokenId> | undefined
}
/** TODO: Add docs */
export class Group extends BaseExpression {
  declare fields: FixedMapView<AstFields & GroupFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & GroupFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableGroup> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableGroup) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    expression: NodeChild<Owned<MutableExpression>> | undefined,
    close: NodeChild<Token> | undefined,
  ) {
    const base = module.baseObject('Group')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      open,
      expression: concreteChild(module, expression, id_),
      close,
    })
    return asOwned(new MutableGroup(module, fields))
  }

  /** TODO: Add docs */
  static new(module: MutableModule, expression: Owned<MutableExpression>) {
    const open = unspaced(Token.new('(', TokenType.OpenSymbol))
    const close = unspaced(Token.new(')', TokenType.CloseSymbol))
    return this.concrete(module, open, unspaced(expression), close)
  }

  /** TODO: Add docs */
  get expression(): Expression | undefined {
    return this.module.get(this.fields.get('expression')?.node) as Expression | undefined
  }

  /** TODO: Add docs */
  *concreteChildren(_printContext: PrintContext): IterableIterator<RawConcreteChild> {
    const { open, expression, close } = getAll(this.fields)
    if (open) yield firstChild(open)
    const spaced = ((open && expression?.whitespace) ?? '') !== ''
    if (expression) yield open ? preferSpacedIf(expression, spaced) : firstChild(expression)
    if (close) yield (open ?? expression) ? preferSpacedIf(close, spaced) : firstChild(close)
  }
}
/** TODO: Add docs */
export class MutableGroup extends Group implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & GroupFields>

  setExpression<T extends MutableExpression>(value: Owned<T> | undefined) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }
}
export interface MutableGroup extends Group, MutableExpression {
  get expression(): MutableExpression | undefined
}
applyMixins(MutableGroup, [MutableAst])

interface NumericLiteralFields {
  tokens: NodeChild<SyncTokenId>[]
}
/** TODO: Add docs */
export class NumericLiteral extends BaseExpression {
  declare fields: FixedMapView<AstFields & NumericLiteralFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & NumericLiteralFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(
    source: string,
    module?: MutableModule,
  ): Owned<MutableNumericLiteral> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableNumericLiteral) return parsed
  }

  /** TODO: Add docs */
  static tryParseWithSign(
    source: string,
    module?: MutableModule,
  ): Owned<MutableNumericLiteral | MutableNegationApp> | undefined {
    const parsed = parseExpression(source, module)
    if (
      parsed instanceof MutableNumericLiteral ||
      (parsed instanceof MutableNegationApp && parsed.argument instanceof MutableNumericLiteral)
    )
      return parsed
  }

  /** TODO: Add docs */
  static concrete(module: MutableModule, tokens: NodeChild<Token>[]) {
    const base = module.baseObject('NumericLiteral')
    const fields = composeFieldData(base, { tokens })
    return asOwned(new MutableNumericLiteral(module, fields))
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const [first, ...rest] = this.fields.get('tokens')
    assertDefined(first)
    yield firstChild(first)
    for (const child of rest) yield ensureUnspaced(child, verbatim)
  }
}
/** TODO: Add docs */
export class MutableNumericLiteral extends NumericLiteral implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & NumericLiteralFields>
}
export interface MutableNumericLiteral extends NumericLiteral, MutableExpression {}
applyMixins(MutableNumericLiteral, [MutableAst])

/** TODO: Add docs */
export function isNumericLiteral(code: string) {
  return is_numeric_literal(code)
}

export interface ArgumentDefinition<T extends TreeRefs = RawRefs> {
  open?: T['token'] | undefined
  open2?: T['token'] | undefined
  suspension?: T['token'] | undefined
  pattern: T['expression']
  type?: ArgumentType<T> | undefined
  close2?: T['token'] | undefined
  defaultValue?: ArgumentDefault<T> | undefined
  close?: T['token'] | undefined
}

interface ArgumentDefault<T extends TreeRefs = RawRefs> {
  equals: T['token']
  expression: T['ast']
}

interface ArgumentType<T extends TreeRefs = RawRefs> {
  operator: T['token']
  type: T['ast']
}

interface DocComment<T extends TreeRefs = RawRefs> {
  open: T['token']
  elements: TextToken<T>[]
}

export interface DocLine<T extends TreeRefs = RawRefs> {
  docs: DocComment<T>
  newlines: T['token'][]
}

interface FunctionAnnotation<T extends TreeRefs = RawRefs> {
  operator: T['token']
  annotation: T['token']
  argument: T['ast'] | undefined
}

interface AnnotationLine<T extends TreeRefs = RawRefs> {
  annotation: FunctionAnnotation<T>
  newlines: T['token'][]
}

interface TypeSignature<T extends TreeRefs = RawRefs> {
  name: T['ast']
  operator: T['token']
  type: T['ast']
}

interface SignatureLine<T extends TreeRefs = RawRefs> {
  signature: TypeSignature<T>
  newlines: T['token'][]
}

export interface FunctionDefFields<T extends TreeRefs = RawRefs> {
  docLine: DocLine<T> | undefined
  docLineMarkdownHash: string | undefined
  docMarkdown: Y.Text
  annotationLines: AnnotationLine<T>[]
  signatureLine: SignatureLine<T> | undefined
  private_: T['token'] | undefined
  name: T['ast']
  argumentDefinitions: ArgumentDefinition<T>[]
  equals: T['token']
  body: T['ast'] | undefined
}
/** TODO: Add docs */
export class FunctionDef extends BaseStatement {
  declare fields: FixedMapView<AstFields & FunctionDefFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & FunctionDefFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableFunctionDef> | undefined {
    const parsed = parseStatement(source, module)
    if (parsed instanceof MutableFunctionDef) return parsed
  }

  /**
   * Returns the Markdown model of the documentation. The returned object may be used to edit the documentation
   * directly, without creating an edit module to explicitly commit it.
   */
  mutableDocumentationMarkdown(): Y.Text {
    return (this.fields as FixedMap<AstFields & FunctionDefFields>).get('docMarkdown')
  }
  /** TODO: Add docs */
  get name(): Expression {
    return this.module.get(this.fields.get('name').node) as Expression
  }
  /** TODO: Add docs */
  get body(): BodyBlock | Expression | undefined {
    return this.module.get(this.fields.get('body')?.node) as Expression | BodyBlock | undefined
  }
  /** TODO: Add docs */
  get argumentDefinitions(): ArgumentDefinition<ConcreteRefs>[] {
    return this.fields
      .get('argumentDefinitions')
      .map((def) => mapRefs(def, rawToConcrete(this.module)))
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    fields: Partial<FunctionDefFields<OwnedRefs>> & {
      name: object
      equals: object
      docMarkdown: object
    },
  ) {
    const base = module.baseObject('FunctionDef')
    const id_ = base.get('id')
    const rawFields = composeFieldData(base, {
      docLine: fields.docLine && mapRefs(fields.docLine, ownedToRaw(module, id_)),
      docLineMarkdownHash: fields.docLineMarkdownHash,
      docMarkdown: fields.docMarkdown,
      annotationLines: (fields.annotationLines ?? []).map((anno) =>
        mapRefs(anno, ownedToRaw(module, id_)),
      ),
      signatureLine: fields.signatureLine && mapRefs(fields.signatureLine, ownedToRaw(module, id_)),
      private_: fields.private_,
      name: concreteChild(module, fields.name, id_),
      argumentDefinitions: (fields.argumentDefinitions ?? []).map((def) =>
        mapRefs(def, ownedToRaw(module, id_)),
      ),
      equals: fields.equals,
      body: concreteChild(module, fields.body, id_),
    })
    return asOwned(new MutableFunctionDef(module, rawFields))
  }

  /** TODO: Add docs */
  static new(
    name: IdentLike,
    args: (ArgumentDefinition<OwnedRefs> | StrictIdentLike)[],
    body: Owned<MutableExpression> | Owned<MutableBodyBlock>,
    options: {
      documentation?: string
      edit?: MutableModule
    } = {},
  ): Owned<MutableFunctionDef> {
    const module = options.edit ?? MutableModule.Transient()
    const argumentDefinitions = args.map((arg) =>
      typeof arg === 'string' || isToken(arg) ?
        {
          pattern: autospaced(Ident.new(module, arg)),
        }
      : arg,
    )
    return MutableFunctionDef.concrete(module, {
      docLine: undefined,
      docLineMarkdownHash: undefined,
      docMarkdown: new Y.Text(options.documentation ?? ''),
      // Note that a function name may not be an operator if the function is not in the body of a type definition, but
      // we can't easily enforce that because we don't currently make a syntactic distinction between top-level
      // functions and type methods.
      name: autospaced(Ident.newAllowingOperators(module, name)),
      argumentDefinitions,
      equals: autospaced(makeEquals()),
      body: autospaced(body),
    })
  }

  /** TODO: Add docs */
  *bodyExpressions(): IterableIterator<Expression | Statement> {
    const body = this.body
    if (body instanceof BodyBlock) {
      yield* body.statements()
    } else if (body) {
      yield body
    }
  }

  /** TODO: Add docs */
  *concreteChildren({ indent, verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const {
      docLine,
      docLineMarkdownHash,
      docMarkdown,
      annotationLines,
      signatureLine,
      private_,
      name,
      argumentDefinitions,
      equals,
      body,
    } = getAll(this.fields)
    // True/false: Previous is newline or non-newline; undefined: No prev yet.
    let prevIsNewline: boolean | undefined = undefined
    const maybeIndented = <T>(nodeChild: NodeChild<T>) => {
      if (prevIsNewline) {
        prevIsNewline = false
        return { whitespace: indent || '', node: nodeChild.node }
      } else if (prevIsNewline == null) {
        prevIsNewline = false
        return firstChild(nodeChild)
      } else {
        return preferUnspaced(nodeChild)
      }
    }
    const docs = functionDocsToConcrete(docMarkdown.toJSON(), docLineMarkdownHash, docLine, indent)
    if (docs) {
      yield* docs
      prevIsNewline = true
    }
    for (const anno of annotationLines) {
      const { operator, annotation, argument } = anno.annotation
      yield maybeIndented(operator)
      yield preferUnspaced(annotation)
      if (argument) yield ensureSpaced(argument, verbatim)
      for (const newline of anno.newlines) yield preferUnspaced(newline)
      prevIsNewline = true
    }
    if (signatureLine) {
      const { name, operator, type } = signatureLine.signature
      yield maybeIndented(name)
      const spaced = (operator.whitespace ?? type.whitespace ?? ' ') !== ''
      yield ensureSpacedOnlyIf(operator, spaced, verbatim)
      yield ensureSpacedOnlyIf(type, spaced, verbatim)
      for (const newline of signatureLine.newlines) yield preferUnspaced(newline)
      prevIsNewline = true
    }
    if (private_) yield maybeIndented(private_)
    yield maybeIndented(name)
    for (const def of argumentDefinitions) yield* argumentDefinitionToConcrete(def, verbatim)
    yield { whitespace: equals.whitespace ?? ' ', node: this.module.getToken(equals.node) }
    if (body)
      yield preferSpacedIf(
        body,
        !!equals.whitespace && !(this.module.tryGet(body.node) instanceof BodyBlock),
      )
  }
}
function* argumentDefinitionToConcrete(def: DeepReadonly<ArgumentDefinition>, verbatim: boolean) {
  const { open, open2, suspension, pattern, type, close2, defaultValue, close } = def
  if (open) yield ensureSpaced(open, verbatim)
  const spacedInsideParen1 = open && ((open2 ?? suspension ?? pattern).whitespace ?? '') !== ''
  if (open2) yield ensureSpacedOnlyIf(open2, spacedInsideParen1 ?? true, verbatim)
  const spacedInsideParen2 = open2 && ((suspension ?? pattern).whitespace ?? '') !== ''
  if (suspension) {
    yield ensureSpacedOnlyIf(suspension, spacedInsideParen2 ?? spacedInsideParen1 ?? true, verbatim)
    yield ensureUnspaced(pattern, verbatim)
  } else {
    yield ensureSpacedOnlyIf(pattern, spacedInsideParen2 ?? spacedInsideParen1 ?? true, verbatim)
  }
  if (type) {
    const spaced = (type.operator.whitespace ?? type.type.whitespace ?? ' ') !== ''
    yield ensureSpacedOnlyIf(type.operator, spaced, verbatim)
    yield ensureSpacedOnlyIf(type.type, spaced, verbatim)
  }
  if (defaultValue) {
    const spaced =
      (defaultValue.equals.whitespace ?? defaultValue.expression.whitespace ?? ' ') !== ''
    yield ensureSpacedOnlyIf(defaultValue.equals, spaced, verbatim)
    yield ensureSpacedOnlyIf(defaultValue.expression, spaced, verbatim)
  }
  if (close2) yield ensureSpacedOnlyIf(close2, spacedInsideParen2 ?? false, verbatim)
  if (close) yield ensureSpacedOnlyIf(close, spacedInsideParen1 ?? false, verbatim)
}
/** TODO: Add docs */
export class MutableFunctionDef extends FunctionDef implements MutableStatement {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & FunctionDefFields>

  setName<T extends MutableExpression>(value: Owned<T>) {
    this.fields.set('name', unspaced(this.claimChild(value)))
  }
  setBody<T extends MutableExpression | MutableBodyBlock>(value: Owned<T> | undefined) {
    this.fields.set('body', unspaced(this.claimChild(value)))
  }
  setArgumentDefinitions(defs: ArgumentDefinition<OwnedRefs>[]) {
    this.fields.set(
      'argumentDefinitions',
      defs.map((def) => mapRefs(def, ownedToRaw(this.module, this.id))),
    )
  }

  /** Returns the body, after converting it to a block if it was empty or an inline expression. */
  bodyAsBlock(): MutableBodyBlock {
    const oldBody = this.body
    if (oldBody instanceof MutableBodyBlock) return oldBody
    const newBody = BodyBlock.new([], this.module)
    if (oldBody) newBody.push(oldBody.take())
    this.setBody(newBody)
    return newBody
  }
}
export interface MutableFunctionDef extends FunctionDef, MutableStatement {
  get name(): MutableExpression
  get body(): MutableExpression | MutableBodyBlock | undefined
}
applyMixins(MutableFunctionDef, [MutableAst])

interface PrintContext {
  verbatim: boolean
  indent: string | null
}

interface AssignmentFields {
  docLine: DocLine | undefined
  docText: Y.Text
  pattern: NodeChild<AstId>
  equals: NodeChild<SyncTokenId>
  expression: NodeChild<AstId>
}
/** TODO: Add docs */
export class Assignment extends BaseStatement {
  declare fields: FixedMapView<AstFields & AssignmentFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & AssignmentFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableAssignment> | undefined {
    const parsed = parseStatement(source, module)
    if (parsed instanceof MutableAssignment) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    docLine: DocLine<OwnedRefs> | undefined,
    pattern: NodeChild<Owned<MutableExpression>>,
    equals: NodeChild<Token>,
    expression: NodeChild<Owned<MutableExpression>>,
  ) {
    const base = module.baseObject('Assignment')
    const id_ = base.get('id')
    const rawDocLine = docLine && mapRefs(docLine, ownedToRaw(module, id_))
    const fields = composeFieldData(base, {
      docLine: rawDocLine,
      docText: new Y.Text(docLineToText(rawDocLine, module) ?? ''),
      pattern: concreteChild(module, pattern, id_),
      equals,
      expression: concreteChild(module, expression, id_),
    })
    return asOwned(new MutableAssignment(module, fields))
  }

  /** TODO: Add docs */
  static new(
    ident: StrictIdentLike | Owned<MutableIdent>,
    expression: Owned<MutableExpression>,
    options: { edit?: MutableModule | undefined; documentation?: string | undefined },
  ) {
    const module = options.edit ?? MutableModule.Transient()
    return Assignment.concrete(
      module,
      options.documentation != null ?
        elementsToDocLine(textToUninterpolatedElements(options.documentation))
      : undefined,
      autospaced(ident instanceof MutableIdent ? ident : Ident.new(module, ident)),
      spaced(makeEquals()),
      spaced(expression),
    )
  }

  /** TODO: Add docs */
  get pattern(): Expression {
    return this.module.get(this.fields.get('pattern').node) as Expression
  }
  /** TODO: Add docs */
  get expression(): Expression {
    return this.module.get(this.fields.get('expression').node) as Expression
  }

  /**
   * Returns the documentation text for editing. The returned object may be used to edit the documentation directly,
   * without creating an edit module to explicitly commit it.
   */
  mutableDocumentationText(): Y.Text {
    return (this.fields as MutableAssignment['fields']).get('docText')
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim, indent }: PrintContext): IterableIterator<RawConcreteChild> {
    const { docLine, docText, pattern, equals, expression } = getAll(this.fields)
    const docTokens = docTextToConcrete(docLine, docText, indent, this.module)
    const prevLine = !!docTokens
    if (docTokens) yield* docTokens
    yield prevLine ? { whitespace: indent || '', node: pattern.node } : firstChild(pattern)
    yield ensureSpacedOnlyIf(equals, expression.whitespace !== '', verbatim)
    yield preferSpaced(expression)
  }
}
/** TODO: Add docs */
export class MutableAssignment extends Assignment implements MutableStatement {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & AssignmentFields>

  setPattern<T extends MutableExpression>(value: Owned<T>) {
    this.fields.set('pattern', unspaced(this.claimChild(value)))
  }
  setExpression<T extends MutableExpression>(value: Owned<T>) {
    setNode(this.fields, 'expression', this.claimChild(value))
  }
}
export interface MutableAssignment extends Assignment, MutableAst {
  isAllowedInStatementContext(): true
  isAllowedInExpressionContext(): boolean
  get pattern(): MutableExpression
  get expression(): MutableExpression
}
applyMixins(MutableAssignment, [MutableAst])

interface BodyBlockFields {
  lines: RawBlockLine[]
}
/** TODO: Add docs */
export class BodyBlock extends BaseExpression {
  declare fields: FixedMapView<AstFields & BodyBlockFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & BodyBlockFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static concrete(module: MutableModule, lines: OwnedBlockLine[]) {
    const base = module.baseObject('BodyBlock')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      lines: lines.map((line) => lineToRaw(line, module, id_)),
    })
    return asOwned(new MutableBodyBlock(module, fields))
  }

  /** TODO: Add docs */
  static new(lines: OwnedBlockLine[], module: MutableModule) {
    return BodyBlock.concrete(module, lines)
  }

  /** TODO: Add docs */
  get lines(): BlockLine[] {
    return this.fields.get('lines').map((line) => lineFromRaw(line, this.module))
  }

  /** TODO: Add docs */
  *statements(): IterableIterator<Statement> {
    for (const line of this.lines) {
      if (line.statement) yield line.statement.node
    }
  }

  /** TODO: Add docs */
  *concreteChildren({ indent }: PrintContext): IterableIterator<RawConcreteChild> {
    for (const line of this.fields.get('lines')) {
      yield preferUnspaced(line.newline)
      if (line.statement) {
        const whitespace: string =
          (line.statement.whitespace && line.statement.whitespace.length > (indent || '').length ?
            line.statement.whitespace
          : undefined) ?? (indent != null ? indent + '    ' : '')
        yield { whitespace, node: line.statement.node }
      }
    }
  }
}
/** TODO: Add docs */
export class MutableBodyBlock extends BodyBlock implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & BodyBlockFields>

  updateLines(map: (lines: OwnedBlockLine[]) => OwnedBlockLine[]) {
    return this.setLines(map(this.takeLines()))
  }
  takeLines(): OwnedBlockLine[] {
    return this.fields.get('lines').map((line) => ownedLineFromRaw(line, this.module))
  }
  setLines(lines: OwnedBlockLine[]) {
    this.fields.set(
      'lines',
      lines.map((line) => lineToRaw(line, this.module, this.id)),
    )
  }

  /** Remove statements matching the given predicate from the block and return them. */
  extractIf(predicate: (statement: Statement) => boolean): OwnedBlockLine[] {
    const extracted = new Array<OwnedBlockLine>()
    this.updateLines((lines) => {
      const remaining: OwnedBlockLine[] = []
      for (const line of lines) {
        const ast = line.statement?.node
        if (!ast) continue
        ;(predicate(ast) ? extracted : remaining).push(line)
      }
      return remaining
    })
    return extracted
  }

  /** Insert the given statement(s) starting at the specified line index. */
  insert(index: number, ...statements: (Owned<MutableStatement> | undefined)[]) {
    const before = this.fields.get('lines').slice(0, index)
    const insertions = statements.map((statement) => ({
      newline: unspaced(Token.new('\n', TokenType.Newline)),
      statement: statement && unspaced(this.claimChild(statement)),
    }))
    const after = this.fields.get('lines').slice(index)
    this.fields.set('lines', [...before, ...insertions, ...after])
  }

  push(statementOrExpression: Owned<MutableStatement> | Owned<MutableExpression>) {
    const oldLines = this.fields.get('lines')
    const statement = toStatement(statementOrExpression, this.module)
    const newLine = {
      newline: unspaced(Token.new('\n', TokenType.Newline)),
      statement: unspaced(this.claimChild(statement)),
    }
    this.fields.set('lines', [...oldLines, newLine])
  }

  filter(keep: (ast: MutableStatement) => boolean) {
    const oldLines = this.fields.get('lines')
    const filteredLines = oldLines.filter((line) => {
      if (!line.statement) return true
      return keep(this.module.get(line.statement.node) as MutableStatement)
    })
    this.fields.set('lines', filteredLines)
  }
}
export interface MutableBodyBlock extends BodyBlock, MutableExpression {
  statements(): IterableIterator<MutableStatement>
}
applyMixins(MutableBodyBlock, [MutableAst])

interface RawLine<T extends TreeRefs> {
  newline: T['token']
  statement: T['statement'] | undefined
}

interface Line<T extends TreeRefs> {
  newline?: T['token'] | undefined
  statement: T['statement'] | undefined
}

type RawBlockLine = RawLine<RawRefs>
export type BlockLine = Line<ConcreteRefs>
export type OwnedBlockLine = Line<OwnedRefs>
export type MutableBlockLine = Line<MutableRefs>

function lineFromRaw(raw: RawBlockLine, module: Module): BlockLine {
  const expression = raw.statement ? (module.get(raw.statement.node) as Statement) : undefined
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    statement:
      expression ?
        {
          whitespace: raw.statement?.whitespace,
          node: expression,
        }
      : undefined,
  }
}

function ownedLineFromRaw(raw: RawBlockLine, module: MutableModule): OwnedBlockLine {
  const expression =
    raw.statement ?
      (module.get(raw.statement.node).takeIfParented() as Owned<MutableStatement>)
    : undefined
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    statement:
      expression ?
        {
          whitespace: raw.statement?.whitespace,
          node: expression,
        }
      : undefined,
  }
}

function lineToRaw(line: OwnedBlockLine, module: MutableModule, block: AstId): RawBlockLine {
  return {
    newline: line.newline ?? unspaced(Token.new('\n', TokenType.Newline)),
    statement:
      line.statement ?
        {
          whitespace: line.statement?.whitespace,
          node: claimChild(module, line.statement.node, block),
        }
      : undefined,
  }
}

interface IdentFields {
  token: NodeChild<SyncTokenId>
}
/** TODO: Add docs */
export class Ident extends BaseExpression {
  declare fields: FixedMapView<AstFields & IdentFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & IdentFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableIdent> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableIdent) return parsed
  }

  /** TODO: Add docs */
  get token(): IdentifierToken {
    return this.module.getToken(this.fields.get('token').node) as IdentifierToken
  }

  /** TODO: Add docs */
  isTypeOrConstructor(): boolean {
    return /^[A-Z]/.test(this.token.code())
  }

  /** TODO: Add docs */
  static concrete(module: MutableModule, token: NodeChild<Token>) {
    const base = module.baseObject('Ident')
    const fields = composeFieldData(base, { token })
    return asOwned(new MutableIdent(module, fields))
  }

  /** TODO: Add docs */
  static new(module: MutableModule, ident: StrictIdentLike) {
    return Ident.concrete(module, unspaced(toIdentStrict(ident)))
  }

  /** @internal */
  static newAllowingOperators(module: MutableModule, ident: IdentLike) {
    return Ident.concrete(module, unspaced(toIdent(ident)))
  }

  /** TODO: Add docs */
  *concreteChildren(_printContext: PrintContext): IterableIterator<RawConcreteChild> {
    yield firstChild(this.fields.get('token'))
  }

  /** TODO: Add docs */
  override code(): Identifier {
    return this.token.code() as Identifier
  }
}
/** TODO: Add docs */
export class MutableIdent extends Ident implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & IdentFields>

  setToken(ident: IdentLike) {
    this.fields.set('token', unspaced(toIdent(ident)))
  }

  override code(): Identifier {
    return this.token.code()
  }
}
export interface MutableIdent extends Ident, MutableExpression {}
applyMixins(MutableIdent, [MutableAst])

interface WildcardFields {
  token: NodeChild<SyncTokenId>
}
/** TODO: Add docs */
export class Wildcard extends BaseExpression {
  declare fields: FixedMapView<AstFields & WildcardFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & WildcardFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableWildcard> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableWildcard) return parsed
  }

  /** TODO: Add docs */
  get token(): Token {
    return this.module.getToken(this.fields.get('token').node)
  }

  /** TODO: Add docs */
  static concrete(module: MutableModule, token: NodeChild<Token>) {
    const base = module.baseObject('Wildcard')
    const fields = composeFieldData(base, { token })
    return asOwned(new MutableWildcard(module, fields))
  }

  /** TODO: Add docs */
  static new(module?: MutableModule) {
    const token = Token.new('_', TokenType.Wildcard)
    return this.concrete(module ?? MutableModule.Transient(), unspaced(token))
  }

  /** TODO: Add docs */
  *concreteChildren(_printContext: PrintContext): IterableIterator<RawConcreteChild> {
    yield firstChild(this.fields.get('token'))
  }
}

/** TODO: Add docs */
export class MutableWildcard extends Wildcard implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & WildcardFields>
}
export interface MutableWildcard extends Wildcard, MutableExpression {}
applyMixins(MutableWildcard, [MutableAst])

type AbstractVectorElement<T extends TreeRefs> = {
  delimiter?: T['token']
  value: T['ast'] | undefined
}
function delimitVectorElement(element: AbstractVectorElement<OwnedRefs>): VectorElement<OwnedRefs> {
  return {
    ...element,
    delimiter: element.delimiter ?? unspaced(Token.new(',', TokenType.Operator)),
  }
}
type VectorElement<T extends TreeRefs> = { delimiter: T['token']; value: T['ast'] | undefined }
interface VectorFields {
  open: NodeChild<Token>
  elements: VectorElement<RawRefs>[]
  close: NodeChild<Token>
}
/** TODO: Add docs */
export class Vector extends BaseExpression {
  declare fields: FixedMapView<AstFields & VectorFields>
  /** TODO: Add docs */
  constructor(module: Module, fields: FixedMapView<AstFields & VectorFields>) {
    super(module, fields)
  }

  /** TODO: Add docs */
  static tryParse(source: string, module?: MutableModule): Owned<MutableVector> | undefined {
    const parsed = parseExpression(source, module)
    if (parsed instanceof MutableVector) return parsed
  }

  /** TODO: Add docs */
  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    elements: AbstractVectorElement<OwnedRefs>[],
    close: NodeChild<Token> | undefined,
  ) {
    const base = module.baseObject('Vector')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      open: open ?? unspaced(Token.new('[', TokenType.OpenSymbol)),
      elements: elements.map(delimitVectorElement).map((e) => mapRefs(e, ownedToRaw(module, id_))),
      close: close ?? unspaced(Token.new(']', TokenType.CloseSymbol)),
    })
    return asOwned(new MutableVector(module, fields))
  }

  /** TODO: Add docs */
  static new(module: MutableModule, elements: Owned[]) {
    return this.concrete(
      module,
      undefined,
      elements.map((value) => ({ value: autospaced(value) })),
      undefined,
    )
  }

  /** TODO: Add docs */
  static tryBuild<T>(
    inputs: Iterable<T>,
    elementBuilder: (input: T, module: MutableModule) => Owned,
    edit?: MutableModule,
  ): Owned<MutableVector>
  /** TODO: Add docs */
  static tryBuild<T>(
    inputs: Iterable<T>,
    elementBuilder: (input: T, module: MutableModule) => Owned | undefined,
    edit?: MutableModule,
  ): Owned<MutableVector> | undefined
  /** TODO: Add docs */
  static tryBuild<T>(
    inputs: Iterable<T>,
    valueBuilder: (input: T, module: MutableModule) => Owned | undefined,
    edit?: MutableModule,
  ): Owned<MutableVector> | undefined {
    const module = edit ?? MutableModule.Transient()
    const elements = new Array<AbstractVectorElement<OwnedRefs>>()
    for (const input of inputs) {
      const value = valueBuilder(input, module)
      if (!value) return
      elements.push({ value: autospaced(value) })
    }
    return Vector.concrete(module, undefined, elements, undefined)
  }

  /** TODO: Add docs */
  static build<T>(
    inputs: Iterable<T>,
    elementBuilder: (input: T, module: MutableModule) => Owned,
    edit?: MutableModule,
  ): Owned<MutableVector> {
    return Vector.tryBuild(inputs, elementBuilder, edit)
  }

  /** TODO: Add docs */
  *concreteChildren({ verbatim }: PrintContext): IterableIterator<RawConcreteChild> {
    const { open, elements, close } = getAll(this.fields)
    yield ensureUnspaced(open, verbatim)
    let isFirst = true
    for (const { delimiter, value } of elements) {
      if (isFirst) {
        if (value) yield preferUnspaced(value)
      } else {
        yield preferUnspaced(delimiter)
        if (value) yield preferSpaced(value)
      }
      isFirst = false
    }
    yield preferUnspaced(close)
  }

  /** TODO: Add docs */
  *values(): IterableIterator<Expression> {
    for (const element of this.fields.get('elements'))
      if (element.value) yield this.module.get(element.value.node) as Expression
  }

  /** TODO: Add docs */
  *enumerate(): IterableIterator<[number, Expression | undefined]> {
    for (const [index, element] of this.fields.get('elements').entries()) {
      yield [index, this.module.get(element.value?.node) as Expression]
    }
  }

  /** TODO: Add docs */
  get length() {
    return this.fields.get('elements').length
  }
}
/** TODO: Add docs */
export class MutableVector extends Vector implements MutableExpression {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & VectorFields>

  push(value: Owned) {
    const elements = this.fields.get('elements')
    const element = this.valueToElement(value)
    this.fields.set('elements', [...elements, element])
  }

  pop(): Owned | undefined {
    const elements = this.fields.get('elements')
    const last = elements[elements.length - 1]?.value?.node
    this.fields.set('elements', elements.slice(0, -1))
    const lastNode = this.module.get(last)
    if (lastNode != null) {
      lastNode.fields.set('parent', undefined)
      return lastNode as Owned
    } else {
      return undefined
    }
  }

  set<T extends MutableExpression>(index: number, value: Owned<T>) {
    const elements = [...this.fields.get('elements')]
    elements[index] = {
      delimiter: elements[index]!.delimiter,
      value: autospaced(this.claimChild(value)),
    }
    this.fields.set('elements', elements)
  }

  splice(start: number, deletedCount: number, ...newValues: Owned[]) {
    const elements = [...this.fields.get('elements')]
    const newElements = newValues.map((value) => this.valueToElement(value))
    elements.splice(start, deletedCount, ...newElements)
    MutableVector.autospaceElement(elements[start + newValues.length])
    this.fields.set('elements', elements)
  }

  /**
   * Move an element inside vector.
   * @param fromIndex index of moved element.
   * @param toIndex new index of moved element.
   *
   * If any index is outside array index range, it's interpreted same as in {@link Array.prototype.splice|}.
   */
  move(fromIndex: number, toIndex: number) {
    const elements = [...this.fields.get('elements')]
    const [element] = elements.splice(fromIndex, 1)
    if (element != null) {
      MutableVector.autospaceElement(element)
      elements.splice(toIndex, 0, element)
      MutableVector.autospaceElement(elements[fromIndex])
      MutableVector.autospaceElement(elements[toIndex + 1])
      this.fields.set('elements', elements)
    }
  }

  keep(predicate: (ast: Expression) => boolean) {
    const elements = this.fields.get('elements')
    // Spacing around opening brackets should be preserved, as it's more natural (see tests).
    const firstSpacing = elements[0]?.value?.whitespace
    const filtered = elements.filter(
      (element) =>
        element.value && predicate(this.module.get(element.value.node) as MutableExpression),
    )
    if (firstSpacing != null && filtered[0]?.value != null) {
      filtered[0] = { ...filtered[0], value: { ...filtered[0].value, whitespace: firstSpacing } }
    }
    this.fields.set('elements', filtered)
  }

  private valueToElement(value: Owned) {
    return mapRefs(
      delimitVectorElement({ value: autospaced(value) }),
      ownedToRaw(this.module, this.id),
    )
  }

  private static autospaceElement(element: VectorElement<RawRefs> | undefined) {
    if (element != null) {
      element.delimiter.whitespace = undefined
      element.value = autospaced(element.value?.node)
    }
  }
}
export interface MutableVector extends Vector, MutableExpression {
  values(): IterableIterator<MutableExpression>
}
applyMixins(MutableVector, [MutableAst])

export type Mutable<T extends Ast = Ast> =
  T extends App ? MutableApp
  : T extends Assignment ? MutableAssignment
  : T extends BodyBlock ? MutableBodyBlock
  : T extends ExpressionStatement ? MutableExpressionStatement
  : T extends FunctionDef ? MutableFunctionDef
  : T extends Generic ? MutableGeneric
  : T extends Group ? MutableGroup
  : T extends Ident ? MutableIdent
  : T extends Import ? MutableImport
  : T extends Invalid ? MutableInvalid
  : T extends NegationApp ? MutableNegationApp
  : T extends NumericLiteral ? MutableNumericLiteral
  : T extends OprApp ? MutableOprApp
  : T extends PropertyAccess ? MutablePropertyAccess
  : T extends TextLiteral ? MutableTextLiteral
  : T extends UnaryOprApp ? MutableUnaryOprApp
  : T extends Vector ? MutableVector
  : T extends Wildcard ? MutableWildcard
  : T extends Statement ? MutableStatement
  : T extends Expression ? MutableExpression
  : MutableAst

/** TODO: Add docs */
export function materializeMutable(module: MutableModule, fields: FixedMap<AstFields>): MutableAst {
  const type = fields.get('type')
  const fieldsForType = fields as FixedMap<any>
  switch (type) {
    case 'App':
      return new MutableApp(module, fieldsForType)
    case 'Assignment':
      return new MutableAssignment(module, fieldsForType)
    case 'BodyBlock':
      return new MutableBodyBlock(module, fieldsForType)
    case 'ExpressionStatement':
      return new MutableExpressionStatement(module, fieldsForType)
    case 'FunctionDef':
      return new MutableFunctionDef(module, fieldsForType)
    case 'Generic':
      return new MutableGeneric(module, fieldsForType)
    case 'Group':
      return new MutableGroup(module, fieldsForType)
    case 'Ident':
      return new MutableIdent(module, fieldsForType)
    case 'Import':
      return new MutableImport(module, fieldsForType)
    case 'Invalid':
      return new MutableInvalid(module, fieldsForType)
    case 'NegationApp':
      return new MutableNegationApp(module, fieldsForType)
    case 'NumericLiteral':
      return new MutableNumericLiteral(module, fieldsForType)
    case 'OprApp':
      return new MutableOprApp(module, fieldsForType)
    case 'PropertyAccess':
      return new MutablePropertyAccess(module, fieldsForType)
    case 'TextLiteral':
      return new MutableTextLiteral(module, fieldsForType)
    case 'UnaryOprApp':
      return new MutableUnaryOprApp(module, fieldsForType)
    case 'AutoscopedIdentifier':
      return new MutableAutoscopedIdentifier(module, fieldsForType)
    case 'Vector':
      return new MutableVector(module, fieldsForType)
    case 'Wildcard':
      return new MutableWildcard(module, fieldsForType)
  }
  bail(`Invalid type: ${type}`)
}

/** TODO: Add docs */
export function materialize(module: Module, fields: FixedMapView<AstFields>): Ast {
  const type = fields.get('type')
  const fields_ = fields as FixedMapView<any>
  switch (type) {
    case 'App':
      return new App(module, fields_)
    case 'Assignment':
      return new Assignment(module, fields_)
    case 'BodyBlock':
      return new BodyBlock(module, fields_)
    case 'ExpressionStatement':
      return new ExpressionStatement(module, fields_)
    case 'FunctionDef':
      return new FunctionDef(module, fields_)
    case 'Generic':
      return new Generic(module, fields_)
    case 'Group':
      return new Group(module, fields_)
    case 'Ident':
      return new Ident(module, fields_)
    case 'Import':
      return new Import(module, fields_)
    case 'Invalid':
      return new Invalid(module, fields_)
    case 'NegationApp':
      return new NegationApp(module, fields_)
    case 'NumericLiteral':
      return new NumericLiteral(module, fields_)
    case 'OprApp':
      return new OprApp(module, fields_)
    case 'PropertyAccess':
      return new PropertyAccess(module, fields_)
    case 'TextLiteral':
      return new TextLiteral(module, fields_)
    case 'UnaryOprApp':
      return new UnaryOprApp(module, fields_)
    case 'AutoscopedIdentifier':
      return new AutoscopedIdentifier(module, fields_)
    case 'Vector':
      return new Vector(module, fields_)
    case 'Wildcard':
      return new Wildcard(module, fields_)
  }
  bail(`Invalid type: ${type}`)
}

export interface FixedMapView<Fields> {
  get<Key extends string & keyof Fields>(key: Key): DeepReadonly<Fields[Key]>
  /**
   * Unsafe. The caller must ensure the yielded values are not modified.
   * @internal
   */
  entries(): IterableIterator<readonly [string, unknown]>
  clone(): FixedMap<Fields>
  has(key: string): boolean
  toJSON(): object
}

export interface FixedMap<Fields> extends FixedMapView<Fields> {
  get<Key extends string & keyof Fields>(
    key: Key,
  ): DeepReadonlyExceptY<Fields[Key]> & DeepReadonly<Fields[Key]>
  set<Key extends string & keyof Fields>(key: Key, value: Fields[Key]): void
}

type DeepReadonlyFields<T> = {
  [K in keyof T]: DeepReadonly<T[K]>
}

function getAll<Fields extends object>(map: FixedMapView<Fields>): DeepReadonlyFields<Fields> {
  return Object.fromEntries(map.entries()) as DeepReadonlyFields<Fields>
}

declare const brandLegalFieldContent: unique symbol
/**
 * Used to add a constraint to all `AstFields`s subtypes ensuring that they were produced by `composeFieldData`, which
 *  enforces a requirement that the provided fields extend `FieldObject`.
 */
interface LegalFieldContent {
  [brandLegalFieldContent]: never
}

/** Modifies the input `map`. Returns the same object with an extended type. */
export function setAll<Fields1, Fields2 extends Record<string, any>>(
  map: FixedMap<Fields1>,
  fields: Fields2,
): FixedMap<Fields1 & Fields2> {
  const map_ = map as FixedMap<Fields1 & Fields2>
  for (const [k, v] of Object.entries(fields)) {
    const k_ = k as string & (keyof Fields1 | keyof Fields2)
    map_.set(k_, v as any)
  }
  return map_
}

/**
 * Modifies the input `map`. Returns the same object with an extended type. The added fields are required to have only
 * types extending `FieldData`; the returned object is branded as `LegalFieldContent`.
 */
export function composeFieldData<Fields1, Fields2 extends FieldObject<RawRefs>>(
  map: FixedMap<Fields1>,
  fields: Fields2,
): FixedMap<Fields1 & Fields2 & LegalFieldContent> {
  return setAll(map, fields) as FixedMap<Fields1 & Fields2 & LegalFieldContent>
}

function claimChild<T extends MutableAst>(
  module: MutableModule,
  child: Owned<T>,
  parent: AstId,
): AstId {
  if (child.module === module) assertEqual(child.fields.get('parent'), undefined)
  const child_ = module.copyIfForeign(child)
  child_.fields.set('parent', parent)
  return child_.id
}

function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned>,
  parent: AstId,
): NodeChild<AstId>
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned> | undefined,
  parent: AstId,
): NodeChild<AstId> | undefined
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token>,
  parent: AstId,
): NodeChild<AstId> | NodeChild<Token>
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token> | undefined,
  parent: AstId,
): NodeChild<AstId> | NodeChild<Token> | undefined
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token> | undefined,
  parent: AstId,
): NodeChild<AstId> | NodeChild<Token> | undefined {
  if (!child) return undefined
  if (isTokenId(child.node)) return child as NodeChild<Token>
  return { ...child, node: claimChild(module, child.node, parent) }
}

type StrictIdentLike = Identifier | IdentifierToken
function toIdentStrict(ident: StrictIdentLike): IdentifierToken
function toIdentStrict(ident: StrictIdentLike | undefined): IdentifierToken | undefined
function toIdentStrict(ident: StrictIdentLike | undefined): IdentifierToken | undefined {
  return (
    ident ?
      isToken(ident) ? ident
      : (Token.new(ident, TokenType.Ident) as IdentifierToken)
    : undefined
  )
}

type IdentLike = IdentifierOrOperatorIdentifier | IdentifierOrOperatorIdentifierToken
function toIdent(ident: IdentLike): IdentifierOrOperatorIdentifierToken
function toIdent(ident: IdentLike | undefined): IdentifierOrOperatorIdentifierToken | undefined
function toIdent(ident: IdentLike | undefined): IdentifierOrOperatorIdentifierToken | undefined {
  return (
    ident ?
      isToken(ident) ? ident
      : (Token.new(ident, TokenType.Ident) as IdentifierOrOperatorIdentifierToken)
    : undefined
  )
}

function makeEquals(): Token {
  return Token.new('=', TokenType.Operator)
}

function nameSpecification(
  name: StrictIdentLike | undefined,
): { name: NodeChild<Token>; equals: NodeChild<Token> } | undefined {
  return name && { name: unspaced(toIdentStrict(name)), equals: unspaced(makeEquals()) }
}

type KeysOfFieldType<Fields, T> = {
  [K in keyof Fields]: Fields[K] extends T ? K : never
}[keyof Fields]
function setNode<Fields, Key extends string & KeysOfFieldType<Fields, NodeChild<AstId>>>(
  map: FixedMap<Fields>,
  key: Key,
  node: AstId,
): void
function setNode<
  Fields,
  Key extends string & KeysOfFieldType<Fields, NodeChild<AstId> | undefined>,
>(map: FixedMap<Fields>, key: Key, node: AstId | undefined): void
function setNode<
  Fields,
  Key extends string & KeysOfFieldType<Fields, NodeChild<AstId> | undefined>,
>(map: FixedMap<Fields>, key: Key, node: AstId | undefined): void {
  // The signature correctly only allows this function to be called if `Fields[Key] instanceof NodeChild<SyncId>`,
  // but it doesn't prove that property to TSC, so we have to cast here.
  const old = map.get(key as string & keyof Fields) as any
  const updated = old ? { ...old, node } : autospaced(node)
  map.set(key, updated as Fields[Key])
}

function autospaced<T extends object | string>(node: T): NodeChild<T>
function autospaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined
/** TODO: Add docs */
function autospaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined {
  if (node === undefined) return node
  return { whitespace: undefined, node }
}

export interface Removed<T extends MutableAst> {
  node: Owned<T>
  placeholder: MutableWildcard | undefined
}
