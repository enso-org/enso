import { swapKeysAndValues } from '@/util/record'
import type {
  Identifier,
  IdentifierOrOperatorIdentifier,
  IdentifierOrOperatorIdentifierToken,
  IdentifierToken,
  Module,
  NodeChild,
  Owned,
  SpanMap,
  SyncTokenId,
} from '.'
import {
  MutableModule,
  ROOT_ID,
  Token,
  asOwned,
  isIdentifier,
  isToken,
  isTokenId,
  newExternalId,
  parentId,
} from '.'
import { assert, assertDefined, assertEqual, bail } from '../util/assert'
import type { Result } from '../util/data/result'
import { Err, Ok } from '../util/data/result'
import type { SourceRangeEdit } from '../util/data/text'
import type { ExternalId, VisualizationMetadata } from '../yjsModel'
import { visMetadataEquals } from '../yjsModel'
import * as RawAst from './generated/ast'
import {
  applyTextEditsToAst,
  parse,
  parseBlock,
  print,
  printAst,
  printBlock,
  syncToCode,
} from './parse'

declare const brandAstId: unique symbol
export type AstId = string & { [brandAstId]: never }

/** @internal */
export interface MetadataFields {
  externalId: ExternalId
}
export interface NodeMetadataFields {
  position?: { x: number; y: number } | undefined
  visualization?: VisualizationMetadata | undefined
}
export type NodeMetadata = FixedMapView<NodeMetadataFields>
export type MutableNodeMetadata = FixedMap<NodeMetadataFields>
export function asNodeMetadata(map: Map<string, unknown>): NodeMetadata {
  return map as unknown as NodeMetadata
}
/** @internal */
interface RawAstFields {
  id: AstId
  type: string
  parent: AstId | undefined
  metadata: FixedMap<MetadataFields>
}
export interface AstFields extends RawAstFields, LegalFieldContent {}
function allKeys<T>(keys: Record<keyof T, any>): (keyof T)[] {
  return Object.keys(keys) as any
}
const astFieldKeys = allKeys<RawAstFields>({
  id: null,
  type: null,
  parent: null,
  metadata: null,
})
export abstract class Ast {
  readonly module: Module
  /** @internal */
  readonly fields: FixedMapView<AstFields>

  get id(): AstId {
    return this.fields.get('id')
  }

  get externalId(): ExternalId {
    const id = this.fields.get('metadata').get('externalId')
    assert(id != null)
    return id
  }

  get nodeMetadata(): NodeMetadata {
    const metadata = this.fields.get('metadata')
    return metadata as FixedMapView<NodeMetadataFields>
  }

  typeName(): string {
    return this.fields.get('type')
  }

  /**
   * Return whether `this` and `other` are the same object, possibly in different modules.
   */
  is<T extends Ast>(other: T): boolean {
    return this.id === other.id
  }

  innerExpression(): Ast {
    // TODO: Override this in `Documented`, `Annotated`, `AnnotatedBuiltin`
    return this
  }

  code(): string {
    return print(this).code
  }

  visitRecursive(visit: (node: Ast | Token) => void): void {
    visit(this)
    for (const child of this.children()) {
      if (isToken(child)) {
        visit(child)
      } else {
        child.visitRecursive(visit)
      }
    }
  }

  visitRecursiveAst(visit: (ast: Ast) => void | boolean): void {
    if (visit(this) === false) return
    for (const child of this.children()) {
      if (!isToken(child)) child.visitRecursiveAst(visit)
    }
  }

  printSubtree(
    info: SpanMap,
    offset: number,
    parentIndent: string | undefined,
    verbatim?: boolean,
  ): string {
    return printAst(this, info, offset, parentIndent, verbatim)
  }

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): IterableIterator<Ast | Token> {
    for (const child of this.concreteChildren()) {
      if (isTokenId(child.node)) {
        yield this.module.getToken(child.node)
      } else {
        const node = this.module.get(child.node)
        if (node) yield node
      }
    }
  }

  get parentId(): AstId | undefined {
    const parentId = this.fields.get('parent')
    if (parentId !== ROOT_ID) return parentId
  }

  parent(): Ast | undefined {
    return this.module.get(this.parentId)
  }

  static parseBlock(source: string, inModule?: MutableModule) {
    return parseBlock(source, inModule)
  }

  static parse(source: string, module?: MutableModule) {
    return parse(source, module)
  }

  ////////////////////

  protected constructor(module: Module, fields: FixedMapView<AstFields>) {
    this.module = module
    this.fields = fields
  }

  /** @internal
   *  Returns child subtrees, including information about the whitespace between them.
   */
  abstract concreteChildren(verbatim?: boolean): IterableIterator<NodeChild>
}
export interface MutableAst {}
export abstract class MutableAst extends Ast {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields>

  setExternalId(id: ExternalId) {
    this.fields.get('metadata').set('externalId', id)
  }

  mutableNodeMetadata(): MutableNodeMetadata {
    const metadata = this.fields.get('metadata')
    return metadata as FixedMap<NodeMetadataFields>
  }

  setNodeMetadata(nodeMeta: NodeMetadataFields) {
    const metadata = this.fields.get('metadata') as unknown as Map<string, unknown>
    for (const [key, value] of Object.entries(nodeMeta))
      if (value !== undefined) metadata.set(key, value)
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

  /** Change the value of the object referred to by the `target` ID. (The initial ID of `replacement` will be ignored.)
   *  Returns the old value, with a new (unreferenced) ID.
   */
  replaceValue<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const replacement_ = this.module.copyIfForeign(replacement)
    const old = this.replace(replacement_)
    replacement_.fields.set('metadata', old.fields.get('metadata').clone())
    old.setExternalId(newExternalId())
    return old
  }

  replaceValueChecked<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const parentId = this.fields.get('parent')
    assertDefined(parentId)
    return this.replaceValue(replacement)
  }

  /** Replace the parent of this object with a reference to a new placeholder object.
   *  Returns the object, now parentless, and the placeholder. */
  takeToReplace(): Removed<this> {
    if (parentId(this)) {
      const placeholder = Wildcard.new(this.module)
      const node = this.replace(placeholder)
      return { node, placeholder }
    } else {
      return { node: asOwned(this), placeholder: undefined }
    }
  }

  /** Replace the parent of this object with a reference to a new placeholder object.
   *  Returns the object, now parentless. */
  take(): Owned<this> {
    return this.replace(Wildcard.new(this.module))
  }

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

  /** Replace the value assigned to the given ID with a placeholder.
   *  Returns the removed value, with a new unreferenced ID.
   **/
  takeValue(): Removed<typeof this> {
    const placeholder = Wildcard.new(this.module)
    const node = this.replaceValue(placeholder)
    return { node, placeholder }
  }

  /** Take this node from the tree, and replace it with the result of applying the given function to it.
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

  /** Take this node from the tree, and replace it with the result of applying the given function to it; transfer the
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
    for (const child of this.concreteChildren()) {
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

  protected claimChild<T extends MutableAst>(child: Owned<T>): AstId
  protected claimChild<T extends MutableAst>(child: Owned<T> | undefined): AstId | undefined
  protected claimChild<T extends MutableAst>(child: Owned<T> | undefined): AstId | undefined {
    return child ? claimChild(this.module, child, this.id) : undefined
  }
}

/** Values that may be found in fields of `Ast` subtypes. */
type FieldData =
  | NodeChild<AstId>
  | NodeChild
  | NodeChild<SyncTokenId>
  | FieldData[]
  | undefined
  | StructuralField
/** Objects that do not directly contain `AstId`s or `SyncTokenId`s, but may have `NodeChild` fields. */
type StructuralField =
  | RawMultiSegmentAppSegment
  | RawBlockLine
  | RawOpenCloseTokens
  | RawNameSpecification
/** Type whose fields are all suitable for storage as `Ast` fields. */
interface FieldObject {
  [field: string]: FieldData
}
/** Returns the fields of an `Ast` subtype that are not part of `AstFields`. */
function* fieldDataEntries<Fields>(map: FixedMapView<Fields>) {
  for (const entry of map.entries()) {
    // All fields that are not from `AstFields` are `FieldData`.
    if (!astFieldKeys.includes(entry[0] as any)) yield entry as [string, FieldData]
  }
}

/** Apply the given function to each `AstId` in the fields of `ast`. For each value that it returns an output, that
 *  output will be substituted for the input ID.
 */
export function rewriteRefs(ast: MutableAst, f: (id: AstId) => AstId | undefined) {
  let fieldsChanged = 0
  for (const [key, value] of fieldDataEntries(ast.fields)) {
    const newValue = rewriteFieldRefs(value, f)
    if (newValue !== undefined) {
      ast.fields.set(key as any, newValue)
      fieldsChanged += 1
    }
  }
  return fieldsChanged
}

/** Copy all fields except the `Ast` base fields from `ast2` to `ast1`. A reference-rewriting function will be applied
 *  to `AstId`s in copied fields; see {@link rewriteRefs}.
 */
export function syncFields(ast1: MutableAst, ast2: Ast, f: (id: AstId) => AstId | undefined) {
  for (const [key, value] of fieldDataEntries(ast2.fields)) {
    const changedValue = rewriteFieldRefs(value, f)
    const newValue = changedValue ?? value
    if (!fieldEqual(ast1.fields.get(key as any), newValue)) ast1.fields.set(key as any, newValue)
  }
}

export function syncNodeMetadata(target: MutableNodeMetadata, source: NodeMetadata) {
  const oldPos = target.get('position')
  const newPos = source.get('position')
  if (oldPos?.x !== newPos?.x || oldPos?.y !== newPos?.y) target.set('position', newPos)
  const newVis = source.get('visualization')
  if (!visMetadataEquals(target.get('visualization'), newVis)) target.set('visualization', newVis)
}

function rewriteFieldRefs(field: FieldData, f: (id: AstId) => AstId | undefined): FieldData {
  if (field === undefined) return field
  if ('node' in field) {
    const child = field.node
    if (isTokenId(child)) return
    const newValue = f(child)
    if (newValue !== undefined) {
      field.node = newValue
      return field
    }
  } else if (Array.isArray(field)) {
    let fieldChanged = false
    field.forEach((subfield, i) => {
      const newValue = rewriteFieldRefs(subfield, f)
      if (newValue !== undefined) {
        field[i] = newValue
        fieldChanged = true
      }
    })
    if (fieldChanged) return field
  } else {
    const fieldObject = field satisfies StructuralField
    let fieldChanged = false
    for (const [key, value] of Object.entries(fieldObject)) {
      const newValue = rewriteFieldRefs(value, f)
      if (newValue !== undefined) {
        // This update is safe because `newValue` was obtained by reading `fieldObject[key]` and modifying it in a
        // type-preserving way.
        ;(fieldObject as any)[key] = newValue
        fieldChanged = true
      }
    }
    if (fieldChanged) return fieldObject
  }
}

function fieldEqual(field1: FieldData, field2: FieldData): boolean {
  if (field1 === undefined) return field2 === undefined
  if (field2 === undefined) return false
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

interface AppFields {
  function: NodeChild<AstId>
  parens: RawOpenCloseTokens | undefined
  nameSpecification: RawNameSpecification | undefined
  argument: NodeChild<AstId>
}
interface RawOpenCloseTokens {
  open: NodeChild<SyncTokenId>
  close: NodeChild<SyncTokenId>
}
interface RawNameSpecification {
  name: NodeChild<SyncTokenId>
  equals: NodeChild<SyncTokenId>
}
export class App extends Ast {
  declare fields: FixedMap<AstFields & AppFields>
  constructor(module: Module, fields: FixedMapView<AstFields & AppFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableApp> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableApp) return parsed
  }

  static concrete(
    module: MutableModule,
    func: NodeChild<Owned>,
    parens: { open: NodeChild<Token>; close: NodeChild<Token> } | undefined,
    nameSpecification: { name: NodeChild<Token>; equals: NodeChild<Token> } | undefined,
    argument: NodeChild<Owned>,
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

  static new(
    module: MutableModule,
    func: Owned,
    argumentName: StrictIdentLike | undefined,
    argument: Owned,
  ) {
    return App.concrete(
      module,
      autospaced(func),
      undefined,
      nameSpecification(argumentName),
      autospaced(argument),
    )
  }

  static PositionalSequence(func: Owned, args: Owned[]): Owned {
    return args.reduce(
      (expression, argument) => App.new(func.module, expression, undefined, argument),
      func,
    )
  }

  get function(): Ast {
    return this.module.get(this.fields.get('function').node)
  }
  get argumentName(): Token | undefined {
    return this.module.getToken(this.fields.get('nameSpecification')?.name.node)
  }
  get argument(): Ast {
    return this.module.get(this.fields.get('argument').node)
  }

  *concreteChildren(verbatim?: boolean): IterableIterator<NodeChild> {
    const { function: function_, parens, nameSpecification, argument } = getAll(this.fields)
    yield ensureUnspaced(function_, verbatim)
    const useParens = !!(parens && (nameSpecification || verbatim))
    const spacedEquals = useParens && !!nameSpecification?.equals.whitespace
    if (useParens) yield ensureSpaced(parens.open, verbatim)
    if (nameSpecification) {
      yield ensureSpacedIf(nameSpecification.name, !useParens, verbatim)
      yield ensureSpacedOnlyIf(nameSpecification.equals, spacedEquals, verbatim)
    }
    yield ensureSpacedOnlyIf(argument, !nameSpecification || spacedEquals, verbatim)
    if (useParens) yield preferUnspaced(parens.close)
  }

  printSubtree(
    info: SpanMap,
    offset: number,
    parentIndent: string | undefined,
    verbatim?: boolean,
  ): string {
    const verbatim_ =
      verbatim ?? (this.function instanceof Invalid || this.argument instanceof Invalid)
    return super.printSubtree(info, offset, parentIndent, verbatim_)
  }
}
function ensureSpacedIf<T>(
  child: NodeChild<T>,
  condition: boolean,
  verbatim: boolean | undefined,
): NodeChild<T> {
  return condition ? ensureSpaced(child, verbatim) : child
}
function ensureSpacedOnlyIf<T>(
  child: NodeChild<T>,
  condition: boolean,
  verbatim: boolean | undefined,
): NodeChild<T> {
  return condition ? ensureSpaced(child, verbatim) : ensureUnspaced(child, verbatim)
}
function ensureSpaced<T>(child: NodeChild<T>, verbatim: boolean | undefined): NodeChild<T> {
  if (verbatim && child.whitespace != null) return child
  return child.whitespace ? child : { whitespace: ' ', ...child }
}
function ensureUnspaced<T>(child: NodeChild<T>, verbatim: boolean | undefined): NodeChild<T> {
  if (verbatim && child.whitespace != null) return child
  return child.whitespace === '' ? child : { whitespace: '', ...child }
}
function preferUnspaced<T>(child: NodeChild<T>): NodeChild<T> {
  return child.whitespace === undefined ? { whitespace: '', ...child } : child
}
export class MutableApp extends App implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & AppFields>

  setFunction<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'function', this.claimChild(value))
  }
  setArgumentName(name: StrictIdentLike | undefined) {
    this.fields.set('nameSpecification', nameSpecification(name))
  }
  setArgument<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }
}
export interface MutableApp extends App, MutableAst {
  get function(): MutableAst
  get argument(): MutableAst
}
applyMixins(MutableApp, [MutableAst])

interface UnaryOprAppFields {
  operator: NodeChild<SyncTokenId>
  argument: NodeChild<AstId> | undefined
}
export class UnaryOprApp extends Ast {
  declare fields: FixedMapView<AstFields & UnaryOprAppFields>
  constructor(module: Module, fields: FixedMapView<AstFields & UnaryOprAppFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableUnaryOprApp> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableUnaryOprApp) return parsed
  }

  static concrete(
    module: MutableModule,
    operator: NodeChild<Token>,
    argument: NodeChild<Owned> | undefined,
  ) {
    const base = module.baseObject('UnaryOprApp')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableUnaryOprApp(module, fields))
  }

  static new(module: MutableModule, operator: Token, argument: Owned | undefined) {
    return this.concrete(module, unspaced(operator), argument ? autospaced(argument) : undefined)
  }

  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get argument(): Ast | undefined {
    return this.module.get(this.fields.get('argument')?.node)
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const { operator, argument } = getAll(this.fields)
    yield operator
    if (argument) yield argument
  }
}
export class MutableUnaryOprApp extends UnaryOprApp implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & UnaryOprAppFields>

  setOperator(value: Token) {
    this.fields.set('operator', unspaced(value))
  }
  setArgument<T extends MutableAst>(argument: Owned<T> | undefined) {
    setNode(this.fields, 'argument', this.claimChild(argument))
  }
}
export interface MutableUnaryOprApp extends UnaryOprApp, MutableAst {
  get argument(): MutableAst | undefined
}
applyMixins(MutableUnaryOprApp, [MutableAst])

interface NegationAppFields {
  operator: NodeChild<SyncTokenId>
  argument: NodeChild<AstId>
}
export class NegationApp extends Ast {
  declare fields: FixedMapView<AstFields & NegationAppFields>
  constructor(module: Module, fields: FixedMapView<AstFields & NegationAppFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableNegationApp> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableNegationApp) return parsed
  }

  static concrete(module: MutableModule, operator: NodeChild<Token>, argument: NodeChild<Owned>) {
    const base = module.baseObject('NegationApp')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableNegationApp(module, fields))
  }

  static new(module: MutableModule, operator: Token, argument: Owned) {
    return this.concrete(module, unspaced(operator), autospaced(argument))
  }

  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get argument(): Ast {
    return this.module.get(this.fields.get('argument').node)
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const { operator, argument } = getAll(this.fields)
    yield operator
    if (argument) yield argument
  }
}
export class MutableNegationApp extends NegationApp implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & NegationAppFields>

  setArgument<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }
}
export interface MutableNegationApp extends NegationApp, MutableAst {
  get argument(): MutableAst
}
applyMixins(MutableNegationApp, [MutableAst])

interface OprAppFields {
  lhs: NodeChild<AstId> | undefined
  operators: NodeChild<SyncTokenId>[]
  rhs: NodeChild<AstId> | undefined
}
export class OprApp extends Ast {
  declare fields: FixedMapView<AstFields & OprAppFields>
  constructor(module: Module, fields: FixedMapView<AstFields & OprAppFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableOprApp> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableOprApp) return parsed
  }

  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned> | undefined,
    operators: NodeChild<Token>[],
    rhs: NodeChild<Owned> | undefined,
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

  static new(
    module: MutableModule,
    lhs: Owned | undefined,
    operator: Token | string,
    rhs: Owned | undefined,
  ) {
    const operatorToken =
      operator instanceof Token ? operator : Token.new(operator, RawAst.Token.Type.Operator)
    return OprApp.concrete(module, unspaced(lhs), [autospaced(operatorToken)], autospaced(rhs))
  }

  get lhs(): Ast | undefined {
    return this.module.get(this.fields.get('lhs')?.node)
  }
  get operator(): Result<Token, NodeChild<Token>[]> {
    const operators = this.fields.get('operators')
    const operators_ = operators.map((child) => ({
      ...child,
      node: this.module.getToken(child.node),
    }))
    const [opr] = operators_
    return opr ? Ok(opr.node) : Err(operators_)
  }
  get rhs(): Ast | undefined {
    return this.module.get(this.fields.get('rhs')?.node)
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const { lhs, operators, rhs } = getAll(this.fields)
    if (lhs) yield lhs
    yield* operators
    if (rhs) yield rhs
  }
}
export class MutableOprApp extends OprApp implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & OprAppFields>

  setLhs<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'lhs', this.claimChild(value))
  }
  setOperator(value: Token) {
    this.fields.set('operators', [unspaced(value)])
  }
  setRhs<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'rhs', this.claimChild(value))
  }
}
export interface MutableOprApp extends OprApp, MutableAst {
  get lhs(): MutableAst | undefined
  get rhs(): MutableAst | undefined
}
applyMixins(MutableOprApp, [MutableAst])

interface PropertyAccessFields {
  lhs: NodeChild<AstId> | undefined
  operator: NodeChild<SyncTokenId>
  rhs: NodeChild<AstId>
}
export class PropertyAccess extends Ast {
  declare fields: FixedMapView<AstFields & PropertyAccessFields>
  constructor(module: Module, fields: FixedMapView<AstFields & PropertyAccessFields>) {
    super(module, fields)
  }

  static tryParse(
    source: string,
    module?: MutableModule,
  ): Owned<MutablePropertyAccess> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutablePropertyAccess) return parsed
  }

  static new(module: MutableModule, lhs: Owned, rhs: IdentLike) {
    const dot = unspaced(Token.new('.', RawAst.Token.Type.Operator))
    return this.concrete(
      module,
      unspaced(lhs),
      dot,
      unspaced(Ident.newAllowingOperators(module, toIdent(rhs))),
    )
  }

  static Sequence(
    segments: [StrictIdentLike, ...StrictIdentLike[]],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent>
  static Sequence(
    segments: [StrictIdentLike, ...StrictIdentLike[], IdentLike],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent>
  static Sequence(
    segments: IdentLike[],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined
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

  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned> | undefined,
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

  get lhs(): Ast | undefined {
    return this.module.get(this.fields.get('lhs')?.node)
  }
  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get rhs(): IdentifierOrOperatorIdentifierToken {
    const ast = this.module.get(this.fields.get('rhs').node)
    assert(ast instanceof Ident)
    return ast.token as IdentifierOrOperatorIdentifierToken
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const { lhs, operator, rhs } = getAll(this.fields)
    if (lhs) yield lhs
    yield operator
    yield rhs
  }
}
export class MutablePropertyAccess extends PropertyAccess implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & PropertyAccessFields>

  setLhs<T extends MutableAst>(value: Owned<T> | undefined) {
    setNode(this.fields, 'lhs', this.claimChild(value))
  }
  setRhs(ident: IdentLike) {
    const node = this.claimChild(Ident.newAllowingOperators(this.module, ident))
    const old = this.fields.get('rhs')
    this.fields.set('rhs', old ? { ...old, node } : unspaced(node))
  }
}
export interface MutablePropertyAccess extends PropertyAccess, MutableAst {
  get lhs(): MutableAst | undefined
}
applyMixins(MutablePropertyAccess, [MutableAst])

interface GenericFields {
  children: NodeChild[]
}
export class Generic extends Ast {
  declare fields: FixedMapView<AstFields & GenericFields>
  constructor(module: Module, fields: FixedMapView<AstFields & GenericFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, children: NodeChild<Owned | Token>[]) {
    const base = module.baseObject('Generic')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      children: children.map((child) => concreteChild(module, child, id_)),
    })
    return asOwned(new MutableGeneric(module, fields))
  }

  concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    return this.fields.get('children')[Symbol.iterator]()
  }
}
export class MutableGeneric extends Generic implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & GenericFields>
}
export interface MutableGeneric extends Generic, MutableAst {}
applyMixins(MutableGeneric, [MutableAst])

interface RawMultiSegmentAppSegment {
  header: NodeChild<Token>
  body: NodeChild<AstId> | undefined
}
interface OwnedMultiSegmentAppSegment {
  header: NodeChild<Token>
  body: NodeChild<Owned> | undefined
}
function multiSegmentAppSegment<T extends MutableAst>(
  header: string,
  body: Owned<T>,
): OwnedMultiSegmentAppSegment
function multiSegmentAppSegment<T extends MutableAst>(
  header: string,
  body: Owned<T> | undefined,
): OwnedMultiSegmentAppSegment | undefined
function multiSegmentAppSegment<T extends MutableAst>(
  header: string,
  body: Owned<T> | undefined,
): OwnedMultiSegmentAppSegment | undefined {
  return {
    header: { node: Token.new(header, RawAst.Token.Type.Ident) },
    body: spaced(body ? (body as any) : undefined),
  }
}

function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment,
  parent: AstId,
): RawMultiSegmentAppSegment
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment,
  parent: AstId,
): RawMultiSegmentAppSegment
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment | undefined,
  parent: AstId,
): RawMultiSegmentAppSegment | undefined
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment | undefined,
  parent: AstId,
): RawMultiSegmentAppSegment | undefined {
  if (!msas) return undefined
  return {
    ...msas,
    body: concreteChild(module, msas.body, parent),
  }
}
interface ImportFields {
  polyglot: RawMultiSegmentAppSegment | undefined
  from: RawMultiSegmentAppSegment | undefined
  import: RawMultiSegmentAppSegment
  all: NodeChild<SyncTokenId> | undefined
  as: RawMultiSegmentAppSegment | undefined
  hiding: RawMultiSegmentAppSegment | undefined
}
export class Import extends Ast {
  declare fields: FixedMapView<AstFields & ImportFields>
  constructor(module: Module, fields: FixedMapView<AstFields & ImportFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableImport> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableImport) return parsed
  }

  get polyglot(): Ast | undefined {
    return this.module.get(this.fields.get('polyglot')?.body?.node)
  }
  get from(): Ast | undefined {
    return this.module.get(this.fields.get('from')?.body?.node)
  }
  get import_(): Ast | undefined {
    return this.module.get(this.fields.get('import').body?.node)
  }
  get all(): Token | undefined {
    return this.module.getToken(this.fields.get('all')?.node)
  }
  get as(): Ast | undefined {
    return this.module.get(this.fields.get('as')?.body?.node)
  }
  get hiding(): Ast | undefined {
    return this.module.get(this.fields.get('hiding')?.body?.node)
  }

  static concrete(
    module: MutableModule,
    polyglot: OwnedMultiSegmentAppSegment | undefined,
    from: OwnedMultiSegmentAppSegment | undefined,
    import_: OwnedMultiSegmentAppSegment,
    all: NodeChild<Token> | undefined,
    as: OwnedMultiSegmentAppSegment | undefined,
    hiding: OwnedMultiSegmentAppSegment | undefined,
  ) {
    const base = module.baseObject('Import')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      polyglot: multiSegmentAppSegmentToRaw(module, polyglot, id_),
      from: multiSegmentAppSegmentToRaw(module, from, id_),
      import: multiSegmentAppSegmentToRaw(module, import_, id_),
      all,
      as: multiSegmentAppSegmentToRaw(module, as, id_),
      hiding: multiSegmentAppSegmentToRaw(module, hiding, id_),
    })
    return asOwned(new MutableImport(module, fields))
  }

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

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const segment = (segment: RawMultiSegmentAppSegment | undefined) => {
      const parts = []
      if (segment) parts.push(segment.header)
      if (segment?.body) parts.push(segment.body)
      return parts
    }
    const { polyglot, from, import: import_, all, as, hiding } = getAll(this.fields)
    yield* segment(polyglot)
    yield* segment(from)
    yield* segment(import_)
    if (all) yield all
    yield* segment(as)
    yield* segment(hiding)
  }
}
export class MutableImport extends Import implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & ImportFields>

  private toRaw(msas: OwnedMultiSegmentAppSegment): RawMultiSegmentAppSegment
  private toRaw(
    msas: OwnedMultiSegmentAppSegment | undefined,
  ): RawMultiSegmentAppSegment | undefined
  private toRaw(
    msas: OwnedMultiSegmentAppSegment | undefined,
  ): RawMultiSegmentAppSegment | undefined {
    return multiSegmentAppSegmentToRaw(this.module, msas, this.id)
  }

  setPolyglot<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set(
      'polyglot',
      value ? this.toRaw(multiSegmentAppSegment('polyglot', value)) : undefined,
    )
  }
  setFrom<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('from', value ? this.toRaw(multiSegmentAppSegment('from', value)) : value)
  }
  setImport<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('import', this.toRaw(multiSegmentAppSegment('import', value)))
  }
  setAll(value: Token | undefined) {
    this.fields.set('all', spaced(value))
  }
  setAs<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('as', this.toRaw(multiSegmentAppSegment('as', value)))
  }
  setHiding<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('hiding', this.toRaw(multiSegmentAppSegment('hiding', value)))
  }
}
export interface MutableImport extends Import, MutableAst {
  get polyglot(): MutableAst | undefined
  get from(): MutableAst | undefined
  get import_(): MutableAst | undefined
  get as(): MutableAst | undefined
  get hiding(): MutableAst | undefined
}
applyMixins(MutableImport, [MutableAst])

const escapePairs = [
  ['\0', '\\0'],
  ['\b', '\\b'],
  ['\f', '\\f'],
  ['\n', '\\n'],
  ['\r', '\\r'],
  ['\t', '\\t'],
  ['\v', '\\v'],
  ["'", "\\'"],
  ['`', '``'],
] as const

const escapeMapping = Object.fromEntries(escapePairs)
const unescapeMapping = Object.fromEntries(escapePairs.map(([k, v]) => [v, k]))

/** Escape a string so it can be safely spliced into an interpolated (`''`) Enso string.
 * NOT USABLE to insert into raw strings. Does not include double-quotes (`"`). */
export function escapeInterpolation(string: string) {
  return string.replace(/[\0\b\f\n\r\t\v'`]/g, (match) => escapeMapping[match]!)
}

/** Interpret all escaped characters from an interpolated (`''`) Enso string. */
export function applyInterpolation(string: string) {
  return string.replace(/\\[0bfnrtv']|``/g, (match) => unescapeMapping[match]!)
}

interface TextLiteralFields {
  open: NodeChild<SyncTokenId> | undefined
  newline: NodeChild<SyncTokenId> | undefined
  elements: NodeChild[]
  close: NodeChild<SyncTokenId> | undefined
}
export class TextLiteral extends Ast {
  declare fields: FixedMapView<AstFields & TextLiteralFields>
  constructor(module: Module, fields: FixedMapView<AstFields & TextLiteralFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableTextLiteral> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableTextLiteral) return parsed
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    newline: NodeChild<Token> | undefined,
    elements: NodeChild<Owned | Token>[],
    close: NodeChild<Token> | undefined,
  ) {
    const base = module.baseObject('TextLiteral')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      open,
      newline,
      elements: elements.map((elem) => concreteChild(module, elem, id_)),
      close,
    })
    return asOwned(new MutableTextLiteral(module, fields))
  }

  static new(rawText: string, module: MutableModule) {
    const open = unspaced(Token.new("'"))
    const elements = [unspaced(Token.new(escapeInterpolation(rawText)))]
    const close = unspaced(Token.new("'"))
    return this.concrete(module, open, undefined, elements, close)
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const { open, newline, elements, close } = getAll(this.fields)
    if (open) yield open
    if (newline) yield newline
    yield* elements
    if (close) yield close
  }

  boundaryTokenVariant(): string | undefined {
    return (this.open || this.close)?.code()
  }

  isInterpolated(): boolean {
    const token = this.boundaryTokenVariant()
    return token === "'" || token === "'''"
  }

  get textContents(): string {
    const combinedCode =
      this.fields
        .get('elements')
        ?.map((element) => this.module.getAny(element.node).code())
        ?.join('') ?? ''
    const isInterpolated = this.isInterpolated()
    return isInterpolated ? applyInterpolation(combinedCode) : combinedCode
  }

  get open(): Token | undefined {
    return this.module.getToken(this.fields.get('open')?.node)
  }

  get close(): Token | undefined {
    return this.module.getToken(this.fields.get('close')?.node)
  }
}
export class MutableTextLiteral extends TextLiteral implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & TextLiteralFields>

  setBoundaries(code: string) {
    this.fields.set('open', unspaced(Token.new(code)))
    this.fields.set('close', unspaced(Token.new(code)))
  }

  setTextContents(rawText: string) {
    const isInterpolated = this.isInterpolated()
    const token = this.boundaryTokenVariant()
    const mustBecomeInterpolated = !isInterpolated && (!token || rawText.includes(token))
    const doEscape = isInterpolated || mustBecomeInterpolated

    if (mustBecomeInterpolated) this.setBoundaries(token === '"""' ? "'''" : "'")

    const codeText = doEscape ? escapeInterpolation(rawText) : rawText
    const newElement = concreteChild(this.module, unspaced(Token.new(codeText)), this.id)
    this.fields.set('elements', [newElement])
  }
}
export interface MutableTextLiteral extends TextLiteral, MutableAst {}
applyMixins(MutableTextLiteral, [MutableAst])

interface DocumentedFields {
  open: NodeChild<SyncTokenId> | undefined
  elements: NodeChild[]
  newlines: NodeChild<SyncTokenId>[]
  expression: NodeChild<AstId> | undefined
}
export class Documented extends Ast {
  declare fields: FixedMapView<AstFields & DocumentedFields>
  constructor(module: Module, fields: FixedMapView<AstFields & DocumentedFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableDocumented> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableDocumented) return parsed
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    elements: NodeChild<Owned | Token>[],
    newlines: NodeChild<Token>[],
    expression: NodeChild<Owned> | undefined,
  ) {
    const base = module.baseObject('Documented')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      open,
      elements: elements.map((elem) => concreteChild(module, elem, id_)),
      newlines,
      expression: concreteChild(module, expression, id_),
    })
    return asOwned(new MutableDocumented(module, fields))
  }

  get expression(): Ast | undefined {
    return this.module.get(this.fields.get('expression')?.node)
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const { open, elements, newlines, expression } = getAll(this.fields)
    if (open) yield open
    yield* elements
    yield* newlines
    if (expression) yield expression
  }
}
export class MutableDocumented extends Documented implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & DocumentedFields>

  setExpression<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }
}
export interface MutableDocumented extends Documented, MutableAst {
  get expression(): MutableAst | undefined
}
applyMixins(MutableDocumented, [MutableAst])

interface InvalidFields {
  expression: NodeChild<AstId>
}
export class Invalid extends Ast {
  declare fields: FixedMapView<AstFields & InvalidFields>
  constructor(module: Module, fields: FixedMapView<AstFields & InvalidFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, expression: NodeChild<Owned>) {
    const base = module.baseObject('Invalid')
    return asOwned(new MutableInvalid(module, invalidFields(module, base, expression)))
  }

  get expression(): Ast {
    return this.module.get(this.fields.get('expression').node)
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    yield this.fields.get('expression')
  }

  printSubtree(
    info: SpanMap,
    offset: number,
    parentIndent: string | undefined,
    _verbatim?: boolean,
  ): string {
    return super.printSubtree(info, offset, parentIndent, true)
  }
}
export function invalidFields(
  module: MutableModule,
  base: FixedMap<AstFields>,
  expression: NodeChild<Owned>,
): FixedMap<AstFields & InvalidFields> {
  const id_ = base.get('id')
  return composeFieldData(base, { expression: concreteChild(module, expression, id_) })
}
export class MutableInvalid extends Invalid implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & InvalidFields>

  /** Private, because it makes more sense to `.replace` the `Invalid` node. */
  private setExpression<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }
}
export interface MutableInvalid extends Invalid, MutableAst {
  /** The `expression` getter is intentionally not narrowed to provide mutable access:
   *  It makes more sense to `.replace` the `Invalid` node. */
}
applyMixins(MutableInvalid, [MutableAst])

interface GroupFields {
  open: NodeChild<SyncTokenId> | undefined
  expression: NodeChild<AstId> | undefined
  close: NodeChild<SyncTokenId> | undefined
}
export class Group extends Ast {
  declare fields: FixedMapView<AstFields & GroupFields>
  constructor(module: Module, fields: FixedMapView<AstFields & GroupFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableGroup> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableGroup) return parsed
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    expression: NodeChild<Owned> | undefined,
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

  static new(module: MutableModule, expression: Owned) {
    const open = unspaced(Token.new('(', RawAst.Token.Type.OpenSymbol))
    const close = unspaced(Token.new(')', RawAst.Token.Type.CloseSymbol))
    return this.concrete(module, open, unspaced(expression), close)
  }

  get expression(): Ast | undefined {
    return this.module.get(this.fields.get('expression')?.node)
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    const { open, expression, close } = getAll(this.fields)
    if (open) yield open
    if (expression) yield expression
    if (close) yield close
  }
}
export class MutableGroup extends Group implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & GroupFields>

  setExpression<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }
}
export interface MutableGroup extends Group, MutableAst {
  get expression(): MutableAst | undefined
}
applyMixins(MutableGroup, [MutableAst])

interface NumericLiteralFields {
  tokens: NodeChild<SyncTokenId>[]
}
export class NumericLiteral extends Ast {
  declare fields: FixedMapView<AstFields & NumericLiteralFields>
  constructor(module: Module, fields: FixedMapView<AstFields & NumericLiteralFields>) {
    super(module, fields)
  }

  static tryParse(
    source: string,
    module?: MutableModule,
  ): Owned<MutableNumericLiteral> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableNumericLiteral) return parsed
  }

  static concrete(module: MutableModule, tokens: NodeChild<Token>[]) {
    const base = module.baseObject('NumericLiteral')
    const fields = composeFieldData(base, { tokens })
    return asOwned(new MutableNumericLiteral(module, fields))
  }

  concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    return this.fields.get('tokens')[Symbol.iterator]()
  }
}
export class MutableNumericLiteral extends NumericLiteral implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & NumericLiteralFields>
}
export interface MutableNumericLiteral extends NumericLiteral, MutableAst {}
applyMixins(MutableNumericLiteral, [MutableAst])

/** The actual contents of an `ArgumentDefinition` are complex, but probably of more interest to the compiler than the
 *  GUI. We just need to represent them faithfully and create the simple cases. */
type ArgumentDefinition = NodeChild<Ast | Token>[]
type RawArgumentDefinition = NodeChild[]
type OwnedArgumentDefinition = NodeChild<Owned | Token>[]

function argumentDefinitionsToRaw(
  module: MutableModule,
  defs: OwnedArgumentDefinition[],
  parent: AstId,
): RawArgumentDefinition[] {
  return defs.map((def) =>
    def.map((part) => ({
      ...part,
      node: part.node instanceof Token ? part.node : claimChild(module, part.node, parent),
    })),
  )
}

interface FunctionFields {
  name: NodeChild<AstId>
  argumentDefinitions: RawArgumentDefinition[]
  equals: NodeChild<SyncTokenId>
  body: NodeChild<AstId> | undefined
}
export class Function extends Ast {
  declare fields: FixedMapView<AstFields & FunctionFields>
  constructor(module: Module, fields: FixedMapView<AstFields & FunctionFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableFunction> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableFunction) return parsed
  }

  get name(): Ast {
    return this.module.get(this.fields.get('name').node)
  }
  get body(): Ast | undefined {
    return this.module.get(this.fields.get('body')?.node)
  }
  get argumentDefinitions(): ArgumentDefinition[] {
    return this.fields.get('argumentDefinitions').map((raw) =>
      raw.map((part) => ({
        ...part,
        node: this.module.getAny(part.node),
      })),
    )
  }

  static concrete(
    module: MutableModule,
    name: NodeChild<Owned>,
    argumentDefinitions: OwnedArgumentDefinition[],
    equals: NodeChild<Token>,
    body: NodeChild<Owned> | undefined,
  ) {
    const base = module.baseObject('Function')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      name: concreteChild(module, name, id_),
      argumentDefinitions: argumentDefinitionsToRaw(module, argumentDefinitions, id_),
      equals,
      body: concreteChild(module, body, id_),
    })
    return asOwned(new MutableFunction(module, fields))
  }

  static new(
    module: MutableModule,
    name: IdentLike,
    argumentDefinitions: OwnedArgumentDefinition[],
    body: Owned,
  ): Owned<MutableFunction> {
    // Note that a function name may not be an operator if the function is not in the body of a type definition, but we
    // can't easily enforce that because we don't currently make a syntactic distinction between top-level functions and
    // type methods.
    return MutableFunction.concrete(
      module,
      unspaced(Ident.newAllowingOperators(module, name)),
      argumentDefinitions,
      spaced(makeEquals()),
      autospaced(body),
    )
  }

  /** Construct a function with simple (name-only) arguments and a body block. */
  static fromStatements(
    module: MutableModule,
    name: IdentLike,
    argumentNames: StrictIdentLike[],
    statements: Owned[],
  ): Owned<MutableFunction> {
    const statements_: OwnedBlockLine[] = statements.map((statement) => ({
      expression: unspaced(statement),
    }))
    const argumentDefinitions = argumentNames.map((name) => [spaced(Ident.new(module, name))])
    const body = BodyBlock.new(statements_, module)
    return MutableFunction.new(module, name, argumentDefinitions, body)
  }

  *bodyExpressions(): IterableIterator<Ast> {
    const body = this.body
    if (body instanceof BodyBlock) {
      yield* body.statements()
    } else if (body) {
      yield body
    }
  }

  *concreteChildren(verbatim?: boolean): IterableIterator<NodeChild> {
    const { name, argumentDefinitions, equals, body } = getAll(this.fields)
    yield name
    for (const def of argumentDefinitions) yield* def
    yield { whitespace: equals.whitespace ?? ' ', node: this.module.getToken(equals.node) }
    if (body)
      yield ensureSpacedOnlyIf(
        body,
        !(this.module.tryGet(body.node) instanceof BodyBlock),
        verbatim,
      )
  }
}
export class MutableFunction extends Function implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & FunctionFields>

  setName<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('name', unspaced(this.claimChild(value)))
  }
  setBody<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('body', unspaced(this.claimChild(value)))
  }
  setArgumentDefinitions(defs: OwnedArgumentDefinition[]) {
    this.fields.set('argumentDefinitions', argumentDefinitionsToRaw(this.module, defs, this.id))
  }

  /** Returns the body, after converting it to a block if it was empty or an inline expression. */
  bodyAsBlock(): MutableBodyBlock {
    const oldBody = this.body
    if (oldBody instanceof MutableBodyBlock) return oldBody
    const newBody = BodyBlock.new([], this.module)
    if (oldBody) newBody.push(oldBody.take())
    return newBody
  }
}
export interface MutableFunction extends Function, MutableAst {
  get name(): MutableAst
  get body(): MutableAst | undefined
}
applyMixins(MutableFunction, [MutableAst])

interface AssignmentFields {
  pattern: NodeChild<AstId>
  equals: NodeChild<SyncTokenId>
  expression: NodeChild<AstId>
}
export class Assignment extends Ast {
  declare fields: FixedMapView<AstFields & AssignmentFields>
  constructor(module: Module, fields: FixedMapView<AstFields & AssignmentFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableAssignment> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableAssignment) return parsed
  }

  static concrete(
    module: MutableModule,
    pattern: NodeChild<Owned>,
    equals: NodeChild<Token>,
    expression: NodeChild<Owned>,
  ) {
    const base = module.baseObject('Assignment')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      pattern: concreteChild(module, pattern, id_),
      equals,
      expression: concreteChild(module, expression, id_),
    })
    return asOwned(new MutableAssignment(module, fields))
  }

  static new(module: MutableModule, ident: StrictIdentLike, expression: Owned) {
    return Assignment.concrete(
      module,
      unspaced(Ident.new(module, ident)),
      spaced(makeEquals()),
      spaced(expression),
    )
  }

  get pattern(): Ast {
    return this.module.get(this.fields.get('pattern').node)
  }
  get expression(): Ast {
    return this.module.get(this.fields.get('expression').node)
  }

  *concreteChildren(verbatim?: boolean): IterableIterator<NodeChild> {
    const { pattern, equals, expression } = getAll(this.fields)
    yield pattern
    yield ensureSpacedOnlyIf(equals, expression.whitespace !== '', verbatim)
    yield expression
  }
}
export class MutableAssignment extends Assignment implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & AssignmentFields>

  setPattern<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('pattern', unspaced(this.claimChild(value)))
  }
  setExpression<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'expression', this.claimChild(value))
  }
}
export interface MutableAssignment extends Assignment, MutableAst {
  get pattern(): MutableAst
  get expression(): MutableAst
}
applyMixins(MutableAssignment, [MutableAst])

interface BodyBlockFields {
  lines: RawBlockLine[]
}
export class BodyBlock extends Ast {
  declare fields: FixedMapView<AstFields & BodyBlockFields>
  constructor(module: Module, fields: FixedMapView<AstFields & BodyBlockFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableBodyBlock> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableBodyBlock) return parsed
  }

  static concrete(module: MutableModule, lines: OwnedBlockLine[]) {
    const base = module.baseObject('BodyBlock')
    const id_ = base.get('id')
    const fields = composeFieldData(base, {
      lines: lines.map((line) => lineToRaw(line, module, id_)),
    })
    return asOwned(new MutableBodyBlock(module, fields))
  }

  static new(lines: OwnedBlockLine[], module: MutableModule) {
    return BodyBlock.concrete(module, lines)
  }

  get lines(): BlockLine[] {
    return this.fields.get('lines').map((line) => lineFromRaw(line, this.module))
  }

  *statements(): IterableIterator<Ast> {
    for (const line of this.lines) {
      if (line.expression) yield line.expression.node
    }
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    for (const line of this.fields.get('lines')) {
      yield line.newline ?? { node: Token.new('\n', RawAst.Token.Type.Newline) }
      if (line.expression) yield line.expression
    }
  }

  printSubtree(
    info: SpanMap,
    offset: number,
    parentIndent: string | undefined,
    verbatim?: boolean,
  ): string {
    return printBlock(this, info, offset, parentIndent, verbatim)
  }
}
export class MutableBodyBlock extends BodyBlock implements MutableAst {
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

  /** Insert the given statement(s) starting at the specified line index. */
  insert(index: number, ...statements: (Owned | undefined)[]) {
    const before = this.fields.get('lines').slice(0, index)
    const insertions = statements.map((statement) => ({
      newline: unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
      expression: statement && unspaced(this.claimChild(statement)),
    }))
    const after = this.fields.get('lines').slice(index)
    this.fields.set('lines', [...before, ...insertions, ...after])
  }

  push(statement: Owned) {
    const oldLines = this.fields.get('lines')
    const newLine = {
      newline: unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
      expression: unspaced(this.claimChild(statement)),
    }
    this.fields.set('lines', [...oldLines, newLine])
  }

  filter(keep: (ast: MutableAst) => boolean) {
    const oldLines = this.fields.get('lines')
    const filteredLines = oldLines.filter((line) => {
      if (!line.expression) return true
      return keep(this.module.get(line.expression.node))
    })
    this.fields.set('lines', filteredLines)
  }
}
export interface MutableBodyBlock extends BodyBlock, MutableAst {
  statements(): IterableIterator<MutableAst>
}
applyMixins(MutableBodyBlock, [MutableAst])

interface RawLine<T> {
  newline: NodeChild<SyncTokenId>
  expression: NodeChild<T> | undefined
}
interface Line<T> {
  newline?: NodeChild<Token> | undefined
  expression: NodeChild<T> | undefined
}

interface RawBlockLine extends RawLine<AstId> {}
export type BlockLine = Line<Ast>
export type OwnedBlockLine = Line<Owned>

function lineFromRaw(raw: RawBlockLine, module: Module): BlockLine {
  const expression = raw.expression ? module.get(raw.expression.node) : undefined
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    expression:
      expression ?
        {
          whitespace: raw.expression?.whitespace,
          node: expression,
        }
      : undefined,
  }
}

function ownedLineFromRaw(raw: RawBlockLine, module: MutableModule): OwnedBlockLine {
  const expression = raw.expression ? module.get(raw.expression.node).takeIfParented() : undefined
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    expression:
      expression ?
        {
          whitespace: raw.expression?.whitespace,
          node: expression,
        }
      : undefined,
  }
}

function lineToRaw(line: OwnedBlockLine, module: MutableModule, block: AstId): RawBlockLine {
  return {
    newline: line.newline ?? unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
    expression:
      line.expression ?
        {
          whitespace: line.expression?.whitespace,
          node: claimChild(module, line.expression.node, block),
        }
      : undefined,
  }
}

interface IdentFields {
  token: NodeChild<SyncTokenId>
}
export class Ident extends Ast {
  declare fields: FixedMapView<AstFields & IdentFields>
  constructor(module: Module, fields: FixedMapView<AstFields & IdentFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableIdent> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableIdent) return parsed
  }

  get token(): IdentifierToken {
    return this.module.getToken(this.fields.get('token').node) as IdentifierToken
  }

  static concrete(module: MutableModule, token: NodeChild<Token>) {
    const base = module.baseObject('Ident')
    const fields = composeFieldData(base, { token })
    return asOwned(new MutableIdent(module, fields))
  }

  static new(module: MutableModule, ident: StrictIdentLike) {
    return Ident.concrete(module, unspaced(toIdentStrict(ident)))
  }

  /** @internal */
  static newAllowingOperators(module: MutableModule, ident: IdentLike) {
    return Ident.concrete(module, unspaced(toIdent(ident)))
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }

  code(): Identifier {
    return this.token.code() as Identifier
  }
}
export class MutableIdent extends Ident implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & IdentFields>

  setToken(ident: IdentLike) {
    this.fields.set('token', unspaced(toIdent(ident)))
  }

  code(): Identifier {
    return this.token.code()
  }
}
export interface MutableIdent extends Ident, MutableAst {}
applyMixins(MutableIdent, [MutableAst])

interface WildcardFields {
  token: NodeChild<SyncTokenId>
}
export class Wildcard extends Ast {
  declare fields: FixedMapView<AstFields & WildcardFields>
  constructor(module: Module, fields: FixedMapView<AstFields & WildcardFields>) {
    super(module, fields)
  }

  static tryParse(source: string, module?: MutableModule): Owned<MutableWildcard> | undefined {
    const parsed = parse(source, module)
    if (parsed instanceof MutableWildcard) return parsed
  }

  get token(): Token {
    return this.module.getToken(this.fields.get('token').node)
  }

  static concrete(module: MutableModule, token: NodeChild<Token>) {
    const base = module.baseObject('Wildcard')
    const fields = composeFieldData(base, { token })
    return asOwned(new MutableWildcard(module, fields))
  }

  static new(module: MutableModule) {
    const token = Token.new('_', RawAst.Token.Type.Wildcard)
    return this.concrete(module, unspaced(token))
  }

  *concreteChildren(_verbatim?: boolean): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }
}

export class MutableWildcard extends Wildcard implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & WildcardFields>
}
export interface MutableWildcard extends Wildcard, MutableAst {}
applyMixins(MutableWildcard, [MutableAst])

export type Mutable<T extends Ast = Ast> =
  T extends App ? MutableApp
  : T extends Assignment ? MutableAssignment
  : T extends BodyBlock ? MutableBodyBlock
  : T extends Documented ? MutableDocumented
  : T extends Function ? MutableFunction
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
  : T extends Wildcard ? MutableWildcard
  : MutableAst

export function materializeMutable(module: MutableModule, fields: FixedMap<AstFields>): MutableAst {
  const type = fields.get('type')
  const fieldsForType = fields as FixedMap<any>
  switch (type) {
    case 'App':
      return new MutableApp(module, fieldsForType)
    case 'UnaryOprApp':
      return new MutableUnaryOprApp(module, fieldsForType)
    case 'NegationApp':
      return new MutableNegationApp(module, fieldsForType)
    case 'OprApp':
      return new MutableOprApp(module, fieldsForType)
    case 'PropertyAccess':
      return new MutablePropertyAccess(module, fieldsForType)
    case 'Generic':
      return new MutableGeneric(module, fieldsForType)
    case 'Import':
      return new MutableImport(module, fieldsForType)
    case 'TextLiteral':
      return new MutableTextLiteral(module, fieldsForType)
    case 'Documented':
      return new MutableDocumented(module, fieldsForType)
    case 'Invalid':
      return new MutableInvalid(module, fieldsForType)
    case 'Group':
      return new MutableGroup(module, fieldsForType)
    case 'NumericLiteral':
      return new MutableNumericLiteral(module, fieldsForType)
    case 'Function':
      return new MutableFunction(module, fieldsForType)
    case 'Assignment':
      return new MutableAssignment(module, fieldsForType)
    case 'BodyBlock':
      return new MutableBodyBlock(module, fieldsForType)
    case 'Ident':
      return new MutableIdent(module, fieldsForType)
    case 'Wildcard':
      return new MutableWildcard(module, fieldsForType)
  }
  bail(`Invalid type: ${type}`)
}

export function materialize(module: Module, fields: FixedMapView<AstFields>): Ast {
  const type = fields.get('type')
  const fields_ = fields as FixedMapView<any>
  switch (type) {
    case 'App':
      return new App(module, fields_)
    case 'UnaryOprApp':
      return new UnaryOprApp(module, fields_)
    case 'NegationApp':
      return new NegationApp(module, fields_)
    case 'OprApp':
      return new OprApp(module, fields_)
    case 'PropertyAccess':
      return new PropertyAccess(module, fields_)
    case 'Generic':
      return new Generic(module, fields_)
    case 'Import':
      return new Import(module, fields_)
    case 'TextLiteral':
      return new TextLiteral(module, fields_)
    case 'Documented':
      return new Documented(module, fields_)
    case 'Invalid':
      return new Invalid(module, fields_)
    case 'Group':
      return new Group(module, fields_)
    case 'NumericLiteral':
      return new NumericLiteral(module, fields_)
    case 'Function':
      return new Function(module, fields_)
    case 'Assignment':
      return new Assignment(module, fields_)
    case 'BodyBlock':
      return new BodyBlock(module, fields_)
    case 'Ident':
      return new Ident(module, fields_)
    case 'Wildcard':
      return new Wildcard(module, fields_)
  }
  bail(`Invalid type: ${type}`)
}

export interface FixedMapView<Fields> {
  get<Key extends string & keyof Fields>(key: Key): Fields[Key]
  entries(): IterableIterator<readonly [string, unknown]>
  clone(): FixedMap<Fields>
  has(key: string): boolean
}

export interface FixedMap<Fields> extends FixedMapView<Fields> {
  set<Key extends string & keyof Fields>(key: Key, value: Fields[Key]): void
}

function getAll<Fields extends object>(map: FixedMapView<Fields>): Fields {
  return Object.fromEntries(map.entries()) as Fields
}

declare const brandLegalFieldContent: unique symbol
/** Used to add a constraint to all `AstFields`s subtypes ensuring that they were produced by `composeFieldData`, which
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

/** Modifies the input `map`. Returns the same object with an extended type. The added fields are required to have only
 *  types extending `FieldData`; the returned object is branded as `LegalFieldContent`. */
export function composeFieldData<Fields1, Fields2 extends FieldObject>(
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
): NodeChild<AstId | Token>
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token> | undefined,
  parent: AstId,
): NodeChild<AstId | Token> | undefined
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token> | undefined,
  parent: AstId,
): NodeChild<AstId | Token> | undefined {
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
      : (Token.new(ident, RawAst.Token.Type.Ident) as IdentifierToken)
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
      : (Token.new(ident, RawAst.Token.Type.Ident) as IdentifierOrOperatorIdentifierToken)
    : undefined
  )
}

function makeEquals(): Token {
  return Token.new('=', RawAst.Token.Type.Operator)
}

function nameSpecification(
  name: StrictIdentLike | undefined,
): { name: NodeChild<Token>; equals: NodeChild<Token> } | undefined {
  return name && { name: autospaced(toIdentStrict(name)), equals: unspaced(makeEquals()) }
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
  const old = map.get(key as string & keyof Fields)
  const updated = old ? { ...old, node } : autospaced(node)
  map.set(key, updated as Fields[Key])
}

function spaced<T extends object | string>(node: T): NodeChild<T>
function spaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined
function spaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined {
  if (node === undefined) return node
  return { whitespace: ' ', node }
}

function unspaced<T extends object | string>(node: T): NodeChild<T>
function unspaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined
function unspaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined {
  if (node === undefined) return node
  return { whitespace: '', node }
}

function autospaced<T extends object | string>(node: T): NodeChild<T>
function autospaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined
function autospaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined {
  if (node === undefined) return node
  return { node }
}

export interface Removed<T extends MutableAst> {
  node: Owned<T>
  placeholder: MutableWildcard | undefined
}
