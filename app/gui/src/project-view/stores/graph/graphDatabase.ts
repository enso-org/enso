import { computeNodeColor } from '@/composables/nodeColors'
import { ComputedValueRegistry, type ExpressionInfo } from '@/stores/project/computedValueRegistry'
import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import { type CallableSuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { AstId, NodeMetadata } from '@/util/ast/abstract'
import { MutableModule } from '@/util/ast/abstract'
import { analyzeBindings, type BindingInfo } from '@/util/ast/bindings'
import { inputNodeFromAst, nodeFromAst, nodeRootExpr } from '@/util/ast/node'
import { tryGetIndex } from '@/util/data/array'
import { recordEqual } from '@/util/data/object'
import { unwrap } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import {
  isIdentifierOrOperatorIdentifier,
  isQualifiedName,
  normalizeQualifiedName,
  tryIdentifier,
} from '@/util/qualifiedName'
import {
  nonReactiveView,
  resumeReactivity,
  resumeShallowReactivity,
  syncSetDiff,
} from '@/util/reactivity'
import * as objects from 'enso-common/src/utilities/data/object'
import * as set from 'lib0/set'
import {
  reactive,
  ref,
  shallowReactive,
  type DeepReadonly,
  type Ref,
  type WatchStopHandle,
} from 'vue'
import { type SourceDocument } from 'ydoc-shared/ast/sourceDocument'
import {
  methodPointerEquals,
  type MethodCall,
  type MethodPointer,
  type StackItem,
} from 'ydoc-shared/languageServerTypes'
import type { Opt } from 'ydoc-shared/util/data/opt'
import type { ExternalId, VisualizationMetadata } from 'ydoc-shared/yjsModel'
import { isUuid, visMetadataEquals } from 'ydoc-shared/yjsModel'

export interface MethodCallInfo {
  methodCall: MethodCall
  methodCallSource: Ast.AstId
  suggestion: CallableSuggestionEntry
}

/** TODO: Add docs */
export class GraphDb {
  nodeIdToNode = new ReactiveDb<NodeId, Node>()
  private readonly nodeSources = new Map<NodeId, { data: NodeSource; stop: WatchStopHandle }>()
  private highestZIndex = 0
  private readonly idToExternalMap = reactive(new Map<Ast.AstId, ExternalId>())
  private readonly idFromExternalMap = reactive(new Map<ExternalId, Ast.AstId>())
  private readonly bindings = new ReactiveDb<AstId, BindingInfo>()
  private readonly identifierToBindingId = new ReactiveIndex(this.bindings, (id, info) => [
    [info.identifier, id],
  ])

  /** TODO: Add docs */
  constructor(
    private suggestionDb: SuggestionDb,
    private groups: Ref<DeepReadonly<Group[]>>,
    private valuesRegistry: ComputedValueRegistry,
  ) {}

  private nodeIdToPatternExprIds = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    const exprs: AstId[] = []
    if (entry.pattern) entry.pattern.visitRecursive((ast) => void exprs.push(ast.id))
    return Array.from(exprs, (expr) => [id, expr])
  })

  private nodeIdToExprIds = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    const exprs: AstId[] = []
    entry.innerExpr.visitRecursive((ast) => void exprs.push(ast.id))
    return Array.from(exprs, (expr) => [id, expr])
  })

  connections = new ReactiveIndex(this.bindings, (alias, info) => {
    const srcNode = this.getPatternExpressionNodeId(alias) ?? this.getExpressionNodeId(alias)
    if (srcNode == null) return []
    return Array.from(this.connectionsFromBindings(info, alias, srcNode))
  })

  nodeDependents = new ReactiveIndex(this.nodeIdToNode, (id) => {
    const result = new Set<NodeId>()
    for (const port of this.getNodeUsages(id)) {
      const portNode = this.getExpressionNodeId(port)
      if (portNode != null) result.add(portNode)
    }
    return Array.from(result, (target) => [id, target])
  })

  private *connectionsFromBindings(
    info: BindingInfo,
    alias: AstId,
    srcNode: NodeId | undefined,
  ): Generator<[AstId, AstId]> {
    for (const usage of info.usages) {
      const targetNode = this.getExpressionNodeId(usage)
      // Display only connections to existing targets and different than source node.
      if (targetNode == null || targetNode === srcNode) continue
      yield [alias, usage]
    }
  }

  /** Output port bindings of the node. Lists all bindings that can be dragged out from a node. */
  nodeOutputPorts = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    if (entry.pattern == null) return []
    const ports = new Set<AstId>()
    entry.pattern.visitRecursive((ast) => {
      if (this.bindings.has(ast.id)) {
        ports.add(ast.id)
        return false
      }
      return true
    })
    return Array.from(ports, (port) => [id, port])
  })

  nodeMainSuggestionId = new ReactiveMapping(this.nodeIdToNode, (_id, entry) => {
    const expressionInfo = this.getExpressionInfo(entry.innerExpr.id)
    const method = expressionInfo?.methodCall?.methodPointer
    if (method == null) return
    return this.suggestionDb.findByMethodPointer(method)
  })

  /** TODO: Add docs */
  getNodeMainSuggestion(id: NodeId) {
    const suggestionId = this.nodeMainSuggestionId.lookup(id)
    if (suggestionId == null) return
    return this.suggestionDb.get(suggestionId)
  }

  nodeColor = new ReactiveMapping(this.nodeIdToNode, (id, entry) => {
    if (entry.colorOverride != null) return entry.colorOverride
    return computeNodeColor(
      () => entry.type,
      () => tryGetIndex(this.groups.value, this.getNodeMainSuggestion(id)?.groupIndex),
      () => this.getExpressionInfo(id)?.typename,
    )
  })

  /** TODO: Add docs */
  getNodeFirstOutputPort(id: NodeId | undefined): AstId | undefined {
    return id ? (set.first(this.nodeOutputPorts.lookup(id)) ?? this.idFromExternal(id)) : undefined
  }

  /** TODO: Add docs */
  *getNodeUsages(id: NodeId): IterableIterator<AstId> {
    const outputPorts = this.nodeOutputPorts.lookup(id)
    for (const outputPort of outputPorts) {
      yield* this.connections.lookup(outputPort)
    }
  }

  /** TODO: Add docs */
  getExpressionNodeId(exprId: AstId | undefined): NodeId | undefined {
    return exprId && set.first(this.nodeIdToExprIds.reverseLookup(exprId))
  }

  /** TODO: Add docs */
  getPatternExpressionNodeId(exprId: AstId | undefined): NodeId | undefined {
    return exprId && set.first(this.nodeIdToPatternExprIds.reverseLookup(exprId))
  }

  /** TODO: Add docs */
  getIdentDefiningNode(ident: string): NodeId | undefined {
    const binding = set.first(this.identifierToBindingId.lookup(ident))
    return this.getPatternExpressionNodeId(binding)
  }

  /** TODO: Add docs */
  getExpressionInfo(id: AstId | ExternalId | undefined): ExpressionInfo | undefined {
    const externalId = isUuid(id) ? id : this.idToExternal(id)
    return externalId && this.valuesRegistry.getExpressionInfo(externalId)
  }

  /** TODO: Add docs */
  getOutputPortIdentifier(source: AstId | undefined): string | undefined {
    return source ? this.bindings.get(source)?.identifier : undefined
  }

  /** TODO: Add docs */
  identifierUsed(ident: string): boolean {
    return this.identifierToBindingId.hasKey(ident)
  }

  /** TODO: Add docs */
  nodeIds(): IterableIterator<NodeId> {
    return this.nodeIdToNode.keys()
  }

  /** TODO: Add docs */
  isNodeId(externalId: ExternalId): externalId is NodeId {
    return this.nodeIdToNode.has(asNodeId(externalId))
  }

  /** TODO: Add docs */
  isKnownFunctionCall(id: AstId): boolean {
    return this.getMethodCallInfo(id) != null
  }

  /** TODO: Add docs */
  getMethodCall(id: AstId): MethodCall | undefined {
    const info = this.getExpressionInfo(id)
    if (info == null) return
    return (
      info.methodCall ?? (info.payload.type === 'Value' ? info.payload.functionSchema : undefined)
    )
  }

  /** TODO: Add docs */
  getMethodCallInfo(id: AstId): MethodCallInfo | undefined {
    const methodCall = this.getMethodCall(id)
    if (methodCall == null) return
    const suggestion = this.suggestionDb.entryByMethodPointer(methodCall.methodPointer)
    if (suggestion == null) return
    return { methodCall, methodCallSource: id, suggestion }
  }

  /** TODO: Add docs */
  getNodeColorStyle(id: NodeId): string {
    return this.nodeColor.lookup(id) ?? 'var(--node-color-no-type)'
  }

  /** TODO: Add docs */
  moveNodeToTop(id: NodeId) {
    const node = this.nodeIdToNode.get(id)
    if (!node) return
    node.zIndex = this.highestZIndex + 1
    this.highestZIndex++
  }

  /** Get the method name from the stack item. */
  stackItemToMethodName(item: StackItem): string | undefined {
    switch (item.type) {
      case 'ExplicitCall': {
        return item.methodPointer.name
      }
      case 'LocalCall': {
        const exprId = item.expressionId
        const info = this.valuesRegistry.getExpressionInfo(exprId)
        return info?.methodCall?.methodPointer.name
      }
    }
  }

  /**
   * Scan the block to identify nodes.
   *
   * Run when nodes are added or deleted, change external ID, or the chain of expressions outside any node's root
   * expression changes.
   */
  updateNodes(
    functionAst_: Ast.FunctionDef,
    { watchEffect }: { watchEffect: (f: () => void) => WatchStopHandle },
  ) {
    const currentNodeIds = new Set<NodeId>()
    const body = [...functionAst_.bodyExpressions()]
    const args = functionAst_.argumentDefinitions
    const update = (
      nodeId: NodeId,
      ast: Ast.Expression | Ast.Statement,
      isInput: boolean,
      isOutput: boolean,
      argIndex: number | undefined,
    ) => {
      const oldNode = nonReactiveView(this.nodeSources.get(nodeId)?.data)
      if (oldNode) {
        const node = resumeShallowReactivity<NodeSource>(oldNode)
        if (oldNode.isOutput !== isOutput) node.isOutput = isOutput
        if (oldNode.isInput !== isInput) node.isInput = isInput
        if (oldNode.argIndex !== argIndex) node.argIndex = argIndex
        if (oldNode.outerAst.id !== ast.id) node.outerAst = ast
      } else {
        const data = shallowReactive({ isOutput, outerAst: ast, isInput, argIndex })
        const stop = watchEffect(() =>
          this.updateNodeStructure(
            nodeId,
            data.outerAst,
            data.isOutput,
            data.isInput,
            data.argIndex,
          ),
        )
        this.nodeSources.set(nodeId, { data, stop })
      }
      currentNodeIds.add(nodeId)
    }
    args.forEach((argDef, index) => {
      const argPattern = argDef.pattern.node
      const nodeId = asNodeId(argPattern.externalId)
      update(nodeId, argPattern, true, false, index)
    })
    body.forEach((outerAst, index) => {
      const nodeId = nodeIdFromOuterAst(outerAst)
      if (!nodeId) return
      const isLastInBlock = index === body.length - 1
      update(nodeId, outerAst, false, isLastInBlock, undefined)
    })
    for (const [nodeId, info] of this.nodeSources.entries()) {
      if (!currentNodeIds.has(nodeId)) {
        info.stop()
        this.nodeIdToNode.delete(nodeId)
        this.nodeSources.delete(nodeId)
      }
    }
  }

  /** Scan a node's content from its outer expression down to, but not including, its inner expression. */
  private updateNodeStructure(
    nodeId: NodeId,
    ast: Ast.Statement | Ast.Expression,
    isOutput: boolean,
    isInput: boolean,
    argIndex?: number,
  ) {
    const newNode =
      isInput ?
        inputNodeFromAst(ast as Ast.Expression, argIndex ?? 0)
      : nodeFromAst(ast as Ast.Statement, isOutput)
    if (!newNode) return
    const oldNode = this.nodeIdToNode.getUntracked(nodeId)
    if (oldNode == null) {
      const nodeMeta = newNode.rootExpr.nodeMetadata
      const pos = nodeMeta.get('position') ?? { x: Infinity, y: Infinity }
      const metadataFields = {
        position: new Vec2(pos.x, pos.y),
        vis: nodeMeta.get('visualization'),
        colorOverride: nodeMeta.get('colorOverride'),
      }
      this.nodeIdToNode.set(nodeId, {
        ...newNode,
        ...metadataFields,
        zIndex: this.highestZIndex,
      })
    } else {
      const {
        type,
        outerAst,
        pattern,
        rootExpr,
        innerExpr,
        primarySubject,
        prefixes,
        conditionalPorts,
        argIndex,
      } = newNode
      const node = resumeReactivity(oldNode)
      if (oldNode.type !== type) node.type = type
      type NodeAstField = objects.ExtractKeys<Node, Ast.Ast | undefined>
      const updateAst = (field: NodeAstField) => {
        if (oldNode[field]?.id !== newNode[field]?.id) node[field] = newNode[field] as any
      }
      const astFields: NodeAstField[] = ['outerAst', 'pattern', 'rootExpr', 'innerExpr']
      astFields.forEach(updateAst)
      if (oldNode.primarySubject !== primarySubject) node.primarySubject = primarySubject
      if (!recordEqual(oldNode.prefixes, prefixes)) node.prefixes = prefixes
      syncSetDiff(node.conditionalPorts, oldNode.conditionalPorts, conditionalPorts)
      // Ensure new fields can't be added to `NodeAstData` without this code being updated.
      const _allFieldsHandled = {
        type,
        outerAst,
        pattern,
        rootExpr,
        innerExpr,
        primarySubject,
        prefixes,
        conditionalPorts,
        argIndex,
      } satisfies AllNodeFieldsFromAst
    }
  }

  /** Deeply scan the function to perform alias-analysis. */
  updateBindings(func: Ast.FunctionDef, moduleSource: Pick<SourceDocument, 'text' | 'getSpan'>) {
    const newBindings = analyzeBindings(func, moduleSource)
    for (const id of this.bindings.keys()) {
      if (!newBindings.has(id)) this.bindings.delete(id)
    }
    for (const [id, newInfo] of newBindings) {
      const oldInfo = this.bindings.getUntracked(id)
      if (oldInfo == null) {
        this.bindings.set(id, newInfo)
      } else {
        const info = resumeReactivity(oldInfo)
        if (oldInfo.identifier !== newInfo.identifier) info.identifier = newInfo.identifier
        syncSetDiff(info.usages, oldInfo.usages, newInfo.usages)
      }
    }
  }

  /** TODO: Add docs */
  updateExternalIds(topLevel: Ast.Ast) {
    const idToExternalNew = new Map()
    const idFromExternalNew = new Map()
    topLevel.visitRecursive((ast) => {
      idToExternalNew.set(ast.id, ast.externalId)
      idFromExternalNew.set(ast.externalId, ast.id)
    })
    const updateMap = <K, V>(map: Map<K, V>, newMap: Map<K, V>) => {
      for (const key of map.keys()) if (!newMap.has(key)) map.delete(key)
      for (const [key, value] of newMap) map.set(key, value)
    }
    updateMap(this.idToExternalMap, idToExternalNew)
    updateMap(this.idFromExternalMap, idFromExternalNew)
  }

  /** Apply the provided metadata updates. */
  updateMetadata(astId: Ast.AstId, changes: NodeMetadata) {
    const node = this.nodeByRootAstId(astId)
    if (!node) return
    const newPos = changes.get('position')
    const newPosVec = newPos && new Vec2(newPos.x, newPos.y)
    if (newPosVec && !newPosVec.equals(node.position)) node.position = newPosVec
    if (changes.has('visualization')) {
      const newVis = changes.get('visualization')
      if (!visMetadataEquals(newVis, node.vis)) node.vis = newVis
    }
    if (changes.has('colorOverride')) {
      node.colorOverride = changes.get('colorOverride')
    }
  }

  /** TODO: Add docs */
  nodeByRootAstId(astId: Ast.AstId): Node | undefined {
    const nodeId = asNodeId(this.idToExternal(astId))
    return nodeId != null ? this.nodeIdToNode.get(nodeId) : undefined
  }

  /** Get the ID of the `Ast` corresponding to the given `ExternalId` as of the last synchronization. */
  idFromExternal(id: ExternalId | undefined): AstId | undefined {
    return id ? this.idFromExternalMap.get(id) : id
  }
  /**
   * Get the external ID corresponding to the given `AstId` as of the last synchronization.
   *
   *  Note that if there is an edit in progress (i.e. a `MutableModule` containing changes that haven't been committed
   *  and observed), this may be different from the value return by calling `toExternal` on the edited `Ast` object.
   *
   *  When performing an edit and obtaining an ID to be sent to the engine, always use `Ast.toExternal`, which gives the
   *  ID the node will have once it is committed.
   *
   *  When looking up a node in data previously obtained from the engine, the choice depends on the situation:
   *  - If the data being looked up should be inherited from the previous holder of the `ExternalId`, use the current
   *    `toExternal`.
   *  - If the data should be associated with the `Ast` that the engine was referring to, use `idToExternal`.
   *  Either choice is an approximation that will be used until the engine provides an update after processing the edit.
   */
  idToExternal(id: AstId | undefined): ExternalId | undefined {
    return id ? this.idToExternalMap.get(id) : undefined
  }

  /**
   * Synchronously replace all instances of specific method pointer usage within the value registry and
   * suggestion database.
   *
   * FIXME: This is a hack in order to make function renaming from within that function work correctly.
   * Execution contexts don't send expression updates about their parent frames, so we end up with an
   * outdated methodPointer on the parent frame's expression. We have to update the valueRegistry and
   * suggestionDb entries to keep it working correctly. Both need to be updated synchronously to avoid
   * flashing.
   */
  insertSyntheticMethodPointerUpdate(
    oldMethodPointer: MethodPointer,
    newMethodPointer: MethodPointer,
  ) {
    for (const value of this.valuesRegistry.db.values()) {
      if (
        value.methodCall != null &&
        methodPointerEquals(value.methodCall.methodPointer, oldMethodPointer)
      ) {
        value.methodCall.methodPointer = newMethodPointer
      }
    }

    const suggestion = this.suggestionDb.findByMethodPointer(oldMethodPointer)
    const suggestionEntry = suggestion != null ? this.suggestionDb.get(suggestion) : null
    if (suggestionEntry != null) {
      DEV: assert(isQualifiedName(newMethodPointer.module))
      DEV: assert(isQualifiedName(newMethodPointer.definedOnType))
      DEV: assert(isIdentifierOrOperatorIdentifier(newMethodPointer.name))
      Object.assign(suggestionEntry, {
        definedIn: normalizeQualifiedName(newMethodPointer.module),
        memberOf: normalizeQualifiedName(newMethodPointer.definedOnType),
        name: newMethodPointer.name,
      })
    }
  }
  /** TODO: Add docs */
  static Mock(registry = ComputedValueRegistry.Mock(), db = new SuggestionDb()): GraphDb {
    return new GraphDb(db, ref([]), registry)
  }

  /** TODO: Add docs */
  mockNode(binding: string, id: NodeId, code?: string): Node {
    const edit = MutableModule.Transient()
    const ident = unwrap(tryIdentifier(binding))
    const expression = Ast.parseExpression(code ?? '0', edit)!
    const outerAst = Ast.Assignment.new(ident, expression, { edit })
    const pattern = outerAst.pattern

    const node: Node = {
      type: 'component',
      position: Vec2.Zero,
      vis: undefined,
      prefixes: { enableRecording: undefined },
      primarySubject: undefined,
      colorOverride: undefined,
      conditionalPorts: new Set(),
      outerAst,
      pattern,
      rootExpr: expression,
      innerExpr: expression,
      zIndex: this.highestZIndex,
      argIndex: undefined,
    }
    const bindingId = pattern.id
    this.nodeIdToNode.set(id, node)
    this.bindings.set(bindingId, { identifier: binding, usages: new Set() })
    return node
  }
}

/** Source code data of the specific node. */
interface NodeSource {
  /** The outer AST of the node (see {@link NodeDataFromAst.outerAst}). */
  outerAst: Ast.Ast
  /**
   * Whether the node is `output` of the function or not. Mutually exclusive with `isInput`.
   * Output node is the last node in a function body and has no pattern.
   */
  isOutput: boolean
  /**
   * Whether the node is `input` of the function or not. Mutually exclusive with `isOutput`.
   * Input node is a function argument.
   */
  isInput: boolean
  /** The index of the argument in the function's argument list, if the node is an input node. */
  argIndex: number | undefined
}

declare const brandNodeId: unique symbol

/** An unique node identifier, shared across all clients. It is the ExternalId of node's root expression. */
export type NodeId = string & ExternalId & { [brandNodeId]: never }
export type NodeType = 'component' | 'output' | 'input'
export function asNodeId(id: ExternalId): NodeId
export function asNodeId(id: ExternalId | undefined): NodeId | undefined
/** TODO: Add docs */
export function asNodeId(id: ExternalId | undefined): NodeId | undefined {
  return id != null ? (id as NodeId) : undefined
}

/** Given the outermost AST for a node, returns its {@link NodeId}. */
export function nodeIdFromOuterAst(outerAst: Ast.Statement | Ast.Expression) {
  const { root } = nodeRootExpr(outerAst)
  return root && asNodeId(root.externalId)
}

/** Given a node, returns its {@link NodeId}. */
export function nodeId({ rootExpr }: { rootExpr: Ast.Expression }): NodeId {
  return asNodeId(rootExpr.externalId)
}

export type NodeDataFromAst = ComponentNodeData | InputNodeData | OutputNodeData

interface AllNodeFieldsFromAst {
  type: NodeType
  /**
   * The statement or top-level expression.
   *
   * If the function has a body block, the nodes derived from the block are statements:
   * - Assignment expressions (`a = b`)
   * - Expression-statements (unnamed nodes and output nodes)
   * If the function has a single-line body, the corresponding node will be an expression.
   *
   * Nodes for the function's inputs have (pattern) expressions as their outer ASTs.
   */
  outerAst: Ast.Statement | Ast.Expression
  /** The left side of the assignment expression, if `outerAst` is an assignment expression. */
  pattern: Ast.Expression | undefined
  /**
   * The value of the node. The right side of the assignment, if `outerAst` is an assignment
   * expression, else the entire `outerAst`.
   */
  rootExpr: Ast.Expression
  /**
   * The expression displayed by the node. This is `rootExpr`, minus the prefixes, which are in
   * `prefixes`.
   */
  innerExpr: Ast.Expression
  /**
   Prefixes that are present in `rootExpr` but omitted in `innerExpr` to ensure a clean output.
   */
  prefixes: Record<'enableRecording', Ast.AstId[] | undefined>
  /** A child AST in a syntactic position to be a self-argument input to the node. */
  primarySubject: Ast.AstId | undefined
  /** Ports that are not targetable by default; they can be targeted while holding the modifier key. */
  conditionalPorts: Set<Ast.AstId>
  /** The index of the argument in the function's argument list, if the node is an input node. */
  argIndex: number | undefined
}

export interface ComponentNodeData extends AllNodeFieldsFromAst {
  type: 'component'
  outerAst: Ast.Statement
}

export interface InputNodeData extends AllNodeFieldsFromAst {
  type: 'input'
  outerAst: Ast.Expression
  argIndex: number
}

/** Type predicate for nodes of type `input`. */
export function isInputNode(node: Node): node is Node & InputNodeData {
  return node.type === 'input'
}

export interface OutputNodeData extends AllNodeFieldsFromAst {
  type: 'output'
  outerAst: Ast.Statement
}

export interface NodeDataFromMetadata {
  position: Vec2
  vis: Opt<VisualizationMetadata>
  colorOverride: Opt<string>
}

export type Node = NodeDataFromAst &
  NodeDataFromMetadata & {
    zIndex: number
  }
