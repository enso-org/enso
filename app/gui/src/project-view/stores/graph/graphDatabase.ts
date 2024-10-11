import { computeNodeColor } from '@/composables/nodeColors'
import { ComputedValueRegistry, type ExpressionInfo } from '@/stores/project/computedValueRegistry'
import { SuggestionDb, type Group } from '@/stores/suggestionDatabase'
import type { SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { assert } from '@/util/assert'
import { Ast, RawAst } from '@/util/ast'
import type { AstId, NodeMetadata } from '@/util/ast/abstract'
import { autospaced, MutableModule } from '@/util/ast/abstract'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { inputNodeFromAst, nodeFromAst, nodeRootExpr } from '@/util/ast/node'
import { MappedKeyMap, MappedSet } from '@/util/containers'
import { tryGetIndex } from '@/util/data/array'
import { recordEqual } from '@/util/data/object'
import { Vec2 } from '@/util/data/vec2'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import {
  nonReactiveView,
  resumeReactivity,
  resumeShallowReactivity,
  syncSetDiff,
} from '@/util/reactivity'
import * as objects from 'enso-common/src/utilities/data/object'
import * as set from 'lib0/set'
import { reactive, ref, shallowReactive, WatchStopHandle, type Ref } from 'vue'
import type { MethodCall, StackItem } from 'ydoc-shared/languageServerTypes'
import type { Opt } from 'ydoc-shared/util/data/opt'
import type { ExternalId, SourceRange, VisualizationMetadata } from 'ydoc-shared/yjsModel'
import { isUuid, sourceRangeKey, visMetadataEquals } from 'ydoc-shared/yjsModel'

export interface MethodCallInfo {
  methodCall: MethodCall
  methodCallSource: Ast.AstId
  suggestion: SuggestionEntry
}

export interface BindingInfo {
  identifier: string
  usages: Set<AstId>
}

/** TODO: Add docs */
export class BindingsDb {
  bindings = new ReactiveDb<AstId, BindingInfo>()
  identifierToBindingId = new ReactiveIndex(this.bindings, (id, info) => [[info.identifier, id]])

  /** TODO: Add docs */
  readFunctionAst(
    func: Ast.Function,
    rawFunc: RawAst.Tree.Function | undefined,
    moduleCode: string,
    getSpan: (id: AstId) => SourceRange | undefined,
  ) {
    // TODO[ao]: Rename 'alias' to 'binding' in AliasAnalyzer and it's more accurate term.
    const analyzer = new AliasAnalyzer(moduleCode, rawFunc)
    analyzer.process()

    const [bindingRangeToTree, bindingIdToRange] = BindingsDb.rangeMappings(func, analyzer, getSpan)

    // Remove old keys.
    for (const key of this.bindings.keys()) {
      const range = bindingIdToRange.get(key)
      if (range == null || !analyzer.aliases.has(range)) {
        this.bindings.delete(key)
      }
    }

    // Add or update bindings.
    for (const [bindingRange, usagesRanges] of analyzer.aliases) {
      const aliasAst = bindingRangeToTree.get(bindingRange)
      assert(aliasAst != null)
      if (aliasAst == null) continue
      const aliasAstId = aliasAst.id
      const info = this.bindings.get(aliasAstId)
      if (info == null) {
        function* usageIds() {
          for (const usageRange of usagesRanges) {
            const usageAst = bindingRangeToTree.get(usageRange)
            assert(usageAst != null)
            if (usageAst != null) yield usageAst.id
          }
        }
        this.bindings.set(aliasAstId, {
          identifier: aliasAst.code(),
          usages: new Set(usageIds()),
        })
      } else {
        const newIdentifier = aliasAst.code()
        if (info.identifier != newIdentifier) info.identifier = newIdentifier
        // Remove old usages.
        for (const usage of info.usages) {
          const range = bindingIdToRange.get(usage)
          if (range == null || !usagesRanges.has(range)) info.usages.delete(usage)
        }
        // Add or update usages.
        for (const usageRange of usagesRanges) {
          const usageAst = bindingRangeToTree.get(usageRange)
          if (usageAst != null && !info.usages.has(usageAst.id)) {
            info.usages.add(usageAst.id)
          }
        }
      }
    }
  }

  /**
   * Create mappings between bindings' ranges and AST
   *
   * The AliasAnalyzer is general and returns ranges, but we're interested in AST nodes. This
   * method creates mappings in both ways. For given range, only the shallowest AST node will be
   * assigned (RawAst.Tree.Identifier, not RawAst.Token.Identifier).
   */
  private static rangeMappings(
    ast: Ast.Ast,
    analyzer: AliasAnalyzer,
    getSpan: (id: AstId) => SourceRange | undefined,
  ): [MappedKeyMap<SourceRange, Ast.Ast>, Map<AstId, SourceRange>] {
    const bindingRangeToTree = new MappedKeyMap<SourceRange, Ast.Ast>(sourceRangeKey)
    const bindingIdToRange = new Map<AstId, SourceRange>()
    const bindingRanges = new MappedSet(sourceRangeKey)
    for (const [binding, usages] of analyzer.aliases) {
      bindingRanges.add(binding)
      for (const usage of usages) bindingRanges.add(usage)
    }
    ast.visitRecursiveAst((ast) => {
      const span = getSpan(ast.id)
      assert(span != null)
      if (bindingRanges.has(span)) {
        bindingRangeToTree.set(span, ast)
        bindingIdToRange.set(ast.id, span)
        return false
      }
      return true
    })
    return [bindingRangeToTree, bindingIdToRange]
  }
}

/** TODO: Add docs */
export class GraphDb {
  nodeIdToNode = new ReactiveDb<NodeId, Node>()
  private readonly nodeSources = new Map<NodeId, { data: NodeSource; stop: WatchStopHandle }>()
  private highestZIndex = 0
  private readonly idToExternalMap = reactive(new Map<Ast.AstId, ExternalId>())
  private readonly idFromExternalMap = reactive(new Map<ExternalId, Ast.AstId>())
  private bindings = new BindingsDb()

  /** TODO: Add docs */
  constructor(
    private suggestionDb: SuggestionDb,
    private groups: Ref<Group[]>,
    private valuesRegistry: ComputedValueRegistry,
  ) {}

  private nodeIdToPatternExprIds = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    const exprs: AstId[] = []
    if (entry.pattern) entry.pattern.visitRecursiveAst((ast) => void exprs.push(ast.id))
    return Array.from(exprs, (expr) => [id, expr])
  })

  private nodeIdToExprIds = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    const exprs: AstId[] = []
    entry.innerExpr.visitRecursiveAst((ast) => void exprs.push(ast.id))
    return Array.from(exprs, (expr) => [id, expr])
  })

  connections = new ReactiveIndex(this.bindings.bindings, (alias, info) => {
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
    entry.pattern.visitRecursiveAst((ast) => {
      if (this.bindings.bindings.has(ast.id)) {
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
    return id ? set.first(this.nodeOutputPorts.lookup(id)) ?? this.idFromExternal(id) : undefined
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
    const binding = set.first(this.bindings.identifierToBindingId.lookup(ident))
    return this.getPatternExpressionNodeId(binding)
  }

  /** TODO: Add docs */
  getExpressionInfo(id: AstId | ExternalId | undefined): ExpressionInfo | undefined {
    const externalId = isUuid(id) ? id : this.idToExternal(id)
    return externalId && this.valuesRegistry.getExpressionInfo(externalId)
  }

  /** TODO: Add docs */
  getOutputPortIdentifier(source: AstId | undefined): string | undefined {
    return source ? this.bindings.bindings.get(source)?.identifier : undefined
  }

  /** TODO: Add docs */
  identifierUsed(ident: string): boolean {
    return this.bindings.identifierToBindingId.hasKey(ident)
  }

  /** TODO: Add docs */
  nodeIds(): IterableIterator<NodeId> {
    return this.nodeIdToNode.keys()
  }

  /** TODO: Add docs */
  isNodeId(externalId: ExternalId): boolean {
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
    const suggestionId = this.suggestionDb.findByMethodPointer(methodCall.methodPointer)
    if (suggestionId == null) return
    const suggestion = this.suggestionDb.get(suggestionId)
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
    functionAst_: Ast.Function,
    { watchEffect }: { watchEffect: (f: () => void) => WatchStopHandle },
  ) {
    const currentNodeIds = new Set<NodeId>()
    const body = [...functionAst_.bodyExpressions()]
    const args = functionAst_.argumentDefinitions
    const update = (
      nodeId: NodeId,
      ast: Ast.Ast,
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
      const nodeId = nodeIdFromOuterExpr(outerAst)
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
    ast: Ast.Ast,
    isOutput: boolean,
    isInput: boolean,
    argIndex?: number,
  ) {
    const newNode = isInput ? inputNodeFromAst(ast, argIndex ?? 0) : nodeFromAst(ast, isOutput)
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
        outerExpr,
        pattern,
        rootExpr,
        innerExpr,
        primarySubject,
        prefixes,
        conditionalPorts,
        docs,
        argIndex,
      } = newNode
      const node = resumeReactivity(oldNode)
      if (oldNode.type !== type) node.type = type
      type NodeAstField = objects.ExtractKeys<Node, Ast.Ast | undefined>
      const updateAst = (field: NodeAstField) => {
        if (oldNode[field]?.id !== newNode[field]?.id) node[field] = newNode[field] as any
      }
      const astFields: NodeAstField[] = ['outerExpr', 'pattern', 'rootExpr', 'innerExpr', 'docs']
      astFields.forEach(updateAst)
      if (oldNode.primarySubject !== primarySubject) node.primarySubject = primarySubject
      if (!recordEqual(oldNode.prefixes, prefixes)) node.prefixes = prefixes
      syncSetDiff(node.conditionalPorts, oldNode.conditionalPorts, conditionalPorts)
      // Ensure new fields can't be added to `NodeAstData` without this code being updated.
      const _allFieldsHandled = {
        type,
        outerExpr,
        pattern,
        rootExpr,
        innerExpr,
        primarySubject,
        prefixes,
        conditionalPorts,
        docs,
        argIndex,
      } satisfies NodeDataFromAst
    }
  }

  /** Deeply scan the function to perform alias-analysis. */
  updateBindings(
    functionAst_: Ast.Function,
    rawFunction: RawAst.Tree.Function | undefined,
    moduleCode: string,
    getSpan: (id: AstId) => SourceRange | undefined,
  ) {
    this.bindings.readFunctionAst(functionAst_, rawFunction, moduleCode, getSpan)
  }

  /** TODO: Add docs */
  updateExternalIds(topLevel: Ast.Ast) {
    const idToExternalNew = new Map()
    const idFromExternalNew = new Map()
    topLevel.visitRecursiveAst((ast) => {
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

  /** TODO: Add docs */
  static Mock(registry = ComputedValueRegistry.Mock(), db = new SuggestionDb()): GraphDb {
    return new GraphDb(db, ref([]), registry)
  }

  /** TODO: Add docs */
  mockNode(binding: string, id: NodeId, code?: string): Node {
    const edit = MutableModule.Transient()
    const pattern = Ast.parse(binding, edit)
    const expression = Ast.parse(code ?? '0', edit)
    const outerExpr = Ast.Assignment.concrete(
      edit,
      autospaced(pattern),
      { node: Ast.Token.new('='), whitespace: ' ' },
      { node: expression, whitespace: ' ' },
    )

    const node: Node = {
      type: 'component',
      position: Vec2.Zero,
      vis: undefined,
      prefixes: { enableRecording: undefined },
      primarySubject: undefined,
      colorOverride: undefined,
      conditionalPorts: new Set(),
      docs: undefined,
      outerExpr,
      pattern,
      rootExpr: Ast.parse(code ?? '0'),
      innerExpr: Ast.parse(code ?? '0'),
      zIndex: this.highestZIndex,
      argIndex: undefined,
    }
    const bindingId = pattern.id
    this.nodeIdToNode.set(id, node)
    this.bindings.bindings.set(bindingId, { identifier: binding, usages: new Set() })
    return node
  }
}

/** Source code data of the specific node. */
interface NodeSource {
  /** The outer AST of the node (see {@link NodeDataFromAst.outerExpr}). */
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

/** Given an expression at the top level of a block, return the `NodeId` for the expression. */
export function nodeIdFromOuterExpr(outerExpr: Ast.Ast) {
  const { root } = nodeRootExpr(outerExpr)
  return root && asNodeId(root.externalId)
}

export interface NodeDataFromAst {
  type: NodeType
  /** The outer expression, usually an assignment expression (`a = b`). */
  outerExpr: Ast.Ast
  /** The left side of the assignment expression, if `outerExpr` is an assignment expression. */
  pattern: Ast.Ast | undefined
  /**
   * The value of the node. The right side of the assignment, if `outerExpr` is an assignment
   * expression, else the entire `outerExpr`.
   */
  rootExpr: Ast.Ast
  /**
   * The expression displayed by the node. This is `rootExpr`, minus the prefixes, which are in
   * `prefixes`.
   */
  innerExpr: Ast.Ast
  /**
    Prefixes that are present in `rootExpr` but omitted in `innerExpr` to ensure a clean output.
   */
  prefixes: Record<'enableRecording', Ast.AstId[] | undefined>
  /** A child AST in a syntactic position to be a self-argument input to the node. */
  primarySubject: Ast.AstId | undefined
  /** Ports that are not targetable by default; they can be targeted while holding the modifier key. */
  conditionalPorts: Set<Ast.AstId>
  /** An AST node containing the node's documentation comment. */
  docs: Ast.Documented | undefined
  /** The index of the argument in the function's argument list, if the node is an input node. */
  argIndex: number | undefined
}

export interface NodeDataFromMetadata {
  position: Vec2
  vis: Opt<VisualizationMetadata>
  colorOverride: Opt<string>
}

export interface Node extends NodeDataFromAst, NodeDataFromMetadata {
  zIndex: number
}
