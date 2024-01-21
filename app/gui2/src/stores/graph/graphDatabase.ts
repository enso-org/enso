import { ComputedValueRegistry, type ExpressionInfo } from '@/stores/project/computedValueRegistry'
import { SuggestionDb, groupColorStyle, type Group } from '@/stores/suggestionDatabase'
import type { SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { bail } from '@/util/assert'
import { Ast, RawAst, RawAstExtended } from '@/util/ast'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { nodeFromAst } from '@/util/ast/node'
import { colorFromString } from '@/util/colors'
import { MappedKeyMap, MappedSet } from '@/util/containers'
import { arrayEquals, byteArraysEqual, tryGetIndex } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { Vec2 } from '@/util/data/vec2'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import * as random from 'lib0/random'
import * as set from 'lib0/set'
import {
  methodPointerEquals,
  type ExpressionUpdate,
  type MethodCall,
  type StackItem,
} from 'shared/languageServerTypes'
import {
  IdMap,
  visMetadataEquals,
  type ExprId,
  type NodeMetadata,
  type SourceRange,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { ref, type Ref } from 'vue'

export interface BindingInfo {
  identifier: string
  usages: Set<ExprId>
}

export class BindingsDb {
  bindings = new ReactiveDb<ExprId, BindingInfo>()
  identifierToBindingId = new ReactiveIndex(this.bindings, (id, info) => [[info.identifier, id]])

  readFunctionAst(ast: RawAstExtended<RawAst.Tree.Function>) {
    // TODO[ao]: Rename 'alias' to 'binding' in AliasAnalyzer and it's more accurate term.
    const analyzer = new AliasAnalyzer(ast.parsedCode, ast.inner)
    analyzer.process()

    const [bindingRangeToTree, bindingIdToRange] = BindingsDb.rangeMappings(ast, analyzer)

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
      if (aliasAst == null) continue
      const info = this.bindings.get(aliasAst.astId)
      if (info == null) {
        function* usageIds() {
          for (const usageRange of usagesRanges) {
            const usageAst = bindingRangeToTree.get(usageRange)
            if (usageAst != null) yield usageAst.astId
          }
        }
        this.bindings.set(aliasAst.astId, {
          identifier: aliasAst.repr(),
          usages: new Set(usageIds()),
        })
      } else {
        const newIdentifier = aliasAst.repr()
        if (info.identifier != newIdentifier) info.identifier = newIdentifier
        // Remove old usages.
        for (const usage of info.usages) {
          const range = bindingIdToRange.get(usage)
          if (range == null || !usagesRanges.has(range)) info.usages.delete(usage)
        }
        // Add or update usages.
        for (const usageRange of usagesRanges) {
          const usageAst = bindingRangeToTree.get(usageRange)
          if (usageAst != null && !info.usages.has(usageAst.astId)) info.usages.add(usageAst.astId)
        }
      }
    }
  }

  /** Create mappings between bindings' ranges and AST
   *
   * The AliasAnalyzer is general and returns ranges, but we're interested in AST nodes. This
   * method creates mappings in both ways. For given range, only the shallowest AST node will be
   * assigned (RawAst.Tree.Identifier, not RawAst.Token.Identifier).
   */
  private static rangeMappings(
    ast: RawAstExtended,
    analyzer: AliasAnalyzer,
  ): [MappedKeyMap<SourceRange, RawAstExtended>, Map<ExprId, SourceRange>] {
    const bindingRangeToTree = new MappedKeyMap<SourceRange, RawAstExtended>(IdMap.keyForRange)
    const bindingIdToRange = new Map<ExprId, SourceRange>()
    const bindingRanges = new MappedSet(IdMap.keyForRange)
    for (const [binding, usages] of analyzer.aliases) {
      bindingRanges.add(binding)
      for (const usage of usages) bindingRanges.add(usage)
    }
    ast.visitRecursive((ast) => {
      if (bindingRanges.has(ast.span())) {
        bindingRangeToTree.set(ast.span(), ast)
        bindingIdToRange.set(ast.astId, ast.span())
        return false
      }
      return true
    })
    return [bindingRangeToTree, bindingIdToRange]
  }
}

export class GraphDb {
  nodeIdToNode = new ReactiveDb<ExprId, Node>()
  private bindings = new BindingsDb()

  constructor(
    private suggestionDb: SuggestionDb,
    private groups: Ref<Group[]>,
    private valuesRegistry: ComputedValueRegistry,
  ) {}

  private nodeIdToPatternExprIds = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    if (entry.pattern == null) return []
    const exprs = new Set<ExprId>()
    entry.pattern.visitRecursive((astOrToken) => exprs.add(astOrToken.exprId))
    return Array.from(exprs, (expr) => [id, expr])
  })

  private nodeIdToExprIds = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    const exprs = new Set<ExprId>()
    entry.rootSpan.visitRecursive((astOrToken) => exprs.add(astOrToken.exprId))
    return Array.from(exprs, (expr) => [id, expr])
  })

  connections = new ReactiveIndex(this.bindings.bindings, (alias, info) => {
    const srcNode = this.getPatternExpressionNodeId(alias)
    // Display connection starting from existing node.
    //TODO[ao]: When implementing input nodes, they should be taken into account here.
    if (srcNode == null) return []
    return Array.from(this.connectionsFromBindings(info, alias, srcNode))
  })

  /** Same as {@link GraphDb.connections}, but also includes connections without source node,
   * e.g. input arguments of the collapsed function.
   */
  allConnections = new ReactiveIndex(this.bindings.bindings, (alias, info) => {
    const srcNode = this.getPatternExpressionNodeId(alias)
    return Array.from(this.connectionsFromBindings(info, alias, srcNode))
  })

  private *connectionsFromBindings(
    info: BindingInfo,
    alias: ExprId,
    srcNode: ExprId | undefined,
  ): Generator<[ExprId, ExprId]> {
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
    const ports = new Set<ExprId>()
    entry.pattern.visitRecursive((ast) => {
      if (this.bindings.bindings.has(ast.exprId)) {
        ports.add(ast.exprId)
        return false
      }
      return true
    })
    return Array.from(ports, (port) => [id, port])
  })

  nodeMainSuggestion = new ReactiveMapping(this.nodeIdToNode, (id, _entry) => {
    const expressionInfo = this.getExpressionInfo(id)
    const method = expressionInfo?.methodCall?.methodPointer
    if (method == null) return
    const suggestionId = this.suggestionDb.findByMethodPointer(method)
    if (suggestionId == null) return
    return this.suggestionDb.get(suggestionId)
  })

  nodeColor = new ReactiveMapping(this.nodeIdToNode, (id, _entry) => {
    const index = this.nodeMainSuggestion.lookup(id)?.groupIndex
    const group = tryGetIndex(this.groups.value, index)
    if (group == null) {
      const typename = this.getExpressionInfo(id)?.typename
      return typename ? colorFromString(typename) : 'var(--node-color-no-type)'
    }
    return groupColorStyle(group)
  })

  getNodeFirstOutputPort(id: ExprId): ExprId {
    return set.first(this.nodeOutputPorts.lookup(id)) ?? id
  }

  getExpressionNodeId(exprId: ExprId | undefined): ExprId | undefined {
    return exprId && set.first(this.nodeIdToExprIds.reverseLookup(exprId))
  }

  getPatternExpressionNodeId(exprId: ExprId | undefined): ExprId | undefined {
    return exprId && set.first(this.nodeIdToPatternExprIds.reverseLookup(exprId))
  }

  getIdentDefiningNode(ident: string): ExprId | undefined {
    const binding = set.first(this.bindings.identifierToBindingId.lookup(ident))
    return this.getPatternExpressionNodeId(binding)
  }

  getExpressionInfo(id: ExprId): ExpressionInfo | undefined {
    return this.valuesRegistry.getExpressionInfo(id)
  }

  getOutputPortIdentifier(source: ExprId): string | undefined {
    return this.bindings.bindings.get(source)?.identifier
  }

  allIdentifiers(): string[] {
    return [...this.bindings.identifierToBindingId.allForward()].map(([ident, _]) => ident)
  }

  identifierUsed(ident: string): boolean {
    return this.bindings.identifierToBindingId.hasKey(ident)
  }

  isKnownFunctionCall(id: ExprId): boolean {
    return this.getMethodCallInfo(id) != null
  }

  getMethodCall(id: ExprId): MethodCall | undefined {
    const info = this.getExpressionInfo(id)
    if (info == null) return
    return (
      info.methodCall ?? (info.payload.type === 'Value' ? info.payload.functionSchema : undefined)
    )
  }

  /**
   * Get a list of all nodes that depend on given node. Includes transitive dependencies.
   */
  dependantNodes(id: ExprId): Set<ExprId> {
    const toVisit = [id]
    const result = new Set<ExprId>()

    let currentNode: ExprId | undefined
    while ((currentNode = toVisit.pop())) {
      const outputPorts = this.nodeOutputPorts.lookup(currentNode)
      for (const outputPort of outputPorts) {
        const connectedPorts = this.connections.lookup(outputPort)
        for (const port of connectedPorts) {
          const portNode = this.getExpressionNodeId(port)
          if (portNode == null) continue
          if (!result.has(portNode)) {
            result.add(portNode)
            toVisit.push(portNode)
          }
        }
      }
    }

    return result
  }

  getMethodCallInfo(
    id: ExprId,
  ):
    | { methodCall: MethodCall; suggestion: SuggestionEntry; partiallyApplied: boolean }
    | undefined {
    const info = this.getExpressionInfo(id)
    if (info == null) return
    const payloadFuncSchema =
      info.payload.type === 'Value' ? info.payload.functionSchema : undefined
    const methodCall = info.methodCall ?? payloadFuncSchema
    if (methodCall == null) return
    const suggestionId = this.suggestionDb.findByMethodPointer(methodCall.methodPointer)
    if (suggestionId == null) return
    const suggestion = this.suggestionDb.get(suggestionId)
    if (suggestion == null) return
    const partiallyApplied = mathodCallEquals(methodCall, payloadFuncSchema)
    return { methodCall, suggestion, partiallyApplied }
  }

  getNodeColorStyle(id: ExprId): string {
    return this.nodeColor.lookup(id) ?? 'var(--node-color-no-type)'
  }

  moveNodeToTop(id: ExprId) {
    this.nodeIdToNode.moveToLast(id)
  }

  /** Get the method name from the stack item. */
  stackItemToMethodName(item: StackItem): string | undefined {
    switch (item.type) {
      case 'ExplicitCall': {
        return item.methodPointer.name
      }
      case 'LocalCall': {
        const exprId = item.expressionId
        const info = this.getExpressionInfo(exprId)
        return info?.methodCall?.methodPointer.name
      }
    }
  }

  readFunctionAst(functionAst_: Ast.Function, getMeta: (id: ExprId) => NodeMetadata | undefined) {
    const currentNodeIds = new Set<ExprId>()
    for (const nodeAst of functionAst_.bodyExpressions()) {
      const newNode = nodeFromAst(nodeAst)
      const nodeId = newNode.rootSpan.exprId
      const node = this.nodeIdToNode.get(nodeId)
      const nodeMeta = getMeta(nodeId)
      currentNodeIds.add(nodeId)
      if (node == null) {
        this.nodeIdToNode.set(nodeId, newNode)
      } else {
        if (
          !byteArraysEqual(
            node.pattern?.astExtended?.contentHash(),
            newNode.pattern?.astExtended?.contentHash(),
          )
        ) {
          node.pattern = newNode.pattern
        }
        if (node.outerExprId !== newNode.outerExprId) {
          node.outerExprId = newNode.outerExprId
        }
        if (
          !byteArraysEqual(
            node.rootSpan.astExtended?.contentHash(),
            newNode.rootSpan.astExtended?.contentHash(),
          )
        ) {
          node.rootSpan = newNode.rootSpan
        }
      }
      if (nodeMeta) {
        this.assignUpdatedMetadata(node ?? newNode, nodeMeta)
      }
    }

    for (const nodeId of this.nodeIdToNode.keys()) {
      if (!currentNodeIds.has(nodeId)) {
        this.nodeIdToNode.delete(nodeId)
      }
    }

    const functionAst = functionAst_.astExtended
    if (functionAst?.isTree(RawAst.Tree.Type.Function)) {
      this.bindings.readFunctionAst(functionAst)
    }
    return currentNodeIds
  }

  assignUpdatedMetadata(node: Node, meta: NodeMetadata) {
    const newPosition = new Vec2(meta.x, -meta.y)
    if (!node.position.equals(newPosition)) {
      node.position = newPosition
    }
    if (!visMetadataEquals(node.vis, meta.vis)) {
      node.vis = meta.vis
    }
  }

  static Mock(registry = ComputedValueRegistry.Mock(), db = new SuggestionDb()): GraphDb {
    return new GraphDb(db, ref([]), registry)
  }

  mockNode(binding: string, id: Ast.AstId, code?: string): Node {
    const pattern = Ast.parse(binding)
    const node: Node = {
      outerExprId: id,
      pattern,
      rootSpan: Ast.parse(code ?? '0'),
      position: Vec2.Zero,
      vis: undefined,
    }
    const bindingId = pattern.exprId
    this.nodeIdToNode.set(id, node)
    this.bindings.bindings.set(bindingId, { identifier: binding, usages: new Set() })
    return node
  }

  mockExpressionUpdate(binding: string, update: Partial<ExpressionUpdate>) {
    const nodeId = this.getIdentDefiningNode(binding)
    if (nodeId == null) bail(`The node with identifier '${binding}' was not found.`)
    const update_: ExpressionUpdate = {
      expressionId: nodeId,
      profilingInfo: update.profilingInfo ?? [],
      fromCache: update.fromCache ?? false,
      payload: update.payload ?? { type: 'Value' },
      ...(update.type ? { type: update.type } : {}),
      ...(update.methodCall ? { methodCall: update.methodCall } : {}),
    }
    this.valuesRegistry.processUpdates([update_])
  }
}

export interface Node {
  outerExprId: Ast.AstId
  pattern: Ast.Ast | undefined
  rootSpan: Ast.Ast
  position: Vec2
  vis: Opt<VisualizationMetadata>
}

/** This should only be used for supplying as initial props when testing.
 * Please do {@link GraphDb.mockNode} with a `useGraphStore().db` after mount. */
export function mockNode(exprId?: Ast.AstId): Node {
  return {
    outerExprId: exprId ?? (random.uuidv4() as Ast.AstId),
    pattern: undefined,
    rootSpan: Ast.parse('0'),
    position: Vec2.Zero,
    vis: undefined,
  }
}

function mathodCallEquals(a: MethodCall | undefined, b: MethodCall | undefined): boolean {
  return (
    a === b ||
    (a != null &&
      b != null &&
      methodPointerEquals(a.methodPointer, b.methodPointer) &&
      arrayEquals(a.notAppliedArguments, b.notAppliedArguments))
  )
}
