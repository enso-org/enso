import { SuggestionDb, groupColorStyle, type Group } from '@/stores/suggestionDatabase'
import type { SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { byteArraysEqual, tryGetIndex } from '@/util/array'
import { Ast, AstExtended } from '@/util/ast'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { colorFromString } from '@/util/colors'
import { ComputedValueRegistry, type ExpressionInfo } from '@/util/computedValueRegistry'
import { MappedKeyMap, MappedSet } from '@/util/containers'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import type { Opt } from '@/util/opt'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import type { MethodCall } from 'shared/languageServerTypes'
import {
  IdMap,
  visMetadataEquals,
  type ContentRange,
  type ExprId,
  type NodeMetadata,
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

  readFunctionAst(ast: AstExtended<Ast.Tree.Function>) {
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
   * assigned (Ast.Tree.Identifier, not Ast.Token.Identifier).
   */
  private static rangeMappings(
    ast: AstExtended,
    analyzer: AliasAnalyzer,
  ): [MappedKeyMap<ContentRange, AstExtended>, Map<ExprId, ContentRange>] {
    const bindingRangeToTree = new MappedKeyMap<ContentRange, AstExtended>(IdMap.keyForRange)
    const bindingIdToRange = new Map<ExprId, ContentRange>()
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
    for (const ast of entry.pattern.walkRecursive()) {
      exprs.add(ast.astId)
    }
    return Array.from(exprs, (expr) => [id, expr])
  })

  private nodeIdToExprIds = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    const exprs = new Set<ExprId>()
    for (const ast of entry.rootSpan.walkRecursive()) {
      exprs.add(ast.astId)
    }
    return Array.from(exprs, (expr) => [id, expr])
  })

  connections = new ReactiveIndex(this.bindings.bindings, (alias, info) => {
    const srcNode = this.getPatternExpressionNodeId(alias)
    // Display connection starting from existing node.
    //TODO[ao]: When implementing input nodes, they should be taken into account here.
    if (srcNode == null) return []
    function* allTargets(db: GraphDb): Generator<[ExprId, ExprId]> {
      for (const usage of info.usages) {
        const targetNode = db.getExpressionNodeId(usage)
        // Display only connections to existing targets and different than source node
        if (targetNode == null || targetNode === srcNode) continue
        yield [alias, usage]
      }
    }
    return Array.from(allTargets(this))
  })

  /** First output port of the node.
   *
   * When the node will be marked as source node for a new one (i.e. the node will be selected
   * when adding), the resulting connection's source will be the main port.
   */
  nodeMainOutputPort = new ReactiveIndex(this.nodeIdToNode, (id, entry) => {
    if (entry.pattern == null) return []
    for (const ast of entry.pattern.walkRecursive()) {
      if (this.bindings.bindings.has(ast.astId)) return [[id, ast.astId]]
    }
    return []
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

  getNodeMainOutputPortIdentifier(id: ExprId): string | undefined {
    const mainPort = set.first(this.nodeMainOutputPort.lookup(id))
    return mainPort != null ? this.bindings.bindings.get(mainPort)?.identifier : undefined
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

  getIdentifierOfConnection(source: ExprId): string | undefined {
    return this.bindings.bindings.get(source)?.identifier
  }

  identifierUsed(ident: string): boolean {
    return this.bindings.identifierToBindingId.hasKey(ident)
  }

  isMethodCall(id: ExprId): boolean {
    return this.getExpressionInfo(id)?.methodCall != null
  }

  getMethodCallInfo(
    id: ExprId,
  ): { methodCall: MethodCall; suggestion: SuggestionEntry } | undefined {
    const methodCall = this.getExpressionInfo(id)?.methodCall
    if (methodCall == null) return
    const suggestionId = this.suggestionDb.findByMethodPointer(methodCall.methodPointer)
    if (suggestionId == null) return
    const suggestion = this.suggestionDb.get(suggestionId)
    if (suggestion == null) return
    return { methodCall, suggestion }
  }

  getNodeColorStyle(id: ExprId): string {
    return this.nodeColor.lookup(id) ?? 'var(--node-color-no-type)'
  }

  moveNodeToTop(id: ExprId) {
    this.nodeIdToNode.moveToLast(id)
  }

  readFunctionAst(
    functionAst: AstExtended<Ast.Tree.Function>,
    getMeta: (id: ExprId) => NodeMetadata | undefined,
  ) {
    const currentNodeIds = new Set<ExprId>()
    if (functionAst) {
      for (const nodeAst of functionAst.visit(getFunctionNodeExpressions)) {
        const newNode = nodeFromAst(nodeAst)
        const nodeId = newNode.rootSpan.astId
        const node = this.nodeIdToNode.get(nodeId)
        const nodeMeta = getMeta(nodeId)
        currentNodeIds.add(nodeId)
        if (node == null) {
          this.nodeIdToNode.set(nodeId, newNode)
        } else {
          if (!byteArraysEqual(node.pattern?.contentHash(), newNode.pattern?.contentHash())) {
            node.pattern = newNode.pattern
          }
          if (node.outerExprId !== newNode.outerExprId) {
            node.outerExprId = newNode.outerExprId
          }
          if (!byteArraysEqual(node.rootSpan.contentHash(), newNode.rootSpan.contentHash())) {
            node.rootSpan = newNode.rootSpan
          }
        }
        if (nodeMeta) {
          this.assignUpdatedMetadata(node ?? newNode, nodeMeta)
        }
      }
    }

    for (const nodeId of this.nodeIdToNode.keys()) {
      if (!currentNodeIds.has(nodeId)) {
        this.nodeIdToNode.delete(nodeId)
      }
    }

    this.bindings.readFunctionAst(functionAst)
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

  mockNode(binding: string, id: ExprId, code?: string) {
    const node = {
      outerExprId: id,
      pattern: AstExtended.parse(binding, IdMap.Mock()),
      rootSpan: AstExtended.parse(code ?? '0', IdMap.Mock()),
      position: Vec2.Zero,
      vis: undefined,
    }
    const bidingId = node.pattern.astId
    this.nodeIdToNode.set(id, node)
    this.bindings.bindings.set(bidingId, { identifier: binding, usages: new Set() })
  }
}

export interface Node {
  outerExprId: ExprId
  pattern: AstExtended<Ast.Tree> | undefined
  rootSpan: AstExtended<Ast.Tree>
  position: Vec2
  vis: Opt<VisualizationMetadata>
}

function nodeFromAst(ast: AstExtended<Ast.Tree>): Node {
  if (ast.isTree(Ast.Tree.Type.Assignment)) {
    return {
      outerExprId: ast.astId,
      pattern: ast.map((t) => t.pattern),
      rootSpan: ast.map((t) => t.expr),
      position: Vec2.Zero,
      vis: undefined,
    }
  } else {
    return {
      outerExprId: ast.astId,
      pattern: undefined,
      rootSpan: ast,
      position: Vec2.Zero,
      vis: undefined,
    }
  }
}

function* getFunctionNodeExpressions(func: Ast.Tree.Function): Generator<Ast.Tree> {
  if (func.body) {
    if (func.body.type === Ast.Tree.Type.BodyBlock) {
      for (const stmt of func.body.statements) {
        if (stmt.expression && stmt.expression.type !== Ast.Tree.Type.Function) {
          yield stmt.expression
        }
      }
    } else {
      yield func.body
    }
  }
}
