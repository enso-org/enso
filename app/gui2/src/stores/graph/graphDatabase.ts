import { SuggestionDb, groupColorStyle, type Group } from '@/stores/suggestionDatabase'
import { tryGetIndex } from '@/util/array'
import { Ast, AstExtended } from '@/util/ast'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { colorFromString } from '@/util/colors'
import { ComputedValueRegistry, type ExpressionInfo } from '@/util/computedValueRegistry'
import { MappedKeyMap, MappedSet } from '@/util/containers'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import type { Opt } from '@/util/opt'
import { qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import { Vec2 } from '@/util/vec2'
import { iteratorFilter, mapIterator } from 'lib0/iterator'
import * as set from 'lib0/set'
import {
  IdMap,
  visMetadataEquals,
  type ContentRange,
  type ExprId,
  type NodeMetadata,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { ref, type Ref } from 'vue'

export interface AliasInfo {
  identifier: AstExtended
  usages: Set<ExprId>
}

export class ConnectionsDb {
  aliases = new ReactiveDb<ExprId, AliasInfo>()
  identifiers = new ReactiveIndex(this.aliases, (alias, info) => [[info.identifier.repr(), alias]])
  connections = new ReactiveIndex(this.aliases, (alias, info) =>
    Array.from(info.usages, (target) => [alias, target]),
  )

  readFunctionAst(ast: AstExtended<Ast.Tree.Function>) {
    const analyzer = new AliasAnalyzer(ast.parsedCode, ast.inner)
    const aliasRangeToTree = new MappedKeyMap<ContentRange, AstExtended | undefined>(
      IdMap.keyForRange,
    )
    const aliasIdToRange = new Map<ExprId, ContentRange>()
    for (const [alias, usages] of analyzer.aliases) {
      aliasRangeToTree.set(alias, undefined)
      for (const usage of usages) aliasRangeToTree.set(usage, undefined)
    }
    ast.visitRecursive((ast) => {
      if (aliasRangeToTree.has(ast.span())) {
        aliasRangeToTree.set(ast.span(), ast)
        aliasIdToRange.set(ast.astId, ast.span())
      }
      return true
    })

    for (const key of this.aliases.keys()) {
      const range = aliasIdToRange.get(key)
      if (range == null || !analyzer.aliases.has(range)) {
        this.aliases.delete(key)
      }
    }

    for (const [alias, usages] of analyzer.aliases) {
      const aliasAst = aliasRangeToTree.get(alias)
      if (aliasAst == null) continue
      const info = this.aliases.getOrAdd(aliasAst.astId, {
        identifier: aliasAst,
        usages: new Set(),
      })
      if (indexedDB.cmp(aliasAst.contentHash(), info.identifier.contentHash())) {
        info.identifier = aliasAst
      }
      for (const usage of info.usages) {
        const range = aliasIdToRange.get(usage)
        if (range == null || !usages.has(range)) info.usages.delete(usage)
      }
      for (const usage of usages) {
        const usageAst = aliasRangeToTree.get(usage)
        if (usageAst != null && !info.usages.has(usageAst.astId)) info.usages.add(usageAst.astId)
      }
    }
  }
}

export class GraphDb {
  nodes = new ReactiveDb<ExprId, Node>()
  private connectionsDb = new ConnectionsDb()

  constructor(
    private suggestionDb: SuggestionDb,
    private groups: Ref<Group[]>,
    private valuesRegistry: ComputedValueRegistry,
  ) {}

  private nodePatternExpressions = new ReactiveIndex(this.nodes, (id, entry) => {
    if (entry.pattern == null) return []
    const exprs = new Set<ExprId>()
    for (const ast of entry.pattern.walkRecursive()) {
      exprs.add(ast.astId)
    }
    return Array.from(exprs, (expr) => [id, expr])
  })

  private nodeExpressions = new ReactiveIndex(this.nodes, (id, entry) => {
    const exprs = new Set<ExprId>()
    for (const ast of entry.rootSpan.walkRecursive()) {
      exprs.add(ast.astId)
    }
    return Array.from(exprs, (expr) => [id, expr])
  })

  nodeByPattern = new ReactiveIndex(this.nodes, (id, entry) => [[entry.pattern?.repr(), id]])

  get connections() {
    return this.connectionsDb.connections
  }

  nodeExpressionInfo = new ReactiveMapping(this.nodes, (id, _entry) =>
    this.valuesRegistry.getExpressionInfo(id),
  )

  nodeMainSuggestion = new ReactiveMapping(this.nodes, (id, _entry) => {
    const expressionInfo = this.nodeExpressionInfo.lookup(id)
    const method = expressionInfo?.methodCall?.methodPointer
    if (method == null) return
    const moduleName = tryQualifiedName(method.definedOnType)
    const methodName = tryQualifiedName(method.name)
    if (!moduleName.ok || !methodName.ok) return
    const qualifiedName = qnJoin(moduleName.value, methodName.value)
    const [suggestionId] = this.suggestionDb.nameToId.lookup(qualifiedName)
    if (suggestionId == null) return
    return this.suggestionDb.get(suggestionId)
  })

  private nodeColors = new ReactiveMapping(this.nodes, (id, _entry) => {
    const index = this.nodeMainSuggestion.lookup(id)?.groupIndex
    const group = tryGetIndex(this.groups.value, index)
    if (group == null) {
      const typename = this.nodeExpressionInfo.lookup(id)?.typename
      return typename ? colorFromString(typename) : 'var(--node-color-no-type)'
    }
    return groupColorStyle(group)
  })

  getNode(id: ExprId): Node | undefined {
    return this.nodes.get(id)
  }

  allNodes(): IterableIterator<[ExprId, Node]> {
    return this.nodes.entries()
  }

  allNodeIds(): IterableIterator<ExprId> {
    return this.nodes.keys()
  }

  getExpressionNodeId(exprId: ExprId | undefined): ExprId | undefined {
    return exprId && set.first(this.nodeExpressions.reverseLookup(exprId))
  }

  getNodeDefiningIdent(ident: string): ExprId | undefined {
    return set.first(this.nodeByPattern.lookup(ident))
  }

  getExpressionInfo(id: ExprId): ExpressionInfo | undefined {
    return this.valuesRegistry.getExpressionInfo(id)
  }

  getIdentifierOfConnection(source: ExprId): AstExtended | undefined {
    return this.connectionsDb.aliases.get(source)?.identifier
  }

  identifierUsed(ident: string): boolean {
    return this.connectionsDb.identifiers.hasKey(ident)
  }

  getNodeColorStyle(id: ExprId): string {
    return (id && this.nodeColors.lookup(id)) ?? 'var(--node-color-no-type)'
  }

  moveNodeToTop(id: ExprId) {
    this.nodes.moveToLast(id)
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
        const node = this.nodes.get(nodeId)
        const nodeMeta = getMeta(nodeId)
        currentNodeIds.add(nodeId)
        if (node == null) {
          this.nodes.set(nodeId, newNode)
          if (nodeMeta) this.assignUpdatedMetadata(newNode, nodeMeta)
        } else {
          if (indexedDB.cmp(node.pattern?.contentHash(), newNode.pattern?.contentHash())) {
            node.pattern = newNode.pattern
          }
          if (node.outerExprId !== newNode.outerExprId) {
            node.outerExprId = newNode.outerExprId
          }
          if (indexedDB.cmp(node.rootSpan.contentHash(), newNode.rootSpan.contentHash()) !== 0) {
            node.rootSpan = newNode.rootSpan
          }
          if (nodeMeta) this.assignUpdatedMetadata(node, nodeMeta)
        }
      }
    }

    for (const nodeId of this.allNodeIds()) {
      if (!currentNodeIds.has(nodeId)) {
        this.nodes.delete(nodeId)
      }
    }
    this.connectionsDb.readFunctionAst(functionAst)
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

  static Mock(registry = ComputedValueRegistry.Mock()): GraphDb {
    return new GraphDb(new SuggestionDb(), ref([]), registry)
  }
}

export interface Node {
  outerExprId: ExprId
  pattern: AstExtended<Ast.Tree> | undefined
  rootSpan: AstExtended<Ast.Tree>
  position: Vec2
  vis: Opt<VisualizationMetadata>
}

export function mockNode(binding: string, id: ExprId, code?: string): Node {
  return {
    outerExprId: id,
    pattern: AstExtended.parse(binding, IdMap.Mock()),
    rootSpan: AstExtended.parse(code ?? '0', IdMap.Mock()),
    position: Vec2.Zero,
    vis: undefined,
  }
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
