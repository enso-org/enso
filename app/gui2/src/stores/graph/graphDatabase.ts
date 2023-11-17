import { SuggestionDb, groupColorStyle, type Group } from '@/stores/suggestionDatabase'
import { tryGetIndex } from '@/util/array'
import { AstExtended, RawAst } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import { Assignment, Ast, Block, Function } from '@/util/ast/abstract'
import { colorFromString } from '@/util/colors'
import { ComputedValueRegistry, type ExpressionInfo } from '@/util/computedValueRegistry'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import type { Opt } from '@/util/opt'
import { qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import {
  IdMap,
  visMetadataEquals,
  type ExprId,
  type NodeMetadata,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { ref, type Ref } from 'vue'

export class GraphDb {
  nodes = new ReactiveDb<ExprId, Node>()
  idents = new ReactiveIndex(this.nodes, (_id, entry) => {
    const idents: [ExprId, string][] = []
    entry.rootSpan.visitRecursive((span) => {
      if (span.isTree(RawAst.Tree.Type.Ident)) {
        idents.push([span.astId, span.repr()])
        return false
      }
      return true
    })
    return idents
  })
  private nodeExpressions = new ReactiveIndex(this.nodes, (id, entry) => {
    const exprs = new Set<ExprId>()
    for (const ast of entry.rootSpan.walkRecursive()) {
      exprs.add(ast.astId)
    }
    return Array.from(exprs, (expr) => [id, expr])
  })
  nodeByBinding = new ReactiveIndex(this.nodes, (id, entry) => [[entry.binding, id]])
  connections = new ReactiveIndex(this.nodes, (id, entry) => {
    const usageEntries: [ExprId, ExprId][] = []
    const usages = this.idents.reverseLookup(entry.binding)
    for (const usage of usages) {
      usageEntries.push([id, usage])
    }
    return usageEntries
  })
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

  getIdentDefiningNode(ident: string): ExprId | undefined {
    return set.first(this.nodeByBinding.lookup(ident))
  }

  getExpressionInfo(id: ExprId): ExpressionInfo | undefined {
    return this.valuesRegistry.getExpressionInfo(id)
  }

  getNodeColorStyle(id: ExprId): string {
    return (id && this.nodeColors.lookup(id)) ?? 'var(--node-color-no-type)'
  }

  moveNodeToTop(id: ExprId) {
    this.nodes.moveToLast(id)
  }

  readFunctionAst(
    functionAst: Function,
    getMeta: (id: ExprId) => NodeMetadata | undefined,
  ) {
    const currentNodeIds = new Set<ExprId>()
    if (functionAst) {
      for (const nodeAst of getFunctionNodeExpressions(functionAst)) {
        const newNode = nodeFromAst(nodeAst)
        const nodeId = newNode.rootSpan.astId
        const node = this.nodes.get(nodeId)
        const nodeMeta = getMeta(nodeId)
        currentNodeIds.add(nodeId)
        if (node == null) {
          this.nodes.set(nodeId, newNode)
          if (nodeMeta) this.assignUpdatedMetadata(newNode, nodeMeta)
        } else {
          if (node.binding !== newNode.binding) {
            node.binding = newNode.binding
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

  constructor(
    private suggestionDb: SuggestionDb,
    private groups: Ref<Group[]>,
    private valuesRegistry: ComputedValueRegistry,
  ) {}

  static Mock(registry = ComputedValueRegistry.Mock()): GraphDb {
    return new GraphDb(new SuggestionDb(), ref([]), registry)
  }
}

export interface Node {
  outerExprId: AstId
  binding: string
  rootSpan: Ast
  position: Vec2
  vis: Opt<VisualizationMetadata>
}

export function mockNode(binding: string, id: ExprId, code?: string): Node {
  return {
    outerExprId: id,
    binding,
    rootSpan: AstExtended.parse(code ?? '0', IdMap.Mock()),
    position: Vec2.Zero,
    vis: undefined,
  }
}

function nodeFromAst(ast: Ast): Node {
  const common = {
    outerExprId: ast._id,
    position: Vec2.Zero,
    vis: undefined,
  }
  if (ast instanceof Assignment) {
    return {
      binding: ast.pattern.code(),
      rootSpan: ast.expression,
      ...common,
    }
  } else {
    return {
      binding: '',
      rootSpan: ast,
      ...common,
    }
  }
}

function* getFunctionNodeExpressions(func: Function): Iterable<Ast> {
  return Array.from(func.bodyExpressions(), (e) => (!(e instanceof Function)))
}
