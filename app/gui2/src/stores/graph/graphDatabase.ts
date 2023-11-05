import type { Group, SuggestionDb } from '@/stores/suggestionDatabase'
import { Ast, AstExtended } from '@/util/ast'
import type { ComputedValueRegistry } from '@/util/computedValueRegistry'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import type { Opt } from '@/util/opt'
import { qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import { Vec2 } from '@/util/vec2'
import {
  visMetadataEquals,
  type ExprId,
  type NodeMetadata,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import type { Ref } from 'vue'

export class GraphDb {
  nodes = new ReactiveDb<ExprId, Node>()
  idents = new ReactiveIndex(this.nodes, (_id, entry) => {
    const idents: [ExprId, string][] = []
    entry.rootSpan.visitRecursive((span) => {
      if (span.isTree(Ast.Tree.Type.Ident)) {
        idents.push([span.astId, span.repr()])
        return false
      }
      return true
    })
    console.log('idents', idents)
    return idents
  })
  nodeExpressions = new ReactiveIndex(this.nodes, (id, entry) => {
    const exprs = new Set<ExprId>()
    for (const ast of entry.rootSpan.walkRecursive()) {
      exprs.add(ast.astId)
    }
    return Array.from(exprs, (expr) => [id, expr])
  })
  connections = new ReactiveIndex(this.nodes, (id, entry) => {
    const allConnections: [ExprId, ExprId][] = []
    const usages = this.idents.reverseLookup(entry.binding)
    console.log('entry.binding', entry.binding)
    console.log('usages', usages)
    for (const usage of usages) {
      allConnections.push([id, usage])
    }
    return allConnections
  })
  nodeMainSuggestion = new ReactiveMapping(this.nodes, (id, _entry) => {
    const expressionInfo = this.valuesRegistry.getExpressionInfo(id)
    const method = expressionInfo?.methodCall?.methodPointer
    if (method == null) return
    const moduleName = tryQualifiedName(method.module)
    const methodName = tryQualifiedName(method.name)
    if (!moduleName.ok || !methodName.ok) return
    const qualifiedName = qnJoin(moduleName.value, methodName.value)
    const [suggestionId] = this.suggestionDb.nameToId.lookup(qualifiedName)
    if (suggestionId == null) return
    return this.suggestionDb.get(suggestionId)
  })
  nodeColor = new ReactiveMapping(this.nodes, (id, _entry) => {
    const index = this.nodeMainSuggestion.lookup(id)?.groupIndex
    return index ? this.groups.value[index] : undefined
  })

  getNode(id: ExprId): Node | undefined {
    return this.nodes.get(id)
  }

  edges() {
    return this.connections.forward.values()
  }

  allNodes(): IterableIterator<[ExprId, Node]> {
    return this.nodes.entries()
  }

  allNodeIds(): IterableIterator<ExprId> {
    return this.nodes.keys()
  }

  getNodeColor(id: ExprId): string | undefined {
    return this.nodeColor.lookup(id)?.color
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
}

export interface Node {
  outerExprId: ExprId
  binding: string
  rootSpan: AstExtended<Ast.Tree>
  position: Vec2
  vis: Opt<VisualizationMetadata>
}

function nodeFromAst(ast: AstExtended<Ast.Tree>): Node {
  if (ast.isTree(Ast.Tree.Type.Assignment)) {
    return {
      outerExprId: ast.astId,
      binding: ast.map((t) => t.pattern).repr(),
      rootSpan: ast.map((t) => t.expr),
      position: Vec2.Zero,
      vis: undefined,
    }
  } else {
    return {
      outerExprId: ast.astId,
      binding: '',
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
