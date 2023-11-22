import { nonDictatedPlacement } from '@/components/ComponentBrowser/placement'
import { SuggestionDb, groupColorStyle, type Group } from '@/stores/suggestionDatabase'
import type { SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { tryGetIndex } from '@/util/array'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import { colorFromString } from '@/util/colors'
import { ComputedValueRegistry, type ExpressionInfo } from '@/util/computedValueRegistry'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import { getTextWidth } from '@/util/measurement'
import type { Opt } from '@/util/opt'
import { Rect } from '@/util/rect'
import theme from '@/util/theme.json'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import type { MethodCall } from 'shared/languageServerTypes'
import {
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
      if (span instanceof Ast.Ident) {
        idents.push([span.exprId, span.code()])
        return false
      }
      return true
    })
    return idents
  })
  private nodeExpressions = new ReactiveIndex(this.nodes, (id, entry) => {
    const exprs = new Set<ExprId>()
    entry.rootSpan.visitRecursive((expr) => exprs.add(expr.exprId))
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
  nodeMainSuggestion = new ReactiveMapping(this.nodes, (id, _entry) => {
    const expressionInfo = this.getExpressionInfo(id)
    const method = expressionInfo?.methodCall?.methodPointer
    if (method == null) return
    const suggestionId = this.suggestionDb.findByMethodPointer(method)
    return this.suggestionDb.get(suggestionId)
  })
  private nodeColors = new ReactiveMapping(this.nodes, (id, _entry) => {
    const index = this.nodeMainSuggestion.lookup(id)?.groupIndex
    const group = tryGetIndex(this.groups.value, index)
    if (group == null) {
      const typename = this.getExpressionInfo(id)?.typename
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

  isMethodCall(id: ExprId): boolean {
    return this.getExpressionInfo(id)?.methodCall != null
  }

  getMethodCallInfo(
    id: ExprId,
  ): { methodCall: MethodCall; suggestion: SuggestionEntry } | undefined {
    const methodCall = this.getExpressionInfo(id)?.methodCall
    if (methodCall == null) return
    const suggestionId = this.suggestionDb.findByMethodPointer(methodCall.methodPointer)
    const suggestion = this.suggestionDb.get(suggestionId)
    if (suggestion == null) return
    return { methodCall, suggestion }
  }

  getNodeColorStyle(id: ExprId): string {
    return (id && this.nodeColors.lookup(id)) ?? 'var(--node-color-no-type)'
  }

  moveNodeToTop(id: ExprId) {
    this.nodes.moveToLast(id)
  }

  getNodeWidth(node: Node) {
    // FIXME [sb]: This should take into account the width of all widgets.
    // This will require a recursive traversal of the `Node`'s children.
    return getTextWidth(node.rootSpan.code(), '11.5px', '"M PLUS 1", sans-serif') * 1.2
  }

  readFunctionAst(functionAst: Ast.Function, getMeta: (id: ExprId) => NodeMetadata | undefined) {
    const currentNodeIds = new Set<ExprId>()
    const nodeRectMap = new Map<ExprId, Rect>()
    let numberOfUnpositionedNodes = 0
    let maxUnpositionedNodeWidth = 0
    if (functionAst) {
      for (const nodeAst of getFunctionNodeExpressions(functionAst)) {
        const newNode = nodeFromAst(nodeAst)
        const nodeId = newNode.rootSpan.exprId
        const node = this.nodes.get(nodeId)
        const nodeMeta = getMeta(nodeId)
        currentNodeIds.add(nodeId)
        if (node == null) {
          this.nodes.set(nodeId, newNode)
        } else {
          if (node.binding !== newNode.binding) {
            node.binding = newNode.binding
          }
          if (node.outerExprId !== newNode.outerExprId) {
            node.outerExprId = newNode.outerExprId
          }
          // TODO
          //if (indexedDB.cmp(node.rootSpan.contentHash(), newNode.rootSpan.contentHash()) !== 0) {
          node.rootSpan = newNode.rootSpan
          //}
        }
        if (!nodeMeta) {
          numberOfUnpositionedNodes += 1
          maxUnpositionedNodeWidth = Math.max(
            maxUnpositionedNodeWidth,
            this.getNodeWidth(node ?? newNode),
          )
        } else {
          this.assignUpdatedMetadata(node ?? newNode, nodeMeta)
          nodeRectMap.set(
            nodeId,
            Rect.FromBounds(
              nodeMeta.x,
              nodeMeta.y,
              nodeMeta.x + this.getNodeWidth(node ?? newNode),
              nodeMeta.y + theme.node.height,
            ),
          )
        }
      }
    }

    for (const nodeId of this.allNodeIds()) {
      if (!currentNodeIds.has(nodeId)) {
        this.nodes.delete(nodeId)
      }
    }

    const nodeRects = [...nodeRectMap.values()]
    const rectsHeight =
      numberOfUnpositionedNodes * (theme.node.height + theme.node.vertical_gap) -
      theme.node.vertical_gap
    const { position: rectsPosition } = nonDictatedPlacement(
      new Vec2(maxUnpositionedNodeWidth, rectsHeight),
      {
        nodeRects,
        // The rest of the properties should not matter.
        selectedNodeRects: [],
        screenBounds: Rect.Zero,
        mousePosition: Vec2.Zero,
      },
    )
    let nodeIndex = 0
    for (const nodeId of this.allNodeIds()) {
      const meta = getMeta(nodeId)
      if (meta) continue
      const node = this.nodes.get(nodeId)!
      const size = new Vec2(this.getNodeWidth(node), theme.node.height)
      const position = new Vec2(
        rectsPosition.x,
        rectsPosition.y + (theme.node.height + theme.node.vertical_gap) * nodeIndex,
      )
      nodeRects.push(new Rect(position, size))
      node.position = new Vec2(position.x, position.y)

      nodeIndex += 1
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
  rootSpan: Ast.Ast
  position: Vec2
  vis: Opt<VisualizationMetadata>
}

export function mockNode(binding: string, id: AstId, code?: string): Node {
  return {
    outerExprId: id,
    binding,
    rootSpan: Ast.parse(code ?? '0'),
    position: Vec2.Zero,
    vis: undefined,
  }
}

function nodeFromAst(ast: Ast.Ast): Node {
  const common = {
    outerExprId: ast._id,
    position: Vec2.Zero,
    vis: undefined,
  }
  if (ast instanceof Ast.Assignment) {
    return {
      binding: ast.pattern?.code() ?? '',
      rootSpan: ast.expression ?? ast,
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

function getFunctionNodeExpressions(func: Ast.Function): Iterable<Ast.Ast> {
  return Array.from(func.bodyExpressions()).filter((e) => !(e instanceof Ast.Function))
}
