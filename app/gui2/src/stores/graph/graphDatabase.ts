import { nonDictatedPlacement } from '@/components/ComponentBrowser/placement'
import { SuggestionDb, groupColorStyle, type Group } from '@/stores/suggestionDatabase'
import { tryGetIndex } from '@/util/array'
import { Ast, AstExtended } from '@/util/ast'
import { AliasAnalyzer } from '@/util/ast/aliasAnalysis'
import { colorFromString } from '@/util/colors'
import { ComputedValueRegistry, type ExpressionInfo } from '@/util/computedValueRegistry'
import { MappedKeyMap, MappedSet } from '@/util/containers'
import { ReactiveDb, ReactiveIndex, ReactiveMapping } from '@/util/database/reactiveDb'
import { getTextWidth } from '@/util/measurement'
import type { Opt } from '@/util/opt'
import { qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import { Rect } from '@/util/rect'
import theme from '@/util/theme.json'
import { Vec2 } from '@/util/vec2'
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

export interface BindingInfo {
  identifier: string
  usages: Set<ExprId>
}

export class BindingsDb {
  bindings = new ReactiveDb<ExprId, BindingInfo>()
  identifiers = new ReactiveIndex(this.bindings, (id, info) => [[info.identifier, id]])

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
  nodes = new ReactiveDb<ExprId, Node>()
  private bindings = new BindingsDb()

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

  nodeMainOutputPort = new ReactiveIndex(this.nodes, (id, entry) => {
    if (entry.pattern == null) return []
    for (const ast of entry.pattern.walkRecursive()) {
      if (this.bindings.bindings.has(ast.astId)) return [[id, ast.astId]]
    }
    return []
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

  getNodeMainOutputPortIdentifier(id: ExprId): string | undefined {
    const mainPort = set.first(this.nodeMainOutputPort.lookup(id))
    return mainPort != null ? this.bindings.bindings.get(mainPort)?.identifier : undefined
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

  getPatternExpressionNodeId(exprId: ExprId | undefined): ExprId | undefined {
    return exprId && set.first(this.nodePatternExpressions.reverseLookup(exprId))
  }

  getIdentDefiningNode(ident: string): ExprId | undefined {
    const binding = set.first(this.bindings.identifiers.lookup(ident))
    return this.getPatternExpressionNodeId(binding)
  }

  getExpressionInfo(id: ExprId): ExpressionInfo | undefined {
    return this.valuesRegistry.getExpressionInfo(id)
  }

  getIdentifierOfConnection(source: ExprId): string | undefined {
    return this.bindings.bindings.get(source)?.identifier
  }

  identifierUsed(ident: string): boolean {
    return this.bindings.identifiers.hasKey(ident)
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
    return getTextWidth(node.rootSpan.repr(), '11.5px', '"M PLUS 1", sans-serif') * 1.2
  }

  readFunctionAst(
    functionAst: AstExtended<Ast.Tree.Function>,
    getMeta: (id: ExprId) => NodeMetadata | undefined,
  ) {
    const currentNodeIds = new Set<ExprId>()
    const nodeRectMap = new Map<ExprId, Rect>()
    let numberOfUnpositionedNodes = 0
    let maxUnpositionedNodeWidth = 0
    if (functionAst) {
      for (const nodeAst of functionAst.visit(getFunctionNodeExpressions)) {
        const newNode = nodeFromAst(nodeAst)
        const nodeId = newNode.rootSpan.astId
        const node = this.nodes.get(nodeId)
        const nodeMeta = getMeta(nodeId)
        currentNodeIds.add(nodeId)
        if (node == null) {
          this.nodes.set(nodeId, newNode)
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
    this.bindings.readFunctionAst(functionAst)

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

  static Mock(registry = ComputedValueRegistry.Mock()): GraphDb {
    return new GraphDb(new SuggestionDb(), ref([]), registry)
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
    this.nodes.set(id, node)
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
