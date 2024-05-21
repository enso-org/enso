import {
  DEFAULT_NODE_SIZE,
  mouseDictatedPlacement,
  seekHorizontal,
  usePlacement,
} from '@/components/ComponentBrowser/placement'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { useGraphStore, type GraphStore, type NodeId } from '@/stores/graph'
import { asNodeId } from '@/stores/graph/graphDatabase'
import type { RequiredImport } from '@/stores/graph/imports'
import type { Typename } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { partition } from '@/util/data/array'
import { filterDefined } from '@/util/data/iterable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import { assert, assertNever } from 'shared/util/assert'
import { mustExtend } from 'shared/util/types'
import { toValue } from 'vue'

export type NodeCreation = ReturnType<typeof useNodeCreation>

type GraphIndependentPlacement =
  | { type: 'fixed'; position: Vec2 }
  | { type: 'mouse' }
  | { type: 'mouseEvent'; position: Vec2 }
type GraphAwarePlacement = { type: 'viewport' } | { type: 'source'; node: NodeId }
export type PlacementStrategy = GraphIndependentPlacement | GraphAwarePlacement

function isIndependent(
  strategy: GraphIndependentPlacement | GraphAwarePlacement,
): strategy is GraphIndependentPlacement {
  if (strategy.type === 'viewport' || strategy.type === 'source') {
    mustExtend<typeof strategy, GraphAwarePlacement>()
    return false
  } else {
    mustExtend<typeof strategy, GraphIndependentPlacement>()
    return true
  }
}

export interface NodeCreationOptions<Placement extends PlacementStrategy = PlacementStrategy> {
  placement: Placement
  expression: string
  type?: Typename | undefined
  documentation?: string | undefined
  metadata?: Ast.NodeMetadataFields | undefined
  requiredImports?: RequiredImport[] | undefined
}

export function useNodeCreation(
  graphStore: GraphStore,
  viewport: ToValue<GraphNavigator['viewport']>,
  sceneMousePos: ToValue<GraphNavigator['sceneMousePos']>,
  onCreated: (nodes: Set<NodeId>) => void,
) {
  function tryMouse() {
    const pos = toValue(sceneMousePos)
    return pos ? mouseDictatedPlacement(pos) : undefined
  }

  function placeNode(placement: PlacementStrategy, place: (nodes?: Iterable<Rect>) => Vec2): Vec2 {
    return (
      placement.type === 'viewport' ? place()
      : placement.type === 'mouse' ? tryMouse() ?? place()
      : placement.type === 'mouseEvent' ? mouseDictatedPlacement(placement.position)
      : placement.type === 'source' ? place(filterDefined([graphStore.visibleArea(placement.node)]))
      : placement.type === 'fixed' ? placement.position
      : assertNever(placement)
    )
  }

  function identity<T>(value: T): T {
    return value
  }

  function placeNodes(nodesOptions: Iterable<NodeCreationOptions>): NodeCreationOptions[] {
    const rects = new Array<Rect>()
    const { place } = usePlacement(rects, viewport)
    const [independentNodesOptions, dependentNodesOptions] = partition(nodesOptions, (options) =>
      isIndependent(options.placement),
    )
    const doPlace =
      (adjust: (pos: Vec2) => Vec2 = identity) =>
      (options: NodeCreationOptions) => {
        const position = adjust(placeNode(options.placement, place)).xy()
        rects.push(new Rect(Vec2.FromXY(position), Vec2.Zero))
        return {
          ...options,
          metadata: { ...options.metadata, position },
        }
      }
    const placedOptions = []
    // Graph-independent placement strategies normally specify an exact position for the node, regardless of other
    // nodes. However, when creating multiple nodes at once, the newly-created nodes should never overlap with each
    // other; so, after determining the intended position of each graph-independent placement its position is adjusted
    // if necessary, considering only the other uncommitted nodes already placed in the same batch.
    const adjust = (pos: Vec2) => seekHorizontal(new Rect(pos, DEFAULT_NODE_SIZE), rects)
    placedOptions.push(...Array.from(independentNodesOptions, doPlace(adjust)))
    rects.push(...graphStore.visibleNodeAreas)
    placedOptions.push(...Array.from(dependentNodesOptions, doPlace()))
    return placedOptions
  }

  function createNodes(nodesOptions: Iterable<NodeCreationOptions>) {
    const placedNodes = placeNodes(nodesOptions)
    if (placedNodes.length === 0) return new Set()
    const methodAst = graphStore.methodAst
    if (!methodAst) {
      console.error(`BUG: Cannot add node: No current function.`)
      return new Set()
    }
    const created = new Set<NodeId>()
    graphStore.edit((edit) => {
      const bodyBlock = edit.getVersion(methodAst).bodyAsBlock()
      for (const options of placedNodes) {
        const { rootExpression, id } = newAssignmentNode(edit, options)
        bodyBlock.push(rootExpression)
        created.add(id)
        assert(options.metadata?.position != null, 'Node should already be placed')
        graphStore.nodeRects.set(id, new Rect(Vec2.FromXY(options.metadata.position), Vec2.Zero))
      }
    })
    onCreated(created)
  }

  function createNode(options: NodeCreationOptions) {
    createNodes([options])
  }

  function newAssignmentNode(edit: Ast.MutableModule, options: NodeCreationOptions) {
    const conflicts = graphStore.addMissingImports(edit, options.requiredImports ?? []) ?? []
    const rhs = Ast.parse(options.expression, edit)
    const inferredPrefix = inferPrefixFromAst(rhs)
    const namePrefix = options.type ? typeToPrefix(options.type) : inferredPrefix
    const ident = graphStore.generateLocallyUniqueIdent(namePrefix)
    rhs.setNodeMetadata(options.metadata ?? {})
    const assignment = Ast.Assignment.new(edit, ident, rhs)
    for (const _conflict of conflicts) {
      // TODO: Substitution does not work, because we interpret imports wrongly. To be fixed in
      // https://github.com/enso-org/enso/issues/9356
      // substituteQualifiedName(edit, assignment, conflict.pattern, conflict.fullyQualified)
    }
    const id = asNodeId(rhs.id)
    const rootExpression =
      options.documentation != null ?
        Ast.Documented.new(options.documentation, assignment)
      : assignment
    return { rootExpression, id, inferredType: inferredPrefix }
  }

  return { createNode, createNodes, placeNode }
}

const operatorCodeToName: Record<string, string> = {
  '+': 'sum',
  '-': 'diff',
  '*': 'prod',
  '/': 'quot',
}

/** Try to infer binding name from AST. This is used when type information from the engine is not available yet. */
function inferPrefixFromAst(expr: Ast.Ast): string | undefined {
  if (expr instanceof Ast.Vector) return 'vector'
  if (expr instanceof Ast.NumericLiteral) return expr.code().includes('.') ? 'float' : 'integer'
  if (expr instanceof Ast.TextLiteral) return 'text'
  if (expr instanceof Ast.OprApp && expr.operator.ok) {
    return operatorCodeToName[expr.operator.value.code()]
  }
  return undefined
}

/** Convert Typename into short binding prefix.
 * In general, we want to use the last segment of the qualified name.
 * In case of generic types, we want to discard any type parameters.
 */
function typeToPrefix(type: Typename): string {
  const [firstPart] = type.split(' ') // Discard type parameters, if any.
  const fqn = tryQualifiedName(firstPart ?? type)
  if (fqn.ok) {
    return qnLastSegment(fqn.value).toLowerCase()
  } else {
    return type.toLowerCase()
  }
}
