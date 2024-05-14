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
import { Ast } from '@/util/ast'
import { partition } from '@/util/data/array'
import { filterDefined } from '@/util/data/iterable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { assertNever } from 'shared/util/assert'
import { mustExtend } from 'shared/util/types'
import { toValue, type ComputedRef, type MaybeRefOrGetter } from 'vue'

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
  documentation?: string | undefined
  metadata?: Ast.NodeMetadataFields | undefined
  requiredImports?: RequiredImport[] | undefined
}

type ToValue<T> = MaybeRefOrGetter<T> | ComputedRef<T>

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

  function placeNodes(nodesOptions: Iterable<NodeCreationOptions>) {
    const rects = new Array<Rect>()
    const { place } = usePlacement(rects, viewport)
    const [independentNodesOptions, dependentNodesOptions] = partition(nodesOptions, (options) =>
      isIndependent(options.placement),
    )
    const doPlace =
      (adjust: (pos: Vec2) => Vec2 = identity) =>
      ({
        placement,
        expression,
        documentation,
        metadata,
        requiredImports,
      }: NodeCreationOptions) => {
        const position = adjust(placeNode(placement, place)).xy()
        rects.push(new Rect(Vec2.FromXY(position), Vec2.Zero))
        return {
          metadata: { ...metadata, position },
          expression,
          documentation,
          withImports: requiredImports ?? [],
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
        const { rootExpression, id } = newAssignmentNode(
          edit,
          graphStore.generateUniqueIdent(),
          options.expression,
          options.metadata,
          options.withImports,
          options.documentation,
        )
        bodyBlock.push(rootExpression)
        created.add(id)
        graphStore.nodeRects.set(id, new Rect(Vec2.FromXY(options.metadata.position), Vec2.Zero))
      }
    })
    onCreated(created)
  }

  function createNode(
    placement: PlacementStrategy,
    expression: string,
    documentation?: string | undefined,
    metadata?: Ast.NodeMetadataFields | undefined,
    requiredImports?: RequiredImport[] | undefined,
  ) {
    createNodes([{ placement, expression, documentation, metadata, requiredImports }])
  }

  function newAssignmentNode(
    edit: Ast.MutableModule,
    ident: Ast.Identifier,
    expression: string,
    metadata: Ast.NodeMetadataFields,
    withImports: RequiredImport[],
    documentation: string | undefined,
  ) {
    const conflicts = graphStore.addMissingImports(edit, withImports) ?? []
    const rhs = Ast.parse(expression, edit)
    rhs.setNodeMetadata(metadata)
    const assignment = Ast.Assignment.new(edit, ident, rhs)
    for (const _conflict of conflicts) {
      // TODO: Substitution does not work, because we interpret imports wrongly. To be fixed in
      // https://github.com/enso-org/enso/issues/9356
      // substituteQualifiedName(edit, assignment, conflict.pattern, conflict.fullyQualified)
    }
    const id = asNodeId(rhs.id)
    const rootExpression =
      documentation != null ? Ast.Documented.new(documentation, assignment) : assignment
    return { rootExpression, id }
  }

  return { createNode, createNodes, placeNode }
}
