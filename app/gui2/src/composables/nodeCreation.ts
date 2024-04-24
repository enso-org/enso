import {
  DEFAULT_NODE_SIZE,
  mouseDictatedPlacement,
  seekHorizontal,
  usePlacement,
} from '@/components/ComponentBrowser/placement'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { useGraphStore, type NodeId } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { Ast } from '@/util/ast'
import { partition } from '@/util/data/array'
import { filterDefined } from '@/util/data/iterable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { assertNever } from 'shared/util/assert'
import { mustExtend } from 'shared/util/types'

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
    mustExtend<GraphAwarePlacement, typeof strategy>()
    return false
  } else {
    mustExtend<GraphIndependentPlacement, typeof strategy>()
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

export function useNodeCreation(
  graphNavigator: GraphNavigator,
  onCreated: (nodes: Set<NodeId>) => void,
) {
  const graphStore = useGraphStore()

  function placeNode(placement: PlacementStrategy, place: (nodes?: Iterable<Rect>) => Vec2): Vec2 {
    return (
      placement.type === 'viewport' ? place()
      : placement.type === 'mouse' ?
        graphNavigator.sceneMousePos ?
          mouseDictatedPlacement(graphNavigator.sceneMousePos)
        : place()
      : placement.type === 'mouseEvent' ? mouseDictatedPlacement(placement.position)
      : placement.type === 'source' ? place(filterDefined([graphStore.visibleArea(placement.node)]))
      : placement.type === 'fixed' ? placement.position
      : assertNever(placement)
    )
  }

  function identity<T>(value: T): T {
    return value
  }

  function createNodes(nodesOptions: Iterable<NodeCreationOptions>) {
    const rects = new Array<Rect>()
    const { place } = usePlacement(rects, graphNavigator.viewport)
    const [independentNodesOptions, dependentNodesOptions] = partition(nodesOptions, (options) =>
      isIndependent(options.placement),
    )
    console.info(`independentNodesOptions: ${JSON.stringify(independentNodesOptions)}`)
    console.info(`dependentNodesOptions: ${JSON.stringify(dependentNodesOptions)}`)
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
    const nodes = graphStore.createNodes(placedOptions)
    if (nodes.length > 0) onCreated(new Set(nodes))
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

  return { createNode, createNodes, placeNode }
}
