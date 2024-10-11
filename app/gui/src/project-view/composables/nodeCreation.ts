import {
  DEFAULT_NODE_SIZE,
  mouseDictatedPlacement,
  seekHorizontal,
  usePlacement,
} from '@/components/ComponentBrowser/placement'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { type GraphStore, type NodeId } from '@/stores/graph'
import { asNodeId } from '@/stores/graph/graphDatabase'
import type { RequiredImport } from '@/stores/graph/imports'
import type { Typename } from '@/stores/suggestionDatabase/entry'
import { Ast } from '@/util/ast'
import { isIdentifier, substituteIdentifier, type Identifier } from '@/util/ast/abstract'
import { partition } from '@/util/data/array'
import { filterDefined } from '@/util/data/iterable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { qnLastSegment, tryQualifiedName } from '@/util/qualifiedName'
import type { ToValue } from '@/util/reactivity'
import { nextTick, toValue } from 'vue'
import { assert, assertNever } from 'ydoc-shared/util/assert'
import { mustExtend } from 'ydoc-shared/util/types'

export type NodeCreation = ReturnType<typeof useNodeCreation>

type GraphIndependentPlacement =
  | { type: 'fixed'; position: Vec2 }
  | { type: 'mouse' }
  | { type: 'mouseRelative'; posOffset: Vec2 }
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
  binding?: string | undefined
  type?: Typename | undefined
  documentation?: string | undefined
  metadata?: Ast.NodeMetadataFields | undefined
  requiredImports?: RequiredImport[] | undefined
}

/** TODO: Add docs */
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

  function tryMouseRelative(offset: Vec2) {
    const pos = toValue(sceneMousePos)
    return pos ? mouseDictatedPlacement(pos.add(offset)) : undefined
  }

  function placeNode(placement: PlacementStrategy, place: (nodes?: Iterable<Rect>) => Vec2): Vec2 {
    return (
      placement.type === 'viewport' ? place()
      : placement.type === 'mouse' ? tryMouse() ?? place()
      : placement.type === 'mouseRelative' ? tryMouseRelative(placement.posOffset) ?? place()
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
    if (!methodAst.ok) {
      methodAst.error.log(`BUG: Cannot add node: No current function.`)
      return new Set()
    }
    const created = new Set<NodeId>()
    const createdIdentifiers = new Set<Identifier>()
    const identifiersRenameMap = new Map<Identifier, Identifier>()
    graphStore.edit((edit) => {
      const statements = new Array<Ast.Owned>()
      for (const options of placedNodes) {
        const rhs = Ast.parse(options.expression, edit)
        const ident = getIdentifier(rhs, options, createdIdentifiers)
        createdIdentifiers.add(ident)
        const { id, rootExpression } = newAssignmentNode(
          edit,
          ident,
          rhs,
          options,
          identifiersRenameMap,
        )
        statements.push(rootExpression)
        created.add(id)
        assert(options.metadata?.position != null, 'Node should already be placed')
        graphStore.nodeRects.set(id, new Rect(Vec2.FromXY(options.metadata.position), Vec2.Zero))
      }
      insertNodeStatements(edit.getVersion(methodAst.value).bodyAsBlock(), statements)
    })
    graphStore.doAfterUpdate(() => onCreated(created))
  }

  /** We resolve import conflicts and substitute identifiers if needed. */
  function afterCreation(
    edit: Ast.MutableModule,
    assignment: Ast.MutableAssignment,
    ident: Ast.Identifier,
    options: NodeCreationOptions,
    identifiersRenameMap: Map<Ast.Identifier, Ast.Identifier>,
  ) {
    // When nodes are copied, we need to substitute original names with newly assigned.
    if (options.binding) {
      if (isIdentifier(options.binding) && options.binding !== ident)
        identifiersRenameMap.set(options.binding, ident)
    }
    for (const [old, replacement] of identifiersRenameMap.entries()) {
      substituteIdentifier(assignment, old, replacement)
    }

    // Resolve import conflicts.
    const conflicts = graphStore.addMissingImports(edit, options.requiredImports ?? []) ?? []
    for (const _conflict of conflicts) {
      // TODO: Substitution does not work, because we interpret imports wrongly. To be fixed in
      // https://github.com/enso-org/enso/issues/9356
      // substituteQualifiedName(assignment, conflict.pattern, conflict.fullyQualified)
    }
  }

  let delayedNodesToCreate: NodeCreationOptions[] = []

  function scheduleCreateNode(options: NodeCreationOptions) {
    delayedNodesToCreate.push(options)
    // Delay node creation to next tick, batch multiple synchronous createNode calls together
    // to avoid node name collisions.
    if (delayedNodesToCreate.length === 1) {
      nextTick(() => {
        const toCreate = delayedNodesToCreate
        delayedNodesToCreate = []
        createNodes(toCreate)
      })
    }
  }

  function newAssignmentNode(
    edit: Ast.MutableModule,
    ident: Ast.Identifier,
    rhs: Ast.Owned,
    options: NodeCreationOptions,
    identifiersRenameMap: Map<Ast.Identifier, Ast.Identifier>,
  ) {
    rhs.setNodeMetadata(options.metadata ?? {})
    const assignment = Ast.Assignment.new(edit, ident, rhs)
    afterCreation(edit, assignment, ident, options, identifiersRenameMap)
    const id = asNodeId(rhs.externalId)
    const rootExpression =
      options.documentation != null ?
        Ast.Documented.new(options.documentation, assignment)
      : assignment
    return { rootExpression, id }
  }

  function getIdentifier(
    expr: Ast.Ast,
    options: NodeCreationOptions,
    alreadyCreated: Set<Ast.Identifier>,
  ): Ast.Identifier {
    const namePrefix =
      options.binding ? existingNameToPrefix(options.binding)
      : options.type ? typeToPrefix(options.type)
      : inferPrefixFromAst(expr)
    const ident = graphStore.generateLocallyUniqueIdent(namePrefix, alreadyCreated)
    return ident
  }

  return { scheduleCreateNode, createNodes, placeNode }
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

/**
 * Convert Typename into short binding prefix.
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

/**
 * Strip number suffix from binding name, effectively returning a valid prefix.
 * The reverse of graphStore.generateLocallyUniqueIdent
 */
function existingNameToPrefix(name: string): string {
  return name.replace(/\d+$/, '')
}

/**
 * Insert the given statements into the given block, at a location appropriate for new nodes.
 *
 * The location will be after any statements in the block that bind identifiers; if the block ends in an expression
 * statement, the location will be before it so that the value of the block will not be affected.
 */
export function insertNodeStatements(bodyBlock: Ast.MutableBodyBlock, statements: Ast.Owned[]) {
  const lines = bodyBlock.lines
  const index =
    lines[lines.length - 1]?.expression?.node.isBindingStatement !== false ?
      lines.length
    : lines.length - 1
  bodyBlock.insert(index, ...statements)
}
