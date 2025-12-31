import {
  GraphDb,
  nodeIdFromOuterAst,
  type NodeId,
} from '$/providers/openedProjects/graph/graphDatabase'
import {
  useUnconnectedEdges,
  type UnconnectedEdge,
} from '$/providers/openedProjects/graph/unconnectedEdges'
import { type RequiredImport } from '$/providers/openedProjects/module/imports'
import { type ProjectStore } from '$/providers/openedProjects/project'
import { type ProjectNameStore } from '$/providers/openedProjects/projectNames'
import { type SuggestionDbStore } from '$/providers/openedProjects/suggestionDatabase'
import { type Typename } from '$/providers/openedProjects/suggestionDatabase/entry'
import type { UpdateHandler, UpdateResult } from '$/providers/openedProjects/widgetRegistry'
import { useCallbackRegistry } from '$/utils/data/callbacks'
import { usePlacement } from '@/components/ComponentBrowser/placement'
import type { PortId } from '@/providers/portInfo'
import { assert, assertNever } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { AstId, Identifier, MutableModule } from '@/util/ast/abstract'
import { isAstId, isIdentifier } from '@/util/ast/abstract'
import { partition } from '@/util/data/array'
import { stringUnionToArray, type Events } from '@/util/data/observable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { primitiveEquals } from '@/util/equals'
import type { MethodPointer } from '@/util/methodPointer'
import { proxyRefs, useWatchContext } from '@/util/reactivity'
import * as iter from 'enso-common/src/utilities/data/iter'
import { andThen, Err, Ok, unwrap, type Result } from 'enso-common/src/utilities/data/result'
import { map, set } from 'lib0'
import {
  computed,
  markRaw,
  nextTick,
  onScopeDispose,
  reactive,
  ref,
  shallowReactive,
  toRef,
  watch,
  watchEffect,
  type ComputedRef,
  type Ref,
  type ShallowReactive,
  type ShallowRef,
} from 'vue'
import type { ExpressionUpdate } from 'ydoc-shared/languageServerTypes'
import { reachable } from 'ydoc-shared/util/data/graph'
import type { ExternalId, VisualizationMetadata } from 'ydoc-shared/yjsModel'
import { visMetadataEquals } from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'
import { type ModuleStore } from '../module'

const FALLBACK_BINDING_PREFIX = 'node'

const VIS_METADATA_DEFAULTS: VisualizationMetadata = {
  identifier: null,
  visible: false,
  width: null,
  height: null,
}

export type {
  Node,
  NodeDataFromAst,
  NodeDataFromMetadata,
  NodeId,
} from '$/providers/openedProjects/graph/graphDatabase'

export interface NodeEditInfo {
  id: NodeId
  initialCursorPos: number
}

/**
 * A registered information about connectible port.
 *
 * When some widget wants to be connectible port, it registers itself using `addPortInstance`.
 * This information is then used to display edges and handle connect/disconnect operations.
 */
export class PortViewInstance {
  /** Constructor making the object non-reactive (only the rect and type are reactive field). */
  constructor(
    public rect: ShallowRef<Rect | undefined>,
    public expectedType: Ref<Typename | undefined>,
    public nodeId: NodeId,
    public onUpdate: UpdateHandler,
  ) {
    markRaw(this)
  }
}

function useAssociatedFlag<K extends string>({
  onCleanup,
}: {
  onCleanup?: (cleanup: (key: K) => void) => void
}): {
  add: (key: K) => void
  delete: (key: K) => void
  get: (key: K) => boolean
  set: (key: K, value: boolean) => void
  exists: ComputedRef<boolean>
} {
  const flagged = shallowReactive(new Set<K>())
  onCleanup?.((key) => flagged.delete(key))
  return {
    add: flagged.add.bind(flagged),
    delete: flagged.delete.bind(flagged),
    get: flagged.has.bind(flagged),
    set: (key: K, value: boolean) => {
      if (value) flagged.add(key)
      else flagged.delete(key)
    },
    exists: computed(() => flagged.size !== 0),
  }
}

function useAssociatedValue<K extends string, V>({
  onCleanup,
}: {
  onCleanup?: (cleanup: (key: K) => void) => void
}): ShallowReactive<Map<K, V>> {
  const values = shallowReactive(new Map<K, V>())
  onCleanup?.((key) => values.delete(key))
  return values
}

/**
 * A store containing state of currently displayed graph.
 */
export type GraphStore = ReturnType<typeof createGraphStore>

/** Constructor of {@link GraphStore} */
export function createGraphStore(
  proj: ProjectStore,
  suggestionDb: SuggestionDbStore,
  projectNames: ProjectNameStore,
  module: ModuleStore,
) {
  const { run: cleanup, register: onCleanup } =
    useCallbackRegistry<Parameters<(key: NodeId) => void>>()

  const nodeState = {
    nodeHovered: useAssociatedFlag({ onCleanup }),
    nodeExtended: useAssociatedFlag({ onCleanup }),
    nodeOutputVisible: useAssociatedFlag({ onCleanup }),
    nodeOutputHovered: useAssociatedFlag({ onCleanup }),
    nodeRects: useAssociatedValue<NodeId, Rect>({ onCleanup }),
    vizRects: useAssociatedValue<NodeId, Rect>({ onCleanup }),
    nodeOutputAnimations: useAssociatedValue<NodeId, number>({ onCleanup }),
  } as const
  const { nodeRects, vizRects, nodeOutputAnimations } = nodeState

  const currentMethodPointer = computed((): Result<MethodPointer> => {
    const executionStackTop = proj.executionContext.getStackTop()
    switch (executionStackTop.type) {
      case 'ExplicitCall': {
        return Ok(executionStackTop.methodPointer)
      }
      case 'LocalCall': {
        const exprId = executionStackTop.expressionId
        const info = db.getExpressionInfo(exprId)
        const ptr = info?.methodCall?.methodPointer
        if (!ptr) return Err("Unknown method pointer of execution stack's top frame")
        return Ok(ptr)
      }
      default:
        return assertNever(executionStackTop)
    }
  })

  function getExecutedMethodAst(edit?: Ast.Module): Result<Ast.FunctionDef> {
    return andThen(currentMethodPointer.value, (ptr) => module.getMethodAst(ptr, edit))
  }

  // The currently visible nodes' areas (including visualization).
  const visibleNodeAreas = computed(() => {
    const existing = iter.filter(nodeRects.entries(), ([id]) => db.isNodeId(id))
    return Array.from(existing, ([id, rect]) => vizRects.get(id) ?? rect)
  })
  function visibleArea(nodeId: NodeId): Rect | undefined {
    if (!db.isNodeId(nodeId)) return
    return vizRects.get(nodeId) ?? nodeRects.get(nodeId)
  }

  const db = new GraphDb(
    suggestionDb.entries,
    toRef(suggestionDb, 'groups'),
    proj.computedValueRegistry,
    projectNames,
  )
  const portInstances = shallowReactive(new Map<PortId, Set<PortViewInstance>>())
  const editedNodeInfo = ref<NodeEditInfo>()
  const immediateMethodAst = computed<Result<Ast.FunctionDef>>(() => getExecutedMethodAst())

  // When renaming a function, we temporarily lose track of edited function AST. Ensure that we
  // still resolve it before the refactor code change is received.
  const lastKnownResolvedMethodAstId = ref<AstId>()
  watch(immediateMethodAst, (ast) => {
    if (ast.ok) lastKnownResolvedMethodAstId.value = ast.value.id
    else console.log('immediateMethodAst', ast.error)
  })
  watch(
    () => proj.executionContext.getStackTop(),
    () => {
      lastKnownResolvedMethodAstId.value = undefined
    },
  )

  const fallbackMethodAst = computed(() => {
    const id = lastKnownResolvedMethodAstId.value
    const ast = id != null ? module.ast?.tryGet(id) : undefined
    if (ast instanceof Ast.FunctionDef) return ast
    return undefined
  })

  const methodAst = computed(() => {
    const imm = immediateMethodAst.value
    if (imm.ok) return imm
    const flb = fallbackMethodAst.value
    if (flb) return Ok(flb)
    return imm
  })

  const unobserveModule = module.observe((update) => {
    if (
      module.root &&
      (update.nodesAdded.size != 0 ||
        update.nodesDeleted.size != 0 ||
        update.nodesUpdated.size != 0 ||
        update.updateRoots.size != 0)
    ) {
      db.updateExternalIds(module.root)
    }
    // We can cast maps of unknown metadata fields to `NodeMetadata` because all `NodeMetadata` fields are optional.
    const nodeMetadataUpdates = update.metadataUpdated as any as {
      id: AstId
      changes: Ast.NodeMetadata
    }[]
    for (const { id, changes } of nodeMetadataUpdates) db.updateMetadata(id, changes)
  })
  onScopeDispose(unobserveModule)

  const watchContext = useWatchContext()

  const afterUpdate: (() => void)[] = []

  /** `func` callback will be executed once after next call to `updateNodes`. */
  function doAfterUpdate(func: () => void) {
    afterUpdate.push(func)
  }

  watchEffect(() => {
    if (!methodAst.value.ok) return
    db.updateNodes(methodAst.value.value, watchContext)
    for (const cb of afterUpdate) {
      cb()
    }
    afterUpdate.length = 0
  })

  watchEffect(() => {
    if (methodAst.value.ok && module.source.text)
      db.updateBindings(methodAst.value.value, module.source)
  })

  /**
   * Generate unique identifier from `prefix` and some numeric suffix.
   * @param prefix - of the identifier
   * @param ignore - a list of identifiers to consider as unavailable. Useful when creating multiple identifiers in a batch.
   */
  function generateLocallyUniqueIdent(
    prefix?: string | undefined,
    ignore: Set<Identifier> = new Set(),
  ): Identifier {
    // FIXME: This implementation is not robust in the context of a synchronized document,
    // as the same name can likely be assigned by multiple clients.
    // Consider implementing a mechanism to repair the document in case of name clashes.
    const identPrefix = prefix && isIdentifier(prefix + 1) ? prefix : FALLBACK_BINDING_PREFIX
    for (let i = 1; ; i++) {
      const ident = identPrefix + i
      assert(isIdentifier(ident))
      if (!db.identifierUsed(ident) && !ignore.has(ident)) return ident
    }
  }

  const unconnectedEdges = useUnconnectedEdges()

  const editedNodeDisconnectedTarget = computed(() =>
    editedNodeInfo.value ?
      db.nodeIdToNode.get(editedNodeInfo.value.id)?.primaryApplication.selfArgument
    : undefined,
  )

  const connectedEdges = computed(() => {
    const edges = new Array<ConnectedEdge>()
    for (const [target, sources] of db.connections.allReverse()) {
      if (target === editedNodeDisconnectedTarget.value) continue
      for (const source of sources) {
        const edge = { source, target }
        if (!unconnectedEdges.isDisconnected(edge)) {
          edges.push(edge)
        }
      }
    }
    return edges
  })

  function deleteNodes(ids: Iterable<NodeId>) {
    return module.edit(async (edit) => {
      const deletedNodes = new Set<NodeId>()
      for (const id of ids) {
        const node = db.nodeIdToNode.get(id)
        if (!node) continue
        if (node.type !== 'component') continue
        const usages = db.getNodeUsages(id)
        for (const usage of usages) {
          const nodeId = getPortPrimaryInstance(usage)?.nodeId
          // Skip ports on already deleted nodes.
          if (nodeId && deletedNodes.has(nodeId)) continue

          const result = await updatePortValue(usage, undefined, edit, false)
          if (!result.ok) return result
        }
        const outerAst = edit.getVersion(node.outerAst)
        if (outerAst.isStatement()) Ast.deleteFromParentBlock(outerAst)
        deletedNodes.add(id)
        cleanup(id)
      }
      return Ok()
    })
  }

  function setNodeContent(id: NodeId, content: string, withImports?: RequiredImport[] | undefined) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    module.edit((edit) => {
      const editExpr = edit.getVersion(node.innerExpr)
      editExpr.syncToCode(content)
      if (withImports) {
        const conflicts = module.addMissingImports(edit, withImports)
        if (conflicts == null) return Ok()
        const wholeAssignment = editExpr.mutableParent()
        if (wholeAssignment == null) {
          console.error('Cannot find parent of the node expression. Conflict resolution failed.')
          // We still want to commit change.
          return Ok()
        }
        for (const _conflict of conflicts) {
          // TODO: Substitution does not work, because we interpret imports wrongly. To be fixed in
          // https://github.com/enso-org/enso/issues/9356
          // substituteQualifiedNameByPattern(wholeAssignment, conflict.pattern, conflict.fullyQualified)
        }
      }
      return Ok()
    })
  }

  const undoManagerStatus = reactive({
    canUndo: false,
    canRedo: false,
    update(m: Y.UndoManager) {
      this.canUndo = m.canUndo()
      this.canRedo = m.canRedo()
    },
  })
  watch(
    () => proj.module?.undoManager,
    (m) => {
      if (m) {
        const update = () => undoManagerStatus.update(m)
        const events = stringUnionToArray<keyof Events<Y.UndoManager>>()(
          'stack-item-added',
          'stack-item-popped',
          'stack-cleared',
          'stack-item-updated',
        )
        events.forEach((event) => m.on(event, update))
      }
    },
  )
  const undoManager = proxyRefs({
    undo() {
      proj.module?.undoManager.undo()
    },
    redo() {
      proj.module?.undoManager.redo()
    },
    undoStackBoundary() {
      proj.module?.undoManager.stopCapturing()
    },
    canUndo: computed(() => undoManagerStatus.canUndo),
    canRedo: computed(() => undoManagerStatus.canRedo),
  })

  function setNodePosition(nodeId: NodeId, position: Vec2) {
    const metadata = module.mutableNodeMetadata(db.idFromExternal(nodeId))
    if (!metadata) return
    const oldPos = metadata.get('position')
    if (oldPos?.x !== position.x || oldPos?.y !== position.y)
      metadata.set('position', { x: position.x, y: position.y })
  }

  function overrideNodeColor(nodeId: NodeId, color: string | undefined) {
    const metadata = module.mutableNodeMetadata(db.idFromExternal(nodeId))
    metadata?.set('colorOverride', color)
  }

  function getNodeColorOverride(node: NodeId) {
    return db.nodeIdToNode.get(node)?.colorOverride ?? undefined
  }

  function setNodeVisualization(nodeId: NodeId, update: Partial<VisualizationMetadata>) {
    const metadata = module.mutableNodeMetadata(db.idFromExternal(nodeId))
    if (!metadata) return
    const data = Object.assign({ ...VIS_METADATA_DEFAULTS }, metadata.get('visualization'), update)
    const normalized = visMetadataEquals(data, VIS_METADATA_DEFAULTS) ? undefined : data
    metadata.set('visualization', normalized)
  }

  function updateNodeRect(nodeId: NodeId, rect: Rect) {
    nodeRects.set(nodeId, rect)
    if (rect.pos.equals(Vec2.Infinity)) {
      nodesToPlace.push(nodeId)
    }
  }

  function updateNodeOutputAnim(nodeId: NodeId, progress: number) {
    nodeOutputAnimations.set(nodeId, progress)
  }

  const nodesToPlace = reactive<NodeId[]>([])
  const { place: placeNode, input: placeInputNode } = usePlacement(visibleNodeAreas, Rect.Zero)

  watch(nodesToPlace, (nodeIds) =>
    nextTick(() => {
      if (nodeIds.length === 0) return
      const [inputNodes, nonInputNodes] = partition(
        nodeIds,
        (id) => db.nodeIdToNode.get(id)?.type === 'input',
      )
      const nonInputNodesSortedByLines = pickInCodeOrder(new Set(nonInputNodes))
      const inputNodesSortedByArgIndex = inputNodes.sort((a, b) => {
        const nodeA = db.nodeIdToNode.get(a)
        const nodeB = db.nodeIdToNode.get(b)
        if (!nodeA || !nodeB) return 0
        return (nodeA.argIndex ?? 0) - (nodeB.argIndex ?? 0)
      })
      const nodesToProcess = [...nonInputNodesSortedByLines, ...inputNodesSortedByArgIndex]
      nodesToPlace.length = 0
      module.batchEdits(() => {
        for (const nodeId of nodesToProcess) {
          const nodeType = db.nodeIdToNode.get(nodeId)?.type
          const rect = nodeRects.get(nodeId)
          if (!rect) continue
          const metadata = module.mutableNodeMetadata(db.idFromExternal(nodeId))
          if (!metadata) continue
          if (metadata.get('position') != null) continue
          let position
          if (nodeType === 'input') {
            const allNodes = [...db.nodeIdToNode.entries()]
            const nonInputNodes = allNodes.filter(([_, node]) => node.type !== 'input')
            const nonInputNodeRects = nonInputNodes.map(([id]) => nodeRects.get(id) ?? Rect.Zero)
            position = placeInputNode(nonInputNodeRects, rect.size)
          } else {
            position = placeNode([], rect.size)
          }
          metadata.set('position', { x: position.x, y: position.y })
          nodeRects.set(nodeId, new Rect(position, rect.size))
        }
      }, 'local:autoLayout')
    }),
  )

  function updateVizRect(id: NodeId, rect: Rect | undefined) {
    if (rect) vizRects.set(id, rect)
    else vizRects.delete(id)
  }

  function unregisterNodeRect(id: NodeId) {
    nodeRects.delete(id)
    vizRects.delete(id)
  }

  function addPortInstance(id: PortId, instance: PortViewInstance) {
    map.setIfUndefined(portInstances, id, set.create).add(instance)
  }

  function removePortInstance(id: PortId, instance: PortViewInstance) {
    const instances = portInstances.get(id)
    if (!instances) return
    instances.delete(instance)
    if (instances.size === 0) portInstances.delete(id)
  }

  function setEditedNode(id: NodeId | null, cursorPosition: number | null) {
    if (!id) {
      editedNodeInfo.value = undefined
      return
    }
    if (cursorPosition == null) {
      console.warn('setEditedNode: cursorPosition is null')
      return
    }
    editedNodeInfo.value = { id, initialCursorPos: cursorPosition }
  }

  function getPortPrimaryInstance(id: PortId): PortViewInstance | undefined {
    const instances = portInstances.get(id)
    return instances && set.first(instances)
  }

  /**
   * Get the bounding rectangle of a port view, within the coordinate system of the node it belongs
   * to. If the port is currently not connected or interacted with, `undefined` may be returned.
   */
  function getPortRelativeRect(id: PortId): Rect | undefined {
    return getPortPrimaryInstance(id)?.rect.value
  }

  function getPortExpectedType(id: PortId): Typename | undefined {
    return getPortPrimaryInstance(id)?.expectedType.value
  }

  function isPortEnabled(id: PortId): boolean {
    return getPortRelativeRect(id) != null
  }

  /**
   * Return the node ID that has the given `id` as its pattern or primary port.
   * Technically this is either a component or the input node, as input nodes do not have patterns.
   */
  function getSourceNodeId(id: AstId): NodeId | undefined {
    return db.getPatternExpressionNodeId(id) || getPortPrimaryInstance(id)?.nodeId
  }

  function getPortNodeId(id: PortId): NodeId | undefined {
    return (isAstId(id) && db.getExpressionNodeId(id)) || getPortPrimaryInstance(id)?.nodeId
  }

  function getOutputPortNodeId(id: PortId): NodeId | undefined {
    if (!isAstId(id)) return undefined
    const [nodeId] = db.nodeOutputPorts.reverseLookup(id)
    return nodeId
  }

  /**
   * Emit a value update to a port view under specific ID. Returns Err if the port view is
   * not registered.
   *
   * The properties are analogous to {@link WidgetUpdate fields}.
   */
  function updatePortValue(
    id: PortId,
    value: Ast.Owned<Ast.MutableExpression> | undefined,
    edit?: MutableModule,
    directInteraction: boolean = true,
  ): UpdateResult | Promise<UpdateResult> {
    const update = getPortPrimaryInstance(id)?.onUpdate
    if (!update) return Err('Port not registered')
    return update({
      edit,
      portUpdate: { value, origin: id },
      directInteraction,
    })
  }

  // expose testing hook
  ;(window as any)._mockExpressionUpdate = mockExpressionUpdate

  function mockExpressionUpdate(
    locator: string | { binding: string; expr: string },
    update: Partial<Omit<ExpressionUpdate, 'expressionId'>>,
  ) {
    const { binding, expr } =
      typeof locator === 'string' ? { binding: locator, expr: undefined } : locator
    const nodeId = db.getIdentDefiningNode(binding)
    assert(nodeId != null)
    let expressionId: ExternalId | undefined
    if (expr) {
      const node = db.nodeIdToNode.get(nodeId)
      assert(node != null)
      Ast.visitRecursive(node.innerExpr, (ast) => {
        if (ast.code() === expr) {
          assert(expressionId == null)
          expressionId = ast.externalId
        }
      })
    } else {
      expressionId = nodeId
    }
    assert(expressionId != null)
    const update_: ExpressionUpdate = {
      expressionId,
      profilingInfo: [],
      fromCache: false,
      payload: { type: 'Value' },
      type: [],
      hiddenType: [],
    }
    Object.assign(update_, update)
    proj.computedValueRegistry.processUpdates([update_])
  }

  /** Iterate over code lines, return node IDs from `ids` set in the order of code positions. */
  function pickInCodeOrder(ids: Set<NodeId>): NodeId[] {
    if (ids.size === 0) return []
    const func = unwrap(getExecutedMethodAst())
    const body = func.bodyExpressions()
    const result: NodeId[] = []
    for (const expr of body) {
      const nodeId = nodeIdFromOuterAst(expr)
      if (nodeId && ids.has(nodeId)) result.push(nodeId)
    }
    return result
  }

  /**
   * Reorders nodes so the `targetNodeId` node is placed after `sourceNodeId`. Does nothing if the
   * relative order is already correct.
   *
   * Additionally, all nodes dependent on the `targetNodeId` that end up being before its new line
   * are also moved after it, keeping their relative order.
   */
  function ensureCorrectNodeOrder(edit: MutableModule, sourceNodeId: NodeId, targetNodeId: NodeId) {
    const sourceExpr = db.nodeIdToNode.get(sourceNodeId)?.outerAst.id
    const targetExpr = db.nodeIdToNode.get(targetNodeId)?.outerAst.id
    const body = edit.getVersion(unwrap(getExecutedMethodAst(edit))).bodyAsBlock()
    assert(sourceExpr != null)
    assert(targetExpr != null)
    const lines = body.lines
    const sourceIdx = lines.findIndex((line) => line.statement?.node.id === sourceExpr)
    const targetIdx = lines.findIndex((line) => line.statement?.node.id === targetExpr)
    assert(sourceIdx != null)
    assert(targetIdx != null)

    // If source is placed after its new target, the nodes needs to be reordered.
    if (sourceIdx > targetIdx) {
      // Find all transitive dependencies of the moved target node.
      const deps = reachable([targetNodeId], (node) => db.nodeDependents.lookup(node))

      const dependantLines = new Set(Array.from(deps, (id) => db.nodeIdToNode.get(id)?.outerAst.id))
      // Include the new target itself in the set of lines that must be placed after source node.
      dependantLines.add(targetExpr)

      // Check if the source depends on target. If that's the case, the edge we are trying to make
      // creates a circular dependency. Reordering doesn't make any sense in that case.
      if (dependantLines.has(sourceExpr)) {
        return 'circular'
      }

      body.updateLines((lines) => {
        // Pick subset of lines to reorder, i.e. lines between and including target and source.
        const linesToSort = lines.splice(targetIdx, sourceIdx - targetIdx + 1)

        // Split those lines into two buckets, whether or not they depend on the target.
        const [linesAfter, linesBefore] = partition(linesToSort, (line) =>
          dependantLines.has(line.statement?.node.id),
        )

        // Recombine all lines after splitting, keeping existing dependants below the target.
        lines.splice(targetIdx, 0, ...linesBefore, ...linesAfter)

        return lines
      })
    } else {
      return false
    }
  }

  function isConnectedSource(portId: AstId): boolean {
    return (
      db.connections.lookup(portId).size > 0 ||
      unconnectedEdges.cbEditedEdge.value?.source === portId ||
      unconnectedEdges.mouseEditedEdge.value?.source === portId
    )
  }

  function isConnectedTarget(portId: PortId): boolean {
    return isAstId(portId) && db.connections.reverseLookup(portId).size > 0
  }

  function isTargetBeingDraggedAwayFrom(portId: PortId): boolean {
    const edge = unconnectedEdges.mouseEditedEdge.value
    return (
      edge?.createdFrom === 'edge' &&
      edge?.target !== portId &&
      edge?.disconnectedEdgeTarget === portId
    )
  }

  function nodeCanBeEntered(id: NodeId): boolean {
    const methodCall = db.getExpressionInfo(id)?.methodCall
    if (!methodCall || !proj.moduleProjectPath?.ok) return false
    if (!methodCall.methodPointer.definedOnType.equals(proj.moduleProjectPath.value)) {
      // Cannot enter node that is not defined on current module.
      // TODO: Support entering nodes in other modules within the same project.
      return false
    }
    return true
  }

  function onBeforeEdit(f: (transaction: Y.Transaction) => void): { unregister: () => void } {
    proj.module?.doc.ydoc.on('beforeTransaction', f)
    return { unregister: () => proj.module?.doc.ydoc.off('beforeTransaction', f) }
  }

  return proxyRefs({
    db: markRaw(db),
    mockExpressionUpdate,
    doAfterUpdate,
    editedNodeInfo,
    visibleNodeAreas,
    visibleArea,
    unregisterNodeRect,
    generateLocallyUniqueIdent,
    deleteNodes,
    pickInCodeOrder,
    ensureCorrectNodeOrder,
    overrideNodeColor,
    getNodeColorOverride,
    setNodeContent,
    setNodePosition,
    setNodeVisualization,
    undoManager,
    updateNodeRect,
    updateNodeOutputAnim,
    updateVizRect,
    addPortInstance,
    removePortInstance,
    getPortRelativeRect,
    getPortExpectedType,
    getPortNodeId,
    getOutputPortNodeId,
    getSourceNodeId,
    isPortEnabled,
    updatePortValue,
    setEditedNode,
    onBeforeEdit,
    isConnectedSource,
    isConnectedTarget,
    isTargetBeingDraggedAwayFrom,
    nodeCanBeEntered,
    connectedEdges,
    currentMethod: proxyRefs({
      ast: methodAst,
      pointer: currentMethodPointer,
    }),
    ...unconnectedEdges,
    ...nodeState,
  })
}

/** An edge, which may be connected or unconnected. */
export type Edge = ConnectedEdge | UnconnectedEdge

export interface ConnectedEdge {
  source: AstId
  target: PortId
}

/** Equality function for {@link ConnectedEdge}. */
export function connectedEdgeEquals(a: ConnectedEdge, b: ConnectedEdge) {
  return primitiveEquals(a.source, b.source) && primitiveEquals(a.target, b.target)
}

/** Check if edge is connected at both ends. */
export function isConnected(edge: Edge): edge is ConnectedEdge {
  return edge.source != null && edge.target != null
}
