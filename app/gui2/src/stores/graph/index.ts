import { usePlacement } from '@/components/ComponentBrowser/placement'
import type { PortId } from '@/providers/portInfo'
import type { WidgetUpdate } from '@/providers/widgetRegistry'
import { GraphDb, type NodeId } from '@/stores/graph/graphDatabase'
import {
  addImports,
  detectImportConflicts,
  filterOutRedundantImports,
  readImports,
  type DetectedConflict,
  type Import,
  type RequiredImport,
} from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { assert, bail } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import { MutableModule, isIdentifier } from '@/util/ast/abstract'
import { RawAst, visitRecursive } from '@/util/ast/raw'
import { partition } from '@/util/data/array'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { map, set } from 'lib0'
import { iteratorFilter } from 'lib0/iterator'
import { defineStore } from 'pinia'
import { SourceDocument } from 'shared/ast/sourceDocument'
import type { ExpressionUpdate, StackItem } from 'shared/languageServerTypes'
import { reachable } from 'shared/util/data/graph'
import type {
  LocalUserActionOrigin,
  Origin,
  SourceRangeKey,
  VisualizationMetadata,
} from 'shared/yjsModel'
import { defaultLocalOrigin, sourceRangeKey, visMetadataEquals } from 'shared/yjsModel'
import type { ShallowRef } from 'vue'
import { computed, markRaw, reactive, ref, shallowReactive, toRef, watch } from 'vue'

const FALLBACK_BINDING_PREFIX = 'node'

export type {
  Node,
  NodeDataFromAst,
  NodeDataFromMetadata,
  NodeId,
} from '@/stores/graph/graphDatabase'

export interface NodeEditInfo {
  id: NodeId
  initialCursorPos: number
}

export class PortViewInstance {
  constructor(
    public rect: ShallowRef<Rect | undefined>,
    public nodeId: NodeId,
    public onUpdate: (update: WidgetUpdate) => void,
  ) {
    markRaw(this)
  }
}

export const useGraphStore = defineStore('graph', () => {
  const proj = useProjectStore()
  const suggestionDb = useSuggestionDbStore()

  proj.setObservedFileName('Main.enso')

  const syncModule = computed(() => proj.module && markRaw(new MutableModule(proj.module.doc.ydoc)))

  const nodeRects = reactive(new Map<NodeId, Rect>())
  const vizRects = reactive(new Map<NodeId, Rect>())
  // The currently visible nodes' areas (including visualization).
  const visibleNodeAreas = computed(() => {
    const existing = iteratorFilter(nodeRects.entries(), ([id]) => db.nodeIdToNode.has(id))
    return Array.from(existing, ([id, rect]) => vizRects.get(id) ?? rect)
  })
  function visibleArea(nodeId: NodeId): Rect | undefined {
    if (!db.nodeIdToNode.has(nodeId)) return
    return vizRects.get(nodeId) ?? nodeRects.get(nodeId)
  }

  const db = new GraphDb(
    suggestionDb.entries,
    toRef(suggestionDb, 'groups'),
    proj.computedValueRegistry,
  )
  const portInstances = shallowReactive(new Map<PortId, Set<PortViewInstance>>())
  const editedNodeInfo = ref<NodeEditInfo>()
  const methodAst = ref<Ast.Function>()

  const unconnectedEdge = ref<UnconnectedEdge>()

  const moduleSource = reactive(SourceDocument.Empty())
  const moduleRoot = ref<Ast.Ast>()
  const topLevel = ref<Ast.BodyBlock>()

  let disconnectSyncModule: undefined | (() => void)
  watch(syncModule, (syncModule) => {
    if (!syncModule) return
    let moduleChanged = true
    disconnectSyncModule?.()
    const handle = syncModule.observe((update) => {
      moduleSource.applyUpdate(syncModule, update)
      handleModuleUpdate(syncModule, moduleChanged, update)
      moduleChanged = false
    })
    disconnectSyncModule = () => {
      syncModule.unobserve(handle)
      moduleSource.clear()
    }
  })

  let toRaw = new Map<SourceRangeKey, RawAst.Tree.Function>()
  function handleModuleUpdate(
    module: Ast.Module,
    moduleChanged: boolean,
    update: Ast.ModuleUpdate,
  ) {
    const root = module.root()
    if (!root) return
    if (moduleRoot.value != root) {
      moduleRoot.value = root
    }
    if (root instanceof Ast.BodyBlock && topLevel.value != root) {
      topLevel.value = root
    }
    // We can cast maps of unknown metadata fields to `NodeMetadata` because all `NodeMetadata` fields are optional.
    const nodeMetadataUpdates = update.metadataUpdated as any as {
      id: AstId
      changes: Ast.NodeMetadata
    }[]
    const dirtyNodeSet = new Set(
      (function* () {
        yield* update.nodesUpdated
        yield* update.nodesAdded
      })(),
    )
    if (moduleChanged || dirtyNodeSet.size !== 0 || update.nodesDeleted.size !== 0) {
      db.updateExternalIds(root)
      toRaw = new Map()
      visitRecursive(Ast.parseEnso(moduleSource.text), (node) => {
        if (node.type === RawAst.Tree.Type.Function) {
          const start = node.whitespaceStartInCodeParsed + node.whitespaceLengthInCodeParsed
          const end = start + node.childrenLengthInCodeParsed
          toRaw.set(sourceRangeKey([start, end]), node)
          return false
        }
        return true
      })
      updateState(dirtyNodeSet)
    }
    if (nodeMetadataUpdates.length !== 0) {
      for (const { id, changes } of nodeMetadataUpdates) db.updateMetadata(id, changes)
    }
  }

  function updateState(dirtyNodes?: Set<AstId>) {
    const module = proj.module
    if (!module) return
    const textContentLocal = moduleSource.text
    if (!textContentLocal) return
    if (!syncModule.value) return
    methodAst.value = methodAstInModule(syncModule.value)
    if (methodAst.value) {
      const methodSpan = moduleSource.getSpan(methodAst.value.id)
      assert(methodSpan != null)
      const rawFunc = toRaw.get(sourceRangeKey(methodSpan))
      assert(rawFunc != null)
      db.readFunctionAst(
        methodAst.value,
        rawFunc,
        textContentLocal,
        (id) => moduleSource.getSpan(id),
        dirtyNodes ?? new Set(),
      )
    }
  }

  function methodAstInModule(mod: Ast.Module) {
    const topLevel = mod.root()
    if (!topLevel) return
    assert(topLevel instanceof Ast.BodyBlock)
    return getExecutedMethodAst(topLevel, proj.executionContext.getStackTop(), db)
  }

  function generateLocallyUniqueIdent(prefix?: string | undefined) {
    // FIXME: This implementation is not robust in the context of a synchronized document,
    // as the same name can likely be assigned by multiple clients.
    // Consider implementing a mechanism to repair the document in case of name clashes.
    for (let i = 1; ; i++) {
      const ident = (prefix ?? FALLBACK_BINDING_PREFIX) + i
      assert(isIdentifier(ident))
      if (!db.identifierUsed(ident)) return ident
    }
  }

  const connectedEdges = computed(() => {
    const disconnectedTargets = new Set<PortId>()
    if (unconnectedEdge.value) {
      const target = unconnectedEdge.value.disconnectedEdgeTarget
      if (target) disconnectedTargets.add(target)
    }
    if (editedNodeInfo.value) {
      const primarySubject = db.nodeIdToNode.get(editedNodeInfo.value.id)?.primarySubject
      if (primarySubject) disconnectedTargets.add(primarySubject)
    }
    const edges = new Array<ConnectedEdge>()
    for (const [target, sources] of db.connections.allReverse()) {
      if (!disconnectedTargets.has(target)) {
        for (const source of sources) {
          edges.push({ source, target })
        }
      }
    }
    return edges
  })

  function createEdgeFromOutput(source: Ast.AstId, event: PointerEvent | undefined) {
    unconnectedEdge.value = { source, target: undefined, event, anchor: { type: 'mouse' } }
  }

  function disconnectSource(edge: Edge, event: PointerEvent | undefined) {
    if (!edge.target) return
    unconnectedEdge.value = {
      source: undefined,
      target: edge.target,
      disconnectedEdgeTarget: edge.target,
      event,
      anchor: { type: 'mouse' },
    }
  }

  function disconnectTarget(edge: Edge, event: PointerEvent | undefined) {
    if (!edge.source || !edge.target) return
    unconnectedEdge.value = {
      source: edge.source,
      target: undefined,
      disconnectedEdgeTarget: edge.target,
      event,
      anchor: { type: 'mouse' },
    }
  }

  function clearUnconnected() {
    unconnectedEdge.value = undefined
  }

  /* Try adding imports. Does nothing if conflict is detected, and returns `DectedConflict` in such case. */
  function addMissingImports(
    edit: MutableModule,
    newImports: RequiredImport[],
  ): DetectedConflict[] | undefined {
    const topLevel = edit.getVersion(moduleRoot.value!)
    if (!(topLevel instanceof Ast.MutableBodyBlock)) {
      console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
      return
    }
    const existingImports = readImports(topLevel)

    const conflicts = []
    const nonConflictingImports = []
    for (const newImport of newImports) {
      const conflictInfo = detectImportConflicts(suggestionDb.entries, existingImports, newImport)
      if (conflictInfo?.detected) {
        conflicts.push(conflictInfo)
      } else {
        nonConflictingImports.push(newImport)
      }
    }
    addMissingImportsDisregardConflicts(edit, nonConflictingImports, existingImports)

    if (conflicts.length > 0) return conflicts
  }

  /* Adds imports, ignores any possible conflicts.
   * `existingImports` are optional and will be used instead of `readImports(topLevel)` if provided. */
  function addMissingImportsDisregardConflicts(
    edit: MutableModule,
    imports: RequiredImport[],
    existingImports?: Import[] | undefined,
  ) {
    if (!imports.length) return
    const topLevel = edit.getVersion(moduleRoot.value!)
    if (!(topLevel instanceof Ast.MutableBodyBlock)) {
      console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
      return
    }
    const existingImports_ = existingImports ?? readImports(topLevel)

    const importsToAdd = filterOutRedundantImports(existingImports_, imports)
    if (!importsToAdd.length) return
    addImports(edit.getVersion(topLevel), importsToAdd)
  }

  function deleteNodes(ids: Iterable<NodeId>) {
    edit(
      (edit) => {
        for (const id of ids) {
          const node = db.nodeIdToNode.get(id)
          if (!node) continue
          const usages = db.getNodeUsages(id)
          for (const usage of usages) updatePortValue(edit, usage, undefined)
          const outerExpr = edit.getVersion(node.outerExpr)
          if (outerExpr) Ast.deleteFromParentBlock(outerExpr)
          nodeRects.delete(id)
        }
      },
      true,
      true,
    )
  }

  function setNodeContent(id: NodeId, content: string, withImports?: RequiredImport[] | undefined) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    edit((edit) => {
      const editExpr = edit.getVersion(node.innerExpr)
      editExpr.syncToCode(content)
      if (withImports) {
        const conflicts = addMissingImports(edit, withImports)
        if (conflicts == null) return
        const wholeAssignment = editExpr.mutableParent()
        if (wholeAssignment == null) {
          console.error('Cannot find parent of the node expression. Conflict resolution failed.')
          return
        }
        for (const _conflict of conflicts) {
          // TODO: Substitution does not work, because we interpret imports wrongly. To be fixed in
          // https://github.com/enso-org/enso/issues/9356
          // substituteQualifiedName(edit, wholeAssignment, conflict.pattern, conflict.fullyQualified)
        }
      }
    })
  }

  function transact(fn: () => void) {
    syncModule.value!.transact(fn)
  }

  const undoManager = {
    undo() {
      proj.module?.undoManager.undo()
    },
    redo() {
      proj.module?.undoManager.redo()
    },
    undoStackBoundary() {
      proj.module?.undoManager.stopCapturing()
    },
  }

  function setNodePosition(nodeId: NodeId, position: Vec2) {
    const nodeAst = syncModule.value?.tryGet(nodeId)
    if (!nodeAst) return
    const oldPos = nodeAst.nodeMetadata.get('position')
    if (oldPos?.x !== position.x || oldPos?.y !== position.y) {
      editNodeMetadata(nodeAst, (metadata) =>
        metadata.set('position', { x: position.x, y: position.y }),
      )
    }
  }

  function overrideNodeColor(nodeId: NodeId, color: string | undefined) {
    const nodeAst = syncModule.value?.tryGet(nodeId)
    if (!nodeAst) return
    editNodeMetadata(nodeAst, (metadata) => {
      metadata.set('colorOverride', color)
    })
  }

  function getNodeColorOverride(node: NodeId) {
    return db.nodeIdToNode.get(node)?.colorOverride ?? undefined
  }

  function normalizeVisMetadata(
    partial: Partial<VisualizationMetadata>,
  ): VisualizationMetadata | undefined {
    const empty: VisualizationMetadata = {
      identifier: null,
      visible: false,
      fullscreen: false,
      width: null,
      height: null,
    }
    const vis: VisualizationMetadata = { ...empty, ...partial }
    if (visMetadataEquals(vis, empty)) return undefined
    else return vis
  }

  function setNodeVisualization(nodeId: NodeId, vis: Partial<VisualizationMetadata>) {
    const nodeAst = syncModule.value?.tryGet(nodeId)
    if (!nodeAst) return
    editNodeMetadata(nodeAst, (metadata) => {
      const data: Partial<VisualizationMetadata> = {
        identifier: vis.identifier ?? metadata.get('visualization')?.identifier ?? null,
        visible: vis.visible ?? metadata.get('visualization')?.visible ?? false,
        fullscreen: vis.fullscreen ?? metadata.get('visualization')?.fullscreen ?? false,
        width: vis.width ?? metadata.get('visualization')?.width ?? null,
        height: vis.height ?? metadata.get('visualization')?.height ?? null,
      }
      metadata.set('visualization', normalizeVisMetadata(data))
    })
  }

  function updateNodeRect(nodeId: NodeId, rect: Rect) {
    nodeRects.set(nodeId, rect)
    if (rect.pos.equals(Vec2.Zero)) {
      nodesToPlace.push(nodeId)
    }
  }

  const nodesToPlace = reactive<NodeId[]>([])
  const { place: placeNode } = usePlacement(visibleNodeAreas, Rect.Zero)

  watch(nodesToPlace, (nodeIds) => {
    if (nodeIds.length === 0) return
    const nodesToProcess = [...nodeIds]
    nodesToPlace.length = 0
    batchEdits(() => {
      for (const nodeId of nodesToProcess) {
        const nodeAst = syncModule.value?.get(nodeId)
        const rect = nodeRects.get(nodeId)
        if (!rect || !nodeAst || nodeAst.nodeMetadata.get('position') != null) continue
        const position = placeNode([], rect.size)
        editNodeMetadata(nodeAst, (metadata) =>
          metadata.set('position', { x: position.x, y: position.y }),
        )
        nodeRects.set(nodeId, new Rect(position, rect.size))
      }
    }, 'local:autoLayout')
  })

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

  function isPortEnabled(id: PortId): boolean {
    return getPortRelativeRect(id) != null
  }

  function getPortNodeId(id: PortId): NodeId | undefined {
    return db.getExpressionNodeId(id as string as Ast.AstId) ?? getPortPrimaryInstance(id)?.nodeId
  }

  /**
   * Emit a value update to a port view under specific ID. Returns `true` if the port view is
   * registered and the update was emitted, or `false` otherwise.
   *
   * NOTE: If this returns `true,` The update handlers called `graph.commitEdit` on their own.
   * Therefore, the passed in `edit` should not be modified afterward, as it is already committed.
   */
  function updatePortValue(edit: MutableModule, id: PortId, value: Ast.Owned | undefined): boolean {
    const update = getPortPrimaryInstance(id)?.onUpdate
    if (!update) return false
    update({ edit, portUpdate: { value, origin: id } })
    return true
  }

  function startEdit(): MutableModule {
    return syncModule.value!.edit()
  }

  /** Apply the given `edit` to the state.
   *
   *  @param skipTreeRepair - If the edit is known not to require any parenthesis insertion, this may be set to `true`
   *  for better performance.
   */
  function commitEdit(
    edit: MutableModule,
    skipTreeRepair?: boolean,
    origin: LocalUserActionOrigin = defaultLocalOrigin,
  ) {
    const root = edit.root()
    if (!(root instanceof Ast.BodyBlock)) {
      console.error(`BUG: Cannot commit edit: No module root block.`)
      return
    }
    if (!skipTreeRepair) Ast.repair(root, edit)
    syncModule.value!.applyEdit(edit, origin)
  }

  /** Edit the AST module.
   *
   *  Optimization options: These are safe to use for metadata-only edits; otherwise, they require extreme caution.
   *
   *  @param skipTreeRepair - If the edit is certain not to produce incorrect or non-canonical syntax, this may be set
   *  to `true` for better performance.
   *  @param direct - Apply all changes directly to the synchronized module; they will be committed even if the callback
   *  exits by throwing an exception.
   */
  function edit<T>(f: (edit: MutableModule) => T, skipTreeRepair?: boolean, direct?: boolean): T {
    const edit = direct ? syncModule.value : syncModule.value?.edit()
    assert(edit != null)
    let result
    edit.transact(() => {
      result = f(edit)
      if (!skipTreeRepair) {
        const root = edit.root()
        assert(root instanceof Ast.BodyBlock)
        Ast.repair(root, edit)
      }
      if (!direct) syncModule.value!.applyEdit(edit)
    })
    return result!
  }

  function batchEdits(f: () => void, origin: Origin = defaultLocalOrigin) {
    assert(syncModule.value != null)
    syncModule.value.transact(f, origin)
  }

  function editNodeMetadata(ast: Ast.Ast, f: (metadata: Ast.MutableNodeMetadata) => void) {
    edit((edit) => f(edit.getVersion(ast).mutableNodeMetadata()), true, true)
  }

  const viewModule = computed(() => syncModule.value!)

  function mockExpressionUpdate(
    locator: string | { binding: string; expr: string },
    update: Partial<ExpressionUpdate>,
  ) {
    const { binding, expr } =
      typeof locator === 'string' ? { binding: locator, expr: undefined } : locator
    const nodeId = db.getIdentDefiningNode(binding)
    if (nodeId == null) bail(`The node with identifier '${binding}' was not found.`)
    let exprId: AstId | undefined
    if (expr) {
      const node = db.nodeIdToNode.get(nodeId)
      node?.innerExpr.visitRecursive((ast) => {
        if (ast instanceof Ast.Ast && ast.code() == expr) {
          exprId = ast.id
        }
      })
    } else {
      exprId = nodeId
    }

    if (exprId == null) {
      const locatorStr =
        typeof locator === 'string' ? locator : `${locator.binding}/${locator.expr}`
      bail(`Cannot find expression located by ${locatorStr}`)
    }

    const update_: ExpressionUpdate = {
      expressionId: db.idToExternal(exprId)!,
      profilingInfo: update.profilingInfo ?? [],
      fromCache: update.fromCache ?? false,
      payload: update.payload ?? { type: 'Value' },
      ...(update.type ? { type: update.type } : {}),
      ...(update.methodCall ? { methodCall: update.methodCall } : {}),
    }
    proj.computedValueRegistry.processUpdates([update_])
  }

  /**
   * Reorders nodes so the `targetNodeId` node is placed after `sourceNodeId`. Does nothing if the
   * relative order is already correct.
   *
   * Additionally, all nodes dependent on the `targetNodeId` that end up being before its new line
   * are also moved after it, keeping their relative order.
   */
  function ensureCorrectNodeOrder(edit: MutableModule, sourceNodeId: NodeId, targetNodeId: NodeId) {
    const sourceExpr = db.nodeIdToNode.get(sourceNodeId)?.outerExpr.id
    const targetExpr = db.nodeIdToNode.get(targetNodeId)?.outerExpr.id
    const body = edit.getVersion(methodAstInModule(edit)!).bodyAsBlock()
    assert(sourceExpr != null)
    assert(targetExpr != null)
    const lines = body.lines
    const sourceIdx = lines.findIndex((line) => line.expression?.node.id === sourceExpr)
    const targetIdx = lines.findIndex((line) => line.expression?.node.id === targetExpr)
    assert(sourceIdx != null)
    assert(targetIdx != null)

    // If source is placed after its new target, the nodes needs to be reordered.
    if (sourceIdx > targetIdx) {
      // Find all transitive dependencies of the moved target node.
      const deps = reachable([targetNodeId], (node) => db.nodeDependents.lookup(node))

      const dependantLines = new Set(
        Array.from(deps, (id) => db.nodeIdToNode.get(id)?.outerExpr.id),
      )
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
          dependantLines.has(line.expression?.node.id),
        )

        // Recombine all lines after splitting, keeping existing dependants below the target.
        lines.splice(targetIdx, 0, ...linesBefore, ...linesAfter)

        return lines
      })
    } else {
      return false
    }
  }

  function isConnectedTarget(portId: PortId): boolean {
    return db.connections.reverseLookup(portId as AstId).size > 0
  }

  return {
    transact,
    db: markRaw(db),
    mockExpressionUpdate,
    editedNodeInfo,
    unconnectedEdge,
    connectedEdges,
    moduleSource,
    nodeRects,
    vizRects,
    visibleNodeAreas,
    visibleArea,
    unregisterNodeRect,
    methodAst,
    generateLocallyUniqueIdent,
    createEdgeFromOutput,
    disconnectSource,
    disconnectTarget,
    clearUnconnected,
    moduleRoot,
    deleteNodes,
    ensureCorrectNodeOrder,
    batchEdits,
    overrideNodeColor,
    getNodeColorOverride,
    setNodeContent,
    setNodePosition,
    setNodeVisualization,
    undoManager,
    topLevel,
    updateNodeRect,
    updateVizRect,
    addPortInstance,
    removePortInstance,
    getPortRelativeRect,
    getPortNodeId,
    isPortEnabled,
    updatePortValue,
    setEditedNode,
    updateState,
    startEdit,
    commitEdit,
    edit,
    viewModule,
    addMissingImports,
    addMissingImportsDisregardConflicts,
    isConnectedTarget,
    currentMethodPointer() {
      const currentMethod = proj.executionContext.getStackTop()
      if (currentMethod.type === 'ExplicitCall') return currentMethod.methodPointer
      return db.getExpressionInfo(currentMethod.expressionId)?.methodCall?.methodPointer
    },
  }
})

interface AnyEdge {
  source: AstId | undefined
  target: PortId | undefined
}

/** An edge, which may be connected or unconnected. */
export type Edge = ConnectedEdge | UnconnectedEdge

export interface ConnectedEdge extends AnyEdge {
  source: AstId
  target: PortId
}

export function isConnected(edge: Edge): edge is ConnectedEdge {
  return edge.source != null && edge.target != null
}

type UnconnectedEdgeAnchor =
  | {
      type: 'mouse'
    }
  | {
      type: 'fixed'
      scenePos: Vec2
    }

interface AnyUnconnectedEdge extends AnyEdge {
  /** If this edge represents an in-progress edit of a connected edge, it is identified by its target expression. */
  disconnectedEdgeTarget?: PortId
  /** A pointer event which caused the unconnected edge */
  event?: PointerEvent | undefined
  /** Identifies what the disconnected end should be attached to. */
  anchor: UnconnectedEdgeAnchor
  /** Unless this is set, the edge will be rendered above nodes. */
  belowNodes?: boolean
  /** CSS value; if provided, overrides any color calculation. */
  color?: string
}
interface UnconnectedSource extends AnyUnconnectedEdge {
  source: undefined
  target: PortId
}
interface UnconnectedTarget extends AnyUnconnectedEdge {
  source: AstId
  target: undefined
  /** If true, the target end should be drawn as with a self-argument arrow. */
  targetIsSelfArgument?: boolean
}
export type UnconnectedEdge = UnconnectedSource | UnconnectedTarget

function getExecutedMethodAst(
  topLevel: Ast.BodyBlock,
  executionStackTop: StackItem,
  db: GraphDb,
): Ast.Function | undefined {
  switch (executionStackTop.type) {
    case 'ExplicitCall': {
      // Assume that the provided AST matches the module in the method pointer. There is no way to
      // actually verify this assumption at this point.
      const ptr = executionStackTop.methodPointer
      return Ast.findModuleMethod(topLevel, ptr.name) ?? undefined
    }
    case 'LocalCall': {
      const exprId = executionStackTop.expressionId
      const info = db.getExpressionInfo(exprId)
      if (!info) return undefined
      const ptr = info.methodCall?.methodPointer
      if (!ptr) return undefined
      return Ast.findModuleMethod(topLevel, ptr.name) ?? undefined
    }
  }
}
