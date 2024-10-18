import { usePlacement } from '@/components/ComponentBrowser/placement'
import { createContextStore } from '@/providers'
import type { PortId } from '@/providers/portInfo'
import type { WidgetUpdate } from '@/providers/widgetRegistry'
import { GraphDb, nodeIdFromOuterExpr, type NodeId } from '@/stores/graph/graphDatabase'
import {
  addImports,
  detectImportConflicts,
  filterOutRedundantImports,
  readImports,
  type DetectedConflict,
  type Import,
  type RequiredImport,
} from '@/stores/graph/imports'
import { useUnconnectedEdges, type UnconnectedEdge } from '@/stores/graph/unconnectedEdges'
import { type ProjectStore } from '@/stores/project'
import { type SuggestionDbStore } from '@/stores/suggestionDatabase'
import { assert, bail } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { AstId, Identifier, MutableModule } from '@/util/ast/abstract'
import { isAstId, isIdentifier } from '@/util/ast/abstract'
import { RawAst, visitRecursive } from '@/util/ast/raw'
import { reactiveModule } from '@/util/ast/reactive'
import { partition } from '@/util/data/array'
import { Rect } from '@/util/data/rect'
import { Err, Ok, mapOk, unwrap, type Result } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import { normalizeQualifiedName, tryQualifiedName } from '@/util/qualifiedName'
import { useWatchContext } from '@/util/reactivity'
import { computedAsync } from '@vueuse/core'
import { map, set } from 'lib0'
import { iteratorFilter } from 'lib0/iterator'
import {
  computed,
  markRaw,
  nextTick,
  proxyRefs,
  reactive,
  ref,
  shallowReactive,
  toRef,
  watch,
  watchEffect,
  type Ref,
  type ShallowRef,
} from 'vue'
import { SourceDocument } from 'ydoc-shared/ast/sourceDocument'
import type {
  ExpressionUpdate,
  Path as LsPath,
  MethodPointer,
} from 'ydoc-shared/languageServerTypes'
import { reachable } from 'ydoc-shared/util/data/graph'
import type {
  LocalUserActionOrigin,
  Origin,
  SourceRangeKey,
  VisualizationMetadata,
} from 'ydoc-shared/yjsModel'
import { defaultLocalOrigin, sourceRangeKey, visMetadataEquals } from 'ydoc-shared/yjsModel'

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

/** TODO: Add docs */
export class PortViewInstance {
  /** TODO: Add docs */
  constructor(
    public rect: ShallowRef<Rect | undefined>,
    public nodeId: NodeId,
    public onUpdate: (update: WidgetUpdate) => void,
  ) {
    markRaw(this)
  }
}

export type GraphStore = ReturnType<typeof useGraphStore>
export const { injectFn: useGraphStore, provideFn: provideGraphStore } = createContextStore(
  'graph',
  (proj: ProjectStore, suggestionDb: SuggestionDbStore) => {
    proj.setObservedFileName('Main.enso')

    const nodeRects = reactive(new Map<NodeId, Rect>())
    const nodeHoverAnimations = reactive(new Map<NodeId, number>())
    const vizRects = reactive(new Map<NodeId, Rect>())
    // The currently visible nodes' areas (including visualization).
    const visibleNodeAreas = computed(() => {
      const existing = iteratorFilter(nodeRects.entries(), ([id]) => db.isNodeId(id))
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
    )
    const portInstances = shallowReactive(new Map<PortId, Set<PortViewInstance>>())
    const editedNodeInfo = ref<NodeEditInfo>()

    const moduleSource = reactive(SourceDocument.Empty())
    const moduleRoot = ref<Ast.BodyBlock>()
    const syncModule = computed(() => moduleRoot.value?.module as Ast.MutableModule | undefined)

    watch(
      () => proj.module,
      (projModule, _, onCleanup) => {
        if (!projModule) return
        const module = reactiveModule(projModule.doc.ydoc, onCleanup)
        const handle = module.observe((update) => {
          const root = module.root()
          if (root instanceof Ast.BodyBlock) {
            moduleRoot.value = root
            moduleSource.applyUpdate(module, update)
            db.updateExternalIds(root)
            // We can cast maps of unknown metadata fields to `NodeMetadata` because all `NodeMetadata` fields are optional.
            const nodeMetadataUpdates = update.metadataUpdated as any as {
              id: AstId
              changes: Ast.NodeMetadata
            }[]
            for (const { id, changes } of nodeMetadataUpdates) db.updateMetadata(id, changes)
          } else {
            moduleRoot.value = undefined
          }
        })
        onCleanup(() => {
          module.unobserve(handle)
          moduleSource.clear()
        })
      },
    )

    const methodAst = computed<Result<Ast.Function>>(() =>
      syncModule.value ? getExecutedMethodAst(syncModule.value) : Err('AST not yet initialized'),
    )

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
      if (!methodAst.value.ok || !moduleSource.text) return
      const method = methodAst.value.value
      const toRaw = new Map<SourceRangeKey, RawAst.Tree.Function>()
      visitRecursive(Ast.rawParseModule(moduleSource.text), (node) => {
        if (node.type === RawAst.Tree.Type.Function) {
          const start = node.whitespaceStartInCodeParsed + node.whitespaceLengthInCodeParsed
          const end = start + node.childrenLengthInCodeParsed
          toRaw.set(sourceRangeKey([start, end]), node)
          return false
        }
        return true
      })
      const methodSpan = moduleSource.getSpan(method.id)
      assert(methodSpan != null)
      const rawFunc = toRaw.get(sourceRangeKey(methodSpan))
      const getSpan = (id: AstId) => moduleSource.getSpan(id)
      db.updateBindings(method, rawFunc, moduleSource.text, getSpan)
    })

    function getExecutedMethodAst(module?: Ast.Module): Result<Ast.Function> {
      const executionStackTop = proj.executionContext.getStackTop()
      switch (executionStackTop.type) {
        case 'ExplicitCall': {
          return getMethodAst(executionStackTop.methodPointer, module)
        }
        case 'LocalCall': {
          const exprId = executionStackTop.expressionId
          const info = db.getExpressionInfo(exprId)
          const ptr = info?.methodCall?.methodPointer
          if (!ptr) return Err("Unknown method pointer of execution stack's top frame")
          return getMethodAst(ptr, module)
        }
      }
    }

    function getMethodAst(ptr: MethodPointer, edit?: Ast.Module): Result<Ast.Function> {
      const topLevel = (edit ?? syncModule.value)?.root()
      if (!topLevel) return Err('Module unavailable')
      assert(topLevel instanceof Ast.BodyBlock)
      const modulePath =
        proj.modulePath ?
          mapOk(proj.modulePath, normalizeQualifiedName)
        : Err('Unknown current module name')
      if (!modulePath?.ok) return modulePath
      const ptrModule = mapOk(tryQualifiedName(ptr.module), normalizeQualifiedName)
      const ptrDefinedOnType = mapOk(tryQualifiedName(ptr.definedOnType), normalizeQualifiedName)
      if (!ptrModule.ok) return ptrModule
      if (!ptrDefinedOnType.ok) return ptrDefinedOnType
      if (ptrModule.value !== modulePath.value)
        return Err('Cannot read method from different module')
      if (ptrModule.value !== ptrDefinedOnType.value)
        return Err('Method pointer is not a module method')
      const method = Ast.findModuleMethod(topLevel, ptr.name)
      if (!method) return Err(`No method with name ${ptr.name} in ${modulePath.value}`)
      return Ok(method)
    }

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
        db.nodeIdToNode.get(editedNodeInfo.value.id)?.primarySubject
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

    /* Try adding imports. Does nothing if conflict is detected, and returns `DectedConflict` in such case. */
    function addMissingImports(
      edit: MutableModule,
      newImports: RequiredImport[],
    ): DetectedConflict[] | undefined {
      if (!moduleRoot.value) {
        console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
        return
      }
      const topLevel = edit.getVersion(moduleRoot.value)
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
      if (!moduleRoot.value) {
        console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
        return
      }
      const topLevel = edit.getVersion(moduleRoot.value)
      const existingImports_ = existingImports ?? readImports(topLevel)

      const importsToAdd = filterOutRedundantImports(existingImports_, imports)
      if (!importsToAdd.length) return
      addImports(edit.getVersion(topLevel), importsToAdd)
    }

    function deleteNodes(ids: Iterable<NodeId>) {
      edit((edit) => {
        const deletedNodes = new Set()
        for (const id of ids) {
          const node = db.nodeIdToNode.get(id)
          if (!node) continue
          if (node.type !== 'component') continue
          const usages = db.getNodeUsages(id)
          for (const usage of usages) {
            const nodeId = getPortPrimaryInstance(usage)?.nodeId
            // Skip ports on already deleted nodes.
            if (nodeId && deletedNodes.has(nodeId)) continue

            updatePortValue(edit, usage, undefined)
          }
          const outerExpr = edit.getVersion(node.outerExpr)
          if (outerExpr) Ast.deleteFromParentBlock(outerExpr)
          nodeRects.delete(id)
          nodeHoverAnimations.delete(id)
          deletedNodes.add(id)
        }
      })
    }

    function setNodeContent(
      id: NodeId,
      content: string,
      withImports?: RequiredImport[] | undefined,
    ) {
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
            // substituteQualifiedName(wholeAssignment, conflict.pattern, conflict.fullyQualified)
          }
        }
      })
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
      const nodeAst = syncModule.value?.tryGet(db.idFromExternal(nodeId))
      if (!nodeAst) return
      const metadata = nodeAst.mutableNodeMetadata()
      const oldPos = metadata.get('position')
      if (oldPos?.x !== position.x || oldPos?.y !== position.y)
        metadata.set('position', { x: position.x, y: position.y })
    }

    function overrideNodeColor(nodeId: NodeId, color: string | undefined) {
      const nodeAst = syncModule.value?.tryGet(db.idFromExternal(nodeId))
      if (!nodeAst) return
      nodeAst.mutableNodeMetadata().set('colorOverride', color)
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
        width: null,
        height: null,
      }
      const vis: VisualizationMetadata = { ...empty, ...partial }
      if (visMetadataEquals(vis, empty)) return undefined
      else return vis
    }

    function setNodeVisualization(nodeId: NodeId, vis: Partial<VisualizationMetadata>) {
      const nodeAst = syncModule.value?.tryGet(db.idFromExternal(nodeId))
      if (!nodeAst) return
      const metadata = nodeAst.mutableNodeMetadata()
      const data: Partial<VisualizationMetadata> = {
        identifier: vis.identifier ?? metadata.get('visualization')?.identifier ?? null,
        visible: vis.visible ?? metadata.get('visualization')?.visible ?? false,
        width: vis.width ?? metadata.get('visualization')?.width ?? null,
        height: vis.height ?? metadata.get('visualization')?.height ?? null,
      }
      metadata.set('visualization', normalizeVisMetadata(data))
    }

    function updateNodeRect(nodeId: NodeId, rect: Rect) {
      nodeRects.set(nodeId, rect)
      if (rect.pos.equals(Vec2.Infinity)) {
        nodesToPlace.push(nodeId)
      }
    }

    function updateNodeHoverAnim(nodeId: NodeId, progress: number) {
      nodeHoverAnimations.set(nodeId, progress)
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
        batchEdits(() => {
          for (const nodeId of nodesToProcess) {
            const nodeType = db.nodeIdToNode.get(nodeId)?.type
            const rect = nodeRects.get(nodeId)
            if (!rect) continue
            const nodeAst = syncModule.value?.get(db.idFromExternal(nodeId))
            if (!nodeAst) continue
            const metadata = nodeAst.mutableNodeMetadata()
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

    /**
     * Emit a value update to a port view under specific ID. Returns `true` if the port view is
     * registered and the update was emitted, or `false` otherwise.
     *
     * NOTE: If this returns `true,` The update handlers called `graph.commitEdit` on their own.
     * Therefore, the passed in `edit` should not be modified afterward, as it is already committed.
     */
    function updatePortValue(
      edit: MutableModule,
      id: PortId,
      value: Ast.Owned | undefined,
    ): boolean {
      const update = getPortPrimaryInstance(id)?.onUpdate
      if (!update) return false
      update({ edit, portUpdate: { value, origin: id } })
      return true
    }

    function startEdit(): MutableModule {
      return syncModule.value!.edit()
    }

    /**
     * Apply the given `edit` to the state.
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

    /**
     * Edit the AST module.
     *
     * Optimization options: These are safe to use for metadata-only edits; otherwise, they require extreme caution.
     *  @param skipTreeRepair - If the edit is certain not to produce incorrect or non-canonical syntax, this may be set
     *  to `true` for better performance.
     */
    function edit<T>(f: (edit: MutableModule) => T, skipTreeRepair?: boolean): T {
      const edit = syncModule.value?.edit()
      assert(edit != null)
      let result
      edit.transact(() => {
        result = f(edit)
        if (!skipTreeRepair) {
          const root = edit.root()
          assert(root instanceof Ast.BodyBlock)
          Ast.repair(root, edit)
        }
        syncModule.value!.applyEdit(edit)
      })
      return result!
    }

    /**
     * Obtain a version of the given `Ast` for direct mutation. The `ast` must exist in the current module.
     *  This can be more efficient than creating and committing an edit, but skips tree-repair and cannot be aborted.
     */
    function getMutable<T extends Ast.Ast>(ast: T): Ast.Mutable<T> {
      return syncModule.value!.getVersion(ast)
    }

    function batchEdits(f: () => void, origin: Origin = defaultLocalOrigin) {
      assert(syncModule.value != null)
      syncModule.value.transact(f, origin)
    }

    const viewModule = computed(() => syncModule.value!)

    // expose testing hook
    ;(window as any)._mockExpressionUpdate = mockExpressionUpdate

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
        exprId = db.idFromExternal(nodeId)
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

    /** Iterate over code lines, return node IDs from `ids` set in the order of code positions. */
    function pickInCodeOrder(ids: Set<NodeId>): NodeId[] {
      assert(syncModule.value != null)
      const func = unwrap(getExecutedMethodAst(syncModule.value))
      const body = func.bodyExpressions()
      const result: NodeId[] = []
      for (const expr of body) {
        const nodeId = nodeIdFromOuterExpr(expr)
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
    function ensureCorrectNodeOrder(
      edit: MutableModule,
      sourceNodeId: NodeId,
      targetNodeId: NodeId,
    ) {
      const sourceExpr = db.nodeIdToNode.get(sourceNodeId)?.outerExpr.id
      const targetExpr = db.nodeIdToNode.get(targetNodeId)?.outerExpr.id
      const body = edit.getVersion(unwrap(getExecutedMethodAst(edit))).bodyAsBlock()
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
      return isAstId(portId) && db.connections.reverseLookup(portId).size > 0
    }

    const modulePath: Ref<LsPath | undefined> = computedAsync(
      async () => {
        const rootId = await proj.projectRootId
        const segments = ['src', 'Main.enso']
        return rootId ? { rootId, segments } : undefined
      },
      undefined,
      { onError: console.error },
    )

    return proxyRefs({
      db: markRaw(db),
      mockExpressionUpdate,
      doAfterUpdate,
      editedNodeInfo,
      moduleSource,
      nodeRects,
      nodeHoverAnimations,
      vizRects,
      visibleNodeAreas,
      visibleArea,
      unregisterNodeRect,
      methodAst,
      getMethodAst,
      generateLocallyUniqueIdent,
      moduleRoot,
      deleteNodes,
      pickInCodeOrder,
      ensureCorrectNodeOrder,
      batchEdits,
      getMutable,
      overrideNodeColor,
      getNodeColorOverride,
      setNodeContent,
      setNodePosition,
      setNodeVisualization,
      undoManager,
      updateNodeRect,
      updateNodeHoverAnim,
      updateVizRect,
      addPortInstance,
      removePortInstance,
      getPortRelativeRect,
      getPortNodeId,
      getSourceNodeId,
      isPortEnabled,
      updatePortValue,
      setEditedNode,
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
      modulePath,
      connectedEdges,
      ...unconnectedEdges,
    })
  },
)

/** An edge, which may be connected or unconnected. */
export type Edge = ConnectedEdge | UnconnectedEdge

export interface ConnectedEdge {
  source: AstId
  target: PortId
}

/** TODO: Add docs */
export function isConnected(edge: Edge): edge is ConnectedEdge {
  return edge.source != null && edge.target != null
}
