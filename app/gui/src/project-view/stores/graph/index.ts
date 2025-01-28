import { usePlacement } from '@/components/ComponentBrowser/placement'
import { createContextStore } from '@/providers'
import type { PortId } from '@/providers/portInfo'
import type { WidgetUpdate } from '@/providers/widgetRegistry'
import { GraphDb, nodeIdFromOuterAst, type NodeId } from '@/stores/graph/graphDatabase'
import {
  addImports,
  analyzeImports,
  detectImportConflicts,
  filterOutRedundantImports,
  type AbstractImport,
  type DetectedConflict,
  type RequiredImport,
} from '@/stores/graph/imports'
import { useUnconnectedEdges, type UnconnectedEdge } from '@/stores/graph/unconnectedEdges'
import { type ProjectStore } from '@/stores/project'
import { type ProjectNameStore } from '@/stores/projectNames'
import { type SuggestionDbStore } from '@/stores/suggestionDatabase'
import { assert, assertDefined, assertNever, bail } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { AstId, Identifier, MutableModule } from '@/util/ast/abstract'
import { isAstId, isIdentifier } from '@/util/ast/abstract'
import { reactiveModule } from '@/util/ast/reactive'
import { partition } from '@/util/data/array'
import { stringUnionToArray, type Events } from '@/util/data/observable'
import { Rect } from '@/util/data/rect'
import { andThen, Err, Ok, unwrap, type Result } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import { type MethodPointer } from '@/util/methodPointer'
import { useWatchContext } from '@/util/reactivity'
import { computedAsync } from '@vueuse/core'
import * as iter from 'enso-common/src/utilities/data/iter'
import { map, set } from 'lib0'
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
import type { ExpressionUpdate, Path as LsPath } from 'ydoc-shared/languageServerTypes'
import { reachable } from 'ydoc-shared/util/data/graph'
import type { LocalUserActionOrigin, Origin, VisualizationMetadata } from 'ydoc-shared/yjsModel'
import { defaultLocalOrigin, visMetadataEquals } from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'

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
export const [provideGraphStore, useGraphStore] = createContextStore(
  'graph',
  (proj: ProjectStore, suggestionDb: SuggestionDbStore, projectNames: ProjectNameStore) => {
    proj.setObservedFileName('Main.enso')

    const nodeRects = reactive(new Map<NodeId, Rect>())
    const nodeHoverAnimations = reactive(new Map<NodeId, number>())
    const vizRects = reactive(new Map<NodeId, Rect>())
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

    const moduleSource = SourceDocument.Empty(reactive)
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

    const immediateMethodAst = computed<Result<Ast.FunctionDef>>(() =>
      syncModule.value ? getExecutedMethodAst(syncModule.value) : Err('AST not yet initialized'),
    )

    // When renaming a function, we temporarily lose track of edited function AST. Ensure that we
    // still resolve it before the refactor code change is received.
    const lastKnownResolvedMethodAstId = ref<AstId>()
    watch(immediateMethodAst, (ast) => {
      if (ast.ok) lastKnownResolvedMethodAstId.value = ast.value.id
      else console.log('immediateMethodAst', ast.error)
    })

    const fallbackMethodAst = computed(() => {
      const id = lastKnownResolvedMethodAstId.value
      const ast = id != null ? syncModule.value?.get(id) : undefined
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
      if (methodAst.value.ok && moduleSource.text)
        db.updateBindings(methodAst.value.value, moduleSource)
    })

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

    function getExecutedMethodAst(module?: Ast.Module): Result<Ast.FunctionDef> {
      return andThen(currentMethodPointer.value, (ptr) => getMethodAst(ptr, module))
    }

    function getMethodAst(ptr: MethodPointer, edit?: Ast.Module): Result<Ast.FunctionDef> {
      const topLevel = (edit ?? syncModule.value)?.root()
      if (!topLevel) return Err('Module unavailable')
      assert(topLevel instanceof Ast.BodyBlock)
      if (!proj.moduleProjectPath?.ok)
        return proj.moduleProjectPath ?? Err('Unknown module project path')
      if (!ptr.module.equals(proj.moduleProjectPath.value))
        return Err('Cannot read method from different module')
      if (!ptr.module.equals(ptr.definedOnType)) return Err('Method pointer is not a module method')
      const method = Ast.findModuleMethod(topLevel, ptr.name)
      if (!method) return Err(`No method with name ${ptr.name} in ${proj.moduleProjectPath.value}`)
      return Ok(method.statement)
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
      const existingImports = analyzeImports(topLevel, projectNames)

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
      existingImports?: AbstractImport[] | undefined,
    ) {
      if (!imports.length) return
      if (!moduleRoot.value) {
        console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
        return
      }
      const topLevel = edit.getVersion(moduleRoot.value)
      const existingImports_ = existingImports ?? analyzeImports(topLevel, projectNames)

      const importsToAdd = filterOutRedundantImports(existingImports_, imports)
      if (!importsToAdd.length) return
      addImports(edit.getVersion(topLevel), importsToAdd, projectNames)
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

            updatePortValue(edit, usage, undefined, false)
          }
          const outerAst = edit.getVersion(node.outerAst)
          if (outerAst.isStatement()) Ast.deleteFromParentBlock(outerAst)
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
      canUndo: computed(() => undoManagerStatus.canUndo),
      canRedo: computed(() => undoManagerStatus.canRedo),
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
     * The properties are analogous to {@link WidgetUpdate fields}.
     *
     * NOTE: If this returns `true,` The update handlers called `graph.commitEdit` on their own.
     * Therefore, the passed in `edit` should not be modified afterward, as it is already committed.
     */
    function updatePortValue(
      edit: MutableModule,
      id: PortId,
      value: Ast.Owned<Ast.MutableExpression> | undefined,
      directInteraction: boolean = true,
    ): boolean {
      const update = getPortPrimaryInstance(id)?.onUpdate
      if (!update) return false
      update({
        edit,
        portUpdate: { value, origin: id },
        directInteraction,
      })
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
      if (!skipTreeRepair) edit.transact(() => Ast.repair(root, edit))
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
      assertDefined(syncModule.value)
      const edit = syncModule.value.edit()
      let result
      edit.transact(() => {
        result = f(edit)
        if (!skipTreeRepair) {
          const root = edit.root()
          assert(root instanceof Ast.BodyBlock)
          Ast.repair(root, edit)
        }
      })
      syncModule.value.applyEdit(edit)
      return result!
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
        type: update.type ?? [],
        ...(update.methodCall ? { methodCall: update.methodCall } : {}),
      }
      proj.computedValueRegistry.processUpdates([update_])
    }

    /** Iterate over code lines, return node IDs from `ids` set in the order of code positions. */
    function pickInCodeOrder(ids: Set<NodeId>): NodeId[] {
      if (ids.size === 0) return []
      assert(syncModule.value != null)
      const func = unwrap(getExecutedMethodAst(syncModule.value))
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
    function ensureCorrectNodeOrder(
      edit: MutableModule,
      sourceNodeId: NodeId,
      targetNodeId: NodeId,
    ) {
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

        const dependantLines = new Set(
          Array.from(deps, (id) => db.nodeIdToNode.get(id)?.outerAst.id),
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

    function isConnectedTarget(portId: PortId): boolean {
      return isAstId(portId) && db.connections.reverseLookup(portId).size > 0
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

    const modulePath: Ref<LsPath | undefined> = computedAsync(
      async () => {
        const rootId = await proj.projectRootId
        const segments = ['src', 'Main.enso']
        return rootId ? { rootId, segments } : undefined
      },
      undefined,
      { onError: console.error },
    )

    function onBeforeEdit(f: (transaction: Y.Transaction) => void): { unregister: () => void } {
      proj.module?.doc.ydoc.on('beforeTransaction', f)
      return { unregister: () => proj.module?.doc.ydoc.off('beforeTransaction', f) }
    }

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
      getMethodAst,
      generateLocallyUniqueIdent,
      moduleRoot,
      deleteNodes,
      pickInCodeOrder,
      ensureCorrectNodeOrder,
      batchEdits,
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
      onBeforeEdit,
      viewModule,
      addMissingImports,
      addMissingImportsDisregardConflicts,
      isConnectedTarget,
      nodeCanBeEntered,
      modulePath,
      connectedEdges,
      currentMethod: proxyRefs({
        ast: methodAst,
        pointer: currentMethodPointer,
      }),
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
