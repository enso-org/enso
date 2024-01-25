import { nonDictatedPlacement } from '@/components/ComponentBrowser/placement'
import type { PortId } from '@/providers/portInfo'
import type { WidgetUpdate } from '@/providers/widgetRegistry'
import { GraphDb, type NodeId } from '@/stores/graph/graphDatabase'
import {
  addImports,
  filterOutRedundantImports,
  readImports,
  recognizeImport,
  type Import,
  type RequiredImport,
} from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { AstId, Module } from '@/util/ast/abstract'
import { MutableModule } from '@/util/ast/abstract'
import { useObserveYjs } from '@/util/crdt'
import { partition } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { map, set } from 'lib0'
import { defineStore } from 'pinia'
import type { ExpressionUpdate, StackItem, Uuid } from 'shared/languageServerTypes'
import {
  IdMap,
  visMetadataEquals,
  type ExternalId,
  type NodeMetadata,
  type SourceRange,
  type VisualizationIdentifier,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { computed, markRaw, reactive, ref, toRef, watch, type Ref, type ShallowRef } from 'vue'

export { type Node, type NodeId } from '@/stores/graph/graphDatabase'

export interface NodeEditInfo {
  id: NodeId
  initialCursorPos: number
}

export class PortViewInstance {
  constructor(
    public rect: ShallowRef<Rect | undefined>,
    public nodeId: NodeId,
    public onUpdate: (update: WidgetUpdate) => void,
  ) {}
}

export const useGraphStore = defineStore('graph', () => {
  const proj = useProjectStore()
  const suggestionDb = useSuggestionDbStore()

  proj.setObservedFileName('Main.enso')

  const data = computed(() => proj.module?.doc.data)
  const metadata = computed(() => proj.module?.doc.metadata)

  const moduleCode = ref(proj.module?.doc.getCode())
  // We need casting here, as type changes in Ref when class has private fields.
  // see https://github.com/vuejs/core/issues/2557
  const idMap = ref(proj.module?.doc.getIdMap()) as Ref<IdMap | undefined>
  const astModule: Module = MutableModule.Observable()
  const moduleRoot = ref<AstId>()
  let moduleDirty = false
  const nodeRects = reactive(new Map<NodeId, Rect>())
  const vizRects = reactive(new Map<NodeId, Rect>())

  const topLevel = computed(() => {
    // The top level of the module is always a block.
    const root = moduleRoot.value
    const topLevel = root != null ? astModule.get(root) : null
    if (topLevel != null && !(topLevel instanceof Ast.BodyBlock)) {
      return null
    } else {
      return topLevel
    }
  })

  // Initialize text and idmap once module is loaded (data != null)
  watch(data, () => {
    if (!moduleCode.value) {
      moduleCode.value = proj.module?.doc.getCode()
      idMap.value = proj.module?.doc.getIdMap()
      if (moduleCode.value && idMap.value) updateState()
    }
  })

  const db = new GraphDb(
    suggestionDb.entries,
    toRef(suggestionDb, 'groups'),
    proj.computedValueRegistry,
  )
  const portInstances = reactive(new Map<PortId, Set<PortViewInstance>>())
  const editedNodeInfo = ref<NodeEditInfo>()
  const methodAst = ref<Ast.Function>()
  const currentNodeIds = ref(new Set<NodeId>())

  const unconnectedEdge = ref<UnconnectedEdge>()

  useObserveYjs(data, (event) => {
    moduleDirty = false
    if (!event.changes.keys.size) return
    const code = proj.module?.doc.getCode()
    if (code) moduleCode.value = code
    const ids = proj.module?.doc.getIdMap()
    if (ids) idMap.value = ids
    if (code && ids) updateState()
  })

  function updateState() {
    const module = proj.module
    if (!module) return
    const idMap_ = idMap.value
    if (!idMap_) return
    module.transact(() => {
      const meta = module.doc.metadata
      const textContentLocal = moduleCode.value
      if (!textContentLocal) return

      const newRoot = Ast.parseTransitional(textContentLocal, idMap_)
      astModule.replace(newRoot.module)
      moduleRoot.value = newRoot.id
      module.doc.setIdMap(idMap_)

      methodAst.value = methodAstInModule(astModule)
      if (methodAst.value) {
        currentNodeIds.value = db.readFunctionAst(methodAst.value, (id) => meta.get(id))
      }
    })
  }

  function methodAstInModule(mod: Module) {
    return getExecutedMethodAst(mod, proj.executionContext.getStackTop(), db)
  }

  useObserveYjs(metadata, (event) => {
    const meta = event.target
    for (const [id, op] of event.changes.keys) {
      if (op.action === 'update' || op.action === 'add') {
        const data = meta.get(id)
        const node = db.nodeIdToNode.get(id as NodeId)
        if (data && node) db.assignUpdatedMetadata(node, data)
      }
    }
  })

  function generateUniqueIdent() {
    for (;;) {
      const ident = randomIdent()
      if (!db.identifierUsed(ident)) return ident
    }
  }

  const edges = computed(() => {
    const disconnectedEdgeTarget = unconnectedEdge.value?.disconnectedEdgeTarget
    const edges = []
    for (const [target, sources] of db.connections.allReverse()) {
      if ((target as string as PortId) === disconnectedEdgeTarget) continue
      for (const source of sources) {
        edges.push({ source, target })
      }
    }
    if (unconnectedEdge.value) {
      edges.push({
        source: unconnectedEdge.value.source,
        target: unconnectedEdge.value.target,
      })
    }
    return edges
  })

  function createEdgeFromOutput(source: Ast.AstId) {
    unconnectedEdge.value = { source }
  }

  function disconnectSource(edge: Edge) {
    if (!edge.target) return
    unconnectedEdge.value = { target: edge.target, disconnectedEdgeTarget: edge.target }
  }

  function disconnectTarget(edge: Edge) {
    if (!edge.source || !edge.target) return
    unconnectedEdge.value = { source: edge.source, disconnectedEdgeTarget: edge.target }
  }

  function clearUnconnected() {
    unconnectedEdge.value = undefined
  }

  function createNode(
    position: Vec2,
    expression: string,
    metadata: NodeMetadata | undefined = undefined,
    withImports: RequiredImport[] | undefined = undefined,
  ): Opt<NodeId> {
    const mod = proj.module
    if (!mod) return
    const meta = metadata ?? {
      x: position.x,
      y: -position.y,
      vis: null,
    }
    meta.x = position.x
    meta.y = -position.y
    const ident = generateUniqueIdent()
    const edit = astModule.edit()
    if (withImports) addMissingImports(edit, withImports)
    const currentFunc = 'main'
    const method = Ast.findModuleMethod(astModule, currentFunc)
    if (!method) {
      console.error(`BUG: Cannot add node: No current function.`)
      return
    }

    let functionBlock: Ast.BodyBlock

    if (method.body instanceof Ast.BodyBlock) {
      functionBlock = method.body
    } else if (method.body != null) {
      functionBlock = edit.takeAndReplaceRef(method.body.id, (node) => {
        return Ast.BodyBlock.new([{ expression: { node } }], edit)
      })
    } else {
      const newMethod: Ast.Function = edit.takeAndReplaceValue(method.id, (func) => {
        assert(func instanceof Ast.Function)
        assert(func.name != null)
        const name = edit.take(func.name.id)?.node
        assert(name != null)
        const body = Ast.BodyBlock.new([], edit)
        return Ast.Function.new(edit, name, func.argNodes(), body)
      })
      assert(newMethod.body instanceof Ast.BodyBlock)
      functionBlock = newMethod.body
    }

    const rhs = Ast.parse(expression, edit)
    const assignment = Ast.Assignment.new(edit, ident, rhs)
    functionBlock.push(edit, assignment)
    commitEdit(edit, new Map([[rhs.id, meta]]))
  }

  function addMissingImports(edit: MutableModule, newImports: RequiredImport[]) {
    if (!newImports.length) return
    const topLevel = moduleRoot.value ? edit.get(moduleRoot.value) : null
    if (!topLevel || !(topLevel instanceof Ast.BodyBlock)) {
      console.error(`BUG: Cannot add required imports: No BodyBlock module root.`)
      return
    }
    const existingImports = readImports(topLevel)
    const importsToAdd = filterOutRedundantImports(existingImports, newImports)
    if (!importsToAdd.length) return
    addImports(edit, topLevel, importsToAdd)
  }

  function deleteNodes(ids: NodeId[]) {
    const edit = astModule.edit()
    for (const id of ids) {
      const node = db.nodeIdToNode.get(id)
      if (!node) return
      proj.module?.doc.metadata.delete(node.outerExprId)
      nodeRects.delete(id)
      edit.delete(node.outerExprId)
    }
    commitEdit(edit)
  }

  function setNodeContent(id: NodeId, content: string) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    const edit = astModule.edit()
    edit.replaceValue(node.rootSpan.id, Ast.parse(content, edit))
    commitEdit(edit)
  }

  function transact(fn: () => void) {
    return proj.module?.transact(fn)
  }

  function stopCapturingUndo() {
    proj.stopCapturingUndo()
  }

  function setNodePosition(nodeId: NodeId, position: Vec2) {
    proj.module?.updateNodeMetadata(db.idToExternal(nodeId), { x: position.x, y: -position.y })
  }

  function normalizeVisMetadata(
    id: Opt<VisualizationIdentifier>,
    visible: boolean | undefined,
  ): VisualizationMetadata | null {
    const vis: VisualizationMetadata = { identifier: id ?? null, visible: visible ?? false }
    if (visMetadataEquals(vis, { identifier: null, visible: false })) return null
    else return vis
  }

  function setNodeVisualizationId(nodeId: NodeId, vis: Opt<VisualizationIdentifier>) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.updateNodeMetadata(db.idToExternal(nodeId), {
      vis: normalizeVisMetadata(vis, node.vis?.visible),
    })
  }

  function setNodeVisualizationVisible(nodeId: NodeId, visible: boolean) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.updateNodeMetadata(db.idToExternal(nodeId), {
      vis: normalizeVisMetadata(node.vis?.identifier, visible),
    })
  }

  function updateNodeRect(nodeId: NodeId, rect: Rect) {
    if (rect.pos.equals(Vec2.Zero) && !metadata.value?.has(nodeId)) {
      const { position } = nonDictatedPlacement(rect.size, {
        nodeRects: [...nodeRects.entries()]
          .filter(([id]) => db.nodeIdToNode.get(id))
          .map(([id, rect]) => vizRects.get(id) ?? rect),
        // The rest of the properties should not matter.
        selectedNodeRects: [],
        screenBounds: Rect.Zero,
        mousePosition: Vec2.Zero,
      })
      const node = db.nodeIdToNode.get(nodeId)
      metadata.value?.set(nodeId, { x: position.x, y: -position.y, vis: node?.vis ?? null })
      nodeRects.set(nodeId, new Rect(position, rect.size))
    } else {
      nodeRects.set(nodeId, rect)
    }
  }

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

  function getPortNodeId(id: PortId): NodeId | undefined {
    return getPortPrimaryInstance(id)?.nodeId ?? db.getExpressionNodeId(id as string as Ast.AstId)
  }

  /**
   * Emit an value update to a port view under specific ID. Returns `true` if the port view is
   * registered and the update was emitted, or `false` otherwise.
   *
   * NOTE: If this returns `true,` The update handlers called `graph.commitEdit` on their own.
   * Therefore the passed in `edit` should not be modified afterwards, as it is already committed.
   */
  function updatePortValue(edit: MutableModule, id: PortId, value: Ast.Owned | undefined): boolean {
    const update = getPortPrimaryInstance(id)?.onUpdate
    if (!update) return false
    update({ edit, portUpdate: { value, origin: id } })
    return true
  }

  function commitEdit(edit: Module, metadataUpdates?: Map<AstId, Partial<NodeMetadata>>) {
    const root = moduleRoot.value
    if (!root) {
      console.error(`BUG: Cannot commit edit: No module root.`)
      return
    }
    const printed = Ast.print(root, edit)
    const module_ = proj.module
    if (!module_) return
    if (moduleDirty) {
      console.warn(
        `An edit has been committed before a previous edit has been observed. The new edit will supersede the previous edit.`,
      )
    }
    moduleDirty = true
    const idMap = new IdMap()
    for (const [tokenKey, id] of printed.info.tokens) {
      const range = Ast.keyToRange(tokenKey)
      idMap.insertKnownId([range.start, range.end], id as Uuid as ExternalId)
    }
    for (const [nodeKey, ids] of printed.info.nodes) {
      const range = Ast.keyToRange(nodeKey)
      idMap.insertKnownId([range.start, range.end], ids[0]! as Uuid as ExternalId)
    }
    module_.transact(() => {
      module_.doc.setIdMap(idMap)
      module_.doc.setCode(printed.code)
      if (metadataUpdates) {
        for (const [id, meta] of metadataUpdates) {
          module_.updateNodeMetadata(db.idToExternal(id), meta)
        }
      }
    })
  }

  function mockExpressionUpdate(binding: string, update: Partial<ExpressionUpdate>) {
    db.mockExpressionUpdate(binding, update)
  }

  function editScope(scope: (edit: MutableModule) => Map<AstId, Partial<NodeMetadata>> | void) {
    const edit = astModule.edit()
    const metadataUpdates = scope(edit)
    commitEdit(edit, metadataUpdates ?? undefined)
  }

  /**
   * Reorders nodes so the `targetNodeId` node is placed after `sourceNodeId`. Does nothing if the
   * relative order is already correct.
   *
   * Additionally all nodes dependent on the `targetNodeId` that end up being before its new line
   * are also moved after it, keeping their relative order.
   */
  function ensureCorrectNodeOrder(edit: MutableModule, sourceNodeId: NodeId, targetNodeId: NodeId) {
    const sourceExpr = db.nodeIdToNode.get(sourceNodeId)?.outerExprId
    const targetExpr = db.nodeIdToNode.get(targetNodeId)?.outerExprId
    const body = methodAstInModule(edit)?.body
    console.log('body', body)
    assert(sourceExpr != null)
    assert(targetExpr != null)
    assert(body instanceof Ast.BodyBlock, 'Current function body must be a BodyBlock')

    const lines = body.takeLines(edit)
    const sourceIdx = lines.findIndex((line) => line.expression?.node.id === sourceExpr)
    const targetIdx = lines.findIndex((line) => line.expression?.node.id === targetExpr)

    assert(sourceIdx != null)
    assert(targetIdx != null)

    // If source is placed after its new target, the nodes needs to be reordered.
    if (sourceIdx > targetIdx) {
      // Find all transitive dependencies of the moved target node.
      const deps = db.dependantNodes(targetNodeId)

      const dependantLines = new Set(Array.from(deps, (id) => db.nodeIdToNode.get(id)?.outerExprId))
      // Include the new target itself in the set of lines that must be placed after source node.
      dependantLines.add(targetExpr)

      // Check if the source depends on target. If that's the case, the edge we are trying to make
      // creates a circular dependency. Reordering doesn't make any sense in that case.
      if (dependantLines.has(sourceExpr)) {
        return 'circular'
      }

      // Pick subset of lines to reorder, i.e. lines between and including target and source.
      const linesToSort = lines.splice(targetIdx, sourceIdx - targetIdx + 1)

      // Split those lines into two buckets, whether or not they depend on the target.
      const [linesAfter, linesBefore] = partition(linesToSort, (line) =>
        dependantLines.has(line.expression?.node.id),
      )

      // Recombine all lines after splitting, keeping existing dependants below the target.
      lines.splice(targetIdx, 0, ...linesBefore, ...linesAfter)

      // Finally apply the reordered lines into the body block as AST edit.
      const ownedBody = edit.take(body.id)
      assert(ownedBody != null)
      edit.replaceValue(ownedBody.placeholder.id, Ast.BodyBlock.new(lines, edit))
      return true
    } else {
      return false
    }
  }

  return {
    transact,
    db: markRaw(db),
    mockExpressionUpdate,
    editedNodeInfo,
    unconnectedEdge,
    edges,
    currentNodeIds,
    moduleCode,
    nodeRects,
    vizRects,
    unregisterNodeRect,
    methodAst,
    astModule,
    createEdgeFromOutput,
    disconnectSource,
    disconnectTarget,
    clearUnconnected,
    moduleRoot,
    createNode,
    deleteNodes,
    ensureCorrectNodeOrder,
    setNodeContent,
    setNodePosition,
    setNodeVisualizationId,
    setNodeVisualizationVisible,
    stopCapturingUndo,
    topLevel,
    updateNodeRect,
    updateVizRect,
    addPortInstance,
    removePortInstance,
    getPortRelativeRect,
    getPortNodeId,
    updatePortValue,
    setEditedNode,
    updateState,
    commitEdit,
    editScope,
    addMissingImports,
  }
})

function randomIdent() {
  return 'operator' + Math.round(Math.random() * 100000)
}

/** An edge, which may be connected or unconnected. */
export type Edge = {
  source: AstId | undefined
  target: PortId | undefined
}

export type UnconnectedEdge = {
  source?: AstId
  target?: PortId
  /** If this edge represents an in-progress edit of a connected edge, it is identified by its target expression. */
  disconnectedEdgeTarget?: PortId
}

function getExecutedMethodAst(
  astModule: Module,
  executionStackTop: StackItem,
  db: GraphDb,
): Ast.Function | undefined {
  switch (executionStackTop.type) {
    case 'ExplicitCall': {
      // Assume that the provided AST matches the module in the method pointer. There is no way to
      // actually verify this assumption at this point.
      const ptr = executionStackTop.methodPointer
      return Ast.findModuleMethod(astModule, ptr.name) ?? undefined
    }
    case 'LocalCall': {
      const exprId = executionStackTop.expressionId
      const info = db.getExpressionInfo(exprId)
      if (!info) return undefined
      const ptr = info.methodCall?.methodPointer
      if (!ptr) return undefined
      return Ast.findModuleMethod(astModule, ptr.name) ?? undefined
    }
  }
}
