import { nonDictatedPlacement } from '@/components/ComponentBrowser/placement'
import type { PortId } from '@/providers/portInfo'
import type { WidgetUpdate } from '@/providers/widgetRegistry'
import { GraphDb } from '@/stores/graph/graphDatabase'
import {
  addImports,
  filterOutRedundantImports,
  recognizeImport,
  type Import,
  type RequiredImport,
} from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { assert, assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import {
  MutableModule,
  ReactiveModule,
  type AstId,
  type Owned,
} from '@/util/ast/abstract'
import { useObserveYjs } from '@/util/crdt'
import { partition } from '@/util/data/array'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { map, set } from 'lib0'
import { defineStore } from 'pinia'
import type { ExpressionUpdate, StackItem } from 'shared/languageServerTypes'
import {
  visMetadataEquals,
  type ExprId,
  type NodeMetadata,
  type SourceRange,
  type VisualizationIdentifier,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { computed, markRaw, reactive, ref, toRef, watch, type ShallowRef } from 'vue'

export { type Node } from '@/stores/graph/graphDatabase'

export interface NodeEditInfo {
  id: ExprId
  initialCursorPos: number
}

export class PortViewInstance {
  constructor(
    public rect: ShallowRef<Rect | undefined>,
    public nodeId: ExprId,
    public onUpdate: (update: WidgetUpdate) => void,
  ) {}
}

export const useGraphStore = defineStore('graph', () => {
  const proj = useProjectStore()
  const suggestionDb = useSuggestionDbStore()

  proj.setObservedFileName('Main.enso')

  const astModule = computed(() => {
    const yModule = proj.module?.doc.getAst()
    return yModule ? new ReactiveModule(yModule) : undefined
  })
  const metadata = computed(() => proj.module?.doc.metadata)

  const moduleCode = computed(() => astModule.value?.root()?.code())
  const nodeRects = reactive(new Map<ExprId, Rect>())
  const vizRects = reactive(new Map<ExprId, Rect>())

  const topLevel = computed(() => {
    // The top level of the module is always a block.
    const topLevel = astModule.value?.root()
    if (topLevel instanceof Ast.BodyBlock) {
      return topLevel
    } else {
      return null
    }
  })

  const db = new GraphDb(
    suggestionDb.entries,
    toRef(suggestionDb, 'groups'),
    proj.computedValueRegistry,
  )
  const portInstances = reactive(new Map<PortId, Set<PortViewInstance>>())
  const editedNodeInfo = ref<NodeEditInfo>()
  const imports = ref<{ import: Import; span: SourceRange }[]>([])
  const methodAst = ref<Ast.Function>()
  const currentNodeIds = ref(new Set<ExprId>())

  const unconnectedEdge = ref<UnconnectedEdge>()

  watch(astModule, () => updateState())
  function updateState() {
    const module = proj.module
    if (!module) return
    const astModule_ = astModule.value
    if (!astModule_) return
    module.transact(() => {
      const meta = module.doc.metadata

      imports.value = []
      const newRoot = astModule_.root()
      if (newRoot) {
        newRoot.visitRecursive((node) => {
          if (node instanceof Ast.Import) {
            const recognized = recognizeImport(node)
            if (recognized) {
              imports.value.push({ import: recognized, span: node.span! })
            }
            return false
          }
          return true
        })
      }

      if (topLevel.value) {
        const methodAst_ = methodAstInModule(topLevel.value)
        methodAst.value = methodAst_
        if (methodAst_) currentNodeIds.value = db.readFunctionAst(methodAst_, (id) => meta.get(id))
      }
    })
  }

  function methodAstInModule(topLevel: Ast.BodyBlock) {
    return getExecutedMethodAst(topLevel, proj.executionContext.getStackTop(), db)
  }

  useObserveYjs(metadata, (event) => {
    const meta = event.target
    for (const [id, op] of event.changes.keys) {
      if (op.action === 'update' || op.action === 'add') {
        const data = meta.get(id)
        const node = db.nodeIdToNode.get(id as ExprId)
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

  function createEdgeFromOutput(source: ExprId) {
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
  ): Opt<ExprId> {
    const mod = astModule.value
    if (!mod) return
    const meta = metadata ?? {
      x: position.x,
      y: -position.y,
      vis: null,
    }
    meta.x = position.x
    meta.y = -position.y
    const ident = generateUniqueIdent()
    const edit = mod.edit()
    if (withImports) addMissingImports(edit, withImports)
    const currentFunc = 'main'
    const method = Ast.findModuleMethod(topLevel.value!, currentFunc)
    if (!method) {
      console.error(`BUG: Cannot add node: No current function.`)
      return
    }
    const rhs = Ast.parse(expression, edit)
    edit
      .get(method)!
      .bodyAsBlock()
      .push(Ast.Assignment.new(edit, ident, rhs))
    commitEdit(edit, new Map([[rhs.exprId, meta]]))
  }

  function addMissingImports(edit: MutableModule, newImports: RequiredImport[]) {
    if (!newImports.length) return
    const topLevel_ = topLevel.value
    if (!topLevel_) {
      console.error(`BUG: Cannot add required imports: No module root.`)
      return
    }
    const importsToAdd = filterOutRedundantImports(imports.value, newImports)
    if (!importsToAdd.length) return
    addImports(edit.get(topLevel_)!, importsToAdd)
  }

  function deleteNodes(ids: ExprId[]) {
    const module = astModule.value
    if (!module) return
    const edit = module.edit()
    for (const id of ids) {
      const node = db.nodeIdToNode.get(id)
      if (!node) return
      proj.module?.doc.metadata.delete(node.outerExpr.exprId)
      nodeRects.delete(id)
      const parent = node.outerExpr.parent()
      if (parent instanceof Ast.BodyBlock)
        edit.get(parent)!.filter((statement) => statement !== node.outerExpr)
    }
    commitEdit(edit)
  }

  function setNodeContent(id: ExprId, content: string) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    setExpression(node.rootSpan, Ast.parse(content))
  }

  function setExpression(ast: Ast.Ast, content: Ast.Owned) {
    const module = astModule.value
    if (!module) return
    const edit = module.edit()
    edit.get(ast)!.replaceValue(content)
    commitEdit(edit)
  }

  function transact(fn: () => void) {
    return proj.module?.transact(fn)
  }

  function stopCapturingUndo() {
    proj.stopCapturingUndo()
  }

  function setNodePosition(nodeId: ExprId, position: Vec2) {
    proj.module?.updateNodeMetadata(nodeId, { x: position.x, y: -position.y })
  }

  function normalizeVisMetadata(
    id: Opt<VisualizationIdentifier>,
    visible: boolean | undefined,
  ): VisualizationMetadata | null {
    const vis: VisualizationMetadata = { identifier: id ?? null, visible: visible ?? false }
    if (visMetadataEquals(vis, { identifier: null, visible: false })) return null
    else return vis
  }

  function setNodeVisualizationId(nodeId: ExprId, vis: Opt<VisualizationIdentifier>) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.updateNodeMetadata(nodeId, { vis: normalizeVisMetadata(vis, node.vis?.visible) })
  }

  function setNodeVisualizationVisible(nodeId: ExprId, visible: boolean) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.updateNodeMetadata(nodeId, {
      vis: normalizeVisMetadata(node.vis?.identifier, visible),
    })
  }

  function updateNodeRect(nodeId: ExprId, rect: Rect) {
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

  function updateVizRect(id: ExprId, rect: Rect | undefined) {
    if (rect) vizRects.set(id, rect)
    else vizRects.delete(id)
  }

  function unregisterNodeRect(id: ExprId) {
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

  function setEditedNode(id: ExprId | null, cursorPosition: number | null) {
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

  function getPortNodeId(id: PortId): ExprId | undefined {
    const primaryInstance = getPortPrimaryInstance(id)?.nodeId
    if (primaryInstance) return primaryInstance
    if (id instanceof Ast.Ast) return db.getExpressionNodeId(id.exprId)
  }

  /**
   * Emit an value update to a port view under specific ID. Returns `true` if the port view is
   * registered and the update was emitted, or `false` otherwise.
   *
   * NOTE: If this returns `true,` The update handlers called `graph.commitEdit` on their own.
   * Therefore the passed in `edit` should not be modified afterwards, as it is already committed.
   */
  function updatePortValue(edit: MutableModule, id: PortId, value: Owned | undefined): boolean {
    const update = getPortPrimaryInstance(id)?.onUpdate
    if (!update) return false
    update({ edit, portUpdate: { value, origin: id } })
    return true
  }

  function commitEdit(edit: MutableModule, metadataUpdates?: Map<AstId, Partial<NodeMetadata>>) {
    if (!edit.root()) {
      console.error(`Refusing to commit edit with no root node set.`)
      return
    }
    const module_ = proj.module
    if (!module_) return
    module_.transact(() => {
      module_.doc.updateAst(edit)
      if (metadataUpdates) {
        for (const [id, meta] of metadataUpdates) {
          module_.updateNodeMetadata(id, meta)
        }
      }
    })
  }

  function mockExpressionUpdate(binding: string, update: Partial<ExpressionUpdate>) {
    db.mockExpressionUpdate(binding, update)
  }

  function editScope(scope: (edit: MutableModule) => Map<AstId, Partial<NodeMetadata>> | void) {
    const module = astModule.value
    if (!module) return
    const edit = module.edit()
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
  function ensureCorrectNodeOrder(edit: MutableModule, sourceNodeId: ExprId, targetNodeId: ExprId) {
    const sourceExpr = db.nodeIdToNode.get(sourceNodeId)?.outerExpr
    const targetExpr = db.nodeIdToNode.get(targetNodeId)?.outerExpr
    const method = methodAstInModule(topLevel.value!)
    const body = method ? edit.get(method)?.bodyAsBlock() : undefined
    console.log('body', body)
    assert(sourceExpr != null)
    assert(targetExpr != null)
    assertDefined(body, 'Current function must be in the module.')

    const lines = body.takeLines()
    const sourceIdx = lines.findIndex((line) => line.expression?.node.is(sourceExpr))
    const targetIdx = lines.findIndex((line) => line.expression?.node.is(targetExpr))

    assert(sourceIdx != null)
    assert(targetIdx != null)

    // If source is placed after its new target, the nodes needs to be reordered.
    if (sourceIdx > targetIdx) {
      // Find all transitive dependencies of the moved target node.
      const deps = db.dependantNodes(targetNodeId)

      const dependantLines = new Set(
        Array.from(deps, (id) => db.nodeIdToNode.get(id)?.outerExpr.exprId),
      )
      // Include the new target itself in the set of lines that must be placed after source node.
      dependantLines.add(targetExpr.exprId)

      // Check if the source depends on target. If that's the case, the edge we are trying to make
      // creates a circular dependency. Reordering doesn't make any sense in that case.
      if (dependantLines.has(sourceExpr.exprId)) {
        return 'circular'
      }

      // Pick subset of lines to reorder, i.e. lines between and including target and source.
      const linesToSort = lines.splice(targetIdx, sourceIdx - targetIdx + 1)

      // Split those lines into two buckets, whether or not they depend on the target.
      const [linesAfter, linesBefore] = partition(linesToSort, (line) =>
        dependantLines.has(line.expression?.node.exprId),
      )

      // Recombine all lines after splitting, keeping existing dependants below the target.
      lines.splice(targetIdx, 0, ...linesBefore, ...linesAfter)

      // Finally apply the reordered lines into the body block as AST edit.
      body.replaceValue(Ast.BodyBlock.new(lines, edit))
      return true
    } else {
      return false
    }
  }

  return {
    transact,
    db: markRaw(db),
    mockExpressionUpdate,
    imports,
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
  source: ExprId | undefined
  target: PortId | undefined
}

export type UnconnectedEdge = {
  source?: ExprId
  target?: PortId
  /** If this edge represents an in-progress edit of a connected edge, it is identified by its target expression. */
  disconnectedEdgeTarget?: PortId
}

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
