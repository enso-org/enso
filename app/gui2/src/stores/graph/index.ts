import { nonDictatedPlacement } from '@/components/ComponentBrowser/placement'
import type { PortId } from '@/providers/portInfo'
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
import { Ast } from '@/util/ast'
import type { AstId, Module } from '@/util/ast/abstract'
import { MutableModule } from '@/util/ast/abstract'
import { useObserveYjs } from '@/util/crdt'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { map, set } from 'lib0'
import { defineStore } from 'pinia'
import type { StackItem } from 'shared/languageServerTypes'
import {
  IdMap,
  visMetadataEquals,
  type ExprId,
  type NodeMetadata,
  type SourceRange,
  type VisualizationIdentifier,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { computed, markRaw, reactive, ref, toRef, watch, type Ref, type ShallowRef } from 'vue'

export { type Node } from '@/stores/graph/graphDatabase'

export interface NodeEditInfo {
  id: ExprId
  initialCursorPos: number
}

export class PortViewInstance {
  constructor(
    public rect: ShallowRef<Rect | undefined>,
    public nodeId: ExprId,
    public onUpdate: (value: unknown, origin: PortId) => void,
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
  const nodeRects = reactive(new Map<ExprId, Rect>())
  const vizRects = reactive(new Map<ExprId, Rect>())
  const portInstances = reactive(new Map<PortId, Set<PortViewInstance>>())
  const editedNodeInfo = ref<NodeEditInfo>()
  const imports = ref<{ import: Import; span: SourceRange }[]>([])
  const methodAst = ref<Ast.Function>()
  const currentNodeIds = ref(new Set<ExprId>())

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
      moduleRoot.value = newRoot.exprId
      module.doc.setIdMap(idMap_)

      imports.value = []
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

      methodAst.value = getExecutedMethodAst(newRoot, proj.executionContext.getStackTop(), db)
      if (methodAst.value) {
        currentNodeIds.value = db.readFunctionAst(methodAst.value, (id) => meta.get(id))
      }
    })
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
    const root = moduleRoot.value
    if (!root) {
      console.error(`BUG: Cannot add node: No module root.`)
      return
    }
    const edit = astModule.edit()
    const importsToAdd = withImports ? filterOutRedundantImports(imports.value, withImports) : []
    // The top level of the module is always a block.
    const topLevel = astModule.get(root)! as Ast.BodyBlock
    if (importsToAdd) addImports(edit, topLevel, importsToAdd)
    const currentFunc = 'main'
    const functionBlock = Ast.functionBlock(astModule, currentFunc)
    if (!functionBlock) {
      console.error(`BUG: Cannot add node: No current function.`)
      return
    }
    const rhs = Ast.parse(expression, edit)
    const assignment = Ast.Assignment.new(edit, ident, rhs)
    functionBlock.push(edit, assignment)
    commitEdit(edit, new Map([[rhs.exprId, meta]]))
  }

  function deleteNode(id: ExprId) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    proj.module?.doc.metadata.delete(node.outerExprId)
    nodeRects.delete(id)
    const root = moduleRoot.value
    if (!root) {
      console.error(`BUG: Cannot delete node: No module root.`)
      return
    }
    const edit = astModule.edit()
    edit.delete(node.outerExprId)
    commitEdit(edit)
  }

  function setNodeContent(id: ExprId, content: string) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    setExpressionContent(node.rootSpan.exprId, content)
  }

  function setExpression(id: ExprId, content: Ast.Ast) {
    const edit = astModule.edit()
    edit.set(Ast.asNodeId(id), content)
    const root = moduleRoot.value
    if (!root) {
      console.error(`BUG: Cannot update node: No module root.`)
      return
    }
    commitEdit(edit)
  }

  function setExpressionContent(id: ExprId, content: string) {
    setExpression(id, Ast.RawCode.new(content))
  }

  function transact(fn: () => void) {
    return proj.module?.transact(fn)
  }

  function stopCapturingUndo() {
    proj.stopCapturingUndo()
  }

  function setNodePosition(nodeId: ExprId, position: Vec2) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
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
    return getPortPrimaryInstance(id)?.nodeId ?? db.getExpressionNodeId(id as string as ExprId)
  }

  /**
   * Emit an value update to a port view under specific ID. Returns `true` if the port view is
   * registered and the update was emitted, or `false` otherwise.
   */
  function updatePortValue(id: PortId, value: Ast.Ast | undefined): boolean {
    const update = getPortPrimaryInstance(id)?.onUpdate
    if (!update) return false
    update(value, id)
    return true
  }

  function commitEdit(edit: Module, metadataUpdates?: Map<AstId, Partial<NodeMetadata>>) {
    const root = moduleRoot.value
    if (!root) {
      console.error(`BUG: Cannot commit edit: No module root.`)
      return
    }
    const ast = edit.get(root)
    if (!ast) return
    const printed = Ast.print(ast.exprId, edit)
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
      idMap.insertKnownId([range.start, range.end], id)
    }
    for (const [nodeKey, ids] of printed.info.nodes) {
      const range = Ast.keyToRange(nodeKey)
      idMap.insertKnownId([range.start, range.end], ids[0]!)
    }
    module_.transact(() => {
      module_.doc.setIdMap(idMap)
      module_.doc.setCode(printed.code)
      if (metadataUpdates) {
        for (const [id, meta] of metadataUpdates) {
          module_.updateNodeMetadata(id, meta)
        }
      }
    })
  }

  return {
    transact,
    db: markRaw(db),
    imports,
    editedNodeInfo,
    unconnectedEdge,
    edges,
    currentNodeIds,
    moduleCode,
    nodeRects,
    vizRects,
    methodAst,
    createEdgeFromOutput,
    disconnectSource,
    disconnectTarget,
    clearUnconnected,
    createNode,
    deleteNode,
    setNodeContent,
    setExpression,
    setExpressionContent,
    setNodePosition,
    setNodeVisualizationId,
    setNodeVisualizationVisible,
    stopCapturingUndo,
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
  ast: Ast.Ast,
  executionStackTop: StackItem,
  db: GraphDb,
): Ast.Function | undefined {
  switch (executionStackTop.type) {
    case 'ExplicitCall': {
      // Assume that the provided AST matches the module in the method pointer. There is no way to
      // actually verify this assumption at this point.
      const ptr = executionStackTop.methodPointer
      return Ast.findModuleMethod(ast.module, ptr.name) ?? undefined
    }
    case 'LocalCall': {
      const exprId = executionStackTop.expressionId
      const info = db.getExpressionInfo(exprId)
      if (!info) return undefined
      const ptr = info.methodCall?.methodPointer
      if (!ptr) return undefined
      return Ast.findModuleMethod(ast.module, ptr.name) ?? undefined
    }
  }
}
