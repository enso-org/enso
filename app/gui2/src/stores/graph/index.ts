import { nonDictatedPlacement } from '@/components/ComponentBrowser/placement'
import { GraphDb } from '@/stores/graph/graphDatabase'
import {
  filterOutRedundantImports,
  recognizeImport,
  requiredImportToText,
  type Import,
  type RequiredImport,
} from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Ast } from '@/util/ast'
import { useObserveYjs } from '@/util/crdt'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { defineStore } from 'pinia'
import type { StackItem } from 'shared/languageServerTypes'
import {
  visMetadataEquals,
  type ContentRange,
  type ExprId,
  type NodeMetadata,
  type VisualizationIdentifier,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { computed, markRaw, reactive, ref, toRef, watch } from 'vue'

export { type Node } from '@/stores/graph/graphDatabase'

export interface NodeEditInfo {
  id: ExprId
  initialCursorPos: number
}
export const useGraphStore = defineStore('graph', () => {
  const proj = useProjectStore()
  const suggestionDb = useSuggestionDbStore()

  proj.setObservedFileName('Main.enso')

  const text = computed(() => proj.module?.doc.contents)
  const metadata = computed(() => proj.module?.doc.metadata)

  const textContent = ref('')

  const db = new GraphDb(
    suggestionDb.entries,
    toRef(suggestionDb, 'groups'),
    proj.computedValueRegistry,
  )
  const nodeRects = reactive(new Map<ExprId, Rect>())
  const vizRects = reactive(new Map<ExprId, Rect>())
  const exprRects = reactive(new Map<ExprId, Rect>())
  const editedNodeInfo = ref<NodeEditInfo>()
  const imports = ref<{ import: Import; span: ContentRange }[]>([])
  const methodAst = ref<Ast.Function>()
  const currentNodeIds = ref(new Set<ExprId>())

  const unconnectedEdge = ref<UnconnectedEdge>()

  useObserveYjs(text, (event) => {
    const delta = event.changes.delta
    if (delta.length === 0) return

    let newContent = ''
    let oldIdx = 0
    for (const op of delta) {
      if (op.retain) {
        newContent += textContent.value.substring(oldIdx, oldIdx + op.retain)
        oldIdx += op.retain
      } else if (op.delete) {
        oldIdx += op.delete
      } else if (op.insert && typeof op.insert === 'string') {
        newContent += op.insert
      } else {
        console.error('Unexpected Yjs operation:', op)
      }
    }
    newContent += textContent.value.substring(oldIdx)
    textContent.value = newContent
    updateState()
  })

  watch(text, (value) => {
    textContent.value = value?.toString() ?? ''
    if (value) updateState()
  })

  function updateState() {
    const module = proj.module
    if (!module) return
    module.transact(() => {
      const idMap = module.getIdMap()
      const meta = module.doc.metadata
      const textContentLocal = textContent.value

      const newRoot = Ast.parseTransitional(textContentLocal, idMap)

      imports.value = []
      newRoot.visitRecursive((node) => {
        if (node instanceof Ast.Import) {
          const recognized = recognizeImport(node)
          if (recognized) {
            imports.value.push({ import: recognized, span: node.astExtended!.span() })
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
      if (target === disconnectedEdgeTarget) continue
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
    if (edge.target)
      unconnectedEdge.value = { target: edge.target, disconnectedEdgeTarget: edge.target }
  }

  function disconnectTarget(edge: Edge) {
    if (edge.source && edge.target)
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
    let importData = undefined
    let additionalOffset = 0
    const importsToAdd = withImports ? filterOutRedundantImports(imports.value, withImports) : []
    if (importsToAdd.length > 0) {
      const lastImport = imports.value[imports.value.length - 1]
      const importOffset = lastImport ? lastImport.span[1] + 1 : 0
      const str = importsToAdd.map((info) => requiredImportToText(info)).join('\n')
      additionalOffset += str.length + 1
      importData = { str, offset: importOffset }
    }
    return mod.insertNewNode(
      mod.doc.contents.length + additionalOffset,
      ident,
      expression,
      meta,
      importData,
    )
  }

  function deleteNode(id: ExprId) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    proj.module?.deleteExpression(node.outerExprId)
    nodeRects.delete(id)
    node.pattern?.visitRecursive((ast) => exprRects.delete(ast.astId))
  }

  function setNodeContent(id: ExprId, content: string) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    setExpressionContent(node.rootSpan.astId, content)
  }

  function setExpressionContent(id: ExprId, content: string) {
    proj.module?.replaceExpressionContent(id, content)
  }

  function transact(fn: () => void) {
    return proj.module?.transact(fn)
  }

  function stopCapturingUndo() {
    proj.stopCapturingUndo()
  }

  function replaceNodeSubexpression(
    nodeId: ExprId,
    range: ContentRange | undefined,
    content: string,
  ) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.replaceExpressionContent(node.rootSpan.astId, content, range)
  }

  function replaceExpressionContent(exprId: ExprId, content: string) {
    proj.module?.replaceExpressionContent(exprId, content)
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

  function updateExprRect(id: ExprId, rect: Rect | undefined) {
    const current = exprRects.get(id)
    if (rect) {
      if (!current || !current.equals(rect)) exprRects.set(id, rect)
    } else {
      if (current) exprRects.delete(id)
    }
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

  return {
    transact,
    db: markRaw(db),
    imports,
    editedNodeInfo,
    unconnectedEdge,
    edges,
    currentNodeIds,
    nodeRects,
    vizRects,
    exprRects,
    methodAst,
    createEdgeFromOutput,
    disconnectSource,
    disconnectTarget,
    clearUnconnected,
    createNode,
    deleteNode,
    setNodeContent,
    setExpressionContent,
    replaceNodeSubexpression,
    replaceExpressionContent,
    setNodePosition,
    setNodeVisualizationId,
    setNodeVisualizationVisible,
    stopCapturingUndo,
    updateNodeRect,
    updateVizRect,
    updateExprRect,
    setEditedNode,
    updateState,
  }
})

function randomIdent() {
  return 'operator' + Math.round(Math.random() * 100000)
}

/** An edge, which may be connected or unconnected. */
export type Edge = {
  source: ExprId | undefined
  target: ExprId | undefined
}

export type UnconnectedEdge = {
  source?: ExprId
  target?: ExprId
  /** If this edge represents an in-progress edit of a connected edge, it is identified by its target expression. */
  disconnectedEdgeTarget?: ExprId
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
