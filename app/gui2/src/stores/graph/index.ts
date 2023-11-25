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
import { DEFAULT_VISUALIZATION_IDENTIFIER } from '@/stores/visualization'
import { Ast, AstExtended, childrenAstNodes, findAstWithRange, readAstSpan } from '@/util/ast'
import { useObserveYjs } from '@/util/crdt'
import type { Opt } from '@/util/opt'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { defineStore } from 'pinia'
import type { StackItem } from 'shared/languageServerTypes'
import {
  decodeRange,
  visMetadataEquals,
  type ContentRange,
  type ExprId,
  type NodeMetadata,
  type VisualizationIdentifier,
  type VisualizationMetadata,
} from 'shared/yjsModel'
import { computed, markRaw, reactive, ref, toRef, watch } from 'vue'
import * as Y from 'yjs'

export { type Node } from '@/stores/graph/graphDatabase'

export interface NodeEditInfo {
  id: ExprId
  range: ContentRange
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
  const exprRects = reactive(new Map<ExprId, Rect>())
  const editedNodeInfo = ref<NodeEditInfo>()
  const imports = ref<{ import: Import; span: ContentRange }[]>([])

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

      const ast = AstExtended.parse(textContentLocal, idMap)
      const updatedMap = idMap.finishAndSynchronize()

      imports.value = []
      ast.visitRecursive((node) => {
        if (node.isTree(Ast.Tree.Type.Import)) {
          const recognized = recognizeImport(node)
          if (recognized) {
            imports.value.push({ import: recognized, span: node.span() })
          }
          return false
        }
        return true
      })

      const methodAst =
        ast.isTree() &&
        ast.tryMap((tree) =>
          getExecutedMethodAst(
            tree,
            textContentLocal,
            proj.executionContext.getStackTop(),
            updatedMap,
          ),
        )
      if (methodAst) {
        db.readFunctionAst(methodAst, (id) => meta.get(id))
      }
    })
  }

  useObserveYjs(metadata, (event) => {
    const meta = event.target
    for (const [id, op] of event.changes.keys) {
      if (op.action === 'update' || op.action === 'add') {
        const data = meta.get(id)
        const node = db.nodeIdToNode.get(id as ExprId)
        if (data && node) {
          db.assignUpdatedMetadata(node, data)
        }
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

  // Create a node from a source expression, and insert it into the graph. The return value will be
  // the new node's ID, or `null` if the node creation fails.
  function createNodeFromSource(position: Vec2, source: ExprId): Opt<ExprId> {
    const sourceNodeName = db.getNodeMainOutputPortIdentifier(source)
    const sourceNodeNameWithDot = sourceNodeName ? sourceNodeName + '.' : ''
    return createNode(position, sourceNodeNameWithDot)
  }

  function deleteNode(id: ExprId) {
    const node = db.nodeIdToNode.get(id)
    if (!node) return
    proj.module?.deleteExpression(node.outerExprId)
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

  function replaceNodeSubexpression(nodeId: ExprId, range: ContentRange, content: string) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.replaceExpressionContent(node.rootSpan.astId, content, range)
  }

  function setNodePosition(nodeId: ExprId, position: Vec2) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.updateNodeMetadata(nodeId, { x: position.x, y: -position.y })
  }

  function normalizeVisMetadata(
    id: Opt<VisualizationIdentifier>,
    visible?: boolean,
  ): VisualizationMetadata | null {
    const vis: VisualizationMetadata = {
      ...(id ?? DEFAULT_VISUALIZATION_IDENTIFIER),
      visible: visible ?? false,
    }
    if (
      visMetadataEquals(vis, {
        ...DEFAULT_VISUALIZATION_IDENTIFIER,
        visible: false,
      })
    )
      return null
    return vis
  }

  function setNodeVisualizationId(nodeId: ExprId, vis: Opt<VisualizationIdentifier>) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.updateNodeMetadata(nodeId, { vis: normalizeVisMetadata(vis, node.vis?.visible) })
  }

  function setNodeVisualizationVisible(nodeId: ExprId, visible: boolean) {
    const node = db.nodeIdToNode.get(nodeId)
    if (!node) return
    proj.module?.updateNodeMetadata(nodeId, { vis: normalizeVisMetadata(node.vis, visible) })
  }

  function updateNodeRect(id: ExprId, rect: Rect) {
    if (rect.pos.equals(Vec2.Zero) && !metadata.value?.has(id)) {
      const { position } = nonDictatedPlacement(rect.size, {
        nodeRects: [...nodeRects.entries()]
          .filter(([id]) => db.nodeIdToNode.get(id))
          .map(([, rect]) => rect),
        // The rest of the properties should not matter.
        selectedNodeRects: [],
        screenBounds: Rect.Zero,
        mousePosition: Vec2.Zero,
      })
      metadata.value?.set(id, { x: position.x, y: -position.y, vis: null })
      nodeRects.set(id, new Rect(position, rect.size))
    } else {
      nodeRects.set(id, rect)
    }
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
    const range: ContentRange = [cursorPosition, cursorPosition]
    editedNodeInfo.value = { id, range }
  }

  return {
    transact,
    db: markRaw(db),
    imports,
    editedNodeInfo,
    unconnectedEdge,
    edges,
    nodeRects,
    exprRects,
    createEdgeFromOutput,
    disconnectSource,
    disconnectTarget,
    clearUnconnected,
    createNode,
    deleteNode,
    setNodeContent,
    setExpressionContent,
    replaceNodeSubexpression,
    setNodePosition,
    setNodeVisualizationId,
    setNodeVisualizationVisible,
    stopCapturingUndo,
    updateNodeRect,
    updateExprRect,
    setEditedNode,
    createNodeFromSource,
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
  ast: Ast.Tree,
  code: string,
  executionStackTop: StackItem,
  updatedIdMap: Y.Map<Uint8Array>,
): Ast.Tree.Function | undefined {
  switch (executionStackTop.type) {
    case 'ExplicitCall': {
      // Assume that the provided AST matches the module in the method pointer. There is no way to
      // actually verify this assumption at this point.
      const ptr = executionStackTop.methodPointer
      const name = ptr.name
      return findModuleMethod(ast, code, name)
    }
    case 'LocalCall': {
      const exprId = executionStackTop.expressionId
      const range = lookupIdRange(updatedIdMap, exprId)
      if (!range) return
      const node = findAstWithRange(ast, range)
      if (node?.type === Ast.Tree.Type.Function) return node
    }
  }
}

function lookupIdRange(updatedIdMap: Y.Map<Uint8Array>, id: ExprId): [number, number] | undefined {
  const doc = updatedIdMap.doc!
  const rangeBuffer = updatedIdMap.get(id)
  if (!rangeBuffer) return
  const decoded = decodeRange(rangeBuffer)
  const index = Y.createAbsolutePositionFromRelativePosition(decoded[0], doc)?.index
  const endIndex = Y.createAbsolutePositionFromRelativePosition(decoded[1], doc)?.index
  if (index == null || endIndex == null) return
  return [index, endIndex]
}

function findModuleMethod(
  moduleAst: Ast.Tree,
  code: string,
  methodName: string,
): Ast.Tree.Function | undefined {
  for (const node of childrenAstNodes(moduleAst)) {
    if (node.type === Ast.Tree.Type.Function && readAstSpan(node.name, code) === methodName)
      return node
  }
}
