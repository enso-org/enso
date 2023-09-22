import { assert, assertNever } from '@/util/assert'
import { useObserveYjs } from '@/util/crdt'
import { parseEnso } from '@/util/ffi'
import type { Opt } from '@/util/opt'
import { Vec2 } from '@/util/vec2'
import * as map from 'lib0/map'
import * as set from 'lib0/set'
import { defineStore } from 'pinia'
import {
  rangeIntersects,
  type ContentRange,
  type ExprId,
  type IdMap,
  type NodeMetadata,
} from 'shared/yjsModel'
import { computed, reactive, ref, watch, watchEffect } from 'vue'
import * as Y from 'yjs'
import { useProjectStore } from './project'

export const useGraphStore = defineStore('graph', () => {
  const proj = useProjectStore()

  proj.setObservedFileName('Main.enso')

  const text = computed(() => proj.module?.contents)
  const metadata = computed(() => proj.module?.metadata)

  const textContent = ref('')

  const nodes = reactive(new Map<ExprId, Node>())
  const exprNodes = reactive(new Map<ExprId, ExprId>())

  useObserveYjs(text, (event) => {
    const delta = event.changes.delta
    if (delta.length === 0) return

    const affectedRanges: ContentRange[] = []
    function addAffectedRange(range: ContentRange) {
      const lastRange = affectedRanges[affectedRanges.length - 1]
      if (lastRange?.[1] === range[0]) {
        lastRange[1] = range[1]
      } else {
        affectedRanges.push(range)
      }
    }

    let newContent = ''
    let oldIdx = 0
    let newIdx = 0
    for (const op of delta) {
      if (op.retain) {
        newContent += textContent.value.substring(oldIdx, oldIdx + op.retain)
        oldIdx += op.retain
        newIdx += op.retain
      } else if (op.delete) {
        addAffectedRange([newIdx, newIdx])
        oldIdx += op.delete
      } else if (op.insert && typeof op.insert === 'string') {
        addAffectedRange([newIdx, newIdx + op.insert.length])
        newContent += op.insert
        newIdx += op.insert.length
      } else {
        console.error('Unexpected Yjs operation:', op)
      }
    }
    newContent += textContent.value.substring(oldIdx)
    textContent.value = newContent
    updateState(affectedRanges)
  })

  watch(text, (value) => {
    textContent.value = value?.toString() ?? ''
    if (value != null) updateState()
  })

  const _parsed = ref([] as Statement[])
  const _parsedEnso = ref<any>()

  watchEffect(() => {})

  function updateState(affectedRanges?: ContentRange[]) {
    const module = proj.module
    if (module == null) return
    module.doc.transact(() => {
      const idMap = module.getIdMap()
      const meta = module.metadata
      const text = module.contents
      const textContentLocal = textContent.value
      const parsed = parseBlock(0, textContentLocal, idMap)

      _parsed.value = parsed
      _parsedEnso.value = parseEnso(textContentLocal)

      const accessed = idMap.accessedSoFar()

      for (const nodeId of nodes.keys()) {
        if (!accessed.has(nodeId)) {
          nodeDeleted(nodeId)
        }
      }
      idMap.finishAndSynchronize()

      for (const stmt of parsed) {
        const exprRange: ContentRange = [stmt.exprOffset, stmt.exprOffset + stmt.expression.length]

        if (affectedRanges != null) {
          while (affectedRanges[0]?.[1] < exprRange[0]) {
            affectedRanges.shift()
          }
          if (affectedRanges.length === 0) break
          const nodeAffected = rangeIntersects(exprRange, affectedRanges[0])
          if (!nodeAffected) continue
        }

        const nodeId = stmt.expression.id
        const node = nodes.get(nodeId)
        const nodeMeta = meta.get(nodeId)
        const nodeContent = textContentLocal.substring(exprRange[0], exprRange[1])
        if (node == null) {
          nodeInserted(stmt, text, nodeContent, nodeMeta)
        } else {
          nodeUpdated(node, stmt, text, nodeContent, nodeMeta)
        }
      }
    })
  }

  useObserveYjs(metadata, (event) => {
    const meta = event.target
    for (const [id, op] of event.changes.keys) {
      if (op.action === 'update') {
        const data = meta.get(id)
        const node = nodes.get(id as ExprId)
        if (data != null && node != null) {
          const pos = new Vec2(data.x, -data.y)
          if (!node.position.equals(pos)) {
            node.position = pos
          }
        }
      } else {
        console.log(op)
      }
    }
  })

  const identDefinitions = reactive(new Map<string, ExprId>())
  const identUsages = reactive(new Map<string, Set<ExprId>>())

  function nodeInserted(stmt: Statement, text: Y.Text, content: string, meta: Opt<NodeMetadata>) {
    const nodeId = stmt.expression.id
    const node: Node = {
      content,
      binding: stmt.binding ?? '',
      rootSpan: stmt.expression,
      position: meta == null ? Vec2.Zero() : new Vec2(meta.x, -meta.y),
      docRange: [
        Y.createRelativePositionFromTypeIndex(text, stmt.exprOffset),
        Y.createRelativePositionFromTypeIndex(text, stmt.exprOffset + stmt.expression.length),
      ],
    }
    identDefinitions.set(node.binding, nodeId)
    addSpanUsages(nodeId, node)
    nodes.set(nodeId, node)
  }

  function nodeUpdated(
    node: Node,
    stmt: Statement,
    text: Y.Text,
    content: string,
    meta: Opt<NodeMetadata>,
  ) {
    clearSpanUsages(stmt.id, node)
    node.content = content
    if (node.binding !== stmt.binding) {
      identDefinitions.delete(node.binding)
      node.binding = stmt.binding ?? ''
      identDefinitions.set(node.binding, stmt.id)
    }
    if (node.rootSpan.id === stmt.expression.id) {
      patchSpan(node.rootSpan, stmt.expression)
    } else {
      node.rootSpan = stmt.expression
    }
    if (meta != null && !node.position.equals(new Vec2(meta.x, -meta.y))) {
      node.position = new Vec2(meta.x, -meta.y)
    }
  }

  function addSpanUsages(id: ExprId, node: Node) {
    for (const [span, offset] of walkSpansBfs(node.rootSpan)) {
      exprNodes.set(span.id, id)
      const ident = node.content.substring(offset, offset + span.length)
      if (span.kind === SpanKind.Ident) {
        map.setIfUndefined(identUsages, ident, set.create).add(span.id)
      }
    }
  }

  function clearSpanUsages(id: ExprId, node: Node) {
    for (const [span, offset] of walkSpansBfs(node.rootSpan)) {
      exprNodes.delete(span.id)
      if (span.kind === SpanKind.Ident) {
        const ident = node.content.substring(offset, offset + span.length)
        const usages = identUsages.get(ident)
        if (usages != null) {
          usages.delete(span.id)
          if (usages.size === 0) {
            identUsages.delete(ident)
          }
        }
      }
    }
  }

  function nodeDeleted(id: ExprId) {
    const node = nodes.get(id)
    nodes.delete(id)
    if (node != null) {
      identDefinitions.delete(node.binding)
      clearSpanUsages(id, node)
    }
  }

  function patchSpan(span: Span, newSpan: Span) {
    assert(span.id === newSpan.id)
    // TODO: deep patching of children of matching ID
    span.length = newSpan.length
    span.kind = newSpan.kind
    span.children = newSpan.children
    // for (let i = 0; i < span.children.length; i++) {
    //   patchSpan(span.children[i], newSpan.children[i])
    // }
  }

  function generateUniqueIdent() {
    let ident: string
    do {
      ident = randomString()
    } while (identDefinitions.has(ident))
    return ident
  }

  const edges = computed(() => {
    const edges = []
    for (const [ident, usages] of identUsages) {
      const source = identDefinitions.get(ident)
      if (source == null) continue
      for (const target of usages) {
        edges.push({ source, target })
      }
    }
    return edges
  })

  function createNode(position: Vec2, expression: string): Opt<ExprId> {
    const mod = proj.module
    if (mod == null) return
    const { contents } = mod

    const meta: NodeMetadata = {
      x: position.x,
      y: -position.y,
    }
    const ident = generateUniqueIdent()
    const content = `${ident} = ${expression}`
    return mod.insertNewNode(contents.length, content, meta)
  }

  function deleteNode(id: ExprId) {
    const mod = proj.module
    if (mod == null) return
    mod.replaceExpressionContent(id, '')
  }

  function setNodeContent(id: ExprId, content: string) {
    const node = nodes.get(id)
    if (node == null) return
    setExpressionContent(node.rootSpan.id, content)
  }

  function setExpressionContent(id: ExprId, content: string) {
    proj.module?.replaceExpressionContent(id, content)
  }

  function replaceNodeSubexpression(id: ExprId, range: ContentRange, content: string) {
    const node = nodes.get(id)
    if (node == null) return
    proj.module?.replaceExpressionContent(node.rootSpan.id, content, range)
  }

  function setNodePosition(id: ExprId, position: Vec2) {
    const node = nodes.get(id)
    if (node == null) return
    proj.module?.updateNodeMetadata(id, { x: position.x, y: -position.y })
  }

  return {
    _parsed,
    _parsedEnso: _parsedEnso,
    nodes,
    exprNodes,
    edges,
    identDefinitions,
    identUsages,
    createNode,
    deleteNode,
    setNodeContent,
    setExpressionContent,
    replaceNodeSubexpression,
    setNodePosition,
  }
})

function randomString() {
  return Math.random().toString(36).substring(2, 10)
}

export interface Node {
  content: string
  binding: string
  rootSpan: Span
  position: Vec2
  docRange: [Y.RelativePosition, Y.RelativePosition]
}

export const enum SpanKind {
  Root = 0,
  Spacing,
  Group,
  Token,
  Ident,
  Literal,
}

export function spanKindName(kind: SpanKind): string {
  switch (kind) {
    case SpanKind.Root:
      return 'Root'
    case SpanKind.Spacing:
      return 'Spacing'
    case SpanKind.Group:
      return 'Group'
    case SpanKind.Token:
      return 'Token'
    case SpanKind.Ident:
      return 'Ident'
    case SpanKind.Literal:
      return 'Literal'
    default:
      assertNever(kind)
  }
}

export interface Span {
  id: ExprId
  kind: SpanKind
  length: number
  children: Span[]
}

function walkSpansBfs(
  span: Span,
  offset: number = 0,
  visitChildren?: (span: Span, offset: number) => boolean,
): IterableIterator<[Span, number]> {
  const stack: [Span, number][] = [[span, offset]]
  return {
    next() {
      if (stack.length === 0) {
        return { done: true, value: undefined }
      }
      const [span, spanOffset] = stack.shift()!
      if (visitChildren?.(span, spanOffset) !== false) {
        let offset = spanOffset
        for (let i = 0; i < span.children.length; i++) {
          const child = span.children[i]
          stack.push([child, offset])
          offset += child.length
        }
      }
      return { done: false, value: [span, spanOffset] }
    },
    [Symbol.iterator]() {
      return this
    },
  }
}

export interface Edge {
  source: ExprId
  target: ExprId
}

interface Statement {
  id: ExprId
  binding: Opt<string>
  exprOffset: number
  expression: Span
}

function parseBlock(offset: number, content: string, idMap: IdMap): Statement[] {
  const stmtRegex = /^( *)(([a-zA-Z0-9_]+) *= *)?(.*)$/gm
  const stmts: Statement[] = []
  content.replace(stmtRegex, (stmt, indent, beforeExpr, binding, expr, index) => {
    if (stmt.trim().length === 0) return stmt
    const pos = offset + index + indent.length
    const id = idMap.getOrInsertUniqueId([pos, pos + stmt.length - indent.length])
    const exprOffset = pos + (beforeExpr?.length ?? 0)
    stmts.push({
      id,
      binding,
      exprOffset,
      expression: parseNodeExpression(exprOffset, expr, idMap),
    })
    return stmt
  })
  return stmts
}

function parseNodeExpression(offset: number, content: string, idMap: IdMap): Span {
  const root = mkSpanGroup(SpanKind.Root)
  let span: Span = root
  let spanOffset = offset
  const stack: [Span, number][] = []

  const tokenRegex = /(?:(".*?"|[0-9]+\b)|(\s+)|([a-zA-Z0-9_]+)|(.))/g
  content.replace(tokenRegex, (token, tokLit, tokSpace, tokIdent, tokSymbol, index) => {
    const pos = offset + index
    if (tokSpace != null) {
      span.children.push(mkSpan(idMap, SpanKind.Spacing, pos, token.length))
    } else if (tokIdent != null) {
      span.children.push(mkSpan(idMap, SpanKind.Ident, pos, token.length))
    } else if (tokLit != null) {
      span.children.push(mkSpan(idMap, SpanKind.Literal, pos, token.length))
    } else if (tokSymbol != null) {
      if (token === '(') {
        stack.push([span, spanOffset])
        span = mkSpanGroup(SpanKind.Group)
        spanOffset = pos
      }

      span.children.push(mkSpan(idMap, SpanKind.Token, pos, token.length))

      if (token === ')') {
        const popped = stack.pop()
        if (popped != null) {
          finishSpanGroup(span, idMap, spanOffset)
          popped[0].children.push(span)
          span = popped[0]
          spanOffset = popped[1]
        }
      }
    }
    return token
  })

  let popped
  while ((popped = stack.pop())) {
    finishSpanGroup(span, idMap, spanOffset)
    popped[0].children.push(span)
    span = popped[0]
    spanOffset = popped[1]
  }

  finishSpanGroup(root, idMap, offset)
  return root
}

const NULL_ID: ExprId = '00000-' as ExprId

function mkSpanGroup(kind: SpanKind): Span {
  return {
    id: NULL_ID,
    kind,
    length: 0,
    children: [],
  }
}

function mkSpan(idMap: IdMap, kind: SpanKind, offset: number, length: number): Span {
  const range: ContentRange = [offset, offset + length]
  return {
    id: idMap.getOrInsertUniqueId(range),
    kind,
    length,
    children: [],
  }
}

function finishSpanGroup(span: Span, idMap: IdMap, offset: number) {
  const totalLength = span.children.reduce((acc, child) => acc + child.length, 0)
  span.length = totalLength
  span.id = idMap.getOrInsertUniqueId([offset, offset + span.length])
}
