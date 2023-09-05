import { computed, reactive, ref, watch, watchEffect } from 'vue'
import { defineStore } from 'pinia'
import { map, set } from 'lib0'
import { Vec2 } from '@/util/vec2'
import { assertNever, assert } from '@/util/assert'
import { useProjectStore } from './project'
import * as Y from 'yjs'
import { useObserveYjs, useObserveYjsDeep } from '@/util/crdt'
import {
  rangeEncloses,
  type ContentRange,
  type ExprId,
  type IdMap,
  type NodeMetadata,
} from '../../shared/yjs-model'
import type { Opt } from '@/util/opt'

export const useGraphStore = defineStore('graph', () => {
  const proj = useProjectStore()

  proj.setProjectName('test')
  proj.setObservedFileName('Main.enso')

  let text = computed(() => proj.module?.contents)
  let metadata = computed(() => proj.module?.metadata)

  const nodes = reactive(new Map<ExprId, Node>())
  const exprNodes = reactive(new Map<ExprId, ExprId>())

  useObserveYjs(text, (event) => {
    readState()
  })

  watch(text, (value) => {
    if (value != null) readState()
  })

  const _parsed = ref([] as Statement[])

  function readState() {
    if (proj.module == null) return
    const idMap = proj.module.getIdMap()
    const meta = proj.module.metadata
    const text = proj.module.contents
    const textContent = text.toString()
    const parsed = parseBlock(0, textContent, idMap)
    _parsed.value = parsed

    const accessed = idMap.accessedSoFar()

    for (const nodeId of nodes.keys()) {
      if (!accessed.has(nodeId)) {
        nodeDeleted(nodeId)
      }
    }
    for (const stmt of parsed) {
      const nodeMeta = meta.get(stmt.id)
      nodeInsertedOrUpdated(stmt, text, textContent, nodeMeta)
    }

    idMap.finishAndSynchronize()
  }

  useObserveYjs(metadata, (event) => {
    const meta = event.target
    for (const [id, op] of event.changes.keys) {
      if (op.action === 'update') {
        const data = meta.get(id)
        let node = nodes.get(id as ExprId)
        if (data != null && node != null) {
          const pos = new Vec2(data.x, data.y)
          if (!node.position.equals(pos)) {
            node.position = pos
          }
        }
      }
    }
  })

  const identDefinitions = reactive(new Map<string, ExprId>())
  const identUsages = reactive(new Map<string, Set<ExprId>>())

  function nodeInsertedOrUpdated(
    stmt: Statement,
    text: Y.Text,
    moduleContent: string,
    meta: Opt<NodeMetadata>,
  ) {
    const id = stmt.id
    let node = nodes.get(id)
    const content = moduleContent.substring(
      stmt.exprOffset,
      stmt.exprOffset + stmt.expression.length,
    )
    if (node == null) {
      node = {
        content,
        binding: stmt.binding ?? '',
        rootSpan: stmt.expression,
        position: meta == null ? Vec2.Zero() : new Vec2(meta.x, meta.y),
        docRange: [
          Y.createRelativePositionFromTypeIndex(text, stmt.exprOffset),
          Y.createRelativePositionFromTypeIndex(text, stmt.exprOffset + stmt.expression.length),
        ],
      }
      identDefinitions.set(node.binding, id)
      addSpanUsages(id, node)
      nodes.set(id, node)
    } else {
      clearSpanUsages(id, node)
      if (node.content !== content) {
        node.content = content
        for (let [span, offset] of walkSpansBfs(stmt.expression, stmt.exprOffset)) {
          if (span.kind === SpanKind.Ident) {
            let ident = moduleContent.substring(offset, offset + span.length)
            map.setIfUndefined(identUsages, ident, set.create).add(span.id)
            exprNodes.set(span.id, id)
          }
        }
      }
      if (node.binding !== stmt.binding) {
        identDefinitions.delete(node.binding)
        node.binding = stmt.binding ?? ''
        identDefinitions.set(node.binding, id)
      }
      if (node.rootSpan.id === stmt.expression.id) {
        patchSpan(node.rootSpan, stmt.expression)
      } else {
        node.rootSpan = stmt.expression
      }
      if (meta != null && !node.position.equals(new Vec2(meta.x, meta.y))) {
        node.position = new Vec2(meta.x, meta.y)
      }
    }

    addSpanUsages(id, node)
  }

  function addSpanUsages(id: ExprId, node: Node) {
    for (let [span, offset] of walkSpansBfs(node.rootSpan)) {
      exprNodes.set(span.id, id)
      let ident = node.content.substring(offset, offset + span.length)
      if (span.kind === SpanKind.Ident) {
        map.setIfUndefined(identUsages, ident, set.create).add(span.id)
      }
    }
  }

  function clearSpanUsages(id: ExprId, node: Node) {
    for (let [span, offset] of walkSpansBfs(node.rootSpan)) {
      exprNodes.delete(span.id)
      if (span.kind === SpanKind.Ident) {
        let ident = node.content.substring(offset, offset + span.length)
        let usages = identUsages.get(ident)
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
    for (let [ident, usages] of identUsages) {
      let source = identDefinitions.get(ident)
      if (source == null) continue
      for (let target of usages) {
        edges.push({ source, target })
      }
    }
    return edges
  })

  function createNode(position: Vec2): Opt<ExprId> {
    const mod = proj.module
    if (mod == null) return
    const { contents } = mod

    const meta: NodeMetadata = {
      x: position.x,
      y: position.y,
    }
    const ident = generateUniqueIdent()
    const content = `${ident} = x`
    return mod.insertNewNode(contents.length, content, meta)
  }

  function deleteNode(id: ExprId) {
    const mod = proj.module
    if (mod == null) return
    mod.setExpressionContent(id, '')
  }

  function setNodeContent(id: ExprId, content: string) {
    const node = nodes.get(id)
    if (node == null) return
    setExpressionContent(node.rootSpan.id, content)
  }

  function setExpressionContent(id: ExprId, content: string) {
    proj.module?.setExpressionContent(id, content)
  }

  function replaceNodeSubexpression(id: ExprId, range: ContentRange, content: string) {
    const node = nodes.get(id)
    if (node == null) return
    const newContent =
      node.content.substring(0, range[0]) + content + node.content.substring(range[1])
    setExpressionContent(node.rootSpan.id, newContent)
  }

  function setNodePosition(id: ExprId, position: Vec2) {
    const node = nodes.get(id)
    if (node == null) return
    proj.module?.updateNodeMetadata(id, { x: position.x, y: position.y })
  }

  return {
    _parsed,
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

function findSpanEnclosing(
  span: Span,
  spanOffset: number,
  range: ContentRange,
): [Span, number] | null {
  let deepestSpan: [Span, number] | null = null
  for (let [innerSpan, offset] of walkSpansBfs(span, spanOffset, (s, offset) =>
    rangeEncloses([offset, offset + s.length], range),
  )) {
    if (rangeEncloses([offset, offset + span.length], range)) {
      deepestSpan = [innerSpan, offset]
    }
  }
  return deepestSpan
}

function walkSpansBfs(
  span: Span,
  offset: number = 0,
  visitChildren?: (span: Span, offset: number) => boolean,
): IterableIterator<[Span, number]> {
  let stack: [Span, number][] = [[span, offset]]
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
  content.replace(stmtRegex, (stmt, ident, beforeExpr, binding, expr, index) => {
    if (stmt.trim().length === 0) return stmt
    const pos = offset + index + ident.length
    const id = idMap.getOrInsertUniqueId([pos, pos + stmt.length])
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

  const tokenRegex = /(?:(\".*?\"|[0-9]+\b)|(\s+)|([a-zA-Z0-9_]+)|(.))/g
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
