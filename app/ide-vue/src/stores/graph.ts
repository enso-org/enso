import { computed, reactive } from 'vue'
import { defineStore } from 'pinia'
import { map, set } from 'lib0'
import { Vec2 } from '@/util/vec2'
import { assertNever } from '@/util/assert'

export const useGraphStore = defineStore('graph', () => {
  let nextId = 1n
  const nodes = reactive(new Map<NodeId, Node>())

  const identDefinitions = reactive(new Map<string, NodeId>())
  const identUsages = reactive(new Map<string, Set<ExprId>>())

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

  function createNode(position: Vec2): NodeId {
    const id = nextId++ as NodeId
    const node: Node = {
      content: '',
      binding: generateUniqueIdent(),
      rootSpan: mkSpan(new Map(), id, SpanKind.Root, 0, 0),
      position,
    }
    nodes.set(id, node)
    identDefinitions.set(node.binding, id)
    return id
  }

  function setNodeContent(id: NodeId, content: string, idMap: IdMap = new Map()) {
    const node = nodes.get(id)

    if (node != null) {
      for (let span of walkSpans(node.rootSpan)) {
        if (span.kind === SpanKind.Ident) {
          let ident = node.content.substring(span.range.start, span.range.end)
          identUsages.get(ident)?.delete(span.id)
        }
      }
      node.content = content
      node.rootSpan = parseNodeExpression(id, node.content, idMap)

      for (let span of walkSpans(node.rootSpan)) {
        if (span.kind === SpanKind.Ident) {
          let ident = node.content.substring(span.range.start, span.range.end)
          map.setIfUndefined(identUsages, ident, set.create).add(span.id)
        }
      }
    }
  }

  function replaceNodeSubexpression(id: NodeId, range: ContentRange, content: string) {
    const node = nodes.get(id)
    if (node == null) return

    const indexAdjust = content.length - (range.end - range.start)
    const idMap = new Map()

    for (const span of walkSpans(node.rootSpan)) {
      const adjustedRange: ContentRange = {
        start: span.range.start >= range.end ? span.range.start + indexAdjust : span.range.start,
        end: span.range.end >= range.end ? span.range.end + indexAdjust : span.range.end,
      }
      idMap.set(idMapKey(adjustedRange), span.id)
    }

    const newContent =
      node.content.substring(0, range.start) + content + node.content.substring(range.end)
    setNodeContent(id, newContent, idMap)
  }

  function updateNodeContentAutoDiff(id: NodeId, content: string) {
    const node = nodes.get(id)
    if (node == null) return
    const commonPrefix = commonPrefixLength(node.content, content)
    const commonSuffix = commonSuffixLength(node.content, content)
    const updateRange = { start: commonPrefix, end: node.content.length - commonSuffix }
    const updateContent = content.substring(commonPrefix, content.length - commonSuffix)
    console.log(updateRange, updateContent)
    replaceNodeSubexpression(id, updateRange, updateContent)
  }

  return {
    nodes,
    edges,
    createNode,
    setNodeContent,
    replaceNodeSubexpression,
    updateNodeContentAutoDiff,
  }
})

function commonPrefixLength(a: string, b: string): number {
  const commonLen = Math.min(a.length, b.length)
  for (let i = 0; i < commonLen; i++) {
    if (a[i] !== b[i]) {
      return i
    }
  }
  return commonLen
}

function commonSuffixLength(a: string, b: string): number {
  const commonLen = Math.min(a.length, b.length)
  for (let i = 0; i < commonLen; i++) {
    if (a[a.length - i - 1] !== b[b.length - i - 1]) {
      return i
    }
  }
  return commonLen
}

function randomString() {
  return Math.random().toString(36).substring(2, 10)
}

declare const BRAND_ExprId: unique symbol
export type ExprId = bigint & { [BRAND_ExprId]: never }

declare const BRAND_NodeId: unique symbol
export type NodeId = bigint & { [BRAND_NodeId]: never }

export interface Node {
  content: string
  binding: string
  rootSpan: Span
  position: Vec2
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
  range: ContentRange
  children: Span[]
}

function findSpanEnclosing(span: Span, range: ContentRange): Span | null {
  let deepestSpan = null
  for (let innerSpan of walkSpans(span, (s) => rangeEncloses(s.range, range))) {
    if (rangeEncloses(innerSpan.range, range)) {
      deepestSpan = innerSpan
    }
  }
  return deepestSpan
}

function walkSpans(span: Span, visitChildren?: (span: Span) => boolean): IterableIterator<Span> {
  let stack: Span[] = [span]
  return {
    next() {
      if (stack.length === 0) {
        return { done: true, value: undefined }
      }
      const span = stack.pop()!
      if (visitChildren?.(span) !== false) {
        stack.push(...span.children)
      }
      return { done: false, value: span }
    },
    [Symbol.iterator]() {
      return this
    },
  }
}

export interface ContentRange {
  start: number
  end: number
}

function rangeEncloses(a: ContentRange, b: ContentRange): boolean {
  return a.start <= b.start && a.end >= b.end
}

export interface TargetEndpoint {
  node: NodeId
  port: ExprId
}

export interface Edge {
  source: NodeId
  target: ExprId
}

const NULL_ID: ExprId = 0n as ExprId

let _nextExprId = 1n
function nextExprId(node: NodeId): ExprId {
  return ((node << 64n) | _nextExprId++) as ExprId
}

type IdMap = Map<string, ExprId>
function idMapKey(range: ContentRange) {
  return `${range.start}-${range.end}`
}

function queryIdMap(idMap: IdMap, range: ContentRange): ExprId | undefined {
  return idMap.get(idMapKey(range))
}

function parseNodeExpression(node: NodeId, content: string, idMap: IdMap): Span {
  const root = mkSpanGroup(SpanKind.Root, 0)
  let span: Span = root
  const stack: Span[] = []

  const tokenRegex = /(?:(\".*?\"|[0-9]+\b)|(\s+)|([a-zA-Z0-9_]+)|(.))/g
  content.replace(tokenRegex, (token, tokLit, tokSpace, tokIdent, tokSymbol, index) => {
    if (tokSpace != null) {
      span.children.push(mkSpan(idMap, node, SpanKind.Spacing, index, token.length))
    } else if (tokIdent != null) {
      span.children.push(mkSpan(idMap, node, SpanKind.Ident, index, token.length))
    } else if (tokLit != null) {
      span.children.push(mkSpan(idMap, node, SpanKind.Literal, index, token.length))
    } else if (tokSymbol != null) {
      if (token === '(') {
        stack.push(span)
        span = mkSpanGroup(SpanKind.Group, index)
      }

      span.children.push(mkSpan(idMap, node, SpanKind.Token, index, token.length))

      if (token === ')') {
        const popped = stack.pop()
        if (popped != null) {
          finishSpanGroup(span, idMap, node)
          popped.children.push(span)
          span = popped
        }
      }
    }
    return ''
  })

  let popped
  while ((popped = stack.pop())) {
    finishSpanGroup(span, idMap, node)
    popped.children.push(span)
    span = popped
  }

  finishSpanGroup(root, idMap, node)
  return root
}

function mkSpanGroup(kind: SpanKind, offset: number): Span {
  return {
    id: NULL_ID,
    kind,
    range: { start: offset, end: offset },
    children: [],
  }
}

function mkSpan(idMap: IdMap, node: NodeId, kind: SpanKind, offset: number, len: number): Span {
  const range = { start: offset, end: offset + len }
  return {
    id: queryIdMap(idMap, range) ?? nextExprId(node),
    kind,
    range,
    children: [],
  }
}

function finishSpanGroup(span: Span, idMap: IdMap, node: NodeId) {
  let lastChild = span.children[span.children.length - 1]
  if (lastChild != null && lastChild != null) {
    span.range.end = lastChild.range.end
  }
  span.id = queryIdMap(idMap, span.range) ?? nextExprId(node)
}
