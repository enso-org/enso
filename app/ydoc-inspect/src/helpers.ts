import { type Ast, type AstId, isToken, MutableModule } from 'ydoc-shared/ast'
import { DistributedProject } from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'
import type { InspectClient } from './client.js'

interface ChannelMeta {
  id: string
  channelName: string
  type: 'json' | 'data'
  createdAt: number
}

interface MetaEntry {
  ts: number
  dir: 'send' | 'receive'
}

interface LogEntry {
  ts: number
  dir: 'send' | 'receive'
  channel: string
  data: unknown | Uint8Array
}

function formatTime(ts: number): string {
  const date = new Date(ts)
  // 'en-GB' ensures 24-hour HH:MM:SS format for fixed-length output across locales.
  const hms = date.toLocaleTimeString('en-GB', { hour12: false })
  const ms = String(date.getMilliseconds()).padStart(3, '0')
  return `${hms}.${ms}`
}

function formatData(data: unknown, maxLen: number): string {
  if (data instanceof Uint8Array) return `<${data.byteLength} bytes>`
  const json = JSON.stringify(data)
  return maxLen > 0 && json.length > maxLen ? json.slice(0, maxLen) + '...' : json
}

/** Format a log entry as a single line: `HH:MM:SS.mmm|channel >> data`. */
export function formatEntry(entry: LogEntry, maxLen: number): string {
  const time = formatTime(entry.ts)
  const arrow = entry.dir === 'send' ? '>>' : '<<'
  const channel = entry.channel
  return `${time}|${channel} ${arrow} ${formatData(entry.data, maxLen)}`
}

/**
 * Creates helper functions for inspecting ydoc channels from Chrome DevTools.
 */
export function createHelpers(doc: Y.Doc, truncate: number) {
  const channelsMap = () => doc.getMap<ChannelMeta>('channels')

  function channels(): ChannelMeta[] {
    const result: ChannelMeta[] = []
    channelsMap().forEach((value) => result.push(value))
    return result
  }

  function parseData(raw: string | Uint8Array): unknown | Uint8Array {
    if (typeof raw !== 'string') return raw
    try {
      return JSON.parse(raw)
    } catch {
      return raw
    }
  }

  function getChannelEntries(channelId: string): LogEntry[] {
    const logArray = doc.getArray<string | Uint8Array>(`log:${channelId}`)
    const metaArray = doc.getArray<string>(`meta:${channelId}`)
    const entries: LogEntry[] = []
    const len = Math.min(logArray.length, metaArray.length)
    for (let i = 0; i < len; i++) {
      const data = parseData(logArray.get(i))
      let meta: MetaEntry
      try {
        meta = JSON.parse(metaArray.get(i))
      } catch {
        continue
      }
      entries.push({ ts: meta.ts, dir: meta.dir, channel: channelId, data })
    }
    return entries
  }

  function messages(channelId?: string, n?: number): LogEntry[] {
    let entries: LogEntry[]
    if (channelId) {
      entries = getChannelEntries(channelId)
    } else {
      entries = []
      for (const meta of channels()) {
        entries.push(...getChannelEntries(meta.id))
      }
      entries.sort((a, b) => a.ts - b.ts)
    }
    return n != null ? entries.slice(-n) : entries
  }

  function filter(channelId?: string, pattern?: string | RegExp): LogEntry[] {
    const entries = messages(channelId)
    if (pattern == null) return entries
    const re = typeof pattern === 'string' ? new RegExp(pattern) : pattern
    return entries.filter((e) => {
      const text = e.data instanceof Uint8Array ? null : JSON.stringify(e.data)
      return text != null && re.test(text)
    })
  }

  function send(channelId: string, message: string | Uint8Array | object): void {
    const sndArray = doc.getArray<string | Uint8Array>(`snd:${channelId}`)
    sndArray.push([
      typeof message === 'object' && !(message instanceof Uint8Array) ?
        JSON.stringify(message)
      : message,
    ])
  }

  function receive(channelId: string, message: string | Uint8Array | object): void {
    const rcvArray = doc.getArray<string | Uint8Array>(`rcv:${channelId}`)
    rcvArray.push([
      typeof message === 'object' && !(message instanceof Uint8Array) ?
        JSON.stringify(message)
      : message,
    ])
  }

  function watch(channelId?: string): () => void {
    const ids = channelId ? [channelId] : channels().map((c) => c.id)
    const cleanups: (() => void)[] = []

    for (const id of ids) {
      const metaArray = doc.getArray<string>(`meta:${id}`)
      const logArray = doc.getArray<string | Uint8Array>(`log:${id}`)
      let lastLen = metaArray.length

      const handler = () => {
        while (lastLen < metaArray.length && lastLen < logArray.length) {
          let meta: MetaEntry
          try {
            meta = JSON.parse(metaArray.get(lastLen))
          } catch {
            lastLen++
            continue
          }
          const raw = logArray.get(lastLen)
          const data = parseData(raw)
          console.log(formatEntry({ ts: meta.ts, dir: meta.dir, channel: id, data }, truncate))
          lastLen++
        }
      }

      metaArray.observe(handler)
      cleanups.push(() => metaArray.unobserve(handler))
    }

    console.log(`Watching ${ids.length} channel(s). Call unwatch() to stop.`)
    return () => {
      for (const cleanup of cleanups) cleanup()
      console.log('Stopped watching.')
    }
  }

  return { channels, messages, filter, send, receive, watch }
}

/**
 * Creates helper functions for inspecting AST structure from Chrome DevTools. Module contents
 * live in per-module Yjs subdocs that are not synced by the project-doc WebSocket. Each module
 * is synced lazily making the ast helper commands async.
 */
export function createAstHelpers(projectDoc: Y.Doc, loadSubdoc: (subdoc: Y.Doc) => Promise<void>) {
  const project = new DistributedProject(projectDoc)

  async function getModule(moduleName?: string): Promise<MutableModule | undefined> {
    const name = moduleName ?? project.moduleNames().find((n) => n.endsWith('Main')) ?? 'Main'
    const distributed = project.openUnloadedModule(name)
    if (!distributed) return undefined
    await loadSubdoc(distributed.doc.ydoc)
    return new MutableModule(distributed.doc.ydoc)
  }

  function modules(): string[] {
    return project.moduleNames()
  }

  async function ast(moduleName?: string): Promise<Ast | undefined> {
    return (await getModule(moduleName))?.root()
  }

  async function tree(moduleName?: string, depth?: number): Promise<void> {
    const root = await ast(moduleName)
    if (!root) {
      console.log('No AST root found.')
      return
    }
    const maxDepth = depth ?? Infinity
    const printNode = (astNode: Ast, level: number) => {
      const indent = '  '.repeat(level)
      const codeSnippet = astNode.code()
      console.log(`${indent}${astNode.typeName} [${astNode.id}] ${codeSnippet}`)
      if (level >= maxDepth) return
      for (const child of astNode.children()) {
        if (!isToken(child)) printNode(child, level + 1)
      }
    }
    printNode(root, 0)
  }

  async function node(id: string): Promise<Ast | undefined> {
    for (const name of project.moduleNames()) {
      const mod = await getModule(name)
      const found = mod?.tryGet(id as AstId)
      if (found) return found
    }
    return undefined
  }

  async function meta(id: string): Promise<object | undefined> {
    const found = await node(id)
    if (!found) return undefined
    return found.serializeMetadata()
  }

  async function code(moduleName?: string): Promise<string | undefined> {
    return (await ast(moduleName))?.code()
  }

  return { modules, ast, tree, node, meta, code }
}

/**
 * Expose inspect client and helpers as globals for Chrome DevTools console.
 * Returns `watch` / `unwatch` closures that track the current watcher.
 */
export function exposeGlobals(
  client: InspectClient,
  helpers: ReturnType<typeof createHelpers>,
  astHelpers?: ReturnType<typeof createAstHelpers>,
) {
  const g = globalThis as Record<string, unknown>
  let unwatchFn: (() => void) | undefined

  g['client'] = client
  g['channels'] = helpers.channels
  g['messages'] = helpers.messages
  g['filter'] = helpers.filter
  g['send'] = helpers.send
  g['receive'] = helpers.receive
  g['watch'] = (channelId?: string) => {
    if (unwatchFn) unwatchFn()
    unwatchFn = helpers.watch(channelId)
    return unwatchFn
  }
  g['unwatch'] = () => {
    if (unwatchFn) {
      unwatchFn()
      unwatchFn = undefined
    } else {
      console.log('Not currently watching.')
    }
  }

  if (astHelpers) {
    g['modules'] = astHelpers.modules
    g['ast'] = astHelpers.ast
    g['tree'] = astHelpers.tree
    g['node'] = astHelpers.node
    g['meta'] = astHelpers.meta
    g['code'] = astHelpers.code
  }

  return {
    watch: g['watch'] as (channelId?: string) => () => void,
    unwatch: g['unwatch'] as () => void,
  }
}
