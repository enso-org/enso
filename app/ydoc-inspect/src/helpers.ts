import * as Y from 'yjs'

interface ChannelMeta {
  id: string
  channelName: string
  type: 'json' | 'binary'
  createdAt: number
}

interface MetaEntry {
  ts: number
  dir: 'send' | 'receive'
}

export interface LogEntry {
  ts: number
  dir: 'send' | 'receive'
  channel: string
  data: unknown | Uint8Array
}

/**
 * Creates helper functions for inspecting ydoc channels from Chrome DevTools.
 */
export function createHelpers(doc: Y.Doc) {
  const channelsMap = () => doc.getMap<ChannelMeta>('channels')

  function listChannels(): ChannelMeta[] {
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
      for (const meta of listChannels()) {
        entries.push(...getChannelEntries(meta.id))
      }
      entries.sort((a, b) => a.ts - b.ts)
    }
    return n != null ? entries.slice(-n) : entries
  }

  function last(channelId?: string, n = 10): LogEntry[] {
    return messages(channelId).slice(-n)
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

  function send(channelId: string, message: string | Uint8Array): void {
    const cmdArray = doc.getArray<string | Uint8Array>(`cmd:${channelId}`)
    cmdArray.push([message])
  }

  function watch(channelId?: string): () => void {
    const ids = channelId ? [channelId] : listChannels().map((c) => c.id)
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
          const arrow = meta.dir === 'send' ? '>>' : '<<'
          const displayData =
            data instanceof Uint8Array ?
              `<binary ${data.byteLength} bytes>`
            : JSON.stringify(data).slice(0, 200)
          console.log(`[${id}] ${arrow} ${displayData}`)
          lastLen++
        }
      }

      metaArray.observe(handler)
      cleanups.push(() => metaArray.unobserve(handler))
    }

    console.log(`Watching ${ids.length} channel(s). Call the returned function to stop.`)
    return () => {
      for (const cleanup of cleanups) cleanup()
      console.log('Stopped watching.')
    }
  }

  return { listChannels, messages, last, filter, send, watch }
}
