import { InspectClient } from './client.js'
import { createHelpers, type LogEntry } from './helpers.js'

function parseArgs(): { host: string; port: string; watch: boolean } {
  const args = process.argv.slice(2)
  let host = 'localhost'
  let port = '30617'
  let watch = true
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--host' && args[i + 1]) host = args[++i]!
    else if (args[i] === '--port' && args[i + 1]) port = args[++i]!
    else if (args[i] === '--no-watch') watch = false
  }
  return { host, port, watch }
}

function printEntry(entry: LogEntry): void {
  const arrow = entry.dir === 'send' ? '>>' : '<<'
  const data =
    entry.data instanceof Uint8Array ?
      `<binary ${entry.data.byteLength} bytes>`
    : JSON.stringify(entry.data).slice(0, 200)
  console.log(`[${entry.channel}] ${arrow} ${data}`)
}

const RETRY_INTERVAL_MS = 2000
const SYNC_TIMEOUT_MS = 5000

const { host, port, watch } = parseArgs()
const url = `ws://${host}:${port}/project/inspect`

const client = new InspectClient()
const helpers = createHelpers(client.doc)

let unwatchFn: (() => void) | undefined

// Expose as globals for Chrome DevTools console
const g = globalThis as Record<string, unknown>
g['client'] = client
g['doc'] = client.doc
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

console.log(`ydoc-inspect: connecting to ${url}`)
console.log('Open chrome://inspect to attach DevTools')
console.log('')
console.log('Available commands:')
console.log('  channels()                   - List all registered channels')
console.log('  messages(channelId?, n?)     - Get messages (optionally for a channel, last n)')
console.log('  filter(channelId?, pattern?) - Filter messages by regex (string or RegExp)')
console.log('  send(channelId, msg)         - Send a message to the client as Language Server')
console.log('  receive(channelId, msg)      - Send a message to Language Server as client')
console.log('  watch(channelId?)            - Watch live messages (returns stop function)')
console.log('  unwatch()                    - Stop watching live messages')
console.log('')

async function connectWithRetry(): Promise<void> {
  while (true) {
    try {
      await client.connect(url)
      console.log('Connected. Syncing inspect data...')
      await new Promise<void>((resolve) => {
        const channelsMap = client.doc.getMap('channels')
        if (channelsMap.size >= 3) {
          resolve()
          return
        }
        const timeout = setTimeout(resolve, SYNC_TIMEOUT_MS)
        const handler = () => {
          if (channelsMap.size >= 3) {
            clearTimeout(timeout)
            channelsMap.unobserve(handler)
            resolve()
          }
        }
        channelsMap.observe(handler)
      })
      const channels = helpers.channels()
      if (channels.length > 0) {
        console.log(`Found ${channels.length} channel(s):`)
        for (const ch of channels) {
          console.log(`  ${ch.id} (${ch.type}) - ${ch.channelName}`)
        }
      } else {
        console.log('No channels registered yet. Connect an IDE client to see channels.')
      }
      if (watch) {
        const existing = helpers.messages()
        if (existing.length > 0) {
          console.log(`\n--- ${existing.length} historical message(s) ---`)
          for (const entry of existing) {
            printEntry(entry)
          }
          console.log('--- live messages ---\n')
        }
        unwatchFn = helpers.watch()
      }
      return
    } catch {
      console.log(
        `Waiting for ydoc-server at ${url} ... (retrying every ${RETRY_INTERVAL_MS / 1000}s)`,
      )
      await new Promise<void>((resolve) => setTimeout(resolve, RETRY_INTERVAL_MS))
    }
  }
}

client.onDisconnect = () => {
  connectWithRetry()
}

connectWithRetry()
