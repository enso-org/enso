import { InspectClient } from './client.js'
import { createHelpers, formatEntry } from './helpers.js'

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

const RETRY_INTERVAL_MS = 2000
const SYNC_TIMEOUT_MS = 5000
/* The number of yjs channels created when gui connects to the Language Server. */
const INITIAL_CHANNELS_NUMBER = 3

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

console.log(`
ydoc-inspect: connecting to ${url}
Open chrome://inspect to attach DevTools

Available commands:
  channels()                   - List all registered channels
  messages(channelId?, n?)     - Get messages (optionally for a channel, last n)
  filter(channelId?, pattern?) - Filter messages by regex (string or RegExp)
  send(channelId, msg)         - Send a message to the client as Language Server
  receive(channelId, msg)      - Send a message to Language Server as client
  watch(channelId?)            - Watch live messages (returns stop function)
  unwatch()                    - Stop watching live messages
`)

async function connectWithRetry(): Promise<void> {
  while (true) {
    try {
      await client.connect(url)
      console.log('Connected. Syncing inspect data...')
      await new Promise<void>((resolve) => {
        const channelsMap = client.doc.getMap('channels')
        if (channelsMap.size >= INITIAL_CHANNELS_NUMBER) {
          resolve()
          return
        }
        const timeout = setTimeout(resolve, SYNC_TIMEOUT_MS)
        const handler = () => {
          if (channelsMap.size >= INITIAL_CHANNELS_NUMBER) {
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
            console.log(formatEntry(entry))
          }
          console.log('--- live messages ---\n')
        }
        unwatchFn = helpers.watch()
      }
      return
    } catch {
      console.log(
        `Waiting for ydoc-server at ${url} ... (retrying in ${RETRY_INTERVAL_MS / 1000}s)`,
      )
      await new Promise<void>((resolve) => setTimeout(resolve, RETRY_INTERVAL_MS))
    }
  }
}

client.onDisconnect = () => {
  connectWithRetry()
}

connectWithRetry()
