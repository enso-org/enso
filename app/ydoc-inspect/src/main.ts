import { InspectClient } from './client.js'
import { createHelpers, exposeGlobals, formatEntry } from './helpers.js'

function parseArgs(): { host: string; port: string; watch: boolean; truncate: number } {
  const args = process.argv.slice(2)
  let host = 'localhost'
  let port = '30617'
  let watch = true
  let truncate = 240
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--host' && args[i + 1]) host = args[++i]!
    else if (args[i] === '--port' && args[i + 1]) port = args[++i]!
    else if (args[i] === '--truncate' && args[i + 1]) truncate = Number(args[++i])
    else if (args[i] === '--no-watch') watch = false
  }
  return { host, port, watch, truncate }
}

const RETRY_INTERVAL_MS = 2000
const SYNC_TIMEOUT_MS = 5000
/* The number of yjs channels created when gui connects to the Language Server. */
const INITIAL_CHANNELS_NUMBER = 3

const { host, port, watch, truncate } = parseArgs()
const url = `ws://${host}:${port}/project/inspect`

let client: InspectClient
let helpers: ReturnType<typeof createHelpers>
let globals: ReturnType<typeof exposeGlobals>
let connecting = false

// Motd
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
  if (connecting) return
  connecting = true
  try {
    if (globals) globals.unwatch()
    client = new InspectClient()
    client.onDisconnect = () => connectWithRetry()
    helpers = createHelpers(client.doc, truncate)
    globals = exposeGlobals(client, helpers)
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
          console.log('No channels registered. Make sure ydoc-server is running in debug mode.')
          return
        }
        if (watch) {
          const existing = helpers.messages()
          if (existing.length > 0) {
            console.log(`\n--- ${existing.length} historical message(s) ---`)
            for (const entry of existing) {
              console.log(formatEntry(entry, truncate))
            }
            console.log('--- live messages ---\n')
          }
          globals.watch()
        }
        return
      } catch {
        console.log(
          `Waiting for ydoc-server at ${url} ... (retrying in ${RETRY_INTERVAL_MS / 1000}s)`,
        )
        await new Promise<void>((resolve) => setTimeout(resolve, RETRY_INTERVAL_MS))
      }
    }
  } finally {
    connecting = false
  }
}

connectWithRetry()
