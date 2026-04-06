import { InspectClient } from './client.js'
import { createHelpers } from './helpers.js'

function parseArgs(): { host: string; port: string } {
  const args = process.argv.slice(2)
  let host = 'localhost'
  let port = '30617'
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--host' && args[i + 1]) host = args[++i]!
    else if (args[i] === '--port' && args[i + 1]) port = args[++i]!
  }
  return { host, port }
}

const RETRY_INTERVAL_MS = 2000

const { host, port } = parseArgs()
const url = `ws://${host}:${port}/project/inspect`

const client = new InspectClient()
const helpers = createHelpers(client.doc)

// Expose as globals for Chrome DevTools console
const g = globalThis as Record<string, unknown>
g['client'] = client
g['doc'] = client.doc
g['listChannels'] = helpers.listChannels
g['messages'] = helpers.messages
g['last'] = helpers.last
g['filter'] = helpers.filter
g['send'] = helpers.send
g['watch'] = helpers.watch

console.log(`ydoc-inspect: connecting to ${url}`)
console.log('Open chrome://inspect to attach DevTools')
console.log('')
console.log('Available commands:')
console.log('  listChannels()              - List all registered channels')
console.log('  messages(channelId?, n?)     - Get messages (optionally for a channel, last n)')
console.log('  last(channelId?, n=10)       - Get last n messages')
console.log('  filter(channelId?, pattern?) - Filter messages by regex (string or RegExp)')
console.log('  send(channelId, msg)         - Send message to Language Server via channel')
console.log('  watch(channelId?)            - Watch live messages (returns stop function)')
console.log('')

async function connectWithRetry(): Promise<void> {
  while (true) {
    try {
      await client.connect(url)
      console.log('Connected. Syncing inspect data...')
      await new Promise<void>((resolve) => setTimeout(resolve, 1000))
      const channels = helpers.listChannels()
      if (channels.length > 0) {
        console.log(`Found ${channels.length} channel(s):`)
        for (const ch of channels) {
          console.log(`  ${ch.id} (${ch.type}) - ${ch.channelName}`)
        }
      } else {
        console.log('No channels registered yet. Connect an IDE client to see channels.')
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
