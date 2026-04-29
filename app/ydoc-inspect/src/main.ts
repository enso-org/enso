/**
 * ydoc-inspect: interactive console for observing and injecting messages on
 * YjsChannels exposed by a running ydoc-server.
 *
 * # Architecture
 *
 * The inspect client maintains two WebSocket connections, each syncing a
 * separate Y.Doc. The **inspect Y.Doc** (`/project/inspect`) is owned by
 * `InspectManager` and stores channel message logs and command arrays. It is
 * created when the server starts in debug mode, so the inspect client can
 * connect immediately. The **project Y.Doc** (`/project/inspect/index`) is the
 * session's `indexDoc` shared with the GUI, containing the project's AST,
 * module list, and metadata. It is only available after a GUI client connects
 * and the `LanguageServerSession` is created.
 *
 * Per-module AST data lives in Yjs subdocs referenced from the project Y.Doc's
 * module map. `y-websocket` does not sync subdocs automatically, so the
 * inspect client opens an additional WebSocket connection to
 * `/project/inspect/<guid>` for each module subdoc whose contents are needed.
 * Subdoc connections are created lazily on first access. The helper AST
 * commands are therefore asynchronous and must be invoked with `await` from
 * the DevTools console. Each subdoc provider is cached per guid, so repeated
 * calls against the same module do not re-open the connection.
 *
 * When debug mode is active, `InspectManager` intercepts channel traffic and
 * exposes the session's documents for AST inspection:
 *
 * **Channel inspection:**
 *
 * 1. `InspectManager` wraps JSON and binary `YjsChannelServer` instances
 * 2. Each new channel gets a tap that copies all messages (with timestamps
 *    and direction) into per-channel Y.Arrays (`log:<id>` and `meta:<id>`) on
 *    the inspect Y.Doc
 * 3. The inspect client syncs the inspect Y.Doc and reads the arrays to
 *    display messages
 * 4. Commands from the inspect client are written to `snd:<id>` / `rcv:<id>`
 *    arrays, consumed by the server, and forwarded to real channels
 *
 * **AST inspection:**
 *
 * 1. When a `LanguageServerSession` is created by ydoc-server, it registers
 *    its doc map (`index` doc + one `WSSharedDoc` per module keyed by subdoc
 *    guid) with `InspectManager` via `registerSession`, making those docs
 *    reachable through the `/project/inspect/<docName>` WebSocket route
 *    handled by `handleDocConnection`
 * 2. The inspect client connects to `/project/inspect/index` to sync the
 *    project root doc, which holds the module map, reading its keys yields
 *    the module names returned by `modules()`
 * 3. On the first AST command touching a given module, the inspect client
 *    opens an additional WebSocket to `/project/inspect/<subdoc.guid>` using
 *    the subdoc's own Y.Doc, so Yjs sync populates its `nodes` map with the
 *    AST data. The `MutableModule` wrapper is then built on top to serve
 *    `ast()`, `tree()`, etc. helper commands
 *
 * ```
 *                                        inspect Y.Doc      +-------------------+
 * +-----------------+  /project/inspect  (channel logs)     | InspectManager    |
 * | ydoc-inspect    |<------------------------------------->| (ydoc-server)     |
 * | (Node.js)       |                                       +------+----------+-+
 * |                 |  /project/inspect/index                  tap |          | tap
 * |                 |<---------------+                 +-----------+--+ +-----+----------+
 * +-----------------+  project Y.Doc |                 | JSON Channel | | Binary Channel |
 *                      (AST data)    |                 +--------------+ +----------------+
 *                                    |
 *                             +------+--------+
 *                             | Session Docs  |
 *                             | (index, ...)  |
 *                             +---------------+
 * ```
 *
 * The inspect Y.Doc is a standard `WSSharedDoc`, so multiple inspect clients
 * can connect simultaneously and observe the same traffic. The project Y.Doc
 * connection provides read access to the AST structure of all loaded modules.
 */

import { InspectClient } from './client.js'
import { createAstHelpers, createHelpers, exposeGlobals, formatEntry } from './helpers.js'

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

/* How long to wait before retrying when the ydoc-server websocket is unreachable. */
const RETRY_INTERVAL_MS = 2000
/* Upper bound on how long to wait for the initial channel registrations after connecting. */
const SYNC_TIMEOUT_MS = 5000
/* How long to wait after the last channel update before assuming the initial batch is complete. */
const SYNC_SETTLE_MS = 250

const { host, port, watch, truncate } = parseArgs()
const url = `ws://${host}:${port}/project/inspect`
const projectUrl = `ws://${host}:${port}/project/inspect/index`

let client: InspectClient
let helpers: ReturnType<typeof createHelpers>
let globals: ReturnType<typeof exposeGlobals>
let connecting = false

const motd = `
ydoc-inspect: connecting to ${url}
Open chrome://inspect to attach DevTools

Channel commands:
  channels()                   - List all registered channels
  messages(channelId?, n?)     - Get messages (optionally for a channel, last n)
  filter(channelId?, pattern?) - Filter messages by regex (string or RegExp)
  send(channelId, msg)         - Send a message to the client as Language Server
  receive(channelId, msg)      - Send a message to Language Server as client
  watch(channelId?)            - Watch live messages (returns stop function)
  unwatch()                    - Stop watching live messages

AST commands:
  modules()                    - List all module names in the project
  ast(moduleName?)             - Get root AST node (defaults to Main)
  tree(moduleName?, depth?)    - Print AST tree structure
  node(id)                     - Look up an AST node by id
  meta(id)                     - Show metadata for a node
  code(moduleName?)            - Print module source code

  help()                       - Print this help message
`

function help(): void {
  console.log(motd)
}
;(globalThis as Record<string, unknown>)['help'] = help

help()

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
          let settleTimer: ReturnType<typeof setTimeout> | undefined
          const finish = () => {
            clearTimeout(overallTimeout)
            if (settleTimer) clearTimeout(settleTimer)
            channelsMap.unobserve(handler)
            resolve()
          }
          const handler = () => {
            if (channelsMap.size === 0) return
            if (settleTimer) clearTimeout(settleTimer)
            settleTimer = setTimeout(finish, SYNC_SETTLE_MS)
          }
          const overallTimeout = setTimeout(finish, SYNC_TIMEOUT_MS)
          channelsMap.observe(handler)
          if (channelsMap.size > 0) handler()
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

        // Connect to the project doc for AST inspection.
        try {
          await client.connectProject(projectUrl)
          const astH = createAstHelpers(client.projectDoc, (subdoc) => client.loadSubdoc(subdoc))
          globals = exposeGlobals(client, helpers, astH)
        } catch (e) {
          console.error('Could not connect to project doc. AST commands will not be available.', e)
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
