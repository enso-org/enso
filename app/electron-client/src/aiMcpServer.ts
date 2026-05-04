/**
 * @file Localhost MCP server hosted by the Electron main process. Exposes a single tool,
 * `evaluateExpression`, that the long-lived `claude` CLI subprocess can call mid-turn to
 * inspect the runtime value of any in-scope binding (or any expression that references in-scope
 * bindings) via the renderer's already-open Language Server connection.
 *
 * Why HTTP over stdio: Claude Code can launch MCP servers itself via stdio, but the server we
 * need has to share state with the Electron main process — it must reach into the renderer's
 * `graphDb` / `executionContext` to evaluate expressions, and it must know which `WebContents`
 * a given turn originated from. Hosting in-process (and pointing the CLI at it via
 * `--mcp-config <file>` with `"type":"http"`) sidesteps cross-process state-sharing entirely.
 *
 * Bridging back to the renderer: the tool handler sends `Channel.aiToolCall` to the active
 * turn's `WebContents` and awaits a matching `Channel.aiToolReply`. Per-call timeout is 30 s.
 * The single-in-flight-turn invariant of `ClaudeAgentSession.runRequest` means tool calls are
 * naturally sequential within a turn, but multiple calls can still overlap in the request table
 * (the model can fan out tool calls within one assistant message), so we key by `requestId`.
 */
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import { ipcMain, type WebContents } from 'electron'
import type { AiToolCallReply, AiToolCallRequest } from 'enso-common/src/ai'
import { createServer, type Server as HttpServer } from 'node:http'
import { randomUUID } from 'node:crypto'
import * as fs from 'node:fs'
import * as path from 'node:path'
import { z } from 'zod'
import { Channel } from './ipc.js'

const TOOL_CALL_TIMEOUT_MS = 30_000

/**
 * Resolve the `WebContents` to dispatch tool calls to. Returns `null` when no AI turn is in
 * flight — the tool handler then replies with a clean error so the model can recover instead of
 * hanging.
 */
export type ActiveSenderResolver = () => WebContents | null

/**
 * Public handle returned by {@link startAiMcpServer}. The `mcpConfigPath` is what the caller
 * passes to the `claude` CLI as `--mcp-config <path>`; `shutdown()` is wired to `before-quit`.
 */
export interface AiMcpServerHandle {
  readonly mcpConfigPath: string
  shutdown(): Promise<void>
}

interface PendingToolCall {
  resolve(reply: AiToolCallReply['result']): void
  timer: NodeJS.Timeout
}

/** In-process MCP server lifecycle. */
export class AiMcpServer {
  private readonly pending = new Map<string, PendingToolCall>()
  private readonly httpServer: HttpServer
  private configPath: string | null = null
  private readonly resolveActiveSender: ActiveSenderResolver
  private listening = false

  /** Wire up the IPC reply listener and the HTTP listener (still on a random port until {@link start}). */
  constructor(resolveActiveSender: ActiveSenderResolver) {
    this.resolveActiveSender = resolveActiveSender
    this.httpServer = createServer((req, res) => void this.handleHttp(req, res))
    ipcMain.on(Channel.aiToolReply, (_event, reply: AiToolCallReply) => {
      this.handleReply(reply)
    })
  }

  /**
   * Start listening on a random localhost port and write the temporary MCP config file so the
   * agent's spawn can pass `--mcp-config`. Resolves with the file path.
   */
  async start(): Promise<string> {
    if (this.listening) {
      if (this.configPath == null) throw new Error('AiMcpServer: started but no config path')
      return this.configPath
    }
    await new Promise<void>((resolve, reject) => {
      this.httpServer.once('error', reject)
      this.httpServer.listen(0, '127.0.0.1', () => {
        this.httpServer.off('error', reject)
        resolve()
      })
    })
    this.listening = true
    const address = this.httpServer.address()
    if (address == null || typeof address === 'string') {
      throw new Error(`AiMcpServer: unexpected address ${String(address)}`)
    }
    const url = `http://127.0.0.1:${address.port}/mcp`
    const config = { mcpServers: { enso: { type: 'http', url } } }
    const file = path.join(
      // os.tmpdir is correct here, but go through Electron's userData-adjacent tempdir if it's
      // available (Electron sets `app.getPath('temp')` to the platform tmp anyway, so tmpdir is
      // a fine fallback used by the headless tests where Electron isn't booted).
      process.env.TMPDIR || '/tmp',
      `enso-claude-mcp-${process.pid}-${randomUUID()}.json`,
    )
    fs.writeFileSync(file, JSON.stringify(config))
    this.configPath = file
    return file
  }

  /** Tear down the server, drop the config file, and reject any in-flight tool calls. */
  async shutdown(): Promise<void> {
    for (const [requestId, pending] of this.pending) {
      clearTimeout(pending.timer)
      pending.resolve({ ok: false, error: 'AI MCP server shutting down' })
      this.pending.delete(requestId)
    }
    if (this.listening) {
      await new Promise<void>((resolve) => {
        this.httpServer.close(() => resolve())
      })
      this.listening = false
    }
    if (this.configPath != null) {
      try {
        fs.unlinkSync(this.configPath)
      } catch {
        // already gone
      }
      this.configPath = null
    }
  }

  // -------------- private --------------

  /**
   * Create a fresh `McpServer` with the `evaluateExpression` tool registered. The SDK rejects
   * `connect()` if a `Protocol` is already wired to a transport, and our HTTP transport is
   * stateless (one transport per request), so we follow the SDK's official stateless example —
   * `simpleStatelessStreamableHttp.js` — and instantiate a server per request.
   */
  private createServer(): McpServer {
    const server = new McpServer(
      { name: 'enso', version: '0.0.0-dev' },
      { capabilities: { tools: {} } },
    )
    server.registerTool(
      'evaluateExpression',
      {
        title: 'Evaluate an Enso expression in the AI insertion scope',
        description:
          'Evaluate a plain Enso expression in the scope where your generated `body` would run. ' +
          'Every in-scope binding the prompt listed is referenceable by name, and one call may ' +
          'stitch several of them together. Use sparingly to learn things you cannot infer from ' +
          'types alone (column names, value previews, join shapes). Each call costs a real LS ' +
          'round-trip (~hundreds of ms to a few seconds). Returns the expression value as JSON, ' +
          'or an error string if the binding/scope is unavailable or evaluation fails.',
        inputSchema: { expression: z.string() },
      },
      async ({ expression }) => {
        const result = await this.dispatchToRenderer({ tool: 'evaluateExpression', expression })
        if (result.ok) {
          return { content: [{ type: 'text' as const, text: JSON.stringify(result.value) }] }
        }
        return {
          content: [{ type: 'text' as const, text: `Error: ${result.error}` }],
          isError: true,
        }
      },
    )
    return server
  }

  private async dispatchToRenderer(
    payload: Omit<AiToolCallRequest, 'requestId'>,
  ): Promise<AiToolCallReply['result']> {
    const sender = this.resolveActiveSender()
    if (sender == null) {
      return { ok: false, error: 'no active AI turn — tool calls only valid mid-turn' }
    }
    if (sender.isDestroyed()) {
      return { ok: false, error: 'renderer was destroyed before the tool call could be dispatched' }
    }
    const requestId = randomUUID()
    return new Promise<AiToolCallReply['result']>((resolve) => {
      const timer = setTimeout(() => {
        if (this.pending.delete(requestId)) {
          resolve({
            ok: false,
            error: `tool call timed out after ${TOOL_CALL_TIMEOUT_MS}ms`,
          })
        }
      }, TOOL_CALL_TIMEOUT_MS)
      this.pending.set(requestId, { resolve, timer })
      const request: AiToolCallRequest = { requestId, ...payload }
      try {
        sender.send(Channel.aiToolCall, request)
      } catch (err) {
        clearTimeout(timer)
        this.pending.delete(requestId)
        resolve({
          ok: false,
          error: `failed to dispatch tool call to renderer: ${(err as Error).message}`,
        })
      }
    })
  }

  private handleReply(reply: AiToolCallReply): void {
    const pending = this.pending.get(reply.requestId)
    if (pending == null) return
    clearTimeout(pending.timer)
    this.pending.delete(reply.requestId)
    pending.resolve(reply.result)
  }

  private async handleHttp(
    req: import('node:http').IncomingMessage,
    res: import('node:http').ServerResponse,
  ): Promise<void> {
    // Stateless mode: each incoming request gets its own short-lived transport AND its own
    // McpServer — the SDK's Protocol class refuses `connect()` if it's already wired to a
    // transport, so reusing a single server across requests fails on the second call. Pattern
    // mirrors the SDK's `simpleStatelessStreamableHttp.js` example. The cast on the options bag
    // works around the SDK's type using a non-optional `() => string` for `sessionIdGenerator`
    // even though the runtime accepts `undefined` to mean "stateless".
    const server = this.createServer()
    const transport = new StreamableHTTPServerTransport({
      sessionIdGenerator: undefined,
    } as unknown as ConstructorParameters<typeof StreamableHTTPServerTransport>[0])
    res.on('close', () => {
      void transport.close()
      void server.close()
    })
    try {
      // The SDK's `Transport` type declares `onclose` as `() => void` (non-optional) under
      // exactOptionalPropertyTypes; the concrete class actually exposes `(() => void) | undefined`.
      // Cast at the call site to bridge the gap without weakening the rest of the file.
      await server.connect(transport as unknown as Parameters<typeof server.connect>[0])
      await transport.handleRequest(req, res)
    } catch (err) {
      console.warn('[AI MCP] error handling request:', err)
      if (!res.headersSent) {
        res.statusCode = 500
        res.end()
      }
    }
  }
}

/** Convenience factory for the singleton case. */
export async function startAiMcpServer(
  resolveActiveSender: ActiveSenderResolver,
): Promise<{ server: AiMcpServer; mcpConfigPath: string }> {
  const server = new AiMcpServer(resolveActiveSender)
  const mcpConfigPath = await server.start()
  return { server, mcpConfigPath }
}
