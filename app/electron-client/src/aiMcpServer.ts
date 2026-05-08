/**
 * @file In-process MCP server exposing the `evaluateExpression` tool to the long-lived `claude`
 * subprocess; bridges tool calls to the active renderer over `Channel.aiToolCall`/`aiToolReply`.
 * Lifecycle, transport choice, and reentrancy notes live in `electron-client/CLAUDE.md`.
 */
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import { ipcMain } from 'electron'
import type { AiToolCallReply, AiToolCallRequest } from 'enso-common/src/ai'
import { randomUUID } from 'node:crypto'
import * as fs from 'node:fs'
import { createServer, type Server as HttpServer } from 'node:http'
import * as os from 'node:os'
import * as path from 'node:path'
import { z } from 'zod'
import type { ActiveRequest } from './claudeAgent.js'
import { Channel } from './ipc.js'

const TOOL_CALL_TIMEOUT_MS = 30_000

/**
 * Resolve the renderer + request id pair driving the currently-in-flight Claude turn. Returns
 * `null` when no AI turn is in flight (or during priming).
 */
export type ActiveRequestResolver = () => ActiveRequest | null

/** Returned by {@link startAiMcpServer}; `mcpConfigPath` is the value for the CLI's `--mcp-config`. */
export interface AiMcpServerHandle {
  readonly mcpConfigPath: string
  shutdown(): Promise<void>
}

interface PendingToolCall {
  resolve(reply: AiToolCallReply['result']): void
  timer: NodeJS.Timeout
}

/**
 * In-process MCP server bound to a random localhost port. Holds the IPC bridge between the
 * `claude` subprocess (HTTP client) and the active renderer (which actually evaluates the tool).
 */
export class AiMcpServer {
  private readonly pending = new Map<string, PendingToolCall>()
  private readonly httpServer: HttpServer
  private configPath: string | null = null
  private readonly resolveActiveRequest: ActiveRequestResolver
  private readonly ipcReplyListener: (event: unknown, reply: AiToolCallReply) => void
  private listening = false

  /** Wire up the IPC reply listener and the HTTP server (still unbound until {@link start}). */
  constructor(resolveActiveRequest: ActiveRequestResolver) {
    this.resolveActiveRequest = resolveActiveRequest
    this.httpServer = createServer((req, res) => void this.handleHttp(req, res))
    this.ipcReplyListener = (_event, reply) => this.handleReply(reply)
    ipcMain.on(Channel.aiToolReply, this.ipcReplyListener)
  }

  /** Start the HTTP listener and write the MCP config file. Returns the config file path. */
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
    const file = path.join(os.tmpdir(), `enso-claude-mcp-${process.pid}-${randomUUID()}.json`)
    fs.writeFileSync(file, JSON.stringify(config))
    this.configPath = file
    return file
  }

  /** Stop the listener, delete the config file, and fail any in-flight tool calls. */
  async shutdown(): Promise<void> {
    ipcMain.removeListener(Channel.aiToolReply, this.ipcReplyListener)
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

  // Fresh server per request: the SDK rejects `connect()` on an already-wired Protocol and our
  // HTTP transport is stateless (one transport per request) — see SDK's `simpleStatelessStreamableHttp`.
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
          'round-trip (~hundreds of ms to a few seconds). The expression must evaluate to Text — ' +
          'pick the encoding yourself with `.to_text`, `.to_display_text`, or `.to_json` (e.g. ' +
          '`<binding>.column_names.to_json`). For expressions that may produce a DataflowError ' +
          'wrap with `.catch_primitive (e -> e.to_display_text)` so the result is still Text. ' +
          'Returns the text the expression produced, or an error string if the binding/scope is ' +
          'unavailable or evaluation fails.',
        inputSchema: { expression: z.string() },
      },
      async ({ expression }) => {
        const result = await this.dispatchToRenderer({ tool: 'evaluateExpression', expression })
        if (result.ok) {
          // Forward the agent-chosen encoding verbatim — JSON-stringifying here would double-encode.
          return { content: [{ type: 'text' as const, text: result.value }] }
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
    payload: Omit<AiToolCallRequest, 'requestId' | 'turnRequestId'>,
  ): Promise<AiToolCallReply['result']> {
    const active = this.resolveActiveRequest()
    if (active == null) {
      return { ok: false, error: 'no active AI turn — tool calls only valid mid-turn' }
    }
    // Defensive: `activeRequest` already filters destroyed senders, but a tightly-timed
    // destruction between resolver invocation and `send()` could still surface here.
    if (active.sender.isDestroyed()) {
      return { ok: false, error: 'renderer was destroyed before the tool call could be dispatched' }
    }
    const sender = active.sender
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
      const request: AiToolCallRequest = { requestId, turnRequestId: active.requestId, ...payload }
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
    const server = this.createServer()
    // The cast works around the SDK typing `sessionIdGenerator` as a non-optional
    // `() => string` even though `undefined` (stateless) is accepted at runtime.
    const transport = new StreamableHTTPServerTransport({
      sessionIdGenerator: undefined,
    } as unknown as ConstructorParameters<typeof StreamableHTTPServerTransport>[0])
    res.on('close', () => {
      void transport.close()
      void server.close()
    })
    try {
      // SDK's `Transport.onclose` is `() => void` under exactOptionalPropertyTypes; the
      // concrete class actually exposes it as optional.
      await server.connect(transport as unknown as Parameters<typeof server.connect>[0])
      await transport.handleRequest(req, res)
    } catch (err) {
      console.warn('[AI MCP] error handling request:', err)
      if (!res.headersSent) {
        res.statusCode = 500
        res.end()
      } else {
        // Headers already sent — can't send a clean error code; tear the socket down so it
        // doesn't linger until the server's keep-alive timeout.
        res.destroy()
      }
    }
  }
}

/** Convenience factory for the singleton case. */
export async function startAiMcpServer(
  resolveActiveRequest: ActiveRequestResolver,
): Promise<{ server: AiMcpServer; mcpConfigPath: string }> {
  const server = new AiMcpServer(resolveActiveRequest)
  const mcpConfigPath = await server.start()
  return { server, mcpConfigPath }
}
