/** JSON-RPC result wrapper */
export function toJSONRPCResult(result: unknown): string {
  return JSON.stringify({ jsonrpc: '2.0', id: 0, result })
}

/** JSON-RPC error wrapper */
export function toJSONRPCError(message: string, data?: unknown): string {
  return JSON.stringify({
    jsonrpc: '2.0',
    id: 0,
    error: { code: 0, message, ...(data != null ? { data } : {}) },
  })
}
