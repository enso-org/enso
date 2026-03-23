/** JSON-RPC result wrapper */
export function toJSONRPCResult(result: unknown): string {
  return JSON.stringify({ jsonrpc: '2.0', id: 0, result })
}

function serializeErrorData(data: unknown): unknown {
  if (data instanceof Error) {
    return {
      name: data.name,
      message: data.message,
      stack: data.stack,
      ...(data.cause != null ? { cause: serializeErrorData(data.cause) } : {}),
    }
  }
  if (Array.isArray(data)) {
    return data.map(serializeErrorData)
  }
  if (data != null && typeof data === 'object') {
    return Object.fromEntries(
      Object.entries(data).map(([key, value]) => [key, serializeErrorData(value)]),
    )
  }
  return data
}

/** JSON-RPC error wrapper */
export function toJSONRPCError(message: string, data?: unknown): string {
  return JSON.stringify({
    jsonrpc: '2.0',
    id: 0,
    error: { code: 0, message, ...(data != null ? { data: serializeErrorData(data) } : {}) },
  })
}
