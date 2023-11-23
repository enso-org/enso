/** Returns {@link Error}s as-is, wraps all other values in an {@link Error}. */
export function toError(error: unknown) {
  return error instanceof Error ? error : new Error(String(error))
}
