export type ProtectedRouteAccess = 'guest' | 'anyLoggedIn' | 'deleted' | undefined

/** Checks whether the route can be entered without resolved session state to avoid unneeded delays. */
export function requiresResolvedSession(access: ProtectedRouteAccess) {
  return access === 'anyLoggedIn' || access === 'deleted'
}

/**
 * (Re-)resolving session is required when navigation crosses access boundaries,
 * and only if the destination needs auth knowledge or auth is still unresolved.
 */
export function shouldWaitForResolvedSession(
  toAccess: ProtectedRouteAccess,
  fromAccess: ProtectedRouteAccess,
  session: unknown,
) {
  return toAccess !== fromAccess && (requiresResolvedSession(toAccess) || session === undefined)
}
