import { Err, ResultError } from 'enso-common/src/utilities/data/result'
import { ref, type Ref } from 'vue'
import type { RepeatedUnauthorizedRecoveryBackoffOptions } from './backoff'
import { REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS } from './constants'

/** Error type propagated through unauthorized recovery flow. */
export type UnauthorizedRecoveryError = ResultError<unknown>

/** Normalize unknown error values into a ResultError instance. */
export function toUnauthorizedRecoveryError(error: unknown): UnauthorizedRecoveryError {
  return error instanceof ResultError ? error : Err(error).error
}

/** Query identifier for a failed unauthorized request. */
export interface UnauthorizedFailedQuery {
  readonly queryHash: string
  readonly queryKey: readonly unknown[]
}

/** Mutable state for unauthorized recovery flow. */
export interface UnauthorizedRecoveryState {
  readonly reconnectingSessionBackoffWaitCount: Ref<number>
  authRecoveryPromise: Promise<boolean> | null
  repeatedUnauthorizedRecoveryPromise: Promise<boolean> | null
  repeatedUnauthorizedRecoveryAttempts: number
  repeatedUnauthorizedDelayMs: number
  unauthorizedRecoveryLastActivityAt: number
  hasRecoveredUnauthorizedSession: boolean
  hasReportedRepeatedUnauthorizedError: boolean
  terminalAuthFailurePromise: Promise<void> | null
  replayedQueryHashes: Set<string>
  pendingRepeatedUnauthorizedQueries: Map<string, readonly unknown[]>
  replayedMutations: WeakSet<object>
}

/** Return whether a query key identifies the usersMe request. */
export function isUsersMeQuery(query: { readonly queryKey: readonly unknown[] }) {
  return query.queryKey[1] === 'usersMe'
}

/** Create initial mutable state for unauthorized recovery logic. */
export function createUnauthorizedRecoveryState(): UnauthorizedRecoveryState {
  return {
    reconnectingSessionBackoffWaitCount: ref(0),
    authRecoveryPromise: null,
    repeatedUnauthorizedRecoveryPromise: null,
    repeatedUnauthorizedRecoveryAttempts: 0,
    repeatedUnauthorizedDelayMs: REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.initialDelayMs,
    unauthorizedRecoveryLastActivityAt: 0,
    hasRecoveredUnauthorizedSession: false,
    hasReportedRepeatedUnauthorizedError: false,
    terminalAuthFailurePromise: null,
    replayedQueryHashes: new Set<string>(),
    pendingRepeatedUnauthorizedQueries: new Map<string, readonly unknown[]>(),
    replayedMutations: new WeakSet<object>(),
  }
}

/** Reset state tracking for unauthorized recovery and replay. */
export function resetUnauthorizedRecoveryState(state: UnauthorizedRecoveryState) {
  state.repeatedUnauthorizedRecoveryAttempts = 0
  state.repeatedUnauthorizedDelayMs = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.initialDelayMs
  state.unauthorizedRecoveryLastActivityAt = 0
  state.hasRecoveredUnauthorizedSession = false
  state.hasReportedRepeatedUnauthorizedError = false
  state.replayedQueryHashes = new Set<string>()
  state.pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
  state.replayedMutations = new WeakSet<object>()
  state.reconnectingSessionBackoffWaitCount.value = 0
}

/** Record unauthorized activity and reset state if activity was stale. */
export function recordUnauthorizedRecoveryActivity(
  state: UnauthorizedRecoveryState,
  resetWindowMs = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.resetWindowMs,
) {
  const now = Date.now()
  if (
    state.unauthorizedRecoveryLastActivityAt > 0 &&
    now - state.unauthorizedRecoveryLastActivityAt > resetWindowMs
  ) {
    resetUnauthorizedRecoveryState(state)
  }
  state.unauthorizedRecoveryLastActivityAt = now
}

/** Queue a failed query to replay after successful recovery. */
export function queueRepeatedUnauthorizedQuery(
  state: UnauthorizedRecoveryState,
  query: UnauthorizedFailedQuery,
) {
  state.pendingRepeatedUnauthorizedQueries.set(query.queryHash, query.queryKey)
}

/** Report repeated unauthorized errors only once per recovery window. */
export function reportRepeatedUnauthorizedErrorOnce(
  state: UnauthorizedRecoveryState,
  error: UnauthorizedRecoveryError,
  report: (error: UnauthorizedRecoveryError) => void,
) {
  if (state.hasReportedRepeatedUnauthorizedError) {
    return
  }
  state.hasReportedRepeatedUnauthorizedError = true
  report(error)
}

/** Return true when repeated unauthorized recovery exhausted its attempts. */
export function hasReachedRepeatedUnauthorizedAttemptLimit(
  state: UnauthorizedRecoveryState,
  options: RepeatedUnauthorizedRecoveryBackoffOptions,
) {
  return state.repeatedUnauthorizedRecoveryAttempts >= options.maxAttempts
}
