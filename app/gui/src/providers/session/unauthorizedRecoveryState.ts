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
export class UnauthorizedRecoveryState {
  /** Number of active backoff waits long enough to trigger the "reconnecting session" UI state. */
  reconnectingSessionBackoffWaitCount: Ref<number>
  /** In-flight primary session recovery promise, used for deduplication of concurrent recovery attempts. */
  authRecoveryPromise: Promise<boolean> | null
  /** In-flight recovery promise after a repeated unauthorized error, used for deduplication of concurrent recovery attempts. */
  repeatedUnauthorizedRecoveryPromise: Promise<boolean> | null
  /** Number of repeated-unauthorized recovery attempts made so far. */
  repeatedUnauthorizedRecoveryAttempts: number
  /** Next base delay for the next repeated-unauthorized exponential backoff. */
  repeatedUnauthorizedDelayMs: number
  /** Timestamp of the most recent unauthorized error, used to reset stale recovery state. */
  unauthorizedRecoveryLastErrorAt: number
  /** Whether the current unauthorized flow has already recovered the session once and is now in replay mode. */
  hasRecoveredUnauthorizedSession: boolean
  /** Whether a repeated unauthorized error has already been reported after last session recovery. */
  hasReportedRepeatedUnauthorizedError: boolean
  /** Single-flight promise for terminal auth failure handling, used for logout/cleanup. */
  terminalAuthFailurePromise: Promise<void> | null
  /** Query hashes already replayed during recovery procedure. */
  replayedQueryHashes: Set<string>
  /** Failed queries waiting to be replayed after repeated unauthorized recovery succeeds. */
  pendingRepeatedUnauthorizedQueries: Map<string, readonly unknown[]>
  /** Mutations already replayed during recovery procedure. */
  replayedMutations: WeakSet<object>

  /** Create state with initial values. */
  constructor() {
    this.reconnectingSessionBackoffWaitCount = ref(0)
    this.authRecoveryPromise = null
    this.repeatedUnauthorizedRecoveryPromise = null
    this.repeatedUnauthorizedRecoveryAttempts = 0
    this.repeatedUnauthorizedDelayMs =
      REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.initialDelayMs
    this.unauthorizedRecoveryLastErrorAt = 0
    this.hasRecoveredUnauthorizedSession = false
    this.hasReportedRepeatedUnauthorizedError = false
    this.terminalAuthFailurePromise = null
    this.replayedQueryHashes = new Set<string>()
    this.pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
    this.replayedMutations = new WeakSet<object>()
  }

  /** Reset state tracking for unauthorized recovery and replay. */
  reset() {
    this.repeatedUnauthorizedRecoveryAttempts = 0
    this.repeatedUnauthorizedDelayMs =
      REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.initialDelayMs
    this.unauthorizedRecoveryLastErrorAt = 0
    this.hasRecoveredUnauthorizedSession = false
    this.hasReportedRepeatedUnauthorizedError = false
    this.replayedQueryHashes = new Set<string>()
    this.pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
    this.replayedMutations = new WeakSet<object>()
    this.reconnectingSessionBackoffWaitCount.value = 0
  }

  /** Record unauthorized activity and reset state if activity was stale. */
  recordUnauthorizedRecoveryError(
    resetWindowMs = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.resetWindowMs,
  ) {
    const now = Date.now()
    if (
      this.unauthorizedRecoveryLastErrorAt > 0 &&
      now - this.unauthorizedRecoveryLastErrorAt > resetWindowMs
    ) {
      this.reset()
    }
    this.unauthorizedRecoveryLastErrorAt = now
  }

  /** Queue a failed query to replay after successful recovery. */
  queueRetryOfUnauthorizedQuery(query: UnauthorizedFailedQuery) {
    this.pendingRepeatedUnauthorizedQueries.set(query.queryHash, query.queryKey)
  }

  /** Report repeated unauthorized errors only once per recovery window. */
  reportRepeatedUnauthorizedErrorOnce(
    error: UnauthorizedRecoveryError,
    report: (error: UnauthorizedRecoveryError) => void,
  ) {
    if (this.hasReportedRepeatedUnauthorizedError) {
      return
    }
    this.hasReportedRepeatedUnauthorizedError = true
    report(error)
  }

  /** Return true when repeated unauthorized recovery exhausted its attempts. */
  hasReachedRepeatedUnauthorizedAttemptLimit(options: RepeatedUnauthorizedRecoveryBackoffOptions) {
    return this.repeatedUnauthorizedRecoveryAttempts >= options.maxAttempts
  }
}
