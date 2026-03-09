import { computed, onScopeDispose, type ComputedRef, type Ref } from 'vue'
import type { QueryClient } from '../../utils/queryClient'
import {
  nextBackoffDelay,
  wait,
  type AuthRecoveryBackoffOptions,
  type RepeatedUnauthorizedRecoveryBackoffOptions,
} from './backoff'
import {
  AUTH_RECOVERY_BACKOFF_DEFAULTS,
  RECONNECTING_SESSION_DELAY_MS,
  REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS,
} from './constants'
import { installUnauthorizedRecoveryHandlers } from './unauthorizedRecoveryHandlers'
import {
  createUnauthorizedRecoveryState,
  hasReachedRepeatedUnauthorizedAttemptLimit,
  isUsersMeQuery,
  recordUnauthorizedRecoveryActivity,
  reportRepeatedUnauthorizedErrorOnce,
  resetUnauthorizedRecoveryState as resetUnauthorizedRecoveryStateInternal,
} from './unauthorizedRecoveryState'

interface UseUnauthorizedRecoveryOptions {
  readonly queryClient: QueryClient
  readonly isLoggingOut: Ref<boolean>
  readonly refreshUserSession: () => Promise<unknown>
  readonly logout: () => Promise<unknown>
  readonly clearSessionToken: () => void
  readonly clearSessionQuery: () => void
  readonly reportSessionExpiredError: (error: unknown) => void
  readonly reportRepeatedUnauthorizedError: (error: unknown) => void
}

/** Install unauthorized handlers and coordinate session recovery/replay flow. */
export function useUnauthorizedRecovery(options: UseUnauthorizedRecoveryOptions): {
  readonly isReconnectingSession: ComputedRef<boolean>
  readonly resetUnauthorizedRecoveryState: () => void
} {
  const state = createUnauthorizedRecoveryState()
  const isReconnectingSession = computed(() => state.reconnectingSessionBackoffWaitCount.value > 0)

  const isAuthRecoveryBlocked = () =>
    state.terminalAuthFailurePromise != null || options.isLoggingOut.value

  const resetUnauthorizedRecoveryState = () => resetUnauthorizedRecoveryStateInternal(state)

  const waitForRecoveryBackoff = async (delayMs: number) => {
    if (delayMs <= RECONNECTING_SESSION_DELAY_MS) {
      await wait(delayMs)
      return
    }

    state.reconnectingSessionBackoffWaitCount.value += 1
    try {
      await wait(delayMs)
    } finally {
      state.reconnectingSessionBackoffWaitCount.value = Math.max(
        0,
        state.reconnectingSessionBackoffWaitCount.value - 1,
      )
    }
  }

  const reportTerminalAuthFailure = (error: unknown) => {
    if (state.terminalAuthFailurePromise) {
      return state.terminalAuthFailurePromise
    }

    state.terminalAuthFailurePromise = (async () => {
      options.reportSessionExpiredError(error)
      options.clearSessionQuery()
      await options.queryClient.cancelQueries({ predicate: isUsersMeQuery })
      const usersMeQueries = options.queryClient
        .getQueryCache()
        .findAll({ predicate: isUsersMeQuery })
      for (const usersMeQuery of usersMeQueries) {
        options.queryClient.setQueryData(usersMeQuery.queryKey, null)
      }
      options.queryClient.removeQueries({ predicate: isUsersMeQuery })
      options.clearSessionToken()
      resetUnauthorizedRecoveryState()
      await options.logout().catch(() => undefined)
    })().finally(() => {
      state.terminalAuthFailurePromise = null
    })

    return state.terminalAuthFailurePromise
  }

  const refreshUserSessionWithBackoff = async (
    backoffOptions: AuthRecoveryBackoffOptions = AUTH_RECOVERY_BACKOFF_DEFAULTS,
  ) => {
    let delayMs = backoffOptions.initialDelayMs

    for (let attempt = 1; attempt <= backoffOptions.maxAttempts; attempt += 1) {
      try {
        const refreshedSession = await options.refreshUserSession()
        if (refreshedSession == null) {
          throw new Error('Session refresh returned null.')
        }
        return refreshedSession
      } catch (error) {
        if (attempt >= backoffOptions.maxAttempts) {
          throw error
        }
        const nextDelay = nextBackoffDelay(delayMs, backoffOptions)
        await waitForRecoveryBackoff(nextDelay.delayMs)
        delayMs = nextDelay.nextDelayMs
      }
    }

    throw new Error('Session refresh exhausted all retries.')
  }

  const recoverSessionAfterUnauthorizedError = () => {
    if (state.authRecoveryPromise) {
      return state.authRecoveryPromise
    }
    if (isAuthRecoveryBlocked()) {
      return Promise.resolve(false)
    }

    state.authRecoveryPromise = (async () => {
      try {
        await refreshUserSessionWithBackoff()
        state.hasRecoveredUnauthorizedSession = true
        recordUnauthorizedRecoveryActivity(state)
        return true
      } catch (error) {
        await reportTerminalAuthFailure(error)
        return false
      } finally {
        state.authRecoveryPromise = null
      }
    })()

    return state.authRecoveryPromise
  }

  const recoverSessionAfterRepeatedUnauthorizedError = (
    error: unknown,
    backoffOptions: RepeatedUnauthorizedRecoveryBackoffOptions = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS,
  ) => {
    if (state.repeatedUnauthorizedRecoveryPromise) {
      return state.repeatedUnauthorizedRecoveryPromise
    }
    if (isAuthRecoveryBlocked()) {
      return Promise.resolve(false)
    }

    recordUnauthorizedRecoveryActivity(state, backoffOptions.resetWindowMs)

    if (hasReachedRepeatedUnauthorizedAttemptLimit(state, backoffOptions)) {
      state.pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
      reportRepeatedUnauthorizedErrorOnce(state, error, options.reportRepeatedUnauthorizedError)
      return Promise.resolve(false)
    }

    state.repeatedUnauthorizedRecoveryAttempts += 1
    const nextDelay = nextBackoffDelay(state.repeatedUnauthorizedDelayMs, backoffOptions)
    state.repeatedUnauthorizedDelayMs = nextDelay.nextDelayMs

    state.repeatedUnauthorizedRecoveryPromise = (async () => {
      await waitForRecoveryBackoff(nextDelay.delayMs)
      if (isAuthRecoveryBlocked()) {
        return false
      }
      const wasRecovered = await recoverSessionAfterUnauthorizedError()
      if (!wasRecovered) {
        return false
      }

      const queuedQueries = state.pendingRepeatedUnauthorizedQueries
      state.pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
      await Promise.allSettled(
        [...queuedQueries.entries()].map(([queryHash, queryKey]) => {
          if (state.replayedQueryHashes.has(queryHash)) {
            return Promise.resolve()
          }
          state.replayedQueryHashes.add(queryHash)
          return options.queryClient.refetchQueries({ queryKey, exact: true })
        }),
      )
      return true
    })().finally(() => {
      state.repeatedUnauthorizedRecoveryPromise = null
    })

    return state.repeatedUnauthorizedRecoveryPromise
  }

  const restoreHandlers = installUnauthorizedRecoveryHandlers({
    queryClient: options.queryClient,
    state,
    recordUnauthorizedRecoveryActivity: () => recordUnauthorizedRecoveryActivity(state),
    recoverSessionAfterUnauthorizedError,
    recoverSessionAfterRepeatedUnauthorizedError,
    reportTerminalAuthFailure,
    reportRepeatedUnauthorizedError: options.reportRepeatedUnauthorizedError,
  })

  onScopeDispose(() => {
    restoreHandlers()
    resetUnauthorizedRecoveryState()
  })

  return { isReconnectingSession, resetUnauthorizedRecoveryState }
}
