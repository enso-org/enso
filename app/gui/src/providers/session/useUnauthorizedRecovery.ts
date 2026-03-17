import { isUsersMeQueryKey } from '$/providers/auth'
import type { QueryClient } from '$/utils/queryClient'
import type { Query } from '@tanstack/query-core'
import { wait } from 'lib0/promise'
import { computed, onScopeDispose, type Ref } from 'vue'
import {
  nextBackoffDelay,
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
  toUnauthorizedRecoveryError,
  UnauthorizedRecoveryState,
  type UnauthorizedRecoveryError,
} from './unauthorizedRecoveryState'

interface UseUnauthorizedRecoveryOptions {
  readonly queryClient: QueryClient
  readonly isLoggingOut: Ref<boolean>
  readonly refreshUserSession: () => Promise<unknown>
  readonly logout: () => Promise<unknown>
  readonly clearSessionToken: () => void
  readonly cancelSessionQuery: () => Promise<void>
  readonly clearSessionQuery: () => void
  readonly reportSessionExpiredError: (error: UnauthorizedRecoveryError) => void
  readonly reportRepeatedUnauthorizedError: (error: UnauthorizedRecoveryError) => void
}

/**
 * Install unauthorized handlers and coordinate session recovery/replay flow.
 *
 * Coordinates recovery from unauthorized query and mutation failures.
 * It single-flights session refresh, replays failed queries and mutations after recovery,
 * applies bounded exponential backoff for repeated unauthorized errors after recovery,
 * and escalates to terminal auth cleanup and logout when the session can no longer be recovered.
 */
export function useUnauthorizedRecovery(options: UseUnauthorizedRecoveryOptions) {
  const state = new UnauthorizedRecoveryState()
  const isReconnectingSession = computed(() => state.reconnectingSessionBackoffWaitCount.value > 0)

  const isAuthRecoveryBlocked = () =>
    state.terminalAuthFailurePromise != null || options.isLoggingOut.value

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

  const reportTerminalAuthFailure = (error: UnauthorizedRecoveryError) => {
    if (state.terminalAuthFailurePromise) {
      return state.terminalAuthFailurePromise
    }

    const isUsersMeQuery = (query: Query) => isUsersMeQueryKey(query.queryKey)
    state.terminalAuthFailurePromise = (async () => {
      options.reportSessionExpiredError(error)

      await Promise.all([
        options.cancelSessionQuery(),
        options.queryClient.cancelQueries({ predicate: isUsersMeQuery }),
      ])

      options.clearSessionQuery()
      const usersMeQueries = options.queryClient
        .getQueryCache()
        .findAll({ predicate: isUsersMeQuery })
      for (const usersMeQuery of usersMeQueries) {
        options.queryClient.setQueryData(usersMeQuery.queryKey, null)
      }
      options.queryClient.removeQueries({ predicate: isUsersMeQuery })
      options.clearSessionToken()
      state.reset()
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
        state.recordUnauthorizedRecoveryError()
        return true
      } catch (error) {
        await reportTerminalAuthFailure(toUnauthorizedRecoveryError(error))
        return false
      } finally {
        state.authRecoveryPromise = null
      }
    })()

    return state.authRecoveryPromise
  }

  const recoverSessionAfterRepeatedUnauthorizedError = (
    error: UnauthorizedRecoveryError,
    backoffOptions: RepeatedUnauthorizedRecoveryBackoffOptions = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS,
  ) => {
    if (state.repeatedUnauthorizedRecoveryPromise) {
      return state.repeatedUnauthorizedRecoveryPromise
    }
    if (isAuthRecoveryBlocked()) {
      return Promise.resolve(false)
    }

    state.recordUnauthorizedRecoveryError(backoffOptions.resetWindowMs)

    if (state.hasReachedRepeatedUnauthorizedAttemptLimit(backoffOptions)) {
      state.pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
      state.reportRepeatedUnauthorizedErrorOnce(error, options.reportRepeatedUnauthorizedError)
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
    recordUnauthorizedRecoveryError: () => state.recordUnauthorizedRecoveryError(),
    recoverSessionAfterUnauthorizedError,
    recoverSessionAfterRepeatedUnauthorizedError,
    reportTerminalAuthFailure,
    reportRepeatedUnauthorizedError: options.reportRepeatedUnauthorizedError,
  })

  onScopeDispose(() => {
    restoreHandlers()
    state.reset()
  })

  return { isReconnectingSession, resetUnauthorizedRecoveryState: () => state.reset() }
}
