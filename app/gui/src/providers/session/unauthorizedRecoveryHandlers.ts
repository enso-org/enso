import { isUnauthorizedError } from 'enso-common/src/services/Backend'
import type { QueryClient } from '../../utils/queryClient'
import { isUsersMeQueryKey } from '../auth'
import {
  queueRepeatedUnauthorizedQuery,
  reportRepeatedUnauthorizedErrorOnce,
  toUnauthorizedRecoveryError,
  type UnauthorizedRecoveryError,
  type UnauthorizedRecoveryState,
} from './unauthorizedRecoveryState'

interface UnauthorizedRecoveryHandlerOptions {
  readonly queryClient: QueryClient
  readonly state: UnauthorizedRecoveryState
  readonly recordUnauthorizedRecoveryActivity: () => void
  readonly recoverSessionAfterUnauthorizedError: () => Promise<boolean>
  readonly recoverSessionAfterRepeatedUnauthorizedError: (
    error: UnauthorizedRecoveryError,
  ) => Promise<boolean>
  readonly reportTerminalAuthFailure: (error: UnauthorizedRecoveryError) => Promise<void>
  readonly reportRepeatedUnauthorizedError: (error: UnauthorizedRecoveryError) => void
}

/**
 * Install query/mutation unauthorized handlers and return a cleanup function.
 */
export function installUnauthorizedRecoveryHandlers(options: UnauthorizedRecoveryHandlerOptions) {
  const queryCache = options.queryClient.getQueryCache()
  const mutationCache = options.queryClient.getMutationCache()
  const previousOnQueryError = queryCache.config.onError ?? (() => undefined)
  const previousOnMutationError = mutationCache.config.onError ?? (() => undefined)

  queryCache.config.onError = (error, query) => {
    previousOnQueryError(error, query)
    if (isUnauthorizedError(error)) {
      const authError = toUnauthorizedRecoveryError(error)
      options.recordUnauthorizedRecoveryActivity()

      if (options.state.hasRecoveredUnauthorizedSession && isUsersMeQueryKey(query.queryKey)) {
        void options.reportTerminalAuthFailure(authError)
        return
      }

      if (options.state.hasRecoveredUnauthorizedSession) {
        if (options.state.replayedQueryHashes.has(query.queryHash)) {
          reportRepeatedUnauthorizedErrorOnce(
            options.state,
            authError,
            options.reportRepeatedUnauthorizedError,
          )
          return
        }
        queueRepeatedUnauthorizedQuery(options.state, query)
        void options.recoverSessionAfterRepeatedUnauthorizedError(authError)
        return
      }

      const queryHash = query.queryHash
      void options.recoverSessionAfterUnauthorizedError().then((wasRecovered) => {
        if (!wasRecovered || options.state.replayedQueryHashes.has(queryHash)) {
          return
        }
        options.state.replayedQueryHashes.add(queryHash)
        return options.queryClient.refetchQueries({ queryKey: query.queryKey, exact: true })
      })
    }
  }

  mutationCache.config.onError = (error, variables, onMutateResult, mutation, context) => {
    previousOnMutationError(error, variables, onMutateResult, mutation, context)
    if (isUnauthorizedError(error)) {
      const authError = toUnauthorizedRecoveryError(error)
      if (options.state.replayedMutations.has(mutation)) {
        reportRepeatedUnauthorizedErrorOnce(
          options.state,
          authError,
          options.reportRepeatedUnauthorizedError,
        )
        return
      }

      const recoverPromise =
        options.state.hasRecoveredUnauthorizedSession ?
          options.recoverSessionAfterRepeatedUnauthorizedError(authError)
        : options.recoverSessionAfterUnauthorizedError()

      void recoverPromise.then((wasRecovered) => {
        if (!wasRecovered || options.state.replayedMutations.has(mutation)) {
          return
        }
        options.state.replayedMutations.add(mutation)
        return mutation.execute(variables)
      })
    }
  }

  return () => {
    queryCache.config.onError = previousOnQueryError
    mutationCache.config.onError = previousOnMutationError
  }
}
