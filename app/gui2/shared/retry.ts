import { wait } from 'lib0/promise'
import { LsRpcError } from './languageServer'

export interface BackoffOptions<E> {
  maxRetries?: number
  retryDelay?: number
  retryDelayMultiplier?: number
  retryDelayMax?: number
  /**
   * Called when the promise return an error result, and the next retry is about to be attempted.
   * When this function returns `false`, the backoff is immediately aborted. When this function is
   * not provided, the backoff will always continue until the maximum number of retries is reached.
   */
  onBeforeRetry?: (error: E, retryCount: number, delay: number) => boolean | void
}

const defaultBackoffOptions: Required<BackoffOptions<unknown>> = {
  maxRetries: 3,
  retryDelay: 1000,
  retryDelayMultiplier: 2,
  retryDelayMax: 10000,
  onBeforeRetry: () => {},
}

/** Retry a failing promise function with exponential backoff. */
export async function exponentialBackoff<T, E>(
  f: () => Promise<T>,
  backoffOptions?: BackoffOptions<E>,
): Promise<T> {
  const options = { ...defaultBackoffOptions, ...backoffOptions }
  for (let retries = 0; ; retries += 1) {
    try {
      const result = await f()
      return result
    } catch (error) {
      if (retries >= options.maxRetries) throw error
      const delay = Math.min(
        options.retryDelayMax,
        options.retryDelay * options.retryDelayMultiplier ** retries,
      )
      if (options.onBeforeRetry(error as E, retries, delay) === false) throw error
      await wait(delay)
    }
  }
}

/**
 * Retry a failing Language Server RPC call with exponential backoff. The provided async function is
 * called on each retry.
 */
export async function rpcWithRetries<T>(
  f: () => Promise<T>,
  backoffOptions?: BackoffOptions<LsRpcError>,
): Promise<T> {
  return await exponentialBackoff(f, backoffOptions)
}
