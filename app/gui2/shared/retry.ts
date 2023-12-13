import { wait } from 'lib0/promise'

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
  onBeforeRetry?: (
    error: E,
    retryCount: number,
    maxRetries: number,
    delay: number,
  ) => boolean | void
  /** Called right before returning. */
  onSuccess?: (retryCount: number) => void
}

const defaultBackoffOptions: Required<BackoffOptions<unknown>> = {
  maxRetries: 3,
  retryDelay: 1000,
  retryDelayMultiplier: 2,
  retryDelayMax: 10000,
  onBeforeRetry: () => {},
  onSuccess: () => {},
}

/** Retry a failing promise function with exponential backoff. */
export async function exponentialBackoff<T, E>(
  f: () => Promise<T>,
  backoffOptions?: BackoffOptions<E>,
): Promise<T> {
  const { maxRetries, retryDelay, retryDelayMultiplier, retryDelayMax, onBeforeRetry, onSuccess } =
    {
      ...defaultBackoffOptions,
      ...backoffOptions,
    }
  for (
    let retries = 0, delay = retryDelay;
    ;
    retries += 1, delay = Math.min(retryDelayMax, delay * retryDelayMultiplier)
  ) {
    try {
      const result = await f()
      onSuccess(retries)
      return result
    } catch (error) {
      if (retries >= maxRetries) throw error
      if (onBeforeRetry(error as E, retries, maxRetries, delay) === false) throw error
      await wait(delay)
    }
  }
}

export const onBeforeRetry: (message: string) => BackoffOptions<any>['onBeforeRetry'] =
  (message) => (error, retryCount, maxRetries, delay) => {
    console.error(message + ` (${retryCount}/${maxRetries} retries), retrying after ${delay}ms...`)
    console.error(error)
  }
