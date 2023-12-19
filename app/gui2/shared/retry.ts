import { wait } from 'lib0/promise'

export interface BackoffOptions<E> {
  maxRetries?: number
  retryDelay?: number
  retryDelayMultiplier?: number
  retryDelayMax?: number
  /** Called when the promise throws an error, and the next retry is about to be attempted.
   * When this function returns `false`, the backoff is immediately aborted. When this function
   * is not provided, the backoff will always continue until the maximum number of retries
   * is reached. * */
  onBeforeRetry?: (
    error: E,
    retryCount: number,
    maxRetries: number,
    delay: number,
  ) => boolean | void
  /** Called right before returning. */
  onSuccess?: (retryCount: number) => void
  /** Called after the final retry, right before throwing an error.
   * Note that `onBeforeRetry` is *not* called on the final retry, as there is nothing after the
   * final retry. */
  onFailure?: (error: E, retryCount: number) => void
}

const defaultBackoffOptions: Required<BackoffOptions<unknown>> = {
  maxRetries: 3,
  retryDelay: 1000,
  retryDelayMultiplier: 2,
  retryDelayMax: 10000,
  onBeforeRetry: () => {},
  onSuccess: () => {},
  onFailure: () => {},
}

/** Retry a failing promise function with exponential backoff. */
export async function exponentialBackoff<T, E>(
  f: () => Promise<T>,
  backoffOptions?: BackoffOptions<E>,
): Promise<T> {
  const {
    maxRetries,
    retryDelay,
    retryDelayMultiplier,
    retryDelayMax,
    onBeforeRetry,
    onSuccess,
    onFailure,
  } = {
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
      if (retries >= maxRetries) {
        onFailure(error as E, retries)
        throw error
      }
      if (onBeforeRetry(error as E, retries, maxRetries, delay) === false) throw error
      await wait(delay)
    }
  }
}

export function defaultOnBeforeRetry(
  description: string,
): NonNullable<BackoffOptions<any>['onBeforeRetry']> {
  return (error, retryCount, maxRetries, delay) => {
    console.error(
      'Could not ' +
        description +
        ` (${retryCount}/${maxRetries} retries), retrying after ${delay}ms...`,
    )
    console.error(error)
  }
}

export function defaultOnFailure(
  description: string,
): NonNullable<BackoffOptions<any>['onFailure']> {
  return (error, retryCount) => {
    console.error(
      'Could not ' + description + ` (${retryCount}/${retryCount} retries), throwing error.`,
    )
    console.error(error)
  }
}

export function defaultOnSuccess(
  description: string,
): NonNullable<BackoffOptions<any>['onSuccess']> {
  return (retryCount) => {
    if (retryCount === 0) return
    console.info(
      'Successfully ' +
        description +
        ` after ${retryCount} ${retryCount === 1 ? 'failure' : 'failures'}.`,
    )
  }
}

/** @param successDescription Should be in past tense, without an initial capital letter.
 * @param errorDescription Should be in present tense, without an initial capital letter. */
export function printingCallbacks(successDescription: string, errorDescription: string) {
  return {
    onBeforeRetry: defaultOnBeforeRetry(errorDescription),
    onSuccess: defaultOnSuccess(successDescription),
    onFailure: defaultOnFailure(errorDescription),
  } satisfies BackoffOptions<unknown>
}
