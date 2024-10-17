import type { ObservableV2 } from 'lib0/observable'
import { wait } from 'lib0/promise'
import type { Result, ResultError } from './data/result'
import { type MockTransportData, MockWebSocketTransport } from './net/MockWSTransport'
import {
  ReconnectingWebSocket,
  ReconnectingWebSocketTransport,
} from './net/ReconnectingWSTransport'

export {
  MockWebSocketTransport,
  ReconnectingWebSocket,
  ReconnectingWebSocketTransport,
  type MockTransportData,
}
interface Disposable {
  dispose(): void
}

/** A scope which controls */
export class AbortScope {
  private ctrl: AbortController = new AbortController()
  /** Get the {@link AbortSignal} for this {@link AbortScope}. */
  get signal() {
    return this.ctrl.signal
  }

  /** Trigger an abort for all listeners of this {@link AbortScope}. */
  dispose(reason?: string) {
    this.ctrl.abort(reason)
  }

  /** Trigger disposal of the given {@link Disposable} when this {@link AbortScope} is aborted. */
  handleDispose(disposable: Disposable) {
    this.signal.throwIfAborted()
    this.onAbort(disposable.dispose.bind(disposable))
  }

  /**
   * Create a new abort scope with lifetime limited by this scope. It can be aborted earlier on its
   * own, but it is guaranteed to be aborted whenever the parent scope aborts.
   */
  child(): AbortScope {
    const child = new AbortScope()
    this.handleDispose(child)
    return child
  }

  /** Call the given callback when this {@link AbortScope} is aborted. */
  onAbort(listener: () => void) {
    if (this.signal.aborted) {
      queueMicrotask(listener)
    } else {
      this.signal.addEventListener('abort', listener, { once: true })
    }
  }

  /**
   * Add the given event listener on the given event on the given observable,
   * removing the event listener when this {@link AbortScope} is aborted.
   */
  handleObserve<
    EVENTS extends { [key in keyof EVENTS]: (...arg0: any[]) => void },
    NAME extends keyof EVENTS & string,
  >(observable: ObservableV2<EVENTS>, name: NAME, f: EVENTS[NAME]) {
    if (this.signal.aborted) return
    observable.on(name, f)
    this.onAbort(() => observable.off(name, f))
    return f
  }
}

export interface BackoffOptions<E> {
  maxRetries?: number
  retryDelay?: number
  retryDelayMultiplier?: number
  retryDelayMax?: number
  /**
   * Called when the promise throws an error, and the next retry is about to be attempted.
   * When this function returns `false`, the backoff is immediately aborted. When this function
   * is not provided, the backoff will always continue until the maximum number of retries
   * is reached. *
   */
  onBeforeRetry?: (
    error: ResultError<E>,
    retryCount: number,
    maxRetries: number,
    delay: number,
  ) => boolean | void
  /** Called right before returning. */
  onSuccess?: (retryCount: number) => void
  /**
   * Called after the final retry, right before throwing an error.
   * Note that `onBeforeRetry` is *not* called on the final retry, as there is nothing after the
   * final retry.
   */
  onFailure?: (error: ResultError<E>, retryCount: number) => void
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
  f: () => Promise<Result<T, E>>,
  backoffOptions?: BackoffOptions<E>,
): Promise<Result<T, E>> {
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
    const result = await f()
    if (result.ok) {
      onSuccess(retries)
      return result
    }
    if (retries >= maxRetries) {
      onFailure(result.error, retries)
      return result
    }
    if (onBeforeRetry(result.error, retries, maxRetries, delay) === false) {
      return result
    }
    await wait(delay)
  }
}

/** An `onBeforeRetry` handler used in {@link printingCallbacks} that logs an error. */
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

/** An `onFailure` handler used in {@link printingCallbacks} that logs an error. */
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

/** An `onSuccess` handler used in {@link printingCallbacks} that logs a message. */
export function defaultOnSuccess(
  description: string,
): NonNullable<BackoffOptions<any>['onSuccess']> {
  return retryCount => {
    if (retryCount === 0) return
    console.info(
      'Successfully ' +
        description +
        ` after ${retryCount} ${retryCount === 1 ? 'failure' : 'failures'}.`,
    )
  }
}

/**
 * @param successDescription Should be in past tense, without an initial capital letter.
 * @param errorDescription Should be in present tense, without an initial capital letter.
 */
export function printingCallbacks(successDescription: string, errorDescription: string) {
  return {
    onBeforeRetry: defaultOnBeforeRetry(errorDescription),
    onSuccess: defaultOnSuccess(successDescription),
    onFailure: defaultOnFailure(errorDescription),
  } satisfies BackoffOptions<unknown>
}
