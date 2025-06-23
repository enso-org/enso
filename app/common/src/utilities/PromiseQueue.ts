/** @file A promise queue. */

/**
 * A promise queue.
 *
 * Based on C++'s `condition_variable`:
 * https://en.cppreference.com/w/cpp/thread/condition_variable.html
 */
export class PromiseQueue {
  private resolveQueue: (() => void)[] = []

  /** Add a new promise to the queue. */
  newPromise(): Promise<void> {
    return new Promise((resolve) => this.resolveQueue.push(resolve))
  }

  /** Resolve all promises in the queue. */
  resolveAll(): Promise<boolean> {
    const success = this.resolveQueue.length !== 0
    for (const resolve of this.resolveQueue.splice(0, this.resolveQueue.length)) {
      resolve()
    }
    // Give the code after the resolved promises time to execute.
    return Promise.resolve(success)
  }

  /** Resolve a single promise in the queue. */
  resolveOne(): Promise<boolean> {
    const resolve = this.resolveQueue.shift()
    resolve?.()
    // Give the code after the resolved promise time to execute.
    return Promise.resolve(resolve != null)
  }
}
