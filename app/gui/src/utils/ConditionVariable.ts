/** @file A promise queue. */

/**
 * A condition variable.
 *
 * See https://en.cppreference.com/w/cpp/thread/condition_variable.html
 *
 * See https://en.wikipedia.org/wiki/Monitor_(synchronization)#Condition_variables_2
 */
export class ConditionVariable {
  private resolveQueue: (() => void)[] = []

  /** Add a new promise to the queue. */
  wait(): Promise<void> {
    return new Promise((resolve) => this.resolveQueue.push(resolve))
  }

  /** Resolve all promises in the queue. */
  notifyAll(): boolean {
    const success = this.resolveQueue.length !== 0
    for (const resolve of this.resolveQueue.splice(0, this.resolveQueue.length)) {
      resolve()
    }
    // Give the code after the resolved promises time to execute.
    return success
  }

  /** Resolve a single promise in the queue. */
  notifyOne(): boolean {
    const resolve = this.resolveQueue.shift()
    resolve?.()
    // Give the code after the resolved promise time to execute.
    return resolve != null
  }
}
