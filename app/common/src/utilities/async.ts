/** @file Utilities related to `async`. */

/** A function to delay for a given number of milliseconds. */
export function delay(ms: number, abortSignal?: AbortSignal) {
  return new Promise((resolve, reject) => {
    const handle = setTimeout(resolve, ms)
    abortSignal?.addEventListener('abort', () => {
      clearTimeout(handle)
      reject(new Error('The `delay` timeout was aborted.'))
    })
  })
}
