/** @file A version of `fetch` which times out after the given duration. */

/** Returns an `AbortController` that aborts after the specified number of seconds. */
export function timeout(timeMs: number) {
  const controller = new AbortController()
  setTimeout(() => controller.abort(), timeMs)
  return controller
}

/** A version of `fetch` which times out after the given duration. */
export async function fetchTimeout(url: string, timeoutMs: number): Promise<unknown> {
  return fetch(url, { signal: timeout(timeoutMs).signal }).then((response) => {
    if (response.status === 200) {
      return response.json()
    } else {
      throw new Error(`Failed to fetch '${url}'. Response status: ${response.status}.`)
    }
  })
}
