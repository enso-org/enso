/** @returns An API supporting registering callback functions and dispatching them. */
export function useCallbackRegistry<Args extends unknown[]>() {
  const callbacks: ((...args: Args) => void)[] = []
  return {
    run: (...args: Args) => callbacks.forEach((callback) => callback(...args)),
    register: (callback: (...args: Args) => void) => void callbacks.push(callback),
  }
}
