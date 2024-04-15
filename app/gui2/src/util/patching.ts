/**
 * Replace a function under given key on provided object and provide a hook that is called right
 * before the original function is executed.
 */
export function hookBeforeFunctionCall(object: any, key: PropertyKey, hook: () => void) {
  const original = object[key] as unknown
  if (typeof original === 'function') {
    object[key] = function (this: unknown, ...args: unknown[]) {
      hook()
      return original.apply(this, args)
    }
  }
}
